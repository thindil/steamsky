--    Copyright 2016-2017 Bartek thindil Jasicki
--
--    This file is part of Steam Sky.
--
--    Steam Sky is free software: you can redistribute it and/or modify
--    it under the terms of the GNU General Public License as published by
--    the Free Software Foundation, either version 3 of the License, or
--    (at your option) any later version.
--
--    Steam Sky is distributed in the hope that it will be useful,
--    but WITHOUT ANY WARRANTY; without even the implied warranty of
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--    GNU General Public License for more details.
--
--    You should have received a copy of the GNU General Public License
--    along with Steam Sky.  If not, see <http://www.gnu.org/licenses/>.

with Crew; use Crew;
with UserInterface; use UserInterface;
with Messages; use Messages;
with ShipModules; use ShipModules;
with Items; use Items;
with Statistics; use Statistics;
with Events; use Events;
with Maps; use Maps;
with Bases; use Bases;
with Missions; use Missions;
with Ships.Cargo; use Ships.Cargo;
with Ships.Crew; use Ships.Crew;
with Ships.Movement; use Ships.Movement;
with Utils; use Utils;

package body Combat is

   function StartCombat
     (EnemyIndex: Positive;
      NewCombat: Boolean := True) return GameStates is
      EnemyShip: ShipRecord;
      PlayerPerception, EnemyPerception: Natural := 0;
      function CountPerception(Spotter, Spotted: ShipRecord) return Natural is
         Result: Natural := 0;
      begin
         for I in Spotter.Crew.Iterate loop
            case Spotter.Crew(I).Order is
               when Pilot =>
                  Result :=
                    Result +
                    GetSkillLevel(Crew_Container.To_Index(I), 5, Spotter);
                  if Spotter = PlayerShip then
                     GainExp(1, 5, Crew_Container.To_Index(I));
                  end if;
               when Gunner =>
                  Result :=
                    Result +
                    GetSkillLevel(Crew_Container.To_Index(I), 5, Spotter);
                  if Spotter = PlayerShip then
                     GainExp(1, 5, Crew_Container.To_Index(I));
                  end if;
               when others =>
                  null;
            end case;
         end loop;
         for Module of Spotted.Modules loop
            if Modules_List(Module.ProtoIndex).MType = HULL then
               Result := Result + Module.Max_Value;
               exit;
            end if;
         end loop;
         return Result;
      end CountPerception;
   begin
      EnemyShip :=
        CreateShip
          (EnemyIndex,
           Null_Unbounded_String,
           PlayerShip.SkyX,
           PlayerShip.SkyY,
           FULL_SPEED);
      Enemy :=
        (Ship => EnemyShip,
         Accuracy => 0,
         Distance => 10000,
         CombatAI => ProtoShips_List(EnemyIndex).CombatAI,
         Evasion => 0,
         Loot => 0,
         Perception => 0);
      if ProtoShips_List(EnemyIndex).Accuracy(2) = 0 then
         Enemy.Accuracy := ProtoShips_List(EnemyIndex).Accuracy(1);
      else
         Enemy.Accuracy :=
           GetRandom
             (ProtoShips_List(EnemyIndex).Accuracy(1),
              ProtoShips_List(EnemyIndex).Accuracy(2));
      end if;
      if ProtoShips_List(EnemyIndex).Evasion(2) = 0 then
         Enemy.Evasion := ProtoShips_List(EnemyIndex).Evasion(1);
      else
         Enemy.Evasion :=
           GetRandom
             (ProtoShips_List(EnemyIndex).Evasion(1),
              ProtoShips_List(EnemyIndex).Evasion(2));
      end if;
      if ProtoShips_List(EnemyIndex).Perception(2) = 0 then
         Enemy.Perception := ProtoShips_List(EnemyIndex).Perception(1);
      else
         Enemy.Perception :=
           GetRandom
             (ProtoShips_List(EnemyIndex).Perception(1),
              ProtoShips_List(EnemyIndex).Perception(2));
      end if;
      if ProtoShips_List(EnemyIndex).Loot(2) = 0 then
         Enemy.Loot := ProtoShips_List(EnemyIndex).Loot(1);
      else
         Enemy.Loot :=
           GetRandom
             (ProtoShips_List(EnemyIndex).Loot(1),
              ProtoShips_List(EnemyIndex).Loot(2));
      end if;
      PilotOrder := 2;
      EngineerOrder := 3;
      EndCombat := False;
      if ProtoShips_List(EnemyIndex).Owner /= Drones then
         EnemyName := GenerateShipName;
      else
         EnemyName := GenerateShipName(Drones);
      end if;
      MessagesStarts := GetLastMessageIndex + 1;
      Guns.Clear;
      for I in PlayerShip.Modules.Iterate loop
         if Modules_List(PlayerShip.Modules(I).ProtoIndex).MType = GUN and
           PlayerShip.Modules(I).Durability > 0 then
            Guns.Append(New_Item => (Modules_Container.To_Index(I), 1));
         end if;
      end loop;
      if NewCombat then
         PlayerPerception := CountPerception(PlayerShip, Enemy.Ship);
         if Enemy.Perception > 0 then
            EnemyPerception := Enemy.Perception;
         else
            EnemyPerception := CountPerception(Enemy.Ship, PlayerShip);
         end if;
         if (PlayerPerception + GetRandom(1, 100)) >
           (EnemyPerception + GetRandom(1, 100)) then
            AddMessage
              ("You spotted " & To_String(Enemy.Ship.Name) & ".",
               OtherMessage);
         else
            if RealSpeed(PlayerShip) < RealSpeed(Enemy.Ship) then
               ShowDialog
                 ("You was attacked by " & To_String(Enemy.Ship.Name) & ".");
               OldSpeed := PlayerShip.Speed;
               return Combat_State;
            end if;
            AddMessage
              ("You spotted " & To_String(Enemy.Ship.Name) & ".",
               OtherMessage);
         end if;
         return Sky_Map_View;
      end if;
      return Combat_State;
   end StartCombat;

   procedure CombatTurn is
      AccuracyBonus, EvadeBonus: Integer := 0;
      PilotIndex,
      EngineerIndex,
      EnemyWeaponIndex,
      EnemyAmmoIndex,
      EnemyPilotIndex: Natural :=
        0;
      DistanceTraveled, SpeedBonus: Integer;
      ShootMessage: Unbounded_String;
      EnemyPilotOrder: Positive := 2;
      HaveFuel: Boolean := False;
      DamageRange: Positive;
      procedure Attack(Ship, EnemyShip: in out ShipRecord) is
         GunnerIndex, Shoots, AmmoIndex, ArmorIndex, WeaponIndex: Natural;
         GunnerOrder: Positive;
         HitChance, HitLocation, LootAmount: Integer;
         FreeSpace: Integer := 0;
         type DamageFactor is digits 2 range 0.0 .. 1.0;
         Damage: DamageFactor := 0.0;
         WeaponDamage: Natural;
         DeathReason: Unbounded_String;
      begin
         Attack_Loop:
         for K in Ship.Modules.Iterate loop
            if Ship.Modules(K).Durability > 0 and
              (Modules_List(Ship.Modules(K).ProtoIndex).MType = GUN or
               Modules_List(Ship.Modules(K).ProtoIndex).MType =
                 BATTERING_RAM) then
               GunnerIndex := 0;
               AmmoIndex := 0;
               if Modules_List(Ship.Modules(K).ProtoIndex).MType = GUN then
                  GunnerIndex := Ship.Modules(K).Owner;
                  if Ship = PlayerShip then
                     if Ship.Modules(K).Owner = 0 then
                        Shoots := 0;
                     else
                        for Gun of Guns loop
                           if Gun(1) = Modules_Container.To_Index(K) then
                              GunnerOrder := Gun(2);
                              exit;
                           end if;
                        end loop;
                        case GunnerOrder is
                           when 2 =>
                              AccuracyBonus := AccuracyBonus + 20;
                              Shoots := 2;
                           when 3 =>
                              Shoots := 4;
                           when 4 =>
                              AccuracyBonus := AccuracyBonus - 10;
                              Shoots := 2;
                           when 5 =>
                              AccuracyBonus := AccuracyBonus - 20;
                              Shoots := 2;
                           when 6 =>
                              Shoots := 2;
                           when others =>
                              Shoots := 0;
                        end case;
                     end if;
                  else
                     Shoots := 2;
                  end if;
                  if Ship.Modules(K).Current_Value >=
                    Ship.Cargo.First_Index and
                    Ship.Modules(K).Current_Value <= Ship.Cargo.Last_Index then
                     if Items_List
                         (Ship.Cargo(Ship.Modules(K).Current_Value).ProtoIndex)
                         .IType =
                       Items_Types
                         (Modules_List(Ship.Modules(K).ProtoIndex).Value) then
                        AmmoIndex := Ship.Modules(K).Current_Value;
                     end if;
                  end if;
                  if AmmoIndex = 0 then
                     for I in Items_List.Iterate loop
                        if Items_List(I).IType =
                          Items_Types
                            (Modules_List(Ship.Modules(K).ProtoIndex)
                               .Value) then
                           for J in Ship.Cargo.Iterate loop
                              if Ship.Cargo(J).ProtoIndex =
                                Objects_Container.To_Index(I) then
                                 AmmoIndex := Cargo_Container.To_Index(J);
                                 UpdateModule
                                   (Ship,
                                    Modules_Container.To_Index(K),
                                    "Current_Value",
                                    Positive'Image(AmmoIndex));
                                 exit;
                              end if;
                           end loop;
                           exit;
                        end if;
                     end loop;
                  end if;
                  if AmmoIndex = 0 then
                     if Ship = PlayerShip then
                        AddMessage
                          ("You don't have ammo to " &
                           To_String(Ship.Modules(K).Name) &
                           "!",
                           CombatMessage);
                     end if;
                     Shoots := 0;
                  elsif Ship.Cargo(AmmoIndex).Amount < Shoots then
                     Shoots := Ship.Cargo(AmmoIndex).Amount;
                  end if;
                  if Enemy.Distance > 5000 then
                     Shoots := 0;
                  end if;
               else
                  if Enemy.Distance > 100 then
                     Shoots := 0;
                  else
                     Shoots := 1;
                  end if;
               end if;
               if Shoots > 0 then
                  if Ship = PlayerShip then
                     HitChance := AccuracyBonus - Enemy.Evasion;
                  else
                     HitChance := Enemy.Accuracy - EvadeBonus;
                  end if;
                  if GunnerIndex > 0 then
                     HitChance :=
                       HitChance + GetSkillLevel(GunnerIndex, 3, Ship);
                  end if;
                  for I in 1 .. Shoots loop
                     if Modules_List(Ship.Modules(K).ProtoIndex).MType =
                       GUN then
                        if Ship = PlayerShip then
                           ShootMessage :=
                             Ship.Crew(GunnerIndex).Name &
                             To_Unbounded_String(" shoots to ") &
                             EnemyName;
                        else
                           ShootMessage :=
                             EnemyName & To_Unbounded_String(" attacks you");
                        end if;
                     else
                        if Ship = PlayerShip then
                           ShootMessage :=
                             To_Unbounded_String("You ram ") & EnemyName;
                        else
                           ShootMessage :=
                             EnemyName & To_Unbounded_String(" attacks you");
                        end if;
                     end if;
                     if GetRandom(1, 100) + HitChance > GetRandom(1, 100) then
                        ShootMessage :=
                          ShootMessage & To_Unbounded_String(" and hit in ");
                        ArmorIndex := 0;
                        for J in
                          EnemyShip.Modules.First_Index ..
                              EnemyShip.Modules.Last_Index loop
                           if EnemyShip.Modules(J).Durability > 0 and
                             Modules_List(EnemyShip.Modules(J).ProtoIndex)
                                 .MType =
                               ARMOR then
                              ArmorIndex := J;
                              exit;
                           end if;
                        end loop;
                        if ArmorIndex > 0 then
                           HitLocation := ArmorIndex;
                        else
                           if Ship = PlayerShip then
                              if GunnerIndex > 0 and
                                GunnerOrder > 3 and
                                GunnerOrder <
                                  7 then -- aim for part of enemy ship
                                 HitLocation := 1;
                                 for J in EnemyShip.Modules.Iterate loop
                                    if
                                      ((GunnerOrder = 4 and
                                        Modules_List
                                            (EnemyShip.Modules(J).ProtoIndex)
                                            .MType =
                                          ENGINE) or
                                       (GunnerOrder = 5 and
                                        ((Modules_List
                                            (EnemyShip.Modules(J).ProtoIndex)
                                            .MType =
                                          TURRET and
                                          EnemyShip.Modules(J).Current_Value >
                                            0) or
                                         Modules_List
                                             (EnemyShip.Modules(J).ProtoIndex)
                                             .MType =
                                           BATTERING_RAM)) or
                                       (GunnerOrder = 6 and
                                        Modules_List
                                            (EnemyShip.Modules(J).ProtoIndex)
                                            .MType =
                                          HULL)) and
                                      EnemyShip.Modules(J).Durability > 0 then
                                       HitLocation :=
                                         Modules_Container.To_Index(J);
                                       exit;
                                    end if;
                                 end loop;
                              else
                                 HitLocation :=
                                   GetRandom
                                     (Enemy.Ship.Modules.First_Index,
                                      Enemy.Ship.Modules.Last_Index);
                              end if;
                           else
                              if Enemy.CombatAI = DISARMER then
                                 HitLocation := 1;
                                 for J in EnemyShip.Modules.Iterate loop
                                    if
                                      ((Modules_List
                                          (EnemyShip.Modules(J).ProtoIndex)
                                          .MType =
                                        TURRET and
                                        EnemyShip.Modules(J).Current_Value >
                                          0) or
                                       Modules_List
                                           (EnemyShip.Modules(J).ProtoIndex)
                                           .MType =
                                         BATTERING_RAM) and
                                      EnemyShip.Modules(J).Durability > 0 then
                                       HitLocation :=
                                         Modules_Container.To_Index(J);
                                       exit;
                                    end if;
                                 end loop;
                              else
                                 HitLocation :=
                                   GetRandom
                                     (PlayerShip.Modules.First_Index,
                                      PlayerShip.Modules.Last_Index);
                              end if;
                           end if;
                           while EnemyShip.Modules(HitLocation).Durability =
                             0 loop
                              HitLocation := HitLocation - 1;
                           end loop;
                        end if;
                        ShootMessage :=
                          ShootMessage &
                          EnemyShip.Modules(HitLocation).Name &
                          To_Unbounded_String(".");
                        Damage :=
                          1.0 -
                          DamageFactor
                            (Float(Ship.Modules(K).Durability) /
                             Float(Ship.Modules(K).MaxDurability));
                        WeaponDamage :=
                          Ship.Modules(K).Max_Value -
                          Natural
                            (Float(Ship.Modules(K).Max_Value) * Float(Damage));
                        if WeaponDamage = 0 then
                           WeaponDamage := 1;
                        end if;
                        if AmmoIndex > 0 then
                           WeaponDamage :=
                             WeaponDamage +
                             Items_List(Ship.Cargo(AmmoIndex).ProtoIndex)
                               .Value;
                        end if;
                        UpdateModule
                          (EnemyShip,
                           HitLocation,
                           "Durability",
                           Integer'Image(0 - WeaponDamage));
                        if EnemyShip.Modules(HitLocation).Durability = 0 then
                           DeathReason := To_Unbounded_String("enemy fire");
                           case Modules_List
                             (EnemyShip.Modules(HitLocation).ProtoIndex)
                             .MType is
                              when HULL | ENGINE =>
                                 EndCombat := True;
                                 if Ship /= PlayerShip then
                                    DeathReason :=
                                      To_Unbounded_String("ship explosion");
                                    Death(1, DeathReason, PlayerShip);
                                 end if;
                              when TURRET =>
                                 WeaponIndex :=
                                   EnemyShip.Modules(HitLocation)
                                     .Current_Value;
                                 if WeaponIndex > 0 then
                                    UpdateModule
                                      (EnemyShip,
                                       WeaponIndex,
                                       "Durability",
                                       "-1000");
                                    if EnemyShip.Modules(WeaponIndex).Owner >
                                      0 then
                                       Death
                                         (EnemyShip.Modules(WeaponIndex).Owner,
                                          DeathReason,
                                          EnemyShip);
                                       for J in
                                         Guns.First_Index ..
                                             Guns.Last_Index loop
                                          if Guns(J)(1) = WeaponIndex then
                                             Guns.Delete
                                             (Index => J, Count => 1);
                                             exit;
                                          end if;
                                       end loop;
                                    end if;
                                 end if;
                              when GUN =>
                                 if EnemyShip.Modules(HitLocation).Owner >
                                   0 then
                                    Death
                                      (EnemyShip.Modules(HitLocation).Owner,
                                       DeathReason,
                                       EnemyShip);
                                 end if;
                                 if Ship /= PlayerShip then
                                    for J in
                                      Guns.First_Index .. Guns.Last_Index loop
                                       if Guns(J)(1) = HitLocation then
                                          Guns.Delete(Index => J, Count => 1);
                                          exit;
                                       end if;
                                    end loop;
                                 end if;
                              when CABIN =>
                                 if EnemyShip.Modules(HitLocation).Owner >
                                   0 then
                                    if EnemyShip.Crew
                                        (EnemyShip.Modules(HitLocation).Owner)
                                        .Order =
                                      Rest then
                                       Death
                                         (EnemyShip.Modules(HitLocation).Owner,
                                          DeathReason,
                                          EnemyShip);
                                    end if;
                                 end if;
                              when others =>
                                 if EnemyShip.Modules(HitLocation).Owner >
                                   0 then
                                    Death
                                      (EnemyShip.Modules(HitLocation).Owner,
                                       DeathReason,
                                       EnemyShip);
                                 end if;
                           end case;
                        end if;
                     else
                        ShootMessage :=
                          ShootMessage & To_Unbounded_String(" and miss.");
                     end if;
                     AddMessage(To_String(ShootMessage), CombatMessage);
                     if AmmoIndex > 0 then
                        UpdateCargo
                          (Ship,
                           Ship.Cargo.Element(AmmoIndex).ProtoIndex,
                           -1);
                     end if;
                     if Ship = PlayerShip then
                        GainExp(1, 3, GunnerIndex);
                     end if;
                     if PlayerShip.Crew(1).Health = 0 then -- player is dead
                        EndCombat := True;
                     end if;
                     if EndCombat then
                        if Ship = PlayerShip then
                           UpdateModule
                             (EnemyShip,
                              1,
                              "Durability",
                              Integer'
                                Image
                                  (0 - EnemyShip.Modules(1).MaxDurability));
                           AddMessage
                             (To_String(EnemyName) & " is destroyed!",
                              CombatMessage);
                           LootAmount := Enemy.Loot;
                           FreeSpace := FreeCargo((0 - LootAmount));
                           if FreeSpace < 0 then
                              LootAmount := LootAmount + FreeSpace;
                           end if;
                           if LootAmount > 0 then
                              AddMessage
                                ("You looted" &
                                 Integer'Image(LootAmount) &
                                 " Charcollum from " &
                                 To_String(EnemyName) &
                                 ".",
                                 CombatMessage);
                              UpdateCargo(Ship, 1, LootAmount);
                           end if;
                           EnemyShip.Speed := FULL_STOP;
                           if SkyMap(Ship.SkyX, Ship.SkyY).EventIndex > 0 then
                              if Events_List
                                  (SkyMap(Ship.SkyX, Ship.SkyY).EventIndex)
                                  .EType =
                                AttackOnBase then
                                 GainRep
                                   (SkyMap(Ship.SkyX, Ship.SkyY).BaseIndex,
                                    5);
                              end if;
                              DeleteEvent
                                (SkyMap(Ship.SkyX, Ship.SkyY).EventIndex);
                           end if;
                           if SkyMap(Ship.SkyX, Ship.SkyY).MissionIndex >
                             0 then
                              if Ship.Missions
                                  (SkyMap(Ship.SkyX, Ship.SkyY).MissionIndex)
                                  .MType =
                                Kill then
                                 if ProtoShips_List
                                     (Ship.Missions
                                        (SkyMap(Ship.SkyX, Ship.SkyY)
                                           .MissionIndex)
                                        .Target)
                                     .Name =
                                   EnemyShip.Name then
                                    UpdateMission
                                      (SkyMap(Ship.SkyX, Ship.SkyY)
                                         .MissionIndex);
                                 end if;
                              end if;
                           end if;
                           UpdateDestroyedShips(EnemyShip.Name);
                        else
                           DrawGame(Combat_State);
                           return;
                        end if;
                        exit Attack_Loop;
                     end if;
                  end loop;
               end if;
            end if;
         end loop Attack_Loop;
      end Attack;
   begin
      for I in PlayerShip.Crew.Iterate loop
         case PlayerShip.Crew(I).Order is
            when Pilot =>
               PilotIndex := Crew_Container.To_Index(I);
               GainExp(1, 1, PilotIndex);
            when Engineer =>
               EngineerIndex := Crew_Container.To_Index(I);
               GainExp(1, 2, EngineerIndex);
            when others =>
               null;
         end case;
      end loop;
      EnemyPilotIndex := FindMember(Pilot, Enemy.Ship);
      for Item of PlayerShip.Cargo loop
         if Items_List(Item.ProtoIndex).IType =
           To_Unbounded_String("Fuel") then
            HaveFuel := True;
            exit;
         end if;
      end loop;
      if not HaveFuel then
         PilotOrder := 1;
         EngineerOrder := 1;
         if EngineerIndex = 0 and PlayerShip.Speed /= FULL_STOP then
            PlayerShip.Speed := FULL_STOP;
         end if;
      end if;
      if PilotIndex > 0 then
         case PilotOrder is
            when 1 =>
               AccuracyBonus := 20;
               EvadeBonus := -10;
            when 2 =>
               AccuracyBonus := 10;
               EvadeBonus := 0;
            when 3 =>
               AccuracyBonus := 0;
               EvadeBonus := 10;
            when 4 =>
               AccuracyBonus := -10;
               EvadeBonus := 20;
            when others =>
               null;
         end case;
         EvadeBonus := EvadeBonus + GetSkillLevel(PilotIndex, 1);
      else
         AccuracyBonus := 20;
         EvadeBonus := -10;
      end if;
      if EnemyPilotIndex > 0 then
         AccuracyBonus :=
           AccuracyBonus - GetSkillLevel(EnemyPilotIndex, 1, Enemy.Ship);
         Enemy.Evasion :=
           Enemy.Evasion + GetSkillLevel(EnemyPilotIndex, 1, Enemy.Ship);
      end if;
      if EngineerIndex > 0 and HaveFuel then
         ChangeShipSpeed(ShipSpeed'Val(EngineerOrder), False);
      end if;
      SpeedBonus := 20 - (RealSpeed(PlayerShip) / 100);
      if SpeedBonus < -10 then
         SpeedBonus := -10;
      end if;
      AccuracyBonus := AccuracyBonus + SpeedBonus;
      EvadeBonus := EvadeBonus - SpeedBonus;
      for I in Enemy.Ship.Modules.Iterate loop
         if Enemy.Ship.Modules(I).Durability > 0 and
           (Modules_List(Enemy.Ship.Modules(I).ProtoIndex).MType = GUN or
            Modules_List(Enemy.Ship.Modules(I).ProtoIndex).MType =
              BATTERING_RAM) then
            if Modules_List(Enemy.Ship.Modules(I).ProtoIndex).MType = GUN then
               DamageRange := 5000;
               if Enemy.Ship.Modules(I).Current_Value >=
                 Enemy.Ship.Cargo.First_Index and
                 Enemy.Ship.Modules(I).Current_Value <=
                   Enemy.Ship.Cargo.Last_Index then
                  if Items_List
                      (Enemy.Ship.Cargo(Enemy.Ship.Modules(I).Current_Value)
                         .ProtoIndex)
                      .IType =
                    Items_Types
                      (Modules_List(Enemy.Ship.Modules(I).ProtoIndex)
                         .Value) then
                     EnemyAmmoIndex := Enemy.Ship.Modules(I).Current_Value;
                  end if;
               end if;
               if EnemyAmmoIndex = 0 then
                  for K in Items_List.Iterate loop
                     if Items_List(K).IType =
                       Items_Types
                         (Modules_List(Enemy.Ship.Modules(I).ProtoIndex)
                            .Value) then
                        for J in Enemy.Ship.Cargo.Iterate loop
                           if Enemy.Ship.Cargo(J).ProtoIndex =
                             Objects_Container.To_Index(K) then
                              EnemyAmmoIndex := Cargo_Container.To_Index(J);
                              exit;
                           end if;
                        end loop;
                        exit;
                     end if;
                  end loop;
               end if;
               if EnemyAmmoIndex = 0 and
                 (Enemy.CombatAI = ATTACKER or Enemy.CombatAI = DISARMER) then
                  Enemy.CombatAI := COWARD;
                  exit;
               end if;
            else
               DamageRange := 100;
            end if;
            EnemyWeaponIndex := Modules_Container.To_Index(I);
            exit;
         end if;
      end loop;
      if EnemyWeaponIndex = 0 and
        (Enemy.CombatAI = ATTACKER or Enemy.CombatAI = DISARMER) then
         Enemy.CombatAI := COWARD;
      end if;
      case Enemy.CombatAI is
         when BERSERKER =>
            if Enemy.Distance > 10 and Enemy.Ship.Speed /= FULL_SPEED then
               Enemy.Ship.Speed :=
                 ShipSpeed'Val(ShipSpeed'Pos(Enemy.Ship.Speed) + 1);
               AddMessage
                 (To_String(EnemyName) & " increases speed.",
                  CombatMessage);
               EnemyPilotOrder := 1;
            elsif Enemy.Distance <= 10 and Enemy.Ship.Speed /= HALF_SPEED then
               Enemy.Ship.Speed :=
                 ShipSpeed'Val(ShipSpeed'Pos(Enemy.Ship.Speed) - 1);
               AddMessage
                 (To_String(EnemyName) & " decreases speed.",
                  CombatMessage);
               EnemyPilotOrder := 2;
            end if;
         when ATTACKER | DISARMER =>
            if Enemy.Distance > DamageRange and
              Enemy.Ship.Speed /= FULL_SPEED then
               Enemy.Ship.Speed :=
                 ShipSpeed'Val(ShipSpeed'Pos(Enemy.Ship.Speed) + 1);
               AddMessage
                 (To_String(EnemyName) & " increases speed.",
                  CombatMessage);
               EnemyPilotOrder := 1;
            elsif Enemy.Distance < DamageRange and
              Enemy.Ship.Speed /= QUARTER_SPEED then
               Enemy.Ship.Speed :=
                 ShipSpeed'Val(ShipSpeed'Pos(Enemy.Ship.Speed) - 1);
               AddMessage
                 (To_String(EnemyName) & " decreases speed.",
                  CombatMessage);
               EnemyPilotOrder := 2;
            end if;
         when COWARD =>
            if Enemy.Distance < 15000 and Enemy.Ship.Speed /= FULL_SPEED then
               Enemy.Ship.Speed :=
                 ShipSpeed'Val(ShipSpeed'Pos(Enemy.Ship.Speed) + 1);
               AddMessage
                 (To_String(EnemyName) & " increases speed.",
                  CombatMessage);
            end if;
            EnemyPilotOrder := 4;
         when others =>
            null;
      end case;
      case EnemyPilotOrder is
         when 1 =>
            AccuracyBonus := AccuracyBonus + 20;
            EvadeBonus := EvadeBonus - 20;
         when 2 =>
            AccuracyBonus := AccuracyBonus + 10;
            EvadeBonus := EvadeBonus - 10;
         when 3 =>
            AccuracyBonus := AccuracyBonus - 10;
            EvadeBonus := EvadeBonus + 10;
         when 4 =>
            AccuracyBonus := AccuracyBonus - 20;
            EvadeBonus := EvadeBonus + 20;
         when others =>
            null;
      end case;
      SpeedBonus := 20 - (RealSpeed(Enemy.Ship) / 100);
      if SpeedBonus < -10 then
         SpeedBonus := -10;
      end if;
      AccuracyBonus := AccuracyBonus + SpeedBonus;
      EvadeBonus := EvadeBonus - SpeedBonus;
      if EnemyPilotOrder < 4 then
         DistanceTraveled := 0 - RealSpeed(Enemy.Ship);
      else
         DistanceTraveled := RealSpeed(Enemy.Ship);
      end if;
      if PilotIndex > 0 then
         case PilotOrder is
            when 1 | 3 =>
               DistanceTraveled := DistanceTraveled - RealSpeed(PlayerShip);
            when 2 =>
               DistanceTraveled := DistanceTraveled + RealSpeed(PlayerShip);
               if DistanceTraveled > 0 and EnemyPilotOrder /= 4 then
                  DistanceTraveled := 0;
               end if;
            when 4 =>
               DistanceTraveled := DistanceTraveled + RealSpeed(PlayerShip);
            when others =>
               null;
         end case;
      else
         DistanceTraveled := DistanceTraveled - RealSpeed(PlayerShip);
      end if;
      Enemy.Distance := Enemy.Distance + DistanceTraveled;
      if Enemy.Distance < 10 then
         Enemy.Distance := 10;
      end if;
      if Enemy.Distance >= 15000 then
         if PilotOrder = 4 then
            AddMessage
              ("You escaped from " & To_String(EnemyName) & ".",
               CombatMessage);
         else
            AddMessage
              (To_String(EnemyName) & " escaped from you.",
               CombatMessage);
         end if;
         EndCombat := True;
         return;
      elsif Enemy.Distance < 15000 and Enemy.Distance >= 10000 then
         AccuracyBonus := AccuracyBonus - 10;
         EvadeBonus := EvadeBonus + 10;
      elsif Enemy.Distance < 5000 and Enemy.Distance >= 1000 then
         AccuracyBonus := AccuracyBonus + 10;
      elsif Enemy.Distance < 1000 then
         AccuracyBonus := AccuracyBonus + 20;
         EvadeBonus := EvadeBonus - 10;
      end if;
      Attack(PlayerShip, Enemy.Ship); -- Player attack
      if not EndCombat then
         Attack(Enemy.Ship, PlayerShip); -- Enemy attack
      end if;
      if not EndCombat then
         UpdateGame(1);
      end if;
   end CombatTurn;

end Combat;
