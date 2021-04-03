--    Copyright 2016-2021 Bartek thindil Jasicki
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

with GNAT.String_Split; use GNAT.String_Split;
with Crew; use Crew;
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
with Log; use Log;
with Goals; use Goals;
with Factions; use Factions;
with Stories; use Stories;
with Config; use Config;
with Trades; use Trades;

package body Combat is

   -- ****iv* Combat/Combat.FactionName
   -- FUNCTION
   -- Name of enemy ship (and its crew) faction
   -- SOURCE
   FactionName: Unbounded_String;
   -- ****

   -- ****iv* Combat/Combat.TurnNumber
   -- FUNCTION
   -- Number of turn of combat
   -- SOURCE
   TurnNumber: Natural;
   -- ****

   function StartCombat
     (EnemyIndex: Unbounded_String; NewCombat: Boolean := True)
      return Boolean is
      EnemyShip: ShipRecord;
      EnemyGuns: Guns_Container.Vector;
      ShootingSpeed: Integer;
      function CountPerception(Spotter, Spotted: ShipRecord) return Natural is
         Result: Natural := 0;
      begin
         Count_Spotter_Perception_Loop :
         for I in Spotter.Crew.Iterate loop
            case Spotter.Crew(I).Order is
               when Pilot =>
                  Result :=
                    Result + GetSkillLevel(Spotter.Crew(I), Perception_Skill);
                  if Spotter = PlayerShip then
                     GainExp(1, Perception_Skill, Crew_Container.To_Index(I));
                  end if;
               when Gunner =>
                  Result :=
                    Result + GetSkillLevel(Spotter.Crew(I), Perception_Skill);
                  if Spotter = PlayerShip then
                     GainExp(1, Perception_Skill, Crew_Container.To_Index(I));
                  end if;
               when others =>
                  null;
            end case;
         end loop Count_Spotter_Perception_Loop;
         Count_Modules_Loop :
         for Module of Spotted.Modules loop
            if Module.MType = HULL then
               Result := Result + Module.MaxModules;
               exit Count_Modules_Loop;
            end if;
         end loop Count_Modules_Loop;
         return Result;
      end CountPerception;
   begin
      EnemyShipIndex := EnemyIndex;
      FactionName := Factions_List(ProtoShips_List(EnemyIndex).Owner).Name;
      HarpoonDuration := 0;
      BoardingOrders.Clear;
      EnemyShip :=
        CreateShip
          (EnemyIndex, Null_Unbounded_String, PlayerShip.SkyX, PlayerShip.SkyY,
           FULL_SPEED);
      -- Enemy ship is trader, generate cargo for it
      if Index(ProtoShips_List(EnemyIndex).Name, To_String(Traders_Name)) >
        0 then
         GenerateTraderCargo(EnemyIndex);
         Update_Cargo_Loop :
         for Item of TraderCargo loop
            UpdateCargo(EnemyShip, Item.ProtoIndex, Item.Amount);
         end loop Update_Cargo_Loop;
         TraderCargo.Clear;
      end if;
      declare
         MinFreeSpace, ItemIndex, CargoItemIndex: Natural := 0;
         ItemAmount: Positive;
         NewItemIndex: Unbounded_String;
      begin
         Count_Free_Space_Loop :
         for Module of EnemyShip.Modules loop
            if Module.MType = CARGO_ROOM and Module.Durability > 0 then
               MinFreeSpace :=
                 MinFreeSpace + Modules_List(Module.ProtoIndex).MaxValue;
            end if;
         end loop Count_Free_Space_Loop;
         MinFreeSpace :=
           Natural
             (Float(MinFreeSpace) *
              (1.0 - (Float(GetRandom(20, 70)) / 100.0)));
         Add_Enemy_Cargo_Loop :
         loop
            exit Add_Enemy_Cargo_Loop when FreeCargo(0, EnemyShip) <=
              MinFreeSpace;
            ItemIndex := GetRandom(1, Positive(Items_List.Length));
            Find_Item_Index_Loop :
            for I in Items_List.Iterate loop
               ItemIndex := ItemIndex - 1;
               if ItemIndex = 0 then
                  NewItemIndex := Objects_Container.Key(I);
                  exit Find_Item_Index_Loop;
               end if;
            end loop Find_Item_Index_Loop;
            ItemAmount :=
              (if EnemyShip.Crew.Length < 5 then GetRandom(1, 100)
               elsif EnemyShip.Crew.Length < 10 then GetRandom(1, 500)
               else GetRandom(1, 1000));
            CargoItemIndex := FindItem(EnemyShip.Cargo, NewItemIndex);
            if CargoItemIndex > 0 then
               EnemyShip.Cargo(CargoItemIndex).Amount :=
                 EnemyShip.Cargo(CargoItemIndex).Amount + ItemAmount;
            else
               if FreeCargo
                   (0 - (Items_List(NewItemIndex).Weight * ItemAmount)) >
                 -1 then
                  EnemyShip.Cargo.Append
                    (New_Item =>
                       (ProtoIndex => NewItemIndex, Amount => ItemAmount,
                        Durability => 100, Name => Null_Unbounded_String,
                        Price => 0));
               end if;
            end if;
         end loop Add_Enemy_Cargo_Loop;
      end;
      EnemyGuns.Clear;
      Count_Enemy_Shooting_Speed_Loop :
      for I in EnemyShip.Modules.Iterate loop
         if (EnemyShip.Modules(I).MType in GUN | HARPOON_GUN) and
           EnemyShip.Modules(I).Durability > 0 then
            if Modules_List(EnemyShip.Modules(I).ProtoIndex).Speed > 0 then
               ShootingSpeed :=
                 (if ProtoShips_List(EnemyIndex).CombatAI = DISARMER then
                    Natural
                      (Float'Ceiling
                         (Float
                            (Modules_List(EnemyShip.Modules(I).ProtoIndex)
                               .Speed) /
                          2.0))
                  else Modules_List(EnemyShip.Modules(I).ProtoIndex).Speed);
            else
               ShootingSpeed :=
                 (if ProtoShips_List(EnemyIndex).CombatAI = DISARMER then
                    Modules_List(EnemyShip.Modules(I).ProtoIndex).Speed - 1
                  else Modules_List(EnemyShip.Modules(I).ProtoIndex).Speed);
            end if;
            EnemyGuns.Append
              (New_Item => (Modules_Container.To_Index(I), 1, ShootingSpeed));
         end if;
      end loop Count_Enemy_Shooting_Speed_Loop;
      Enemy :=
        (Ship => EnemyShip, Accuracy => 0, Distance => 10000,
         CombatAI => ProtoShips_List(EnemyIndex).CombatAI, Evasion => 0,
         Loot => 0, Perception => 0, HarpoonDuration => 0, Guns => EnemyGuns);
      Enemy.Accuracy :=
        (if ProtoShips_List(EnemyIndex).Accuracy(2) = 0 then
           ProtoShips_List(EnemyIndex).Accuracy(1)
         else GetRandom
             (ProtoShips_List(EnemyIndex).Accuracy(1),
              ProtoShips_List(EnemyIndex).Accuracy(2)));
      Enemy.Evasion :=
        (if ProtoShips_List(EnemyIndex).Evasion(2) = 0 then
           ProtoShips_List(EnemyIndex).Evasion(1)
         else GetRandom
             (ProtoShips_List(EnemyIndex).Evasion(1),
              ProtoShips_List(EnemyIndex).Evasion(2)));
      Enemy.Perception :=
        (if ProtoShips_List(EnemyIndex).Perception(2) = 0 then
           ProtoShips_List(EnemyIndex).Perception(1)
         else GetRandom
             (ProtoShips_List(EnemyIndex).Perception(1),
              ProtoShips_List(EnemyIndex).Perception(2)));
      Enemy.Loot :=
        (if ProtoShips_List(EnemyIndex).Loot(2) = 0 then
           ProtoShips_List(EnemyIndex).Loot(1)
         else GetRandom
             (ProtoShips_List(EnemyIndex).Loot(1),
              ProtoShips_List(EnemyIndex).Loot(2)));
      if PilotOrder = 0 then
         PilotOrder := 2;
         EngineerOrder := 3;
      end if;
      EndCombat := False;
      EnemyName := GenerateShipName(ProtoShips_List(EnemyIndex).Owner);
      MessagesStarts := GetLastMessageIndex + 1;
      declare
         Old_Guns_List: constant Guns_Container.Vector := Guns;
         Same_Lists: Boolean := True;
      begin
         Guns.Clear;
         Set_Player_Guns_Loop :
         for I in PlayerShip.Modules.Iterate loop
            if (PlayerShip.Modules(I).MType in GUN | HARPOON_GUN) and
              PlayerShip.Modules(I).Durability > 0 then
               Guns.Append
                 (New_Item =>
                    (Modules_Container.To_Index(I), 1,
                     Modules_List(PlayerShip.Modules(I).ProtoIndex).Speed));
            end if;
         end loop Set_Player_Guns_Loop;
         if Old_Guns_List.Length > 0 and
           Old_Guns_List.Length = Guns.Length then
            Compare_Lists_Loop :
            for I in Guns.First_Index .. Guns.Last_Index loop
               if Guns(I)(1) /= Old_Guns_List(I)(1) then
                  Same_Lists := False;
                  exit Compare_Lists_Loop;
               end if;
            end loop Compare_Lists_Loop;
            if Same_Lists then
               Guns := Old_Guns_List;
            end if;
         end if;
      end;
      if NewCombat then
         declare
            PlayerPerception: constant Natural :=
              CountPerception(PlayerShip, Enemy.Ship);
            EnemyPerception: Natural := 0;
         begin
            OldSpeed := PlayerShip.Speed;
            EnemyPerception :=
              (if Enemy.Perception > 0 then Enemy.Perception
               else CountPerception(Enemy.Ship, PlayerShip));
            if (PlayerPerception + GetRandom(1, 50)) >
              (EnemyPerception + GetRandom(1, 50)) then
               AddMessage
                 ("You spotted " & To_String(Enemy.Ship.Name) & ".",
                  OtherMessage);
            else
               if RealSpeed(PlayerShip) < RealSpeed(Enemy.Ship) then
                  LogMessage
                    ("You were attacked by " & To_String(Enemy.Ship.Name),
                     Log.Combat);
                  AddMessage
                    (To_String(Enemy.Ship.Name) & " intercepted you.",
                     CombatMessage);
                  return True;
               end if;
               AddMessage
                 ("You spotted " & To_String(Enemy.Ship.Name) & ".",
                  OtherMessage);
            end if;
         end;
         return False;
      end if;
      TurnNumber := 0;
      LogMessage
        ("Started combat with " & To_String(Enemy.Ship.Name), Log.Combat);
      return True;
   end StartCombat;

   procedure CombatTurn is
      AccuracyBonus, EvadeBonus: Integer := 0;
      PilotIndex, EngineerIndex, EnemyWeaponIndex, EnemyAmmoIndex,
      EnemyPilotIndex, AmmoIndex2: Natural := 0;
      DistanceTraveled, SpeedBonus: Integer;
      ShootMessage, Message: Unbounded_String;
      EnemyPilotOrder: Positive := 2;
      DamageRange: Positive := 10000;
      FreeSpace: Integer := 0;
      procedure Attack(Ship, EnemyShip: in out ShipRecord) is
         GunnerIndex: Crew_Container.Extended_Index;
         AmmoIndex: Inventory_Container.Extended_Index;
         ArmorIndex, WeaponIndex: Modules_Container.Extended_Index;
         Shoots: Natural;
         GunnerOrder: Positive;
         HitChance, HitLocation, CurrentAccuracyBonus: Integer;
         Damage: Damage_Factor := 0.0;
         WeaponDamage: Integer;
         EnemyNameOwner: constant Unbounded_String :=
           EnemyName & To_Unbounded_String(" (") & FactionName &
           To_Unbounded_String(")");
         procedure RemoveGun(ModuleIndex: Positive) is
         begin
            if EnemyShip = PlayerShip then
               Remove_Gun_Loop :
               for J in Guns.First_Index .. Guns.Last_Index loop
                  if Guns(J)(1) = ModuleIndex then
                     Guns.Delete(Index => J);
                     exit Remove_Gun_Loop;
                  end if;
               end loop Remove_Gun_Loop;
            end if;
         end RemoveGun;
         function FindEnemyModule(MType: ModuleType) return Natural is
         begin
            Find_Enemy_Module_Loop :
            for I in EnemyShip.Modules.Iterate loop
               if Modules_List(EnemyShip.Modules(I).ProtoIndex).MType =
                 MType and
                 EnemyShip.Modules(I).Durability > 0 then
                  return Modules_Container.To_Index(I);
               end if;
            end loop Find_Enemy_Module_Loop;
            return 0;
         end FindEnemyModule;
         procedure FindHitWeapon is
         begin
            Find_Weapon_Location_Loop :
            for J in EnemyShip.Modules.Iterate loop
               if
                 ((EnemyShip.Modules(J).MType = TURRET
                   and then EnemyShip.Modules(J).GunIndex > 0) or
                  Modules_List(EnemyShip.Modules(J).ProtoIndex).MType =
                    BATTERING_RAM) and
                 EnemyShip.Modules(J).Durability > 0 then
                  HitLocation := Modules_Container.To_Index(J);
                  return;
               end if;
            end loop Find_Weapon_Location_Loop;
         end FindHitWeapon;
      begin
         if Ship = PlayerShip then
            LogMessage("Player's round.", Log.Combat);
         else
            LogMessage("Enemy's round.", Log.Combat);
         end if;
         Attack_Loop :
         for K in Ship.Modules.Iterate loop
            if Ship.Modules(K).Durability = 0 or
              (Ship.Modules(K).MType not in GUN | BATTERING_RAM |
                   HARPOON_GUN) then
               goto End_Of_Attack_Loop;
            end if;
            GunnerIndex := 0;
            AmmoIndex := 0;
            if Ship.Modules(K).MType = HARPOON_GUN then
               AmmoIndex2 := Ship.Modules(K).HarpoonIndex;
            elsif Ship.Modules(K).MType = GUN then
               AmmoIndex2 := Ship.Modules(K).AmmoIndex;
            end if;
            if Ship.Modules(K).MType in GUN | HARPOON_GUN then
               GunnerIndex := Ship.Modules(K).Owner(1);
               LogMessage
                 ("Gunner index:" & Natural'Image(GunnerIndex) & ".",
                  Log.Combat);
               if Ship = PlayerShip then
                  Shoots := 0;
                  if GunnerIndex > 0 then
                     Count_Player_Shoots_Loop :
                     for Gun of Guns loop
                        if Gun(1) = Modules_Container.To_Index(K) then
                           GunnerOrder := Gun(2);
                           if Gun(3) > 0 then
                              Shoots := Gun(3);
                              if GunnerOrder /= 3 then
                                 Shoots :=
                                   Natural(Float'Ceiling(Float(Shoots) / 2.0));
                              end if;
                              LogMessage
                                ("Player Shoots (no cooldown):" &
                                 Natural'Image(Shoots),
                                 Log.Combat);
                           elsif Gun(3) < 0 then
                              Shoots := 0;
                              Gun(3) := Gun(3) + 1;
                              if Gun(3) = 0 then
                                 Shoots := 1;
                                 Gun(3) :=
                                   (if GunnerOrder = 3 then
                                      Modules_List
                                        (PlayerShip.Modules(Gun(1)).ProtoIndex)
                                        .Speed
                                    else Modules_List
                                        (PlayerShip.Modules(Gun(1)).ProtoIndex)
                                        .Speed -
                                      1);
                              end if;
                              LogMessage
                                ("Player Shoots (after cooldown):" &
                                 Natural'Image(Shoots),
                                 Log.Combat);
                           end if;
                           exit Count_Player_Shoots_Loop;
                        end if;
                     end loop Count_Player_Shoots_Loop;
                     LogMessage
                       ("Shoots test3:" & Natural'Image(Shoots), Log.Combat);
                     if Ship.Crew(GunnerIndex).Order /= Gunner then
                        GunnerOrder := 1;
                     end if;
                     case GunnerOrder is
                        when 1 =>
                           if Shoots > 0 then
                              Shoots := 0;
                           end if;
                        when 2 =>
                           CurrentAccuracyBonus := AccuracyBonus + 20;
                        when 4 =>
                           CurrentAccuracyBonus := AccuracyBonus - 10;
                        when 5 =>
                           CurrentAccuracyBonus := AccuracyBonus - 20;
                        when others =>
                           null;
                     end case;
                  end if;
               else
                  Count_Enemy_Shoots_Loop :
                  for Gun of Enemy.Guns loop
                     if Gun(1) = Modules_Container.To_Index(K) then
                        if Gun(3) > 0 then
                           Shoots := Gun(3);
                        elsif Gun(3) < 0 then
                           Shoots := 0;
                           Gun(3) := Gun(3) + 1;
                           if Gun(3) = 0 then
                              Shoots := 1;
                              Gun(3) :=
                                (if Enemy.CombatAI = DISARMER then
                                   Modules_List
                                     (Ship.Modules(Gun(1)).ProtoIndex)
                                     .Speed -
                                   1
                                 else Modules_List
                                     (Ship.Modules(Gun(1)).ProtoIndex)
                                     .Speed);
                           end if;
                        end if;
                        exit Count_Enemy_Shoots_Loop;
                     end if;
                  end loop Count_Enemy_Shoots_Loop;
                  if Ship.Crew.Length > 0 and GunnerIndex = 0 then
                     Shoots := 0;
                  end if;
               end if;
               if AmmoIndex2 in Ship.Cargo.First_Index .. Ship.Cargo.Last_Index
                 and then Items_List(Ship.Cargo(AmmoIndex2).ProtoIndex).IType =
                   Items_Types
                     (Modules_List(Ship.Modules(K).ProtoIndex).Value) then
                  AmmoIndex := AmmoIndex2;
               end if;
               if AmmoIndex = 0 then
                  Find_Ammo_Index_Loop :
                  for I in Items_List.Iterate loop
                     if Items_List(I).IType =
                       Items_Types
                         (Modules_List(Ship.Modules(K).ProtoIndex).Value) then
                        Get_Ammo_Index_Loop :
                        for J in Ship.Cargo.Iterate loop
                           if Ship.Cargo(J).ProtoIndex =
                             Objects_Container.Key(I) then
                              AmmoIndex := Inventory_Container.To_Index(J);
                              if Ship.Modules(K).MType = HARPOON_GUN then
                                 Ship.Modules(K).HarpoonIndex := AmmoIndex;
                              elsif Ship.Modules(K).MType = GUN then
                                 Ship.Modules(K).AmmoIndex := AmmoIndex;
                              end if;
                              exit Get_Ammo_Index_Loop;
                           end if;
                        end loop Get_Ammo_Index_Loop;
                        exit Find_Ammo_Index_Loop when AmmoIndex > 0;
                     end if;
                  end loop Find_Ammo_Index_Loop;
               end if;
               if AmmoIndex = 0 then
                  if Ship = PlayerShip then
                     AddMessage
                       ("You don't have ammo to " &
                        To_String(Ship.Modules(K).Name) & "!",
                        CombatMessage, RED);
                  end if;
                  Shoots := 0;
               elsif Ship.Cargo(AmmoIndex).Amount < Shoots then
                  Shoots := Ship.Cargo(AmmoIndex).Amount;
               end if;
               if Enemy.Distance > 5000 then
                  Shoots := 0;
               end if;
               if Ship.Modules(K).MType = HARPOON_GUN and Shoots > 0 then
                  Shoots := 1;
                  if Enemy.Distance > 2000 then
                     Shoots := 0;
                  end if;
                  if FindEnemyModule(ARMOR) > 0 then
                     Shoots := 0;
                  end if;
               end if;
               if Ship.Modules(K).MType = GUN and Shoots > 0 then
                  case Items_List(Ship.Cargo(AmmoIndex).ProtoIndex).Value(2) is
                     when 2 =>
                        if Ship = PlayerShip then
                           CurrentAccuracyBonus := CurrentAccuracyBonus - 10;
                        else
                           EvadeBonus := EvadeBonus + 10;
                        end if;
                     when 3 =>
                        if Ship = PlayerShip then
                           CurrentAccuracyBonus := CurrentAccuracyBonus + 10;
                        else
                           EvadeBonus := EvadeBonus - 10;
                        end if;
                     when others =>
                        null;
                  end case;
               end if;
            else
               if Enemy.Distance > 100 then
                  Shoots := 0;
               else
                  Shoots := (if Ship.Modules(K).CoolingDown then 0 else 1);
               end if;
               Ship.Modules(K).CoolingDown := not Ship.Modules(K).CoolingDown;
            end if;
            LogMessage("Shoots:" & Integer'Image(Shoots), Log.Combat);
            if Shoots > 0 then
               HitChance :=
                 (if Ship = PlayerShip then
                    CurrentAccuracyBonus - Enemy.Evasion
                  else Enemy.Accuracy - EvadeBonus);
               if GunnerIndex > 0 then
                  HitChance :=
                    HitChance +
                    GetSkillLevel(Ship.Crew(GunnerIndex), Gunnery_Skill);
               end if;
               if HitChance < -48 then
                  HitChance := -48;
               end if;
               LogMessage
                 ("Player Accuracy:" & Integer'Image(CurrentAccuracyBonus) &
                  " Player Evasion:" & Integer'Image(EvadeBonus),
                  Log.Combat);
               LogMessage
                 ("Enemy Evasion:" & Integer'Image(Enemy.Evasion) &
                  " Enemy Accuracy:" & Integer'Image(Enemy.Accuracy),
                  Log.Combat);
               LogMessage
                 ("Chance to hit:" & Integer'Image(HitChance), Log.Combat);
               Shooting_Loop :
               for I in 1 .. Shoots loop
                  if Ship = PlayerShip then
                     ShootMessage :=
                       (if Ship.Modules(K).MType in GUN | HARPOON_GUN then
                          Ship.Crew(GunnerIndex).Name &
                          To_Unbounded_String(" shoots at ") & EnemyNameOwner
                        else To_Unbounded_String("You ram ") & EnemyNameOwner);
                  else
                     ShootMessage :=
                       EnemyNameOwner & To_Unbounded_String(" attacks");
                  end if;
                  if HitChance + GetRandom(1, 50) >
                    GetRandom(1, HitChance + 50) then
                     ShootMessage :=
                       ShootMessage & To_Unbounded_String(" and hits ");
                     ArmorIndex := FindEnemyModule(ARMOR);
                     if ArmorIndex > 0 then
                        HitLocation := ArmorIndex;
                     else
                        if Ship = PlayerShip then
                           if GunnerIndex > 0
                             and then GunnerOrder in
                               4 ..
                                     6 then -- aim for part of enemy ship
                              HitLocation := 0;
                              case GunnerOrder is
                                 when 4 =>
                                    HitLocation := FindEnemyModule(ENGINE);
                                 when 5 =>
                                    HitLocation := 0;
                                    FindHitWeapon;
                                    if HitLocation = 0 then
                                       HitLocation :=
                                         FindEnemyModule(BATTERING_RAM);
                                    end if;
                                 when 6 =>
                                    HitLocation := FindEnemyModule(HULL);
                                 when others =>
                                    HitLocation := 1;
                              end case;
                              if HitLocation = 0 then
                                 HitLocation := 1;
                              end if;
                           else
                              HitLocation :=
                                GetRandom
                                  (Enemy.Ship.Modules.First_Index,
                                   Enemy.Ship.Modules.Last_Index);
                           end if;
                        else
                           if Enemy.CombatAI = DISARMER then
                              HitLocation := 1;
                              FindHitWeapon;
                           else
                              HitLocation :=
                                GetRandom
                                  (PlayerShip.Modules.First_Index,
                                   PlayerShip.Modules.Last_Index);
                           end if;
                        end if;
                        Get_Hit_Location_Loop :
                        while EnemyShip.Modules(HitLocation).Durability =
                          0 loop
                           HitLocation := HitLocation - 1;
                           exit Attack_Loop when HitLocation = 0;
                        end loop Get_Hit_Location_Loop;
                     end if;
                     ShootMessage :=
                       ShootMessage & EnemyShip.Modules(HitLocation).Name &
                       To_Unbounded_String(".");
                     Damage :=
                       1.0 -
                       Damage_Factor
                         (Float(Ship.Modules(K).Durability) /
                          Float(Ship.Modules(K).MaxDurability));
                     if Ship.Modules(K).MType = HARPOON_GUN then
                        WeaponDamage :=
                          Ship.Modules(K).Duration -
                          Natural
                            (Float(Ship.Modules(K).Duration) * Float(Damage));
                     elsif Ship.Modules(K).MType = GUN then
                        WeaponDamage :=
                          Ship.Modules(K).Damage -
                          Natural
                            (Float(Ship.Modules(K).Damage) * Float(Damage));
                     elsif Ship.Modules(K).MType = BATTERING_RAM then
                        WeaponDamage :=
                          Ship.Modules(K).Damage2 -
                          Natural
                            (Float(Ship.Modules(K).Damage2) * Float(Damage));
                        WeaponDamage :=
                          (if SpeedBonus < 0 then
                             WeaponDamage +
                             (abs (SpeedBonus) *
                              (CountShipWeight(Ship) / 5000))
                           else WeaponDamage + (CountShipWeight(Ship) / 5000));
                     end if;
                     if WeaponDamage = 0 then
                        WeaponDamage := 1;
                     end if;
                     if AmmoIndex > 0 then
                        WeaponDamage :=
                          WeaponDamage +
                          Items_List(Ship.Cargo(AmmoIndex).ProtoIndex).Value
                            (1);
                     end if;
                     WeaponDamage :=
                       (if Ship = PlayerShip then
                          Integer
                            (Float(WeaponDamage) *
                             Float(NewGameSettings.PlayerDamageBonus))
                        else Integer
                            (Float(WeaponDamage) *
                             Float(NewGameSettings.Enemy_Damage_Bonus)));
                     if ArmorIndex = 0 then
                        if Ship.Modules(K).MType = HARPOON_GUN then
                           Count_Damage_Loop :
                           for Module of EnemyShip.Modules loop
                              if Module.MType = HULL then
                                 WeaponDamage :=
                                   WeaponDamage - (Module.MaxModules / 10);
                                 if WeaponDamage < 1 then
                                    WeaponDamage := 1;
                                 end if;
                                 exit Count_Damage_Loop;
                              end if;
                           end loop Count_Damage_Loop;
                           if Ship = PlayerShip then
                              Enemy.HarpoonDuration :=
                                Enemy.HarpoonDuration + WeaponDamage;
                           else
                              HarpoonDuration :=
                                HarpoonDuration + WeaponDamage;
                           end if;
                           WeaponDamage := 1;
                        elsif Ship.Modules(K).MType = BATTERING_RAM then
                           if Ship = PlayerShip then
                              Enemy.HarpoonDuration :=
                                Enemy.HarpoonDuration + 2;
                           else
                              HarpoonDuration := HarpoonDuration + 2;
                           end if;
                        end if;
                     end if;
                     DamageModule
                       (EnemyShip, HitLocation, WeaponDamage,
                        "enemy fire in ship combat");
                     if EnemyShip.Modules(HitLocation).Durability = 0 then
                        case Modules_List
                          (EnemyShip.Modules(HitLocation).ProtoIndex)
                          .MType is
                           when HULL | ENGINE =>
                              EndCombat := True;
                           when TURRET =>
                              if EnemyShip = PlayerShip then
                                 WeaponIndex :=
                                   EnemyShip.Modules(HitLocation).GunIndex;
                                 if WeaponIndex > 0 then
                                    EnemyShip.Modules(WeaponIndex)
                                      .Durability :=
                                      0;
                                    RemoveGun(WeaponIndex);
                                 end if;
                              end if;
                           when GUN =>
                              if EnemyShip = PlayerShip then
                                 RemoveGun(HitLocation);
                              end if;
                           when others =>
                              null;
                        end case;
                     end if;
                     if Ship = PlayerShip then
                        AddMessage
                          (To_String(ShootMessage), CombatMessage, GREEN);
                     else
                        AddMessage
                          (To_String(ShootMessage), CombatMessage, YELLOW);
                     end if;
                  else
                     ShootMessage :=
                       ShootMessage & To_Unbounded_String(" and misses.");
                     if Ship = PlayerShip then
                        AddMessage
                          (To_String(ShootMessage), CombatMessage, BLUE);
                     else
                        AddMessage
                          (To_String(ShootMessage), CombatMessage, CYAN);
                     end if;
                  end if;
                  if AmmoIndex > 0 then
                     UpdateCargo
                       (Ship => Ship, CargoIndex => AmmoIndex, Amount => -1);
                  end if;
                  if Ship = PlayerShip and GunnerIndex > 0 then
                     GainExp(2, Gunnery_Skill, GunnerIndex);
                  end if;
                  if PlayerShip.Crew(1).Health = 0 then -- player is dead
                     EndCombat := True;
                  end if;
                  exit Attack_Loop when EndCombat;
               end loop Shooting_Loop;
            end if;
            <<End_Of_Attack_Loop>>
         end loop Attack_Loop;
      end Attack;
      procedure MeleeCombat
        (Attackers, Defenders: in out Crew_Container.Vector;
         PlayerAttack: Boolean) is
         AttackDone, Riposte: Boolean;
         AttackerIndex, DefenderIndex: Positive;
         OrderIndex: Natural;
         function CharacterAttack
           (AttackerIndex, DefenderIndex: Positive; PlayerAttack2: Boolean)
            return Boolean is
            HitChance, Damage: Integer;
            HitLocation: constant Positive := GetRandom(3, 6);
            LocationNames: constant array(3 .. 6) of Unbounded_String :=
              (To_Unbounded_String("head"), To_Unbounded_String("torso"),
               To_Unbounded_String("leg"), To_Unbounded_String("arm"));
            AttackSkill, BaseDamage: Natural;
            Wounds: Damage_Factor := 0.0;
            MessageColor: Message_Color;
            Attacker: Member_Data :=
              (if PlayerAttack2 then PlayerShip.Crew(AttackerIndex)
               else Enemy.Ship.Crew(AttackerIndex));
            Defender: Member_Data :=
              (if PlayerAttack2 then Enemy.Ship.Crew(DefenderIndex)
               else PlayerShip.Crew(DefenderIndex));
            AttackMessage: Unbounded_String :=
              (if PlayerAttack2 then
                 Attacker.Name & To_Unbounded_String(" attacks ") &
                 Defender.Name & To_Unbounded_String(" (") & FactionName &
                 To_Unbounded_String(")")
               else Attacker.Name & To_Unbounded_String(" (") & FactionName &
                 To_Unbounded_String(")") & To_Unbounded_String(" attacks ") &
                 Defender.Name);
         begin
            BaseDamage := Attacker.Attributes(Strength_Index)(1);
            if Attacker.Equipment(1) > 0 then
               BaseDamage :=
                 BaseDamage +
                 Items_List
                   (Attacker.Inventory(Attacker.Equipment(1)).ProtoIndex)
                   .Value
                   (2);
            end if;
         -- Count damage based on attacker wounds, fatigue, hunger and thirst
            Wounds := 1.0 - Damage_Factor(Float(Attacker.Health) / 100.0);
            Damage :=
              (BaseDamage - Integer(Float(BaseDamage) * Float(Wounds)));
            if Attacker.Thirst > 40 then
               Wounds := 1.0 - Damage_Factor(Float(Attacker.Thirst) / 100.0);
               Damage := Damage - (Integer(Float(BaseDamage) * Float(Wounds)));
            end if;
            if Attacker.Hunger > 80 then
               Wounds := 1.0 - Damage_Factor(Float(Attacker.Hunger) / 100.0);
               Damage := Damage - (Integer(Float(BaseDamage) * Float(Wounds)));
            end if;
            Damage :=
              (if PlayerAttack2 then
                 Integer
                   (Float(Damage) *
                    Float(NewGameSettings.PlayerMeleeDamageBonus))
               else Integer
                   (Float(Damage) *
                    Float(NewGameSettings.EnemyMeleeDamageBonus)));
            if Attacker.Equipment(1) > 0 then
               AttackSkill :=
                 GetSkillLevel
                   (Attacker,
                    Items_List
                      (Attacker.Inventory(Attacker.Equipment(1)).ProtoIndex)
                      .Value
                      (3));
               HitChance := AttackSkill + GetRandom(1, 50);
            else
               HitChance :=
                 GetSkillLevel(Attacker, Unarmed_Skill) + GetRandom(1, 50);
            end if;
            HitChance :=
              HitChance -
              (GetSkillLevel(Defender, Dodge_Skill) + GetRandom(1, 50));
            Count_Hit_Chance_Loop :
            for I in 3 .. 6 loop
               if Defender.Equipment(I) > 0 then
                  HitChance :=
                    HitChance +
                    Items_List
                      (Defender.Inventory(Defender.Equipment(I)).ProtoIndex)
                      .Value
                      (3);
               end if;
            end loop Count_Hit_Chance_Loop;
            if Defender.Equipment(HitLocation) > 0 then
               Damage :=
                 Damage -
                 Items_List
                   (Defender.Inventory(Defender.Equipment(HitLocation))
                      .ProtoIndex)
                   .Value
                   (2);
            end if;
            if Defender.Equipment(2) > 0 then
               Damage :=
                 Damage -
                 Items_List
                   (Defender.Inventory(Defender.Equipment(2)).ProtoIndex)
                   .Value
                   (2);
            end if;
            if Attacker.Equipment(1) = 0 then
               declare
                  DamageBonus: Natural :=
                    GetSkillLevel(Attacker, Unarmed_Skill) / 200;
               begin
                  if DamageBonus = 0 then
                     DamageBonus := 1;
                  end if;
                  Damage := Damage + DamageBonus;
               end;
            end if;
            if Factions_List(Defender.Faction).Flags.Contains
                (To_Unbounded_String("naturalarmor")) then
               Damage := Damage / 2;
            end if;
            if
              (Factions_List(Attacker.Faction).Flags.Contains
                 (To_Unbounded_String("toxicattack")) and
               Attacker.Equipment(1) = 0) and
              not Factions_List(Defender.Faction).Flags.Contains
                (To_Unbounded_String("diseaseimmune")) then
               Damage :=
                 (if Damage * 10 < 30 then Damage * 10 else Damage + 30);
            end if;
            if Damage < 1 then
               Damage := 1;
            end if;
            -- Count damage based on damage type of weapon
            if Attacker.Equipment(1) > 0 then
               if Items_List
                   (Attacker.Inventory(Attacker.Equipment(1)).ProtoIndex)
                   .Value
                   (5) =
                 1 then -- cutting weapon
                  Damage := Integer(Float(Damage) * 1.5);
               elsif Items_List
                   (Attacker.Inventory(Attacker.Equipment(1)).ProtoIndex)
                   .Value
                   (5) =
                 2 then -- impale weapon
                  Damage := Damage * 2;
               end if;
            end if;
            if HitChance < 1 then
               AttackMessage :=
                 AttackMessage & To_Unbounded_String(" and misses.");
               MessageColor := (if PlayerAttack then BLUE else CYAN);
               if not PlayerAttack then
                  GainExp(2, Dodge_Skill, DefenderIndex);
                  Defender.Skills := PlayerShip.Crew(DefenderIndex).Skills;
                  Defender.Attributes :=
                    PlayerShip.Crew(DefenderIndex).Attributes;
               end if;
            else
               AttackMessage :=
                 AttackMessage & To_Unbounded_String(" and hit ") &
                 LocationNames(HitLocation) & To_Unbounded_String(".");
               MessageColor := (if PlayerAttack2 then GREEN else YELLOW);
               if Attacker.Equipment(1) > 0 then
                  DamageItem
                    (Attacker.Inventory, Attacker.Equipment(1), AttackSkill,
                     AttackerIndex);
               end if;
               if Defender.Equipment(HitLocation) > 0 then
                  DamageItem
                    (Defender.Inventory, Defender.Equipment(HitLocation), 0,
                     DefenderIndex);
               end if;
               if PlayerAttack2 then
                  if Attacker.Equipment(1) > 0 then
                     GainExp
                       (2,
                        Items_List
                          (Attacker.Inventory(Attacker.Equipment(1))
                             .ProtoIndex)
                          .Value
                          (3),
                        AttackerIndex);
                  else
                     GainExp(2, Unarmed_Skill, AttackerIndex);
                  end if;
                  Attacker.Skills := PlayerShip.Crew(AttackerIndex).Skills;
                  Attacker.Attributes :=
                    PlayerShip.Crew(AttackerIndex).Attributes;
               end if;
               Defender.Health :=
                 (if Damage > Defender.Health then 0
                  else Defender.Health - Damage);
            end if;
            AddMessage(To_String(AttackMessage), CombatMessage, MessageColor);
            Attacker.Tired :=
              (if Attacker.Tired + 1 > Skill_Range'Last then Skill_Range'Last
               else Attacker.Tired + 1);
            Defender.Tired :=
              (if Defender.Tired + 1 > Skill_Range'Last then Skill_Range'Last
               else Defender.Tired + 1);
            if PlayerAttack2 then
               PlayerShip.Crew(AttackerIndex) := Attacker;
               Enemy.Ship.Crew(DefenderIndex) := Defender;
            else
               PlayerShip.Crew(DefenderIndex) := Defender;
               Enemy.Ship.Crew(AttackerIndex) := Attacker;
            end if;
            if Defender.Health = 0 then
               if PlayerAttack2 then
                  Death
                    (DefenderIndex,
                     Attacker.Name &
                     To_Unbounded_String(" blow in melee combat"),
                     Enemy.Ship);
                  Change_Boarding_Order_Loop :
                  for Order of BoardingOrders loop
                     if Order >= DefenderIndex then
                        Order := Order - 1;
                     end if;
                  end loop Change_Boarding_Order_Loop;
                  UpdateKilledMobs(Defender, FactionName);
                  UpdateGoal(KILL, FactionName);
                  if Enemy.Ship.Crew.Length = 0 then
                     EndCombat := True;
                  end if;
               else
                  OrderIndex := 0;
                  Change_Order_Loop :
                  for I in PlayerShip.Crew.Iterate loop
                     if PlayerShip.Crew(I).Order = Boarding then
                        OrderIndex := OrderIndex + 1;
                     end if;
                     if Crew_Container.To_Index(I) = DefenderIndex then
                        BoardingOrders.Delete(Index => OrderIndex);
                        OrderIndex := OrderIndex - 1;
                        exit Change_Order_Loop;
                     end if;
                  end loop Change_Order_Loop;
                  Death
                    (DefenderIndex,
                     Attacker.Name &
                     To_Unbounded_String(" blow in melee combat"),
                     PlayerShip);
                  if DefenderIndex = 1 then -- Player is dead
                     EndCombat := True;
                  end if;
               end if;
               return False;
            else
               return True;
            end if;
         end CharacterAttack;
      begin
         AttackerIndex := Attackers.First_Index;
         OrderIndex := 1;
         Attackers_Attacks_Loop :
         while AttackerIndex <=
           Attackers.Last_Index loop -- Boarding party attacks first
            Riposte := True;
            if Attackers(AttackerIndex).Order /= Boarding then
               goto End_Of_Attacker_Loop;
            end if;
            AttackDone := False;
            if PlayerAttack then
               exit Attackers_Attacks_Loop when OrderIndex >
                 BoardingOrders.Last_Index;
               if BoardingOrders(OrderIndex) in
                   Defenders.First_Index .. Defenders.Last_Index then
                  DefenderIndex := BoardingOrders(OrderIndex);
                  Riposte :=
                    CharacterAttack
                      (AttackerIndex, DefenderIndex, PlayerAttack);
                  if not EndCombat and Riposte then
                     if Enemy.Ship.Crew(DefenderIndex).Order /= Defend then
                        GiveOrders
                          (Enemy.Ship, DefenderIndex, Defend, 0, False);
                     end if;
                     Riposte :=
                       CharacterAttack
                         (DefenderIndex, AttackerIndex, not PlayerAttack);
                  else
                     Riposte := True;
                  end if;
                  AttackDone := True;
               elsif BoardingOrders(OrderIndex) = -1 then
                  GiveOrders(PlayerShip, AttackerIndex, Rest);
                  BoardingOrders.Delete(Index => OrderIndex);
                  OrderIndex := OrderIndex - 1;
                  AttackDone := True;
               end if;
               OrderIndex := OrderIndex + 1;
            end if;
            if not AttackDone then
               Defenders_Riposte_Loop :
               for Defender in
                 Defenders.First_Index .. Defenders.Last_Index loop
                  if Defenders(Defender).Order = Defend then
                     Riposte :=
                       CharacterAttack(AttackerIndex, Defender, PlayerAttack);
                     if not EndCombat and Riposte then
                        Riposte :=
                          CharacterAttack
                            (Defender, AttackerIndex, not PlayerAttack);
                     else
                        Riposte := True;
                     end if;
                     AttackDone := True;
                     exit Defenders_Riposte_Loop;
                  end if;
               end loop Defenders_Riposte_Loop;
            end if;
            if not AttackDone then
               DefenderIndex :=
                 GetRandom(Defenders.First_Index, Defenders.Last_Index);
               if PlayerAttack then
                  GiveOrders(Enemy.Ship, DefenderIndex, Defend, 0, False);
               else
                  GiveOrders(PlayerShip, DefenderIndex, Defend, 0, False);
               end if;
               Riposte :=
                 CharacterAttack
                   (AttackerIndex => AttackerIndex,
                    DefenderIndex => DefenderIndex,
                    PlayerAttack2 => PlayerAttack);
               if not EndCombat and Riposte then
                  Riposte :=
                    CharacterAttack
                      (AttackerIndex => DefenderIndex,
                       DefenderIndex => AttackerIndex,
                       PlayerAttack2 => not PlayerAttack);
               else
                  Riposte := True;
               end if;
            end if;
            <<End_Of_Attacker_Loop>>
            exit Attackers_Attacks_Loop when EndCombat;
            if Riposte then
               AttackerIndex := AttackerIndex + 1;
            end if;
         end loop Attackers_Attacks_Loop;
         DefenderIndex := Defenders.First_Index;
         Defenders_Attacks_Loop :
         while DefenderIndex <= Defenders.Last_Index loop -- Defenders attacks
            Riposte := True;
            if Defenders(DefenderIndex).Order = Defend then
               Attackers_Riposte_Loop :
               for Attacker in
                 Attackers.First_Index .. Attackers.Last_Index loop
                  if Attackers(Attacker).Order = Boarding then
                     Riposte :=
                       CharacterAttack
                         (DefenderIndex, Attacker, not PlayerAttack);
                     if not EndCombat and Riposte then
                        Riposte :=
                          CharacterAttack
                            (Attacker, DefenderIndex, PlayerAttack);
                     end if;
                     exit Attackers_Riposte_Loop;
                  end if;
               end loop Attackers_Riposte_Loop;
            end if;
            if Riposte then
               DefenderIndex := DefenderIndex + 1;
            end if;
         end loop Defenders_Attacks_Loop;
         if FindMember(Boarding) = 0 then
            UpdateOrders(Enemy.Ship);
         end if;
      end MeleeCombat;
   begin
      if FindItem(Inventory => PlayerShip.Cargo, ItemType => Fuel_Type) =
        0 then
         AddMessage
           ("Ship fall from sky due to lack of fuel.", OtherMessage, RED);
         Death(1, To_Unbounded_String("fall of the ship"), PlayerShip);
         EndCombat := True;
         return;
      end if;
      declare
         ChanceForRun: Integer;
      begin
         TurnNumber := TurnNumber + 1;
         case Enemy.CombatAI is
            when ATTACKER =>
               ChanceForRun := TurnNumber - 120;
            when BERSERKER =>
               ChanceForRun := TurnNumber - 200;
            when DISARMER =>
               ChanceForRun := TurnNumber - 60;
            when others =>
               null;
         end case;
         if ChanceForRun > 1 and then GetRandom(1, 100) < ChanceForRun then
            Enemy.CombatAI := COWARD;
         end if;
      end;
      Pilot_Engineer_Experience_Loop :
      for I in PlayerShip.Crew.Iterate loop
         case PlayerShip.Crew(I).Order is
            when Pilot =>
               PilotIndex := Crew_Container.To_Index(I);
               GainExp(2, Piloting_Skill, PilotIndex);
            when Engineer =>
               EngineerIndex := Crew_Container.To_Index(I);
               GainExp(2, Engineering_Skill, EngineerIndex);
            when others =>
               null;
         end case;
      end loop Pilot_Engineer_Experience_Loop;
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
         EvadeBonus :=
           EvadeBonus +
           GetSkillLevel(PlayerShip.Crew(PilotIndex), Piloting_Skill);
      else
         AccuracyBonus := 20;
         EvadeBonus := -10;
      end if;
      EnemyPilotIndex := FindMember(Pilot, Enemy.Ship.Crew);
      if EnemyPilotIndex > 0 then
         AccuracyBonus :=
           AccuracyBonus -
           GetSkillLevel(Enemy.Ship.Crew(EnemyPilotIndex), Piloting_Skill);
      end if;
      if EngineerIndex > 0 or
        Factions_List(PlayerShip.Crew(1).Faction).Flags.Contains
          (To_Unbounded_String("sentientships")) then
         Message :=
           To_Unbounded_String(ChangeShipSpeed(ShipSpeed'Val(EngineerOrder)));
         if Length(Message) > 0 then
            AddMessage(To_String(Message), OrderMessage, RED);
         end if;
      end if;
      SpeedBonus := 20 - (RealSpeed(PlayerShip) / 100);
      if SpeedBonus < -10 then
         SpeedBonus := -10;
      end if;
      AccuracyBonus := AccuracyBonus + SpeedBonus;
      EvadeBonus := EvadeBonus - SpeedBonus;
      Enemy_Weapon_Loop :
      for I in Enemy.Ship.Modules.Iterate loop
         if Enemy.Ship.Modules(I).Durability = 0 or
           (Enemy.Ship.Modules(I).MType not in GUN | BATTERING_RAM |
                HARPOON_GUN) then
            goto End_Of_Enemy_Weapon_Loop;
         end if;
         if Enemy.Ship.Modules(I).MType in GUN | HARPOON_GUN then
            if Enemy.Ship.Modules(I).MType = GUN and DamageRange > 5000 then
               DamageRange := 5000;
            elsif DamageRange > 2000 then
               DamageRange := 2000;
            end if;
            AmmoIndex2 :=
              (if Enemy.Ship.Modules(I).MType = GUN then
                 Enemy.Ship.Modules(I).AmmoIndex
               else Enemy.Ship.Modules(I).HarpoonIndex);
            if AmmoIndex2 in
                Enemy.Ship.Cargo.First_Index ..
                      Enemy.Ship.Cargo.Last_Index then
               if Items_List(Enemy.Ship.Cargo(AmmoIndex2).ProtoIndex).IType =
                 Items_Types
                   (Modules_List(Enemy.Ship.Modules(I).ProtoIndex).Value) then
                  EnemyAmmoIndex := AmmoIndex2;
               end if;
            end if;
            if EnemyAmmoIndex = 0 then
               Enemy_Ammo_Index_Loop :
               for K in Items_List.Iterate loop
                  if Items_List(K).IType =
                    Items_Types
                      (Modules_List(Enemy.Ship.Modules(I).ProtoIndex)
                         .Value) then
                     Find_Enemy_Ammo_Index_Loop :
                     for J in Enemy.Ship.Cargo.Iterate loop
                        if Enemy.Ship.Cargo(J).ProtoIndex =
                          Objects_Container.Key(K) then
                           EnemyAmmoIndex := Inventory_Container.To_Index(J);
                           exit Find_Enemy_Ammo_Index_Loop;
                        end if;
                     end loop Find_Enemy_Ammo_Index_Loop;
                     exit Enemy_Ammo_Index_Loop when EnemyAmmoIndex > 0;
                  end if;
               end loop Enemy_Ammo_Index_Loop;
            end if;
            if EnemyAmmoIndex = 0 and
              (Enemy.CombatAI in ATTACKER | DISARMER) then
               Enemy.CombatAI := COWARD;
               exit Enemy_Weapon_Loop;
            end if;
         elsif DamageRange > 100 then
            DamageRange := 100;
         end if;
         EnemyWeaponIndex := Modules_Container.To_Index(I);
         <<End_Of_Enemy_Weapon_Loop>>
      end loop Enemy_Weapon_Loop;
      if EnemyWeaponIndex = 0 and (Enemy.CombatAI in ATTACKER | DISARMER) then
         Enemy.CombatAI := COWARD;
      end if;
      case Enemy.CombatAI is
         when BERSERKER =>
            if Enemy.Distance > 10 and Enemy.Ship.Speed /= FULL_SPEED then
               Enemy.Ship.Speed :=
                 ShipSpeed'Val(ShipSpeed'Pos(Enemy.Ship.Speed) + 1);
               AddMessage
                 (To_String(EnemyName) & " increases speed.", CombatMessage);
               EnemyPilotOrder := 1;
            elsif Enemy.Distance <= 10 and Enemy.Ship.Speed = FULL_SPEED then
               Enemy.Ship.Speed :=
                 ShipSpeed'Val(ShipSpeed'Pos(Enemy.Ship.Speed) - 1);
               AddMessage
                 (To_String(EnemyName) & " decreases speed.", CombatMessage);
               EnemyPilotOrder := 2;
            end if;
         when ATTACKER | DISARMER =>
            if Enemy.Distance > DamageRange and
              Enemy.Ship.Speed /= FULL_SPEED then
               Enemy.Ship.Speed :=
                 ShipSpeed'Val(ShipSpeed'Pos(Enemy.Ship.Speed) + 1);
               AddMessage
                 (To_String(EnemyName) & " increases speed.", CombatMessage);
               EnemyPilotOrder := 1;
            elsif Enemy.Distance < DamageRange and
              Enemy.Ship.Speed > QUARTER_SPEED then
               Enemy.Ship.Speed :=
                 ShipSpeed'Val(ShipSpeed'Pos(Enemy.Ship.Speed) - 1);
               AddMessage
                 (To_String(EnemyName) & " decreases speed.", CombatMessage);
               EnemyPilotOrder := 2;
            end if;
         when COWARD =>
            if Enemy.Distance < 15000 and Enemy.Ship.Speed /= FULL_SPEED then
               Enemy.Ship.Speed :=
                 ShipSpeed'Val(ShipSpeed'Pos(Enemy.Ship.Speed) + 1);
               AddMessage
                 (To_String(EnemyName) & " increases speed.", CombatMessage);
            end if;
            EnemyPilotOrder := 4;
         when others =>
            null;
      end case;
      if Enemy.HarpoonDuration > 0 then
         Enemy.Ship.Speed := FULL_STOP;
         AddMessage
           (To_String(EnemyName) & " is stopped by your ship.", CombatMessage);
      elsif Enemy.Ship.Speed = FULL_STOP then
         Enemy.Ship.Speed := QUARTER_SPEED;
      end if;
      if HarpoonDuration > 0 then
         PlayerShip.Speed := FULL_STOP;
         AddMessage("You are stopped by enemy ship.", CombatMessage);
      end if;
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
      DistanceTraveled :=
        (if EnemyPilotOrder < 4 then -(RealSpeed(Enemy.Ship))
         else RealSpeed(Enemy.Ship));
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
              ("You escaped the " & To_String(EnemyName) & ".", CombatMessage);
         else
            AddMessage
              (To_String(EnemyName) & " escaped from you.", CombatMessage);
         end if;
         Kill_Boarding_Party_Loop :
         for I in PlayerShip.Crew.Iterate loop
            if PlayerShip.Crew(I).Order = Boarding then
               Death
                 (Crew_Container.To_Index(I),
                  To_Unbounded_String("enemy crew"), PlayerShip, False);
            end if;
         end loop Kill_Boarding_Party_Loop;
         EndCombat := True;
         return;
      elsif Enemy.Distance < 15000 and Enemy.Distance >= 10000 then
         AccuracyBonus := AccuracyBonus - 10;
         EvadeBonus := EvadeBonus + 10;
         LogMessage("Distance: long", Log.Combat);
      elsif Enemy.Distance < 5000 and Enemy.Distance >= 1000 then
         AccuracyBonus := AccuracyBonus + 10;
         LogMessage("Distance: medium", Log.Combat);
      elsif Enemy.Distance < 1000 then
         AccuracyBonus := AccuracyBonus + 20;
         EvadeBonus := EvadeBonus - 10;
         LogMessage("Distance: short or close", Log.Combat);
      end if;
      Attack(PlayerShip, Enemy.Ship); -- Player attack
      if not EndCombat then
         Attack(Enemy.Ship, PlayerShip); -- Enemy attack
      end if;
      if not EndCombat then
         declare
            HaveBoardingParty: Boolean := False;
         begin
            Check_For_Boarding_Party_Loop :
            for Member of PlayerShip.Crew loop
               if Member.Order = Boarding then
                  HaveBoardingParty := True;
                  exit Check_For_Boarding_Party_Loop;
               end if;
            end loop Check_For_Boarding_Party_Loop;
            Check_For_Enemy_Boarding_Party :
            for Member of Enemy.Ship.Crew loop
               if Member.Order = Boarding then
                  HaveBoardingParty := True;
                  exit Check_For_Enemy_Boarding_Party;
               end if;
            end loop Check_For_Enemy_Boarding_Party;
            if Enemy.HarpoonDuration > 0 or HarpoonDuration > 0 or
              HaveBoardingParty then
               if not EndCombat and
                 Enemy.Ship.Crew.Length >
                   0 then -- Characters combat (player boarding party)
                  MeleeCombat(PlayerShip.Crew, Enemy.Ship.Crew, True);
               end if;
               if not EndCombat and
                 Enemy.Ship.Crew.Length >
                   0 then -- Characters combat (enemy boarding party)
                  MeleeCombat(Enemy.Ship.Crew, PlayerShip.Crew, False);
               end if;
            end if;
         end;
      end if;
      if not EndCombat then
         if Enemy.HarpoonDuration > 0 then
            Enemy.HarpoonDuration := Enemy.HarpoonDuration - 1;
         end if;
         if HarpoonDuration > 0 then
            HarpoonDuration := HarpoonDuration - 1;
         end if;
         if Enemy.HarpoonDuration > 0 or
           HarpoonDuration >
             0 then -- Set defenders/boarding party on player ship
            UpdateOrders(PlayerShip, True);
         end if;
         Update_Game(1, True);
      elsif PlayerShip.Crew(1).Health > 0 then
         declare
            WasBoarded: Boolean := False;
            LootAmount: Integer;
         begin
            if FindMember(Boarding) > 0 then
               WasBoarded := True;
            end if;
            Enemy.Ship.Modules(1).Durability := 0;
            AddMessage(To_String(EnemyName) & " is destroyed!", CombatMessage);
            LootAmount := Enemy.Loot;
            FreeSpace := FreeCargo((0 - LootAmount));
            if FreeSpace < 0 then
               LootAmount := LootAmount + FreeSpace;
            end if;
            if LootAmount > 0 then
               AddMessage
                 ("You looted" & Integer'Image(LootAmount) & " " &
                  To_String(Money_Name) & " from " & To_String(EnemyName) &
                  ".",
                  CombatMessage);
               UpdateCargo(PlayerShip, Money_Index, LootAmount);
            end if;
            FreeSpace := FreeCargo(0);
            if WasBoarded and FreeSpace > 0 then
               Message :=
                 To_Unbounded_String
                   ("Additionally, your boarding party takes from ") &
                 EnemyName & To_Unbounded_String(":");
               Looting_Loop :
               for Item of Enemy.Ship.Cargo loop
                  LootAmount := Item.Amount / 5;
                  FreeSpace := FreeCargo((0 - LootAmount));
                  if FreeSpace < 0 then
                     LootAmount := LootAmount + FreeSpace;
                  end if;
                  if Items_List(Item.ProtoIndex).Price = 0 and
                    Item.ProtoIndex /= Money_Index then
                     LootAmount := 0;
                  end if;
                  if LootAmount > 0 then
                     if Item /= Enemy.Ship.Cargo.First_Element then
                        Message := Message & To_Unbounded_String(",");
                     end if;
                     UpdateCargo(PlayerShip, Item.ProtoIndex, LootAmount);
                     Message :=
                       Message & Positive'Image(LootAmount) &
                       To_Unbounded_String(" ") &
                       Items_List(Item.ProtoIndex).Name;
                     FreeSpace := FreeCargo(0);
                     exit Looting_Loop when Item =
                       Enemy.Ship.Cargo.Last_Element or
                       FreeSpace = 0;
                  end if;
               end loop Looting_Loop;
               AddMessage(To_String(Message) & ".", CombatMessage);
               if CurrentStory.Index /= Null_Unbounded_String then
                  declare
                     Step: constant Step_Data :=
                       (if CurrentStory.CurrentStep = 0 then
                          Stories_List(CurrentStory.Index).StartingStep
                        elsif CurrentStory.CurrentStep > 0 then
                          Stories_List(CurrentStory.Index).Steps
                            (CurrentStory.CurrentStep)
                        else Stories_List(CurrentStory.Index).FinalStep);
                     Tokens: Slice_Set;
                  begin
                     if Step.FinishCondition = LOOT then
                        Create(Tokens, To_String(CurrentStory.Data), ";");
                        if Slice(Tokens, 2) = "any" or
                          Slice(Tokens, 2) = To_String(EnemyShipIndex) then
                           if ProgressStory then
                              case Step.FinishCondition is
                                 when LOOT =>
                                    UpdateCargo
                                      (PlayerShip,
                                       To_Unbounded_String(Slice(Tokens, 1)),
                                       1);
                                 when others =>
                                    null;
                              end case;
                           end if;
                        end if;
                     end if;
                  end;
               else
                  StartStory(FactionName, DROPITEM);
               end if;
            end if;
            Give_Orders_Loop :
            for I in PlayerShip.Crew.Iterate loop
               if PlayerShip.Crew(I).Order = Boarding then
                  GiveOrders(PlayerShip, Crew_Container.To_Index(I), Rest);
               elsif PlayerShip.Crew(I).Order = Defend then
                  GiveOrders(PlayerShip, Crew_Container.To_Index(I), Rest);
               end if;
            end loop Give_Orders_Loop;
         end;
         Enemy.Ship.Speed := FULL_STOP;
         PlayerShip.Speed := OldSpeed;
         if SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex > 0 then
            if Events_List(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex)
                .EType =
              AttackOnBase then
               GainRep(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex, 5);
            end if;
            DeleteEvent(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex);
         end if;
         if SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).MissionIndex > 0
           and then
             AcceptedMissions
               (SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).MissionIndex)
               .MType =
             Destroy
           and then
             ProtoShips_List
               (AcceptedMissions
                  (SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).MissionIndex)
                  .ShipIndex)
               .Name =
             Enemy.Ship.Name then
            UpdateMission
              (SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).MissionIndex);
         end if;
         declare
            LostReputationChance: Positive range 10 .. 40 := 10;
         begin
            if ProtoShips_List(EnemyShipIndex).Owner =
              PlayerShip.Crew(1).Faction then
               LostReputationChance := 40;
            end if;
            if GetRandom(1, 100) < LostReputationChance then
               GainRep(Enemy.Ship.HomeBase, -100);
            end if;
         end;
         UpdateDestroyedShips(Enemy.Ship.Name);
         UpdateGoal(DESTROY, EnemyShipIndex);
         if CurrentGoal.TargetIndex /= Null_Unbounded_String then
            UpdateGoal(DESTROY, ProtoShips_List(EnemyShipIndex).Owner);
         end if;
         if CurrentStory.Index /= Null_Unbounded_String then
            declare
               FinishCondition: constant StepConditionType :=
                 (if CurrentStory.CurrentStep = 0 then
                    Stories_List(CurrentStory.Index).StartingStep
                      .FinishCondition
                  elsif CurrentStory.CurrentStep > 0 then
                    Stories_List(CurrentStory.Index).Steps
                      (CurrentStory.CurrentStep)
                      .FinishCondition
                  else Stories_List(CurrentStory.Index).FinalStep
                      .FinishCondition);
               Tokens: Slice_Set;
            begin
               if FinishCondition /= DESTROYSHIP then
                  return;
               end if;
               Create(Tokens, To_String(CurrentStory.Data), ";");
               if PlayerShip.SkyX = Positive'Value(Slice(Tokens, 1)) and
                 PlayerShip.SkyY = Positive'Value(Slice(Tokens, 2)) and
                 EnemyShipIndex = To_Unbounded_String(Slice(Tokens, 3)) then
                  if not ProgressStory(True) then
                     return;
                  end if;
               end if;
            end;
         end if;
      end if;
   end CombatTurn;

end Combat;
