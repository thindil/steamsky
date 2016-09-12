--    Copyright 2016 Bartek thindil Jasicki
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

with Ada.Numerics.Discrete_Random; use Ada.Numerics;
with Crew; use Crew;
with UserInterface; use UserInterface;
with Messages; use Messages;
with ShipModules; use ShipModules;
with Game; use Game;

package body Combat is
    
    procedure StartCombat(EnemyIndex : Positive) is
        EnemyShip : ShipRecord;
    begin
        EnemyShip := CreateShip(EnemyIndex, Null_Unbounded_String,
            PlayerShip.SkyX, PlayerShip.SkyY, HALF_SPEED, True);
            Enemy := (Ship => EnemyShip, DamageRange => Enemies_List.Element(EnemyIndex).DamageRange, Accuracy
            => Enemies_List.Element(EnemyIndex).Accuracy, Distance => 10000,
            CombatAI => Enemies_List.Element(EnemyIndex).CombatAI, Evasion =>
            Enemies_List.Element(EnemyIndex).Evasion);
        PilotOrder := 2;
        EngineerOrder := 3;
        GunnerOrder := 1;
        EndCombat := False;
        EnemyName := Enemy.Ship.Name;
        MessagesStarts := MessagesAmount(CombatMessage);
    end StartCombat;

    procedure CombatTurn is
        type Roll_Range is range 1..100;
        subtype PlayerMod_Range is Positive range PlayerShip.Modules.First_Index..PlayerShip.Modules.Last_Index;
        subtype EnemyMod_Range is Positive range Enemy.Ship.Modules.First_Index..Enemy.Ship.Modules.Last_Index;
        package Rand_Roll is new Discrete_Random(Roll_Range);
        package PlayerMod_Roll is new Discrete_Random(PlayerMod_Range);
        package EnemyMod_Roll is new Discrete_Random(EnemyMod_Range);
        Generator : Rand_Roll.Generator;
        Generator2 : PlayerMod_Roll.Generator;
        Generator3 : EnemyMod_Roll.Generator;
        AccuracyBonus, EvadeBonus : Integer := 0;
        PilotIndex, EngineerIndex, GunnerIndex, WeaponIndex, AmmoIndex,
            ArmorIndex, EnemyWeaponIndex, EnemyArmorIndex : Natural := 0;
        Shoots : Integer;
        HitChance : Integer;
        ShootMessage : Unbounded_String;
        HitLocation : Integer;
        LootAmount : Integer;
        FreeSpace : Integer := 0;
        DistanceTraveled : Integer;
        EnemyPilotOrder : Positive := 2;
        DeathReason : Unbounded_String;
    begin
        Rand_Roll.Reset(Generator);
        PlayerMod_Roll.Reset(Generator2);
        EnemyMod_Roll.Reset(Generator3);
        for I in PlayerShip.Crew.First_Index..PlayerShip.Crew.Last_Index loop
            case PlayerShip.Crew.Element(I).Order is
                when Pilot =>
                    PilotIndex := I;
                when Engineer =>
                    EngineerIndex := I;
                when Gunner =>
                    GunnerIndex := I;
                when others =>
                    null;
            end case;
        end loop;
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
        else
            AccuracyBonus := 20;
            EvadeBonus := -10;
        end if;
        if EngineerIndex > 0 then
            case EngineerOrder is
                when 1 =>
                    AccuracyBonus := AccuracyBonus + 40;
                    EvadeBonus := EvadeBonus - 40;
                when 2 =>
                    AccuracyBonus := AccuracyBonus + 10;
                    EvadeBonus := EvadeBonus - 10;
                when 4 =>
                    AccuracyBonus := AccuracyBonus - 10;
                    EvadeBonus := EvadeBonus + 10;
                when others =>
                    null;
            end case;
            ChangeShipSpeed(ShipSpeed'Val(EngineerOrder));
        else
            AccuracyBonus := AccuracyBonus + 40;
            EvadeBonus := EvadeBonus - 40;
            if PlayerShip.Speed /= FULL_STOP then
                ChangeShipSpeed(FULL_STOP);
            end if;
        end if;
        for I in Enemy.Ship.Modules.First_Index..Enemy.Ship.Modules.Last_Index loop
            if Enemy.Ship.Modules.Element(I).Durability > 0 then
                case Modules_List(Enemy.Ship.Modules.Element(I).ProtoIndex).MType is
                    when GUN | BATTERING_RAM =>
                        EnemyWeaponIndex := I;
                    when ARMOR =>
                        EnemyArmorIndex := I;
                    when others =>
                        null;
                end case;
            end if;
        end loop;
        if EnemyWeaponIndex = 0 and Enemy.CombatAI = ATTACKER then
            Enemy.CombatAI := COWARD;
        end if;
        case Enemy.CombatAI is
            when BERSERKER =>
                if Enemy.Distance > 10 and Enemy.Ship.Speed /= FULL_SPEED then
                    Enemy.Ship.Speed := ShipSpeed'Val(ShipSpeed'Pos(Enemy.Ship.Speed) + 1);
                    AddMessage(To_String(EnemyName) & " increases speed.", CombatMessage);
                    EnemyPilotOrder := 1;
                elsif Enemy.Distance <= 10 and Enemy.Ship.Speed /= HALF_SPEED then
                    Enemy.Ship.Speed := ShipSpeed'Val(ShipSpeed'Pos(Enemy.Ship.Speed) - 1);
                    AddMessage(To_String(EnemyName) & " decreases speed.", CombatMessage);
                    EnemyPilotOrder := 2;
                end if;
            when ATTACKER =>
                if Enemy.Distance > Enemy.DamageRange  and Enemy.Ship.Speed /= FULL_SPEED then
                    Enemy.Ship.Speed := ShipSpeed'Val(ShipSpeed'Pos(Enemy.Ship.Speed) + 1);
                    AddMessage(To_String(EnemyName) & " increases speed.", CombatMessage);
                    EnemyPilotOrder := 1;
                elsif Enemy.Distance < Enemy.DamageRange and Enemy.Ship.Speed /= QUARTER_SPEED then
                    Enemy.Ship.Speed := ShipSpeed'Val(ShipSpeed'Pos(Enemy.Ship.Speed) - 1);
                    AddMessage(To_String(EnemyName) & " decreases speed.", CombatMessage);
                    EnemyPilotOrder := 2;
                end if;
            when COWARD =>
                if Enemy.Distance < 15000 and Enemy.Ship.Speed /= FULL_SPEED then
                    Enemy.Ship.Speed := ShipSpeed'Val(ShipSpeed'Pos(Enemy.Ship.Speed) + 1);
                    AddMessage(To_String(EnemyName) & " increases speed.", CombatMessage);
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
        case Enemy.Ship.Speed is
            when FULL_STOP =>
                AccuracyBonus := AccuracyBonus + 40;
                EvadeBonus := EvadeBonus - 40;
            when QUARTER_SPEED =>
                AccuracyBonus := AccuracyBonus + 10;
                EvadeBonus := EvadeBonus - 10;
            when FULL_SPEED =>
                AccuracyBonus := AccuracyBonus - 10;
                EvadeBonus := EvadeBonus + 10;
            when others =>
                null;
        end case;
        if EnemyPilotOrder < 4 then
            DistanceTraveled := 0 - RealSpeed(Enemy.Ship);
        else
            DistanceTraveled := RealSpeed(Enemy.Ship);
        end if;
        if PilotIndex > 0 and EngineerIndex > 0 then
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
        end if;
        Enemy.Distance := Enemy.Distance + DistanceTraveled;
        if Enemy.Distance < 10 then
            Enemy.Distance := 10;
        end if;
        if Enemy.Distance >= 15000 then
            if PilotOrder = 4 then
                AddMessage("You escaped from " & To_String(EnemyName) & ".", CombatMessage);
            else
                AddMessage(To_String(EnemyName) & " escaped from you.", CombatMessage);
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
        for I in PlayerShip.Modules.First_Index..PlayerShip.Modules.Last_Index loop
            if PlayerShip.Modules.Element(I).Durability > 0 then
                case Modules_List(PlayerShip.Modules.Element(I).ProtoIndex).MType is
                    when GUN | BATTERING_RAM =>
                        WeaponIndex := I;
                    when ARMOR =>
                        ArmorIndex := I;
                    when others =>
                        null;
                end case;
            end if;
        end loop;
        if GunnerIndex = 0 then
            Shoots := -1;
        else
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
        if WeaponIndex = 0 then
            Shoots := -3;
        else
            for I in PlayerShip.Cargo.First_Index..PlayerShip.Cargo.Last_Index loop
                if PlayerShip.Cargo.Element(I).ProtoIndex = PlayerShip.Modules.Element(WeaponIndex).Current_Value then
                    AmmoIndex := I;
                    exit;
                end if;
            end loop;
        end if;
        if AmmoIndex = 0 then
            if WeaponIndex > 0 then
                Shoots := -2;
            end if;
        elsif PlayerShip.Cargo.Element(AmmoIndex).Amount < Shoots then
            Shoots := PlayerShip.Cargo.Element(AmmoIndex).Amount;
        end if;
        if Shoots = -3 then
            AddMessage("You don't have weapon to attack!", CombatMessage);
        elsif Shoots = -2 then
            AddMessage("You don't have ammo to your gun!", CombatMessage);
        elsif Shoots > 0 then -- Player attacks
            HitChance := AccuracyBonus + PlayerShip.Crew.Element(GunnerIndex).Skills(3, 1) - Enemy.Evasion;
            for I in 1..Shoots loop
                ShootMessage := PlayerShip.Crew.Element(GunnerIndex).Name & To_Unbounded_String(" shoots to ") & 
                    EnemyName;
                if Integer(Rand_Roll.Random(Generator)) + HitChance > Integer(Rand_Roll.Random(Generator)) then
                    ShootMessage := ShootMessage & To_Unbounded_String(" and hit in ");
                    if EnemyArmorIndex > 0 then
                        HitLocation := EnemyArmorIndex;
                    else
                        if GunnerOrder > 3 and GunnerOrder < 7 then -- aim for part of enemy ship
                            HitLocation := 1;
                            for J in Enemy.Ship.Modules.First_Index..Enemy.Ship.Modules.Last_Index loop
                                if (GunnerOrder = 4 and Modules_List.Element(Enemy.Ship.Modules.Element(J).ProtoIndex).MType = ENGINE) or
                                    (GunnerOrder = 5 and (Modules_List.Element(Enemy.Ship.Modules.Element(J).ProtoIndex).MType = GUN or 
                                    Modules_List.Element(Enemy.Ship.Modules.Element(J).ProtoIndex).MType = BATTERING_RAM)) or
                                    (GunnerOrder = 6 and Modules_List.Element(Enemy.Ship.Modules.Element(J).ProtoIndex).MType = HULL) then
                                        HitLocation := J;
                                        exit;
                                end if;
                            end loop;
                        else
                            HitLocation := Integer(EnemyMod_Roll.Random(Generator3));
                        end if;
                        while Enemy.Ship.Modules.Element(HitLocation).Durability = 0 loop
                            HitLocation := HitLocation - 1;
                        end loop;
                    end if;
                    ShootMessage := ShootMessage & Enemy.Ship.Modules.Element(HitLocation).Name &
                        To_Unbounded_String(".");
                    UpdateModule(Enemy.Ship, HitLocation, "Durability", 
                        Integer'Image(0 - PlayerShip.Modules.Element(WeaponIndex).Max_Value));
                    if Enemy.Ship.Modules.Element(HitLocation).Durability = 0 then
                        case Modules_List.Element(Enemy.Ship.Modules.Element(HitLocation).ProtoIndex).MType is
                            when HULL | ENGINE =>
                                EndCombat := True;
                            when TURRET =>
                                UpdateModule(Enemy.Ship, Enemy.Ship.Modules.Element(HitLocation).Current_Value,
                                    "Durability", "-1000");
                            when others =>
                                null;
                        end case;
                    end if;
                else
                    ShootMessage := ShootMessage & To_Unbounded_String(" and miss.");
                end if;
                AddMessage(To_String(ShootMessage), CombatMessage);
                if EndCombat then
                    Shoots := I;
                    UpdateModule(Enemy.Ship, 1, "Durability", Integer'Image(0 - Enemy.Ship.Modules.Element(1).MaxDurability));
                    AddMessage(To_String(EnemyName) & " is destroyed!", CombatMessage);
                    LootAmount := (Integer(Rand_Roll.Random(Generator))) * 2;
                    FreeSpace := FreeCargo((0 - LootAmount));
                    if FreeSpace < 0 then
                        LootAmount := LootAmount + FreeSpace;
                    end if;
                    if LootAmount > 0 then
                        AddMessage("You looted" & Integer'Image(LootAmount) & " Charcollum from " & 
                            To_String(EnemyName) & ".", CombatMessage);
                        UpdateCargo(1, LootAmount);
                    end if;
                    exit;
                end if;
            end loop;
            UpdateCargo(PlayerShip.Cargo.Element(AmmoIndex).ProtoIndex, (0 - Shoots));
            GainExp(Shoots, 3, GunnerIndex);
        end if;
        if not EndCombat and Enemy.Distance <= Enemy.DamageRange and EnemyWeaponIndex > 0 then -- Enemy attack
            HitChance := Enemy.Accuracy - EvadeBonus;
            ShootMessage := EnemyName & To_Unbounded_String(" attacks you and ");
            if Integer(Rand_Roll.Random(Generator)) + HitChance > Integer(Rand_Roll.Random(Generator)) then
                ShootMessage := ShootMessage & To_Unbounded_String("hits in ");
                if ArmorIndex > 0 then
                    UpdateModule(PlayerShip, ArmorIndex, "Durability", Integer'Image(0 - 
                        Enemy.Ship.Modules(EnemyWeaponIndex).Max_Value));
                    ShootMessage := ShootMessage & To_Unbounded_String("armor.");
                else
                    HitLocation := Integer(PlayerMod_Roll.Random(Generator2));
                    while PlayerShip.Modules.Element(HitLocation).Durability = 0 loop
                        HitLocation := HitLocation - 1;
                    end loop;
                    ShootMessage := ShootMessage & PlayerShip.Modules.Element(HitLocation).Name &
                        To_Unbounded_String(".");
                    UpdateModule(PlayerShip, HitLocation, "Durability", Integer'Image(0 - 
                        Enemy.Ship.Modules(EnemyWeaponIndex).Max_Value));
                    if PlayerShip.Modules.Element(HitLocation).Durability = 0 then
                        DeathReason := To_Unbounded_String("enemy fire");
                        case Modules_List.Element(PlayerShip.Modules.Element(HitLocation).ProtoIndex).MType is
                            when HULL | ENGINE =>
                                AddMessage(To_String(ShootMessage), CombatMessage);
                                DeathReason := To_Unbounded_String("ship explosion");
                                Death(1, DeathReason);
                            when TURRET =>
                                UpdateModule(PlayerShip, PlayerShip.Modules.Element(HitLocation).Current_Value, 
                                    "Durability", "-1000");
                            when others =>
                                null;
                        end case;
                        if PlayerShip.Modules.Element(HitLocation).Owner > 0 then
                            if (Modules_List.Element(PlayerShip.Modules.Element(HitLocation).ProtoIndex).MType = CABIN and
                            PlayerShip.Crew.Element(PlayerShip.Modules.Element(HitLocation).Owner).Order = Rest) or
                            (Modules_List.Element(PlayerShip.Modules.Element(HitLocation).ProtoIndex).MType /= CABIN) then
                                Death(PlayerShip.Modules.Element(HitLocation).Owner, DeathReason);
                            end if;
                        end if;
                    end if;
                    if PlayerShip.Crew.Element(1).Health = 0 then -- player is dead
                        EndCombat := True;
                        DrawGame(Combat_State);
                        return;
                    end if;
                end if;
            else
                ShootMessage := ShootMessage & To_Unbounded_String("miss.");
            end if;
            AddMessage(To_String(ShootMessage), CombatMessage);
        end if;
        UpdateGame(1);
    end CombatTurn;

end Combat;
