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
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Bases; use Bases;
with Maps; use Maps;
with Ships; use Ships;
with Crew; use Crew;

package body Game is

    procedure NewGame is
        type Rand_Range is range 1..1024;
        type Bases_Range is range 0..2;
        package Rand_Int is new Discrete_Random(Rand_Range);
        package Rand_Base is new Discrete_Random(Bases_Range);
        Generator : Rand_Int.Generator;
        Generator2 : Rand_Base.Generator;
        PosX, PosY : Rand_Range;
        RandomBase : Rand_Range;
        BaseType : Bases_Range;
        ShipModules : Modules_Container.Vector;
        ShipCargo : Cargo_Container.Vector; 
        ShipCrew : Crew_Container.Vector;
        Goods : Goods_Array;
    begin
        -- Set Game time
        GameDate := (Year => 1600, Month => 3, Day => 1, Hour => 8, Minutes => 0);
        -- Generate world
        Rand_Int.Reset(Generator);
        Rand_Base.Reset(Generator2);
        SkyMap := (others => (others => (BaseIndex => 0)));
        for I in Rand_Range loop
            PosX := Rand_Int.Random(Generator);
            PosY := Rand_Int.Random(Generator);
            BaseType := Rand_Base.Random(Generator2);
            case BaseType is
                when Bases_Types'Pos(Industrial) =>
                    Goods := ((To_Unbounded_String("Basic rations"), 1, 2,
                    False), (To_Unbounded_String("Water"), 1, 2, False),
                    (To_Unbounded_String("20mm ammo"), 1, 3, True));
                when Bases_Types'Pos(Agricultural) =>
                    Goods := ((To_Unbounded_String("Basic rations"), 1, 1,
                    True), (To_Unbounded_String("Water"), 1, 1, True),
                    (To_Unbounded_String("20mm ammo"), 1, 5, False));
                when Bases_Types'Pos(Refinery) =>
                    Goods := ((To_Unbounded_String("Basic rations"), 1, 2,
                    False), (To_Unbounded_String("Water"), 1, 2, False),
                    (To_Unbounded_String("20mm ammo"), 1, 5, False));
            end case;
            SkyMap(Integer(PosX), Integer(PosY)) := (BaseIndex => Integer(I));
            SkyBases(Integer(I)) := (Name => To_Unbounded_String("Base" & Rand_Range'Image(I)),
                Visited => False, SkyX => Integer(PosX), SkyY => Integer(PosY),
                BaseType => Bases_Types'Val(BaseType), Goods => Goods);
        end loop;
        -- Place player ship in random base
        RandomBase := Rand_Int.Random(Generator);
        -- Create player ship with modules
        ShipModules.Append(New_Item => (Name => To_Unbounded_String("Hawk hull"), 
            MType => HULL, Weight => 8000, Current_Value => 10,
            Max_Value => 10, Durability => 500, MaxDurability => 500));
        ShipModules.Append(New_Item => (Name => To_Unbounded_String("Small engine"), 
            MType => ENGINE, Weight => 1000, Current_Value => 0,
            Max_Value => 2000, Durability => 100, MaxDurability => 100));
        ShipModules.Append(New_Item => (Name => To_Unbounded_String("Your Cabin"), 
            MTYPE => CABIN, Weight => 200, Current_Value => 20, Max_Value =>
            20, Durability => 100, MaxDurability => 100));
        ShipModules.Append(New_Item => (Name => To_Unbounded_String("Pilot Cabin"), 
            MTYPE => CABIN, Weight => 200, Current_Value => 20, Max_Value =>
            20, Durability => 100, MaxDurability => 100));
        ShipModules.Append(New_Item => (Name => To_Unbounded_String("Engineer Cabin"), 
            MTYPE => CABIN, Weight => 200, Current_Value => 20, Max_Value =>
            20, Durability => 100, MaxDurability => 100));
        ShipModules.Append(New_Item => (Name => To_Unbounded_String("Gunner Cabin"), 
            MTYPE => CABIN, Weight => 200, Current_Value => 20, Max_Value =>
            20, Durability => 100, MaxDurability => 100));
        ShipModules.Append(New_Item => (Name => To_Unbounded_String("Cocpit"), 
            MTYPE => COCPIT, Weight => 200, Current_Value => 0, Max_Value =>
            0, Durability => 100, MaxDurability => 100));
        ShipModules.Append(New_Item => (Name => To_Unbounded_String("Alchemy Lab"), 
            MTYPE => ALCHEMY_LAB, Weight => 400, Current_Value => 0, Max_Value =>
            0, Durability => 100, MaxDurability => 100));
        ShipModules.Append(New_Item => (Name => To_Unbounded_String("Cargo space"), 
            MTYPE => CARGO, Weight => 0, Current_Value => 0, Max_Value =>
            5000, Durability => 10, MaxDurability => 10));
        ShipModules.Append(New_Item => (Name => To_Unbounded_String("Cargo space"), 
            MTYPE => CARGO, Weight => 0, Current_Value => 0, Max_Value =>
            5000, Durability => 10, MaxDurability => 10));
        ShipModules.Append(New_Item => (Name => To_Unbounded_String("Turret"), 
            MTYPE => TURRET, Weight => 50, Current_Value => 0, Max_Value =>
            0, Durability => 100, MaxDurability => 100));
        ShipModules.Append(New_Item => (Name => To_Unbounded_String("20mm gun"), 
            MTYPE => GUN, Weight => 30, Current_Value => 1, Max_Value =>
            10, Durability => 20, MaxDurability => 20));
        -- Add cargo to ship
        ShipCargo.Append(New_Item => (Name => To_Unbounded_String("Charcollum"),
            Weight => 2000, Amount => 2000));
        ShipCargo.Append(New_Item => (Name => To_Unbounded_String("Basic rations"),
            Weight => 100, Amount => 100));
        ShipCargo.Append(New_Item => (Name => To_Unbounded_String("Water"),
            Weight => 200, Amount => 200));
        ShipCargo.Append(New_Item => (Name => To_Unbounded_String("20mm ammo"), 
            Weight => 500, Amount => 500));
        -- Add crew to ship
        ShipCrew.Append(New_Item => (Name => To_Unbounded_String("You"),
            Health => 100, Tired => 0, Skills => ((0, 0), (0, 0), (0, 0), (1,0)), 
            Hunger => 0, Thirst => 0, Order => Duty)); 
        ShipCrew.Append(New_Item => (Name => To_Unbounded_String("Pilot"),
            Health => 100, Tired => 0, Skills => ((1, 0), (0, 0), (0, 0), (0,0)), 
            Hunger => 0, Thirst => 0, Order => Pilot)); 
        ShipCrew.Append(New_Item => (Name => To_Unbounded_String("Engineer"),
            Health => 100, Tired => 0, Skills => ((0, 0), (1, 0), (0, 0), (0,0)), 
            Hunger => 0, Thirst => 0, Order => Engineer)); 
        ShipCrew.Append(New_Item => (Name => To_Unbounded_String("Gunner"),
            Health => 100, Tired => 0, Skills => ((0, 0), (0, 0), (1, 0), (0, 0)),
            Hunger => 0, Thirst => 0, Order => Gunner)); 
        PlayerShip := (SkyX => SkyBases(Integer(RandomBase)).SkyX, SkyY =>
            SkyBases(Integer(RandomBase)).SkyY, Speed => DOCKED, Modules =>
            ShipModules, Cargo => ShipCargo, Crew => ShipCrew);
        SkyBases(Integer(RandomBase)).Visited := True;
    end NewGame;

    procedure UpdateGame(Minutes : Positive) is
        AddedHours, AddedMinutes : Natural;
    begin
        -- Update time
        AddedMinutes := Minutes rem 60;
        AddedHours := Minutes / 60;
        GameDate.Minutes := GameDate.Minutes + AddedMinutes;
        if GameDate.Minutes > 59 then
            GameDate.Minutes := GameDate.Minutes - 60;
            GameDate.Hour := GameDate.Hour + 1;
        end if;
        GameDate.Hour := GameDate.Hour + AddedHours;
        if GameDate.Hour > 23 then
            GameDate.Hour := GameDate.Hour - 24;
            GameDate.Day := GameDate.Day + 1;
        end if;
        if GameDate.Day > 30 then
            GameDate.Day := 1;
            GameDate.Month := GameDate.Month + 1;
        end if;
        if GameDate.Month > 12 then
            GameDate.Month := 1;
            GameDate.Year := GameDate.Year + 1;
        end if;
    end UpdateGame;

end Game;
