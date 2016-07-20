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

with Ada.Numerics.Discrete_Random;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Bases; use Bases;
with Maps; use Maps;
with Ships; use Ships;
with Crew; use Crew;

package body Game is

    procedure NewGame is
        type Rand_Range is range 1..1024;
        package Rand_Int is new Ada.Numerics.Discrete_Random(Rand_Range);
        Generator : Rand_Int.Generator;
        PosX, PosY : Rand_Range;
        RandomBase : Rand_Range;
        ShipModules : Modules_Container.Vector;
        ShipCargo : Cargo_Container.Vector; 
        ShipCrew : Crew_Container.Vector;
    begin
        -- Generate world
        Rand_Int.Reset(Generator);
        SkyMap := (others => (others => (BaseIndex => 0)));
        for I in Rand_Range loop
            PosX := Rand_Int.Random(Generator);
            PosY := Rand_Int.Random(Generator);
            SkyMap(Integer(PosX), Integer(PosY)) := (BaseIndex => Integer(I));
            SkyBases(Integer(I)) := (Name => To_Unbounded_String("Base " & Rand_Range'Image(I)),
                Visited => False, SkyX => Integer(PosX), SkyY => Integer(PosY));
        end loop;
        -- Place player ship in random base
        RandomBase := Rand_Int.Random(Generator);
        -- Create player ship (with modules and cargo)
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
        ShipCargo.Append(New_Item => (Name => To_Unbounded_String("Charcollum"),
            Weight => 2000, Amount => 2000));
        ShipCargo.Append(New_Item => (Name => To_Unbounded_String("Basic rations"),
            Weight => 100, Amount => 100));
        ShipCargo.Append(New_Item => (Name => To_Unbounded_String("Water"),
            Weight => 200, Amount => 200));
        ShipCargo.Append(New_Item => (Name => To_Unbounded_String("20mm ammo"), 
            Weight => 500, Amount => 500));
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

end Game;
