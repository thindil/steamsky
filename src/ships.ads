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

with Terminal_Interface.Curses; use Terminal_Interface.Curses;
with Ada.Containers.Vectors; use Ada.Containers;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Crew; use Crew;
with Game; use Game;

package Ships is
    type ShipSpeed is (DOCKED, FULL_STOP, QUARTER_SPEED, HALF_SPEED,
        FULL_SPEED);
    type ModuleType is (ENGINE, CABIN, COCPIT, TURRET, GUN, CARGO, ALCHEMY_LAB,
        HULL, ARMOR);
    type ModuleData is -- Data structure for ship modules
        record
            Name : Unbounded_String; -- Name of module
            MType : ModuleType; -- Type of module
            Weight : Natural; -- Weight of module
            Current_Value : Integer; -- For engine, current power, depends on module
            Max_Value : Integer; -- For engine, max power, depends on module
            Durability : Integer; -- 0 = destroyed
            MaxDurability : Integer; -- Base durability
        end record;
    package Modules_Container is new Vectors(Positive, ModuleData); 
    type CargoData is -- Data structure for ship cargo
        record
            ProtoIndex : Positive; -- Index of prototype
            Amount : Positive; -- Amount of cargo
        end record;
    package Cargo_Container is new Vectors(Positive, CargoData);
    package Crew_Container is new Vectors(Positive, Member_Data);
    type ShipRecord is -- Data structure for ships
        record
            Name : Unbounded_String; -- Ship name
            SkyX : Integer; -- X coordinate on sky map
            SkyY : Integer; -- Y coordinate on sky map
            Speed : ShipSpeed; -- Speed of ship
            Craft : Natural; -- Recipe number to craft on ship
            Modules : Modules_Container.Vector; -- List of ship modules
            Cargo : Cargo_Container.Vector; -- List of ship cargo
            Crew : Crew_Container.Vector; -- List of ship crew
        end record;
    PlayerShip : ShipRecord;
    
    procedure MoveShip(ShipIndex, X, Y : Integer); -- Move selected ship
    procedure DockShip(Docking : Boolean); -- Dock/Undock ship at base
    procedure ChangeShipSpeed(SpeedValue : ShipSpeed); -- Change speed of ship
    procedure UpdateCargo(ProtoIndex : Positive; Amount : Integer); -- Update selected item in ship cargo
    procedure UpdateModule(ModuleIndex : Positive; Field : String; Value : Integer); -- Update selected module in ship
    function FreeCargo(Amount : Integer) return Integer; -- Return available space in cargo after adding/extracting Amount
    procedure ShowShipInfo; -- Show informations about ship status and cargo
    function ShipInfoKeys(Key : Key_Code) return GameStates; -- Handle keys in ship info menu

end Ships;
