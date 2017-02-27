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

with Ada.Containers.Vectors; use Ada.Containers;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Crew; use Crew;
with Game; use Game;
with Missions; use Missions;
with Bases; use Bases;

package Ships is
    type ShipSpeed is (DOCKED, FULL_STOP, QUARTER_SPEED, HALF_SPEED,
        FULL_SPEED);
    type ShipCombatAi is (NONE, BERSERKER, ATTACKER, COWARD, DISARMER);
    type ShipUpgrade is (NONE, DURABILITY, MAX_VALUE, VALUE);
    type ModuleData is -- Data structure for ship modules
        record
            Name : Unbounded_String; -- Name of module
            ProtoIndex : Positive; -- Index of module prototype
            Weight : Natural; -- Weight of module
            Current_Value : Integer; -- For engine, current power, depends on module
            Max_Value : Integer; -- For engine, max power, depends on module
            Durability : Integer; -- 0 = destroyed
            MaxDurability : Integer; -- Base durability
            Owner : Natural; -- Crew member owner of module (mostly for cabins)
            UpgradeProgress : Integer; -- Progress of module upgrade
            UpgradeAction : ShipUpgrade; -- Type of module upgrade
        end record;
    package Modules_Container is new Vectors(Positive, ModuleData); 
    type CargoData is -- Data structure for ship cargo
        record
            ProtoIndex : Positive; -- Index of prototype
            Amount : Positive; -- Amount of cargo
            Name : Unbounded_String; -- Name of item if different than default
            Durability : Natural; -- Current durability of item
        end record;
    package Cargo_Container is new Vectors(Positive, CargoData);
    package Crew_Container is new Vectors(Positive, Member_Data);
    type ShipRecord is -- Data structure for ships
        record
            Name : Unbounded_String; -- Ship name
            SkyX : Integer; -- X coordinate on sky map
            SkyY : Integer; -- Y coordinate on sky map
            Speed : ShipSpeed; -- Speed of ship
            Modules : Modules_Container.Vector; -- List of ship modules
            Cargo : Cargo_Container.Vector; -- List of ship cargo
            Crew : Crew_Container.Vector; -- List of ship crew
            UpgradeModule : Natural; -- Number of module to upgrade
            DestinationX : Integer; -- Destination X coordinate
            DestinationY : Integer; -- Destination Y coordinate
            RepairModule : Natural; -- Number of module to repair as first
            Missions : Mission_Container.Vector; -- List of accepted missions
            Description : Unbounded_String; -- Description of ship
        end record;
    type ProtoCrewData is -- Data structure for crew prototypes
        record
            Skills : Skills_Container.Vector; -- Skills of crew member
            Order : Crew_Orders; -- Order for crew member
        end record;
    package ProtoCrew_Container is new Vectors(Positive, ProtoCrewData);
    type ProtoShipData is -- Data structure for ship prototypes
        record
            Name : Unbounded_String; -- Prototype name
            Modules : Positive_Container.Vector; -- List of ship modules
            Accuracy : Positive; -- Bonus to hit for ship
            CombatAI : ShipCombatAi; -- Behaviour of ship in combat
            Evasion : Positive; -- Bonus to evade attacks
            LootMin : Positive; -- Minimal amount of loot from ship
            LootMax : Positive; -- Maximum amount of loot from ship
            Perception : Positive; -- Bonus to spot player ship first
            Cargo : Cargo_Container.Vector; -- List of ship cargo
            CombatValue : Positive; -- Combat value of ship (used to generate enemies)
            Crew : ProtoCrew_Container.Vector; -- List of ship crew
            Description : Unbounded_String; -- Description of ship
            Owner : Bases_Owners; -- Faction to which ship belong
        end record;
    package ProtoShips_Container is new Vectors(Positive, ProtoShipData);
    ProtoShips_List : ProtoShips_Container.Vector;
    Enemies_List : ProtoShips_Container.Vector;
    PlayerShip : ShipRecord;
    
    function MoveShip(ShipIndex, X, Y : Integer) return Natural; -- Move selected ship
    procedure DockShip(Docking : Boolean); -- Dock/Undock ship at base
    procedure ChangeShipSpeed(SpeedValue : ShipSpeed; ShowInfo : Boolean := True); -- Change speed of ship
    procedure UpdateModule(Ship : in out ShipRecord; ModuleIndex : Positive; Field : String; Value : String); -- Update selected module
    function CreateShip(ProtoIndex : Positive; Name : Unbounded_String; X, Y:
        Integer; Speed : ShipSpeed; Enemy : Boolean := False) return ShipRecord; -- Create new ship
    function LoadShips return Boolean; -- Load ships from file, returns False if file not found
    function CountShipWeight(Ship : ShipRecord) return Positive; -- Count weight of ship (with modules and cargo)
    function RealSpeed(Ship : ShipRecord) return Natural; -- Return real ship speed in meters per minute
    procedure StartUpgrading(ModuleIndex, UpgradeType : Positive); -- Set upgrading order
    procedure UpgradeShip(Minutes : Positive); -- Upgrade selected module on ship
    procedure RepairShip(Minutes : Positive); -- Repair ship modules
    function GenerateShipName(Owner : Bases_Owners := Any) return Unbounded_String; -- Generate random name for ship

end Ships;
