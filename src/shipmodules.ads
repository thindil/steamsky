--    Copyright 2016-2019 Bartek thindil Jasicki
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Containers.Hashed_Maps; use Ada.Containers;
with DOM.Readers; use DOM.Readers;

package ShipModules is

   type ModuleType is
     (ANY, ENGINE, CABIN, COCKPIT, TURRET, GUN, CARGO, HULL, ARMOR,
      BATTERING_RAM, ALCHEMY_LAB, FURNACE, WATER_COLLECTOR, WORKSHOP,
      GREENHOUSE, MEDICAL_ROOM, HARPOON_GUN, TRAINING_ROOM);
   -- Data structure for prototypes of ship modules
   type BaseModule_Data is record
      Name: Unbounded_String; -- Name of module
      MType: ModuleType; -- Type of module
      Weight: Natural; -- Base weight of module
      Value: Integer; -- For engine base power, depends on module
      MaxValue: Integer; -- For gun, damage, depends on module
      Durability: Integer; -- Base durability of module
      RepairMaterial: Unbounded_String; -- Material needed for repair module
      RepairSkill: Positive; -- Skill needed for repair module
      Price: Natural; -- Price for module in shipyards
      InstallTime: Positive; -- Amount of minutes needed for install/remove module
      Unique: Boolean; -- Did ship can have installed only one that module
      Size: Natural; -- How many space in ship this module take
      Description: Unbounded_String; -- Description of module
      MaxOwners: Natural; -- How many owners module can have
   end record;
   package BaseModules_Container is new Hashed_Maps(Unbounded_String,
      BaseModule_Data, Ada.Strings.Unbounded.Hash, "=");
   -- List of ship modules available in game
   Modules_List: BaseModules_Container.Map;

   -- Load modules from files
   procedure LoadShipModules(Reader: Tree_Reader);

end ShipModules;
