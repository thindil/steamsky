--    Copyright 2016-2020 Bartek thindil Jasicki
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

-- ****h* Steamsky/ShipModules
-- FUNCTION
-- Provided code to manipulate ship modules prototypes
-- SOURCE
package ShipModules is
-- ****

   -- ****t* ShipModules/ModuleType
   -- FUNCTION
   -- Types of ship modules
   -- SOURCE
   type ModuleType is
     (ANY, ENGINE, CABIN, COCKPIT, TURRET, GUN, CARGO, HULL, ARMOR,
      BATTERING_RAM, ALCHEMY_LAB, FURNACE, WATER_COLLECTOR, WORKSHOP,
      GREENHOUSE, MEDICAL_ROOM, HARPOON_GUN, TRAINING_ROOM);
   -- ****

   -- ****t* ShipModules/BaseModule_Data
   -- FUNCTION
   -- Data structure for prototypes of ship modules
   -- PARAMETERS
   -- Name           - Name of module
   -- MType          - Type of module
   -- Weight         - Base weight of module
   -- Value          - For engine base power, depends on module
   -- MaxValue       - For gun, damage, depends on module
   -- Durability     - Base durability of module
   -- RepairMaterial - Material needed for repair module
   -- RepairSkill    - Skill needed for repair module
   -- Price          - Price for module in shipyards
   -- InstallTime    - Amount of minutes needed for install/remove module
   -- Unique         - Did ship can have installed only one that module
   -- Size           - How many space in ship this module take
   -- Description    - Description of module
   -- MaxOwners      - How many owners module can have
   -- Speed          - How fast the gun shoots in combat
   -- Reputation     - Minimal reputation in base needed to buy that module
   -- SOURCE
   type BaseModule_Data is record
      Name: Unbounded_String;
      MType: ModuleType;
      Weight: Natural;
      Value: Integer;
      MaxValue: Integer;
      Durability: Integer;
      RepairMaterial: Unbounded_String;
      RepairSkill: Positive;
      Price: Natural;
      InstallTime: Positive;
      Unique: Boolean;
      Size: Positive;
      Description: Unbounded_String;
      MaxOwners: Natural;
      Speed: Integer;
      Reputation: Integer;
   end record;
   -- ****

   -- ****t* ShipModules/BaseModules_Container
   -- FUNCTION
   -- Used for store prototypes of modules
   -- SOURCE
   package BaseModules_Container is new Hashed_Maps(Unbounded_String,
      BaseModule_Data, Ada.Strings.Unbounded.Hash, "=");
   -- ****

   -- ****v* ShipModules/Modules_List
   -- FUNCTION
   -- List of ship modules available in game
   -- SOURCE
   Modules_List: BaseModules_Container.Map;
   -- ****

   -- ****f* ShipModules/LoadShipModules
   -- FUNCTION
   -- Load modules from files
   -- PARAMETERS
   -- Reader - XML Reader from which ship modules data will be read
   -- SOURCE
   procedure LoadShipModules(Reader: Tree_Reader);
   -- ****

end ShipModules;
