--    Copyright 2016-2023 Bartek thindil Jasicki
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

with Game; use Game;

-- ****h* ShipModules/ShipModules
-- FUNCTION
-- Provided code to manipulate ship modules prototypes
-- SOURCE
package ShipModules is
-- ****

   -- ****t* ShipModules/ShipModules.Module_Type
   -- FUNCTION
   -- Types of ship modules
   -- SOURCE
   type Module_Type is
     (ANY, ENGINE, CABIN, COCKPIT, TURRET, GUN, CARGO, HULL, ARMOR,
      BATTERING_RAM, ALCHEMY_LAB, FURNACE, WATER_COLLECTOR, WORKSHOP,
      GREENHOUSE, MEDICAL_ROOM, HARPOON_GUN, TRAINING_ROOM) with
      Default_Value => ANY;
      -- ****

      --## rule off REDUCEABLE_SCOPE
      -- ****d* ShipModules/ShipModules.Default_Module_Type
      -- FUNCTION
      -- Default value for modules type
      -- SOURCE
   Default_Module_Type: constant Module_Type := ANY;
   -- ****
   --## rule on REDUCEABLE_SCOPE

   -- ****t* ShipModules/ShipModules.Module_Size
   -- FUNCTION
   -- Range of size of ships' modules
   -- HISTORY
   -- 7.4 - Added
   -- SOURCE
   subtype Module_Size is Positive range 1 .. 10;
   -- ****

   -- ****t* ShipModules/ShipModules.Owners_Amount
   -- FUNCTION
   -- Range of allowed owners for ships' modules
   -- HISTORY
   -- 7.4 - Added
   -- SOURCE
   subtype Owners_Amount is Natural range 0 .. 10;
   -- ****

   -- ****s* ShipModules/ShipModules.Base_Module_Data
   -- FUNCTION
   -- Data structure for prototypes of ship modules
   -- PARAMETERS
   -- Name            - Name of module
   -- M_Type          - Type of module
   -- Weight          - Base weight of module
   -- Value           - For engine base power, depends on module
   -- Max_Value       - For gun, damage, depends on module
   -- Durability      - Base durability of module
   -- Repair_Material - Material needed for repair module
   -- Repair_Skill    - Skill needed for repair module
   -- Price           - Price for module in shipyards
   -- Install_Time    - Amount of minutes needed for install/remove module
   -- Unique          - Did ship can have installed only one that module
   -- Size            - How many space in ship this module take
   -- Description     - Description of module
   -- Max_Owners      - How many owners module can have
   -- Speed           - How fast the gun shoots in combat
   -- Reputation      - Minimal reputation in base needed to buy that module
   -- SOURCE
   type Base_Module_Data is record
      Name: Tiny_String.Bounded_String;
      M_Type: Module_Type;
      Weight: Natural := 0;
      Value: Integer := 0;
      Max_Value: Integer := 0;
      Durability: Integer := 0;
      Repair_Material: Tiny_String.Bounded_String;
      Repair_Skill: SkillsData_Container.Extended_Index;
      Price: Natural := 0;
      Install_Time: Positive := 1;
      Unique: Boolean;
      Size: Module_Size := 1;
      Description: Short_String.Bounded_String;
      Max_Owners: Owners_Amount := 0;
      Speed: Integer := 0;
      Reputation: Reputation_Range;
   end record;
   -- ****

   --## rule off REDUCEABLE_SCOPE
   -- ****d* ShipModules/ShipModules.Empty_Base_Module
   -- FUNCTION
   -- Empty prototype module data
   -- SOURCE
   Empty_Base_Module: constant Base_Module_Data := (others => <>);
   -- ****
   --## rule on REDUCEABLE_SCOPE

   -- ****f* ShipModules/ShipModules.Get_Module_Type
   -- FUNCTION
   -- Get type of selected module (replace all underscore with spaces)
   -- PARAMETERS
   -- Module_Index - Index of module in prototypes list
   -- RETURNS
   -- Formatted type of module
   -- SOURCE
   function Get_Module_Type(Module_Index: Positive) return String with
      Post => Get_Module_Type'Result'Length > 0;
      -- ****

-- Temporary code to interact with Nim

   function Get_Module(Index: Positive) return Base_Module_Data;

   function Get_Modules_Amount return Positive with
      Import => True,
      Convention => C,
      External_Name => "getAdaModulesAmount";

end ShipModules;
