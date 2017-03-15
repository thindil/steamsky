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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors; use Ada.Containers;

package ShipModules is

   type ModuleType is
     (ANY,
      ENGINE,
      CABIN,
      COCKPIT,
      TURRET,
      GUN,
      CARGO,
      HULL,
      ARMOR,
      BATTERING_RAM,
      ALCHEMY_LAB,
      FURNACE,
      WATER_COLLECTOR,
      WORKSHOP,
      GREENHOUSE,
      MEDICAL_ROOM);
   type BaseModule_Data is -- Data structure for prototypes of ship modules
   record
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
   end record;
   package BaseModules_Container is new Vectors(Positive, BaseModule_Data);
   Modules_List: BaseModules_Container
     .Vector; -- Lost of ship modules available in game

   function LoadShipModules
     return Boolean; -- Load modules from file, returns False if file not found

end ShipModules;
