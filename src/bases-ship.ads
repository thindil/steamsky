--    Copyright 2017-2019 Bartek thindil Jasicki
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

-- ****h* Steamsky/Bases.Ship
-- FUNCTION
-- Provides code to manipulate player ship in sky bases
-- SOURCE
package Bases.Ship is
-- ****

   -- ****e* Bases.Ship/BasesShip_Nothing_To_Repair
   -- FUNCTION
   -- Raised when there is nothing to repair
   -- SOURCE
   BasesShip_Nothing_To_Repair: exception;
   -- ****
   -- ****e* Bases.Ship/BasesShip_Unique_Module
   -- FUNCTION
   -- Raised when player try install another same unique module
   -- SOURCE
   BasesShip_Unique_Module: exception;
   -- ****
   -- ****e* Bases.Ship/BasesShip_Installation_Error
   -- FUNCTION
   -- Raised when problems with installing ship module occurs
   -- SOURCE
   BasesShip_Installation_Error: exception;
   -- ****
   -- ****e* Bases.Ship/BasesShip_Removing_Error
   -- FUNCTION
   -- Raised when problems with removing ship module occurs
   -- SOURCE
   BasesShip_Removing_Error: exception;
   -- ****

   -- ****f* Bases.Ship/RepairShip
   -- FUNCTION
   -- Repairs player ship in bases
   -- PARAMETERS
   -- ModuleIndex - Index of player ship module to repair or 0 to repair whole
   --               ship
   -- SOURCE
   procedure RepairShip(ModuleIndex: Integer);
   -- ****
-- ****f* Bases.Ship/UpgradeShip
-- FUNCTION
-- Install or remove modules on player ship
-- PARAMETERS
-- Install     - If True, perform module installation on player ship. On False,
   --           remove module
   -- ModuleIndex - Index of prototype module to install or remove
   -- SOURCE
   procedure UpgradeShip(Install: Boolean; ModuleIndex: Unbounded_String);
   -- ****
   -- ****f* Bases.Ship/PayForDock
   -- FUNCTION
   -- Pay daily fee for docking
   -- SOURCE
   procedure PayForDock;
   -- ****
   -- ****f* Bases.Ship/RepairCost
   -- FUNCTION
   -- Count cost and time of repairs of player ship
   -- PARAMETERS
   -- Cost        - Overall cost of repair of player ship
   -- Time        - Time needed for repair of player ship
   -- ModuleIndex - Index of module on player ship to repair or 0 to repair
   --               all damage modules
   -- RESULT
   -- Parameters Cost and Time
   -- SOURCE
   procedure RepairCost(Cost, Time: in out Natural; ModuleIndex: Integer);
   -- ****

end Bases.Ship;
