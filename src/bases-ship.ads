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

package Bases.Ship is

-- ****v* Bases.Ship/BasesShip_Nothing_To_Repair
-- FUNCTION
-- Raised when there is nothing to repair
-- SOURCE
   BasesShip_Nothing_To_Repair: exception;
-- ****
-- ****v* Bases.Ship/BasesShip_Unique_Module
-- FUNCTION
-- Raised when player try install another same unique module
-- SOURCE
   BasesShip_Unique_Module: exception;
-- ****
-- ****v* Bases.Ship/BasesShip_Installation_Error
-- FUNCTION
-- Raised when problems with installing ship module occurs
-- SOURCE
   BasesShip_Installation_Error: exception;
-- ****
-- ****v* Bases.Ship/BasesShip_Removing_Error
-- FUNCTION
-- Raised when problems with removing ship module occurs
-- SOURCE
   BasesShip_Removing_Error: exception;
-- ****

-- ****f* Bases.Ship/RepairShip
-- FUNCTION
-- Repairs playership in bases
-- SOURCE
   procedure RepairShip(ModuleIndex: Integer);
-- ****
-- ****f* Bases.Ship/UpgradeShip
-- FUNCTION
-- Install/remove modules on ship
-- SOURCE
   procedure UpgradeShip(Install: Boolean; ModuleIndex: Unbounded_String);
-- ****
-- ****f* Bases.Ship/PayForDock;
-- FUNCTION
-- Pay daily fee for docking
-- SOURCE
   procedure PayForDock;
-- ****
-- ****f* Bases.Ship/RepairCost
-- FUNCTION
-- Count cost/time of repairs of ship
-- SOURCE
   procedure RepairCost(Cost, Time: in out Natural; ModuleIndex: Integer);
-- ****

end Bases.Ship;
