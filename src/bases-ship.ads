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

   -- Raised when there is nothing to repair
   BasesShip_Nothing_To_Repair: exception;
   -- Raised when player try install another same unique module
   BasesShip_Unique_Module: exception;
   -- Raised when problems with installing ship module occurs
   BasesShip_Installation_Error: exception;
   -- Raised when problems with removing ship module occurs
   BasesShip_Removing_Error: exception;

   -- Repairs playership in bases
   procedure RepairShip(ModuleIndex: Integer);
   -- Install/remove modules on ship
   procedure UpgradeShip(Install: Boolean; ModuleIndex: Unbounded_String);
   -- Pay daily fee for docking
   procedure PayForDock;
   -- Count cost/time of repairs of ship
   procedure RepairCost(Cost, Time: in out Natural; ModuleIndex: Integer);

end Bases.Ship;
