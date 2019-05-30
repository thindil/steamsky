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

   BasesShip_Nothing_To_Repair: exception; -- Raised when there is nothing to repair
   BasesShip_Unique_Module: exception; -- Raised when player try install another same unique module
   BasesShip_Installation_Error: exception; -- Raised when problems with installing ship module occurs
   BasesShip_Removing_Error: exception; -- Raised when problems with removing ship module occurs

   procedure RepairShip(ModuleIndex: Integer); -- Repairs playership in bases
   procedure UpgradeShip
     (Install: Boolean;
      ModuleIndex: Unbounded_String); -- Install/remove modules on ship
   procedure PayForDock; -- Pay daily fee for docking
   procedure RepairCost
     (Cost, Time: in out Natural;
      ModuleIndex: Integer); -- Count cost/time of repairs of ship

end Bases.Ship;
