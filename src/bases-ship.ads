--    Copyright 2017 Bartek thindil Jasicki
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

   function RepairShip
     (ModuleIndex: Integer)
     return String; -- Repairs playership in bases, returns empty string if all ok otherwise error message
   function UpgradeShip
     (Install: Boolean;
      ModuleIndex: Positive)
     return String; -- Install/remove modules on ship, returns empty string if all ok otherwise error message
   procedure PayForDock; -- Pay daily fee for docking
   procedure RepairCost
     (Cost, Time: in out Natural;
      ModuleIndex: Integer); -- Count cost/time of repairs of ship

end Bases.Ship;
