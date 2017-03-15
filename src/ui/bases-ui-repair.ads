--    Copyright 2016 Bartek thindil Jasicki
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

package Bases.UI.Repair is

   procedure RepairCost
     (Cost,
      Time,
      ModuleIndex: in out Natural); -- Count cost/time of repairs of ship
   procedure ShowRepair; -- Show repair window
   function RepairKeys
     (Key: Key_Code) return GameStates; -- Handle keys in ship repairs

end Bases.UI.Repair;
