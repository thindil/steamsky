--    Copyright 2018-2019 Bartek thindil Jasicki
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

with Gtkada.Builder; use Gtkada.Builder;
with Ships; use Ships;

package Crew.UI is

   procedure CreateCrewUI
     (NewBuilder: Gtkada_Builder); -- Create infterace for show player ship crew info
   procedure ShowCrewUI; -- Show interface for show player ship crew info

private

   Builder: Gtkada_Builder; -- Gtk builder for user interface
   MemberIndex,
   ItemIndex: Positive; -- Crew member and item from inventory indexes
   procedure SetOrdersList; -- Set orders for selected crew member
   procedure ShowOrdersForAll; -- Show list of orders for all crew members
   procedure RefreshInventory; -- Refresh informations about selected crew member inventory
   procedure SetActiveItem; -- Set active item in inventory list
   procedure RefreshCrewInfo; -- Refresh crew list
   procedure SetActiveMember(NewMemberIndex: Natural := 0) with
      Pre => NewMemberIndex <=
      PlayerShip.Crew.Last_Index; -- Set active crew member in crew list

end Crew.UI;
