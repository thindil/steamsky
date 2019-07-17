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

   -- Create infterace for show player ship crew info
   procedure CreateCrewUI(NewBuilder: Gtkada_Builder);
   -- Show interface for show player ship crew info
   procedure ShowCrewUI;

private

   -- Gtk builder for user interface
   Builder: Gtkada_Builder;
   -- Crew member and item from inventory indexes
   MemberIndex, ItemIndex: Positive;
   -- Set orders for selected crew member
   procedure SetOrdersList;
   -- Show list of orders for all crew members
   procedure ShowOrdersForAll;
   -- Refresh informations about selected crew member inventory
   procedure RefreshInventory;
   -- Set active item in inventory list
   procedure SetActiveItem;
   -- Refresh crew list
   procedure RefreshCrewInfo;
   -- Set active crew member in crew list
   procedure SetActiveMember(NewMemberIndex: Natural := 0) with
      Pre => NewMemberIndex <= PlayerShip.Crew.Last_Index;

end Crew.UI;
