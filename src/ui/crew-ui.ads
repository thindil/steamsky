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

with Ships; use Ships;

-- ****h* Steamsky/Crew.UI
-- FUNCTION
-- Provides code for crew info UI
-- SOURCE
package Crew.UI is
-- ****

   -- ****f* Crew.UI/CreateCrewUI
   -- FUNCTION
   -- Create infterace for show player ship crew info
   -- SOURCE
   procedure CreateCrewUI;
   -- ****
   -- ****f* Crew.UI/ShowCrewUI
   -- FUNCTION
   -- Show interface for show player ship crew info
   -- SOURCE
   procedure ShowCrewUI;
   -- ****

private

   -- ****v* Crew.UI/MemberIndex, ItemIndex
   -- FUNCTION
   -- Crew member and item from inventory indexes
   -- SOURCE
   MemberIndex, ItemIndex: Positive;
   -- ****
   -- ****f* Crew.UI/SetOrdersList
   -- FUNCTION
   -- Set orders for selected crew member
   -- SOURCE
   procedure SetOrdersList;
   -- ****
   -- ****f* Crew.UI/ShowOrdersForAll
   -- FUNCTION
   -- Show list of orders for all crew members
   -- SOURCE
   procedure ShowOrdersForAll;
   -- ****
   -- ****f* Crew.UI/RefreshInventory
   -- FUNCTION
   -- Refresh information about selected crew member inventory
   -- SOURCE
   procedure RefreshInventory;
   -- ****
   -- ****f* Crew.UI/SetActiveItem
   -- FUNCTION
   -- Set active item in inventory list
   -- SOURCE
   procedure SetActiveItem;
   -- ****
   -- ****f* Crew.UI/RefreshCrewInfo
   -- FUNCTION
   -- Refresh crew list
   -- SOURCE
   procedure RefreshCrewInfo;
   -- ****
   -- ****f* Crew.UI/SetActiveMember
   -- FUNCTION
   -- Set active crew member in crew list
   -- PARAMETERS
   -- NewMemberIndex - Crew index of new active member. Default is 0.
   -- SOURCE
   procedure SetActiveMember(NewMemberIndex: Natural := 0) with
      Pre => NewMemberIndex <= PlayerShip.Crew.Last_Index;
      -- ****

end Crew.UI;
