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

package Crew.Inventory is

   procedure UpdateInventory
     (MemberIndex: Positive; Amount: Integer;
      ProtoIndex, Durability, InventoryIndex: Natural :=
        0); -- Update member inventory
   function FreeInventory
     (MemberIndex: Positive; Amount: Integer)
      return Integer; -- Return available space in crew member inventory after adding/extracting Amount
   procedure TakeOffItem
     (MemberIndex,
      ItemIndex: Positive); -- Remove selected item from character equipment
   function ItemIsUsed
     (MemberIndex, ItemIndex: Positive)
      return Boolean; -- Check if selected crew member use this item
   function FindTools
     (MemberIndex: Positive; ItemType: Unbounded_String; Order: Crew_Orders)
      return Natural; -- Search for specified tools in character and ship cargo, return 0 if tools not found otherwise index of tool in character inventory
end Crew.Inventory;
