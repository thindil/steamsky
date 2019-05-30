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

with Ships; use Ships;

package Crew.Inventory is

   procedure UpdateInventory
     (MemberIndex: Positive; Amount: Integer;
      ProtoIndex: Unbounded_String := Null_Unbounded_String;
      Durability, InventoryIndex, Price: Natural := 0) with
      Pre =>
      (MemberIndex <= PlayerShip.Crew.Last_Index and
       InventoryIndex <=
         PlayerShip.Crew(MemberIndex).Inventory
           .Last_Index); -- Update member inventory
   function FreeInventory
     (MemberIndex: Positive; Amount: Integer) return Integer with
      Pre => MemberIndex <=
      PlayerShip.Crew
        .Last_Index; -- Return available space in crew member inventory after adding/extracting Amount
   procedure TakeOffItem(MemberIndex, ItemIndex: Positive) with
      Pre =>
      (MemberIndex <= PlayerShip.Crew.Last_Index and
       ItemIndex <=
         PlayerShip.Crew(MemberIndex).Inventory
           .Last_Index); -- Remove selected item from character equipment
   function ItemIsUsed(MemberIndex, ItemIndex: Positive) return Boolean with
      Pre =>
      (MemberIndex <= PlayerShip.Crew.Last_Index and
       ItemIndex <=
         PlayerShip.Crew(MemberIndex).Inventory
           .Last_Index); -- Check if selected crew member use this item
   function FindTools
     (MemberIndex: Positive; ItemType: Unbounded_String; Order: Crew_Orders)
      return Natural with
      Pre =>
      (MemberIndex <= PlayerShip.Crew.Last_Index and
       ItemType /=
         Null_Unbounded_String); -- Search for specified tools in character and ship cargo, return 0 if tools not found otherwise index of tool in character inventory
end Crew.Inventory;
