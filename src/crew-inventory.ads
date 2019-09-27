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

-- ****h* Steamsky/Crew.Inventory
-- FUNCTION
-- Provide code for manipulate player ship crew members inventory
-- SOURCE
package Crew.Inventory is
-- ****

   -- ****f* Crew.Inventory/UpdateInventory
   -- FUNCTION
   -- Update member inventory
   -- PARAMETERS
   -- MemberIndex    - Crew index of member which will be have updated the
   --                  inventory
   -- Amount         - Amount of items to add or delete from inventory
   -- ProtoIndex     - Prototype index of item to add or delete. Can be
   --                  empty if InventoryIndex is set
   -- Durability     - Durability of item to add or delete from inventory
   -- InventoryIndex - Item index in crew member inventory. Can be empty if
   --                  ProtoIndex is set
   -- Price          - Price of the item
   -- SOURCE
   procedure UpdateInventory
     (MemberIndex: Positive; Amount: Integer;
      ProtoIndex: Unbounded_String := Null_Unbounded_String;
      Durability, InventoryIndex, Price: Natural := 0) with
      Pre =>
      (MemberIndex <= PlayerShip.Crew.Last_Index and
       InventoryIndex <= PlayerShip.Crew(MemberIndex).Inventory.Last_Index),
      Test_Case => ("Test_UpdateInventory", Nominal);
      -- ****

      -- ****f* Crew.Inventory/FreeInventory
      -- FUNCTION
      -- Return available space in crew member inventory after adding or
      -- extracting Amount
      -- PARAMETERS
      -- MemberIndex - Crew index of the member which inventory will be checked
      -- Amount      - Amount of kilogram to add or remove during check
      -- RESULT
      -- Amount of available space in kilograms
      -- SOURCE
   function FreeInventory
     (MemberIndex: Positive; Amount: Integer) return Integer with
      Pre => MemberIndex <= PlayerShip.Crew.Last_Index,
      Test_Case => ("Test_FreeInventory", Nominal);
      -- ****

      -- ****f* Crew.Inventory/TakeOffItem
      -- FUNCTION
      -- Remove selected item from character equipment
      -- PARAMETERS
      -- MemberIndex - Crew index of the member from which item willl be taken
      --               off
      -- ItemIndex   - Inventory index of item to take off
      -- SOURCE
   procedure TakeOffItem(MemberIndex, ItemIndex: Positive) with
      Pre =>
      (MemberIndex <= PlayerShip.Crew.Last_Index and
       ItemIndex <= PlayerShip.Crew(MemberIndex).Inventory.Last_Index);
      -- ****

      -- ****f* Crew.Inventory/ItemIsUsed
      -- FUNCTION
      -- Check if selected crew member use this item
      -- PARAMETERS
      -- MemberIndex - Crew index of the member which will be checked
      -- ItemIndex   - Iventory index of the item which will be checked
      -- SOURCE
   function ItemIsUsed(MemberIndex, ItemIndex: Positive) return Boolean with
      Pre =>
      (MemberIndex <= PlayerShip.Crew.Last_Index and
       ItemIndex <= PlayerShip.Crew(MemberIndex).Inventory.Last_Index);
      -- ****

      -- ****f* Crew.Inventory/FindTools
      -- FUNCTION
      -- Search for specified tools in character and ship cargo
      -- PARAMETERS
      -- MemberIndex - Crew index of the member which will be checked
      -- ItemType    - Type of item which will be looking for
      -- Order       - Order which crew member will be doing when he/she find
      --               proper tool
      -- RESULT
      -- Selected crew member inventory index of the tool or 0 if tool was not
      -- found
      -- SOURCE
   function FindTools
     (MemberIndex: Positive; ItemType: Unbounded_String; Order: Crew_Orders)
      return Natural with
      Pre =>
      (MemberIndex <= PlayerShip.Crew.Last_Index and
       ItemType /= Null_Unbounded_String);
      -- ****

end Crew.Inventory;
