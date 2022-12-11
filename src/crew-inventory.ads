--    Copyright 2017-2022 Bartek thindil Jasicki
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

-- ****h* Crew/Inventory
-- FUNCTION
-- Provide code for manipulate player ship crew members inventory
-- SOURCE
package Crew.Inventory is
-- ****

   -- ****f* Inventory/Inventory.Update_Inventory
   -- FUNCTION
   -- Update member inventory
   -- PARAMETERS
   -- Member_Index    - Crew index of member which will be have updated the
   --                   inventory
   -- Amount          - Amount of items to add or delete from inventory
   -- Proto_Index     - Prototype index of item to add or delete. Can be
   --                   empty if InventoryIndex is set
   -- Durability      - Durability of item to add or delete from inventory
   -- Inventory_Index - Item index in crew member inventory. Can be empty if
   --                   ProtoIndex is set
   -- Price           - Price of the item
   -- Ship            - The ship to which the crew member belongs
   -- HISTORY
   -- 6.9 - Added Ship parameter
   -- 7.5 - Renamed to Update_Inventory, changed parameters names to
   --       Member_Index, Proto_Index and Inventory_Index
   -- SOURCE
   procedure Update_Inventory
     (Member_Index: Positive; Amount: Integer;
      Proto_Index: Objects_Container.Extended_Index := 0;
      Durability: Items_Durability := 0; Inventory_Index, Price: Natural := 0;
      Ship: in out Ship_Record) with
      Pre =>
      (Member_Index <= Ship.Crew.Last_Index and
       Inventory_Index <=
         Inventory_Container.Last_Index
           (Container => Ship.Crew(Member_Index).Inventory)),
      Test_Case => (Name => "Test_UpdateInventory", Mode => Nominal);
      -- ****

      -- ****f* Inventory/Inventory.Free_Inventory
      -- FUNCTION
      -- Return available space in crew member inventory after adding or
      -- extracting Amount
      -- PARAMETERS
      -- Member_Index - Crew index of the member which inventory will be checked
      -- Amount       - Amount of kilogram to add or remove during check
      -- Update_Nim   - If true, update crew and the selected inventory in Nim
      -- RESULT
      -- Amount of available space in kilograms
      -- HISTORY
      -- 7.5 - Renamed to Free_Inventory, changed parameter name to Member_Index
      -- SOURCE
   function Free_Inventory
     (Member_Index: Positive; Amount: Integer; Update_Nim: Boolean := True)
      return Integer with
      Pre => Member_Index <= Player_Ship.Crew.Last_Index;
      -- ****

      -- ****f* Inventory/Inventory.Take_Off_Item
      -- FUNCTION
      -- Remove selected item from character equipment
      -- PARAMETERS
      -- Member_Index - Crew index of the member from which item willl be taken
      --                off
      -- Item_Index   - Inventory index of item to take off
      -- HISTORY
      -- 7.5 - Renamed to Take_Off_Item, changed parameters names to
      --       Member_Index and Item_Index
      -- SOURCE
   procedure Take_Off_Item
     (Member_Index, Item_Index: Positive; Update_Nim: Boolean := True) with
      Pre =>
      (Member_Index <= Player_Ship.Crew.Last_Index and
       Item_Index <=
         Inventory_Container.Last_Index
           (Container => Player_Ship.Crew(Member_Index).Inventory));
      -- ****

      -- ****f* Inventory/Inventory.Item_Is_Used
      -- FUNCTION
      -- Check if selected crew member use this item
      -- PARAMETERS
      -- Member_Index - Crew index of the member which will be checked
      -- Item_Index   - Iventory index of the item which will be checked
      -- Update_Nim   - If true, update crew and the selected inventory in Nim
      -- HISTORY
      -- 7.5 - Renamed to Item_Is_Used, changed parameters names to Member_Index
      --       and Item_Index
      -- SOURCE
   function Item_Is_Used
     (Member_Index, Item_Index: Positive; Update_Nim: Boolean := True)
      return Boolean with
      Pre =>
      (Member_Index <= Player_Ship.Crew.Last_Index and
       Item_Index <=
         Inventory_Container.Last_Index
           (Container => Player_Ship.Crew(Member_Index).Inventory));
      -- ****

      -- ****f* Inventory/Inventory.Find_Tools
      -- FUNCTION
      -- Search for specified tools in character and ship cargo
      -- PARAMETERS
      -- Member_Index - Crew index of the member which will be checked
      -- Item_Type    - Type of item which will be looking for
      -- Order        - Order which crew member will be doing when he/she find
      --                proper tool
      -- Tool_Quality - Minimal quality of tool to find. Default value is 100
      -- RESULT
      -- Selected crew member inventory index of the tool or 0 if tool was not
      -- found
      -- HISTORY
      -- 7.5 - Renamed to Find_Tools, changed parameters names to Member_Index,
      --       Item_Type and Tool_Quality
      -- SOURCE
   function Find_Tools
     (Member_Index: Positive; Item_Type: Tiny_String.Bounded_String;
      Order: Crew_Orders; Tool_Quality: Positive := 100) return Natural with
      Pre =>
      (Member_Index <= Player_Ship.Crew.Last_Index and
       Tiny_String.Length(Source => Item_Type) > 0),
      Test_Case => (Name => "Test_FindTools", Mode => Nominal);
      -- ****

end Crew.Inventory;
