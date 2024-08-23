--    Copyright 2016-2024 Bartek thindil Jasicki
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

with Ada.Containers.Formal_Vectors; use Ada.Containers;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Game; use Game;

-- ****h* Items/Items
-- FUNCTION
-- Provice code for manipulate items
-- SOURCE
package Items is
-- ****

   -- ****t* Items/Items.Values_Range
   -- FUNCTION
   -- Used to set amount of additional data for items
   -- HISTORY
   -- 7.4 - Added
   -- SOURCE
   subtype Values_Range is Positive range 1 .. 5;
   -- ****

   -- ****s* Items/Items.Object_Data
   -- FUNCTION
   -- Data structure for objects prototypes
   -- PARAMETERS
   -- Name        - Name of item
   -- Weight      - Weight of item
   -- I_Type      - Type of item
   -- Price       - Price of item in bases
   -- Value       - Additional item data (damage for ammo, etc)
   -- Show_Type   - Displayed type of item (can be group of items, renamed
   --               type, etc)
   -- Description - Description of item
   -- Reputation  - Minimal reputation in base needed to buy that module
   -- SOURCE
   type Object_Data is record
      Name: Tiny_String.Bounded_String;
      Weight: Positive := 1;
      I_Type: Tiny_String.Bounded_String;
      Price: Natural := 0;
      Value: Integer_Array (Values_Range);
      Show_Type: Tiny_String.Bounded_String;
      Description: Short_String.Bounded_String;
      Reputation: Reputation_Range;
   end record;
   -- ****

   --## rule off REDUCEABLE_SCOPE
   -- ****d* Items/Items.Empty_Object
   -- FUNCTION
   -- Default, empty object prototype
   -- SOURCE
   Empty_Object: constant Object_Data := (others => <>);
   -- ****
   --## rule on REDUCEABLE_SCOPE

   -- ****t* Items/Items.Items_Durability
   -- FUNCTION
   -- Used to mark items durability
   -- SOURCE
   subtype Items_Durability is Natural range 0 .. 101;
   -- ****

   -- ****d* Items/Items.Default_Item_Durability
   -- FUNCTION
   -- Default value for items durability
   -- SOURCE
   Default_Item_Durability: constant Items_Durability := 100;
   -- ****

   -- ****s* Items/Items.Inventory_Data
   -- FUNCTION
   -- Data structure for item in inventory
   -- PARAMETERS
   -- Proto_Index - Index of prototype
   -- Amount      - Amount of item
   -- Name        - Name of item if different than default
   -- Durability  - Current durability of item
   -- Price       - Price for which item was bought
   -- SOURCE
   type Inventory_Data is record
      Proto_Index: Natural;
      Amount: Positive := 1;
      Name: Tiny_String.Bounded_String;
      Durability: Items_Durability;
      Price: Natural := 0;
   end record;
   -- ****

   --## rule off REDUCEABLE_SCOPE
   -- ****d* Items/Items.Empty_Item
   -- FUNCTION
   -- Default, empty item for inventory
   -- SOURCE
   Empty_Item: constant Inventory_Data := (others => <>);
   -- ****
   --## rule on REDUCEABLE_SCOPE

   -- ****t* Items/Items.Inventory_Container
   -- FUNCTION
   -- Used to store inventory data
   -- SOURCE
   package Inventory_Container is new Formal_Vectors
     (Index_Type => Positive, Element_Type => Inventory_Data);
   -- ****

      -- ****f* Items/Items.Get_Item_Damage
      -- FUNCTION
      -- Get description of item damage
      -- PARAMETERS
      -- Item_Durability - Numeric value of current durability of the item
      -- To_Lower        - If true, convert description to lower cases. Default
      --                   is false
      -- With_Colors     - If true, add colors' tags to the description. Default
      --                   value is false
      -- RESULT
      -- Description of item damage level
      -- SOURCE
   function Get_Item_Damage
     (Item_Durability: Items_Durability;
      To_Lower, With_Colors: Boolean := False) return String with
      Post => Get_Item_Damage'Result'Length > 0;
      -- ****

      -- ****f* Items/Items.Get_Item_Name
      -- FUNCTION
      -- Get name of item in ship cargo or character inventory
      -- Item        - Item to get it name
      -- Damage_Info - If true, include description of the item damage in name.
      --               Default is true.
      -- To_Lower    - If true, convert damage info to lower case. Default is
      --               true.
      -- RESULT
      -- Name of item with additional damage level info
      -- SOURCE
   function Get_Item_Name
     (Item: Inventory_Data; Damage_Info, To_Lower: Boolean := True)
      return String with
      Post => Get_Item_Name'Result'Length > 0;
      -- ****

      -- ****f* Items/Items.Find_Item
      -- FUNCTION
      -- Find item in ship cargo or character inventory
      -- PARAMETERS
      -- Inventory   - Inventory in which item will be looking for
      -- Proto_Index - Prototype index of item. Can be empty if ItemType is set
      -- Item_Type   - Type of item to search. Can be empty if ProtoIndex is set
      -- Durability  - Durability of item to search. Can be empty
      -- Quality     - Quality of item to search. Can be empty
      -- RESULT
      -- Iventory index of item or 0 if item was not found
      -- SOURCE
   function Find_Item
     (Inventory: Inventory_Container.Vector; Proto_Index: Natural := 0;
      Item_Type: Tiny_String.Bounded_String := Tiny_String.Null_Bounded_String;
      Durability: Items_Durability := Items_Durability'Last;
      Quality: Positive := 100) return Natural with
      Post => Find_Item'Result <=
      Inventory_Container.Last_Index(Container => Inventory);
      -- ****

   -- ****f* Items/Items.Get_Item_Chance_To_Damage
   -- FUNCTION
   -- Get item chance to damage info
   -- PARAMETERS
   -- Item_Data - Numeric chance to damage for selected item
   -- RESULT
   -- String with chance to damage level description
   -- SOURCE
   function Get_Item_Chance_To_Damage(Item_Data: Natural) return String with
      Post => Get_Item_Chance_To_Damage'Result'Length > 0;
      -- ****

-- Temporary code to interact with Nim

   --## rule off TYPE_INITIAL_VALUES
   type Nim_Inventory_Data is record
      Proto_Index: Natural;
      Amount: Positive := 1;
      Name: chars_ptr;
      Durability: Items_Durability;
      Price: Natural := 0;
   end record;
   type Nim_Inventory_Array is array(0 .. 127) of Nim_Inventory_Data;
   --## rule on TYPE_INITIAL_VALUES

   function Inventory_To_Nim
     (Inventory: Inventory_Container.Vector) return Nim_Inventory_Array;

   function Inventory_From_Nim
     (Inventory: Nim_Inventory_Array; Size: Positive)
      return Inventory_Container.Vector;

   function Get_Proto_Item(Index: Positive) return Object_Data;

   function Get_Proto_Amount return Positive with
      Import => True,
      Convention => C,
      External_Name => "getAdaProtoAmount";

end Items;
