--    Copyright 2016-2022 Bartek thindil Jasicki
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Formal_Vectors; use Ada.Containers;
with Ada.Containers.Hashed_Maps;
with DOM.Readers; use DOM.Readers;
with Game; use Game;
limited with Ships;

-- ****h* Items/Items
-- FUNCTION
-- Provice code for manipulate items
-- SOURCE
package Items is
-- ****

   -- ****v* Items/Items.Items_Types
   -- FUNCTION
   -- Types of items
   -- SOURCE
   Items_Types: TinyString_Container.Vector;
   -- ****

   -- ****s* Items/Items.Object_Data
   -- FUNCTION
   -- Data structure for objects prototypes
   -- PARAMETERS
   -- Name         - Name of item
   -- Weight       - Weight of item
   -- I_Type       - Type of item
   -- Price        - Price of item in bases
   -- Value        - Additional item data (damage for ammo, etc)
   -- Show_Type    - Displayed type of item (can be group of items, renamed
   --               type, etc)
   -- Description  - Description of item
   -- Reputation   - Minimal reputation in base needed to buy that module
   -- SOURCE
   type Object_Data is record
      Name: Tiny_String.Bounded_String;
      Weight: Positive := 1;
      I_Type: Tiny_String.Bounded_String;
      Price: Natural := 0;
      Value: Integer_Container.Vector;
      Show_Type: Tiny_String.Bounded_String;
      Description: Short_String.Bounded_String;
      Reputation: Reputation_Range;
   end record;
   -- ****

   -- ****t* Items/Items.Objects_Container
   -- FUNCTION
   -- Used to store items data
   -- SOURCE
   package Objects_Container is new Hashed_Maps
     (Key_Type => Tiny_String.Bounded_String, Element_Type => Object_Data,
      Hash => Tiny_String_Hash, Equivalent_Keys => Tiny_String."=");
   -- ****

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
      Proto_Index: Tiny_String.Bounded_String;
      Amount: Positive := 1;
      Name: Tiny_String.Bounded_String;
      Durability: Items_Durability;
      Price: Natural := 0;
   end record;
   -- ****

   -- ****t* Items/Items.Inventory_Container
   -- FUNCTION
   -- Used to store inventory data
   -- SOURCE
   package Inventory_Container is new Formal_Vectors
     (Index_Type => Positive, Element_Type => Inventory_Data);
   -- ****

   -- ****v* Items/Items.Items_List
   -- FUNCTION
   -- List of item available in game
   -- SOURCE
   Items_List: Objects_Container.Map;
   -- ****

   -- ****v* Items/Items.Tools_List
   -- FUNCTION
   -- List of all tools types in game
   -- SOURCE
   Tools_List: TinyString_Container.Vector;
   -- ****

   -- ****v* Items/Items.Weapons_List
   -- FUNCTION
   -- List of indexes of all weapons in game
   -- SOURCE
   Weapons_List: TinyString_Container.Vector;
   -- ****

   -- ****v* Items/Items.Shields_List
   -- FUNCTION
   -- List of indexes of all shields in game
   -- SOURCE
   Shields_List: TinyString_Container.Vector;
   -- ****

   -- ****v* Items/Items.Head_Armors_List
   -- FUNCTION
   -- List of indexes of all head armors in game
   -- SOURCE
   Head_Armors_List: TinyString_Container.Vector;
   -- ****

   -- ****v* Items/Items.Chest_Armors_List
   -- FUNCTION
   -- List of indexes of all chest armors in game
   -- SOURCE
   Chest_Armors_List: TinyString_Container.Vector;
   -- ****

   -- ****v* Items/Items.Arms_Armors_List
   -- FUNCTION
   -- List of indexes of all arms armors in game
   -- SOURCE
   Arms_Armors_List: TinyString_Container.Vector;
   -- ****

   -- ****v* Items/Items.Legs_Armors_List
   -- FUNCTION
   -- List of indexes of all legs armors in game
   -- SOURCE
   Legs_Armors_List: TinyString_Container.Vector;
   -- ****

   -- ****f* Items/Items.Load_Items
   -- FUNCTION
   -- Load items from files
   -- PARAMETERS
   -- Reader - XML Reader from which items data will be read
   -- SOURCE
   procedure Load_Items(Reader: Tree_Reader);
   -- ****

   -- ****f* Items/Items.Find_Proto_Item
   -- FUNCTION
   -- Search for map index of selected item
   -- PARAMETERS
   -- Item_Type - Item type of item which map index is looking for
   -- RESULT
   -- Map index of item or empty string if item not found
   -- SOURCE
   function Find_Proto_Item
     (Item_Type: Tiny_String.Bounded_String)
      return Tiny_String.Bounded_String with
      Pre => Tiny_String.Length(Source => Item_Type) > 0,
      Test_Case => (Name => "Test_FindProtoItem", Mode => Nominal);
      -- ****

      -- ****f* Items/Items.Get_Item_Damage
      -- FUNCTION
      -- Get description of item damage
      -- PARAMETERS
      -- Item_Durability - Numeric value of current durability of the item
      -- To_Lower        - If true, convert description to lower cases. Default
      --                   is false
      -- RESULT
      -- Description of item damage level
      -- SOURCE
   function Get_Item_Damage
     (Item_Durability: Items_Durability; To_Lower: Boolean := False)
      return String with
      Post => Get_Item_Damage'Result'Length > 0,
      Test_Case => (Name => "Test_GetItemDamage", Mode => Nominal);
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
      Post => Get_Item_Name'Result'Length > 0,
      Test_Case => (Name => "Test_GetItemName", Mode => Nominal);
      -- ****

      -- ****f* Items/Items.Damage_Item
      -- FUNCTION
      -- Check if item in ship cargo or character inventory was damaged
      -- PARAMETERS
      -- Inventory    - Inventory in which selected item is
      -- Item_Index   - Inventory index of selected item
      -- Skill_Level  - Level of skill character which uses that item. Default
      --                is 0
      -- Member_Index - Index of crew member of player ship which uses that
      --                item. Default is 0
      -- Ship         - The ship in which the item will be check for damage
      -- RESULT
      -- Updated inventory in which item was
      -- HISTORY
      -- 6.9 - Added Ship parameter
      -- SOURCE
   procedure Damage_Item
     (Inventory: in out Inventory_Container.Vector; Item_Index: Positive;
      Skill_Level, Member_Index: Natural := 0;
      Ship: in out Ships.Ship_Record) with
      Pre =>
      (Item_Index <= Inventory_Container.Last_Index(Container => Inventory)),
      Test_Case => (Name => "Test_DamageItem", Mode => Nominal);
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
     (Inventory: Inventory_Container.Vector;
      Proto_Index: Tiny_String.Bounded_String :=
        Tiny_String.Null_Bounded_String;
      Item_Type: Tiny_String.Bounded_String := Tiny_String.Null_Bounded_String;
      Durability: Items_Durability := Items_Durability'Last;
      Quality: Positive := 100) return Natural with
      Post => Find_Item'Result <=
      Inventory_Container.Last_Index(Container => Inventory),
      Test_Case => (Name => "Test_FindItem", Mode => Nominal);
      -- ****

      -- ****f* Items/Items.Set_Tools_List
      -- FUNCTION
      -- Fill tools types list
      -- SOURCE
   procedure Set_Tools_List;
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
      Post => Get_Item_Chance_To_Damage'Result'Length > 0,
      Test_Case => (Name => "Test_GetItemChanceToDamage", Mode => Nominal);
      -- ****

end Items;
