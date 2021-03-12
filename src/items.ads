--    Copyright 2016-2021 Bartek thindil Jasicki
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
with Ada.Strings.Unbounded.Hash;
with Ada.Containers.Vectors; use Ada.Containers;
with Ada.Containers.Hashed_Maps;
with DOM.Readers; use DOM.Readers;
with Game; use Game;

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
   Items_Types: UnboundedString_Container.Vector;
   -- ****

   -- ****s* Items/Items.Object_Data
   -- FUNCTION
   -- Data structure for objects prototypes
   -- PARAMETERS
   -- Name        - Name of item
   -- Weight      - Weight of item
   -- IType       - Type of item
   -- Price      - Price of item in bases
   -- Value       - Additional item data (damage for ammo, etc)
   -- ShowType    - Displayed type of item (can be group of items, renamed
   --               type, etc)
   -- Description - Description of item
   -- Reputation     - Minimal reputation in base needed to buy that module
   -- SOURCE
   type Object_Data is record
      Name: Unbounded_String;
      Weight: Positive;
      IType: Unbounded_String;
      Price: Natural;
      Value: Integer_Container.Vector;
      ShowType: Unbounded_String;
      Description: Unbounded_String;
      Reputation: Reputation_Range;
   end record;
   -- ****

   -- ****t* Items/Items.Objects_Container
   -- FUNCTION
   -- Used to store items data
   -- SOURCE
   package Objects_Container is new Hashed_Maps(Unbounded_String, Object_Data,
      Ada.Strings.Unbounded.Hash, "=");
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

   -- ****s* Items/Items.InventoryData
   -- FUNCTION
   -- Data structure for item in inventory
   -- PARAMETERS
   -- ProtoIndex - Index of prototype
   -- Amount     - Amount of item
   -- Name       - Name of item if different than default
   -- Durability - Current durability of item
   -- Price      - Price for which item was bought
   -- SOURCE
   type InventoryData is record
      ProtoIndex: Unbounded_String;
      Amount: Positive;
      Name: Unbounded_String;
      Durability: Items_Durability;
      Price: Natural;
   end record;
   -- ****

   -- ****t* Items/Items.Inventory_Container
   -- FUNCTION
   -- Used to store inventory data
   -- SOURCE
   package Inventory_Container is new Vectors(Positive, InventoryData);
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
   Tools_List: UnboundedString_Container.Vector;
   -- ****

   -- ****v* Items/Items.Weapons_List
   -- FUNCTION
   -- List of indexes of all weapons in game
   -- SOURCE
   Weapons_List: UnboundedString_Container.Vector;
   -- ****

   -- ****v* Items/Items.Shields_List
   -- FUNCTION
   -- List of indexes of all shields in game
   -- SOURCE
   Shields_List: UnboundedString_Container.Vector;
   -- ****

   -- ****v* Items/Items.HeadArmors_List
   -- FUNCTION
   -- List of indexes of all head armors in game
   -- SOURCE
   HeadArmors_List: UnboundedString_Container.Vector;
   -- ****

   -- ****v* Items/Items.ChestArmors_List
   -- FUNCTION
   -- List of indexes of all chest armors in game
   -- SOURCE
   ChestArmors_List: UnboundedString_Container.Vector;
   -- ****

   -- ****v* Items/Items.ArmsArmors_List
   -- FUNCTION
   -- List of indexes of all arms armors in game
   -- SOURCE
   ArmsArmors_List: UnboundedString_Container.Vector;
   -- ****

   -- ****v* Items/Items.LegsArmors_List
   -- FUNCTION
   -- List of indexes of all legs armors in game
   -- SOURCE
   LegsArmors_List: UnboundedString_Container.Vector;
   -- ****

   -- ****f* Items/Items.LoadItems
   -- FUNCTION
   -- Load items from files
   -- PARAMETERS
   -- Reader - XML Reader from which items data will be read
   -- SOURCE
   procedure LoadItems(Reader: Tree_Reader);
   -- ****

   -- ****f* Items/Items.FindProtoItem
   -- FUNCTION
   -- Search for map index of selected item
   -- PARAMETERS
   -- ItemType - Item type of item which map index is looking for
   -- RESULT
   -- Map index of item or empty string if item not found
   -- SOURCE
   function FindProtoItem
     (ItemType: Unbounded_String) return Unbounded_String with
      Pre => (ItemType /= Null_Unbounded_String),
      Test_Case => (Name => "Test_FindProtoItem", Mode => Nominal);
      -- ****

      -- ****f* Items/Items.GetItemDamage
      -- FUNCTION
      -- Get description of item damage
      -- PARAMETERS
      -- ItemDurability - Numeric value of current durability of the item
      -- ToLower        - If true, convert description to lower cases. Default
      --                  is false
      -- RESULT
      -- Description of item damage level
      -- SOURCE
   function GetItemDamage
     (ItemDurability: Items_Durability; ToLower: Boolean := False)
      return String with
      Test_Case => (Name => "Test_GetItemDamage", Mode => Robustness);
      -- ****

      -- ****f* Items/Items.GetItemName
      -- FUNCTION
      -- Get name of item in ship cargo or character inventory
      -- Item       - Item to get it name
      -- DamageInfo - If true, include description of the item damage in name.
      --              Default is true.
      -- ToLower    - If true, convert damage info to lower case. Default is
      --              true.
      -- RESULT
      -- Name of item with additional damage level info
      -- SOURCE
   function GetItemName
     (Item: InventoryData; DamageInfo, ToLower: Boolean := True)
      return String with
      Test_Case => (Name => "Test_GetItemName", Mode => Robustness);
      -- ****

      -- ****f* Items/Items.DamageItem
      -- FUNCTION
      -- Check if item in ship cargo or character inventory was damaged
      -- PARAMETERS
      -- Inventory   - Inventory in which selected item is
      -- ItemIndex   - Inventory index of selected item
      -- SkillLevel  - Level of skill character which uses that item. Default
      --               is 0
      -- MemberIndex - Index of crew member of player ship which uses that
      --               item. Default is 0
      -- RESULT
      -- Updated inventory in which item was
      -- SOURCE
   procedure DamageItem
     (Inventory: in out Inventory_Container.Vector; ItemIndex: Positive;
      SkillLevel, MemberIndex: Natural := 0) with
      Pre => (ItemIndex <= Inventory.Last_Index),
      Test_Case => (Name => "Test_DamageItem", Mode => Nominal);
      -- ****

      -- ****f* Items/Items.FindItem
      -- FUNCTION
      -- Find item in ship cargo or character inventory
      -- PARAMETERS
      -- Inventory  - Inventory in which item will be looking for
      -- ProtoIndex - Prototype index of item. Can be empty if ItemType is set
      -- ItemType   - Type of item to search. Can be empty if ProtoIndex is set
      -- Durability - Durability of item to search. Can be empty
      -- Quality    - Quality of item to search. Can be empty
      -- RESULT
      -- Iventory index of item or 0 if item was not found
      -- SOURCE
   function FindItem
     (Inventory: Inventory_Container.Vector;
      ProtoIndex, ItemType: Unbounded_String := Null_Unbounded_String;
      Durability: Items_Durability := Items_Durability'Last;
      Quality: Positive := 100) return Natural with
      Test_Case => (Name => "Test_FindItem", Mode => Robustness);
      -- ****

      -- ****f* Items/Items.SetToolsList
      -- FUNCTION
      -- Fill tools types list
      -- SOURCE
   procedure SetToolsList;
   -- ****

   -- ****f* Items/Items.GetItemChanceToDamage
   -- FUNCTION
   -- Get item chance to damage info
   -- PARAMETERS
   -- ItemData: Numeric chance to damage for selected item
   -- RESULT
   -- String with chance to damage level description
   -- SOURCE
   function GetItemChanceToDamage(ItemData: Natural) return String with
      Test_Case => (Name => "Test_GetItemChanceToDamage", Mode => Robustness);
      -- ****

end Items;
