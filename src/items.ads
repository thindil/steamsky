--    Copyright 2016-2019 Bartek thindil Jasicki
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

package Items is

-- ****v* Items/Items_Types
-- FUNCTION
-- Types of items
-- SOURCE
   Items_Types: UnboundedString_Container.Vector;
-- ****
-- ****t* Items/Object_Prices
-- FUNCTION
-- Prices of item in bases
-- SOURCE
   type Object_Prices is array(1 .. 5) of Natural;
-- ****
-- ****t* Items/Object_Buyable
-- FUNCTION
-- Did item is buyable in bases
-- SOURCE
   type Object_Buyable is array(1 .. 5) of Boolean;
-- ****
-- ****t* Items/Object_Data
-- FUNCTION
-- Data structure for objects prototypes
-- SOURCE
   type Object_Data is record
      Name: Unbounded_String; -- Name of item
      Weight: Positive; -- Weight of item
      IType: Unbounded_String; -- Type of item
      Prices: Object_Prices; -- Prices of item in bases
      Buyable: Object_Buyable; -- Did item is buyable in selected bases
      Value: Integer_Container
        .Vector; -- Additional item data (damage for ammo, etc)
      ShowType: Unbounded_String; -- Displayed type of item (can be group of items, renamed type, etc)
      Description: Unbounded_String; -- Description of item
   end record;
-- ****

-- ****t* Items/Objects_Container
-- SOURCE
   package Objects_Container is new Hashed_Maps(Unbounded_String, Object_Data,
      Ada.Strings.Unbounded.Hash, "=");
-- ****

-- ****t* Items/InventoryData
-- FUNCTION
-- Data structure for item in inventory
-- SOURCE
   type InventoryData is record
      ProtoIndex: Unbounded_String; -- Index of prototype
      Amount: Positive; -- Amount of item
      Name: Unbounded_String; -- Name of item if different than default
      Durability: Natural; -- Current durability of item
      Price: Natural; -- Price for which item was bought
   end record;
-- ****

-- ****t* Items/Inventory_Container
-- SOURCE
   package Inventory_Container is new Vectors(Positive, InventoryData);
-- ****

-- ****v* Items/Items_List
-- FUNCTION
-- List of item available in game
-- SOURCE
   Items_List: Objects_Container.Map;
-- ****
-- ****v* Items/Tools_List
-- FUNCTION
-- List of all tools types in game
-- SOURCE
   Tools_List: UnboundedString_Container.Vector;
-- ****
-- ****v* Items/Weapons_List
-- FUNCTION
-- List of indexes of all weapons in game
-- SOURCE
   Weapons_List: UnboundedString_Container.Vector;
-- ****
-- ****v* Items/Shields_List
-- FUNCTION
-- List of indexes of all shields in game
-- SOURCE
   Shields_List: UnboundedString_Container.Vector;
-- ****
-- ****v* Items/HeadArmors_List
-- FUNCTION
-- List of indexes of all head armors in game
-- SOURCE
   HeadArmors_List: UnboundedString_Container.Vector;
-- ****
-- ****v* Items/ChestArmors_List
-- FUNCTION
-- List of indexes of all chest armors in game
-- SOURCE
   ChestArmors_List: UnboundedString_Container.Vector;
-- ****
-- ****v* Items/ArmsArmors_List
-- FUNCTION
-- List of indexes of all arms armors in game
-- SOURCE
   ArmsArmors_List: UnboundedString_Container.Vector;
-- ****
-- ****v* Items/LegsArmors_List
-- FUNCTION
-- List of indexes of all legs armors in game
-- SOURCE
   LegsArmors_List: UnboundedString_Container.Vector;
-- ****

-- ****f* Items/LoadItems
-- FUNCTION
-- Load items from files
-- SOURCE
   procedure LoadItems(Reader: Tree_Reader);
-- ****
-- ****f* Items/FindProtoItem
-- FUNCTION
-- Return map index of item or empty string if item not found
-- SOURCE
   function FindProtoItem(ItemType: Unbounded_String) return Unbounded_String;
-- ****
-- ****f* Items/GetItemDamage
-- FUNCTION
-- Get description of item damage
-- SOURCE
   function GetItemDamage
     (ItemDurability: Natural; ToLower: Boolean := False) return String;
-- ****
-- ****f* Items/GetItemName
-- FUNCTION
-- Get name of item in ship cargo or character inventory
-- SOURCE
   function GetItemName
     (Item: InventoryData; DamageInfo, ToLower: Boolean := True) return String;
-- ****
-- ****f* Items/DamageItem
-- FUNCTION
-- Check if item in ship cargo or character inventory was damaged
-- SOURCE
   procedure DamageItem
     (Inventory: in out Inventory_Container.Vector; ItemIndex: Positive;
      SkillLevel, MemberIndex: Natural := 0);
-- ****
-- ****f* Items/FindItem
-- FUNCTION
-- Find item in ship cargo or character inventory, return item index or 0 if item not found
-- SOURCE
   function FindItem
     (Inventory: Inventory_Container.Vector;
      ProtoIndex, ItemType: Unbounded_String := Null_Unbounded_String;
      Durability: Natural := 101) return Natural;
-- ****
-- ****f* Items/SetToolsList;
-- FUNCTION
-- Fill tools types list
-- SOURCE
   procedure SetToolsList;
-- ****

end Items;
