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

   Items_Types: UnboundedString_Container.Vector; -- Types of items
   type Object_Prices is array(1 .. 5) of Natural; -- Prices of item in bases
   type Object_Buyable is
     array(1 .. 5) of Boolean; -- Did item is buyable in bases
   type Object_Data is -- Data structure for objects prototypes
   record
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
   package Objects_Container is new Hashed_Maps(Unbounded_String, Object_Data,
      Ada.Strings.Unbounded.Hash, "=");
   type InventoryData is -- Data structure for item in inventory
   record
      ProtoIndex: Unbounded_String; -- Index of prototype
      Amount: Positive; -- Amount of item
      Name: Unbounded_String; -- Name of item if different than default
      Durability: Natural; -- Current durability of item
      Price: Natural; -- Price for which item was bought
   end record;
   package Inventory_Container is new Vectors(Positive, InventoryData);
   Items_List: Objects_Container.Map; -- List of item available in game
   Tools_List: UnboundedString_Container
     .Vector; -- List of all tools types in game
   Weapons_List: UnboundedString_Container
     .Vector; -- List of indexes of all weapons in game
   Shields_List: UnboundedString_Container
     .Vector; -- List of indexes of all shields in game
   HeadArmors_List: UnboundedString_Container
     .Vector; -- List of indexes of all head armors in game
   ChestArmors_List: UnboundedString_Container
     .Vector; -- List of indexes of all chest armors in game
   ArmsArmors_List: UnboundedString_Container
     .Vector; -- List of indexes of all arms armors in game
   LegsArmors_List: UnboundedString_Container
     .Vector; -- List of indexes of all legs armors in game

   procedure LoadItems(Reader: Tree_Reader); -- Load items from files
   function FindProtoItem
     (ItemType: Unbounded_String)
      return Unbounded_String; -- Return map index of item or empty string if item not found
   function GetItemDamage
     (ItemDurability: Natural; ToLower: Boolean := False)
      return String; -- Get description of item damage
   function GetItemName
     (Item: InventoryData; DamageInfo, ToLower: Boolean := True)
      return String; -- Get name of item in ship cargo or character inventory
   procedure DamageItem
     (Inventory: in out Inventory_Container.Vector; ItemIndex: Positive;
      SkillLevel, MemberIndex: Natural :=
        0); -- Check if item in ship cargo or character inventory was damaged
   function FindItem
     (Inventory: Inventory_Container.Vector;
      ProtoIndex, ItemType: Unbounded_String := Null_Unbounded_String;
      Durability: Natural := 101)
      return Natural; -- Find item in ship cargo or character inventory, return item index or 0 if item not found
   procedure SetToolsList; -- Fill tools types list

end Items;
