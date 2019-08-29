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

with Ada.Containers.Hashed_Maps; use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with DOM.Readers; use DOM.Readers;
with Crew; use Crew;
with Game; use Game;

-- ****h* Steamsky/Mobs
-- FUNCTION
-- Provides code for manipulate mobiles
-- SOURCE
package Mobs is
-- ****

   -- ****t* Mobs/MobInventoryRecord
   -- FUNCTION
   -- Data structure for mobs inventory
   -- PARAMETERS
   -- ProtoIndex - Proto index of item in mob inventory
   -- MinAmount  - Minimal amount of item in mob inventory
   -- MaxAmount  - Maximum amount of item in mob inventory
   -- SOURCE
   type MobInventoryRecord is record
      ProtoIndex: Unbounded_String;
      MinAmount: Natural;
      MaxAmount: Natural;
   end record;
   -- ****

   -- ****t* Mobs/MobInventory_Container
   -- FUNCTION
   -- Used for store mobiles inventories
   -- SOURCE
   package MobInventory_Container is new Vectors(Positive, MobInventoryRecord);
   -- ****

   -- ****t* Mobs/ProtoMobRecord
   -- FUNCTION
   -- Data structure for mobs prototypes
   -- PARAMETERS
   -- Skills     - Names indexes, levels and experience in skills of mob
   -- Attributes - Levels and experience in attributes of mob
   -- Order      - Current order for mob
   -- Priorities - Priority of orders of mob
   -- Inventory  - List of mob inventory
   -- Equipment  - Items indexes from inventory used by mob: 1 - weapon,
   --              2 - shield, 3 - helmet, 4 - torso, 5 - arms, 6 - legs,
   --              7 - tool
   -- SOURCE
   type ProtoMobRecord is record
      Skills: Skills_Container.Vector;
      Attributes: Attributes_Container.Vector;
      Order: Crew_Orders;
      Priorities: Natural_Array(1 .. 12);
      Inventory: MobInventory_Container.Vector;
      Equipment: Natural_Array(1 .. 7);
   end record;
   -- ****

   -- ****t* Mobs/ProtoMobs_Container
   -- FUNCTION
   -- Used to store mobiles
   -- SOURCE
   package ProtoMobs_Container is new Hashed_Maps(Unbounded_String,
      ProtoMobRecord, Ada.Strings.Unbounded.Hash, "=");
   -- ****

   -- ****v* Mobs/ProtoMobs_List
   -- FUNCTION
   -- List of prototypes of all mobiles available in the game
   -- SOURCE
   ProtoMobs_List: ProtoMobs_Container.Map;
   -- ****

   -- ****e* Mobs/Mobs_Invalid_Data
   -- FUNCTION
   -- Raised when invalid data found in mobs file
   -- SOURCE
   Mobs_Invalid_Data: exception;
   -- ****

   -- ****f* Mobs/LoadMobs
   -- FUNCTION
   -- Load mobs from files
   -- PARAMETERS
   -- Reader - XML Reader from which data will be read
   -- SOURCE
   procedure LoadMobs(Reader: Tree_Reader);
   -- ****

end Mobs;
