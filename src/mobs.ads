--    Copyright 2017-2021 Bartek thindil Jasicki
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
with Factions; use Factions;
with Game; use Game;

-- ****h* Mobs/Mobs
-- FUNCTION
-- Provides code for manipulate mobiles
-- SOURCE
package Mobs is
-- ****

   -- ****s* Mobs/Mobs.MobInventoryRecord
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

   -- ****t* Mobs/Mobs.MobInventory_Container
   -- FUNCTION
   -- Used for store mobiles inventories
   -- SOURCE
   package MobInventory_Container is new Vectors(Positive, MobInventoryRecord);
   -- ****

   -- ****s* Mobs/Mobs.ProtoMobRecord
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
      Equipment: Equipment_Array;
   end record;
   -- ****

   -- ****t* Mobs/Mobs.ProtoMobs_Container
   -- FUNCTION
   -- Used to store mobiles
   -- SOURCE
   package ProtoMobs_Container is new Hashed_Maps
     (Unbounded_String,
      ProtoMobRecord,
      Ada.Strings.Unbounded.Hash,
      "=");
   -- ****

   -- ****v* Mobs/Mobs.ProtoMobs_List
   -- FUNCTION
   -- List of prototypes of all mobiles available in the game
   -- SOURCE
   ProtoMobs_List: ProtoMobs_Container.Map;
   -- ****

   -- ****e* Mobs/Mobs.Mobs_Invalid_Data
   -- FUNCTION
   -- Raised when invalid data found in mobs file
   -- SOURCE
   Mobs_Invalid_Data: exception;
   -- ****

   -- ****f* Mobs/Mobs.LoadMobs
   -- FUNCTION
   -- Load mobs from files
   -- PARAMETERS
   -- Reader - XML Reader from which data will be read
   -- SOURCE
   procedure LoadMobs(Reader: Tree_Reader);
   -- ****

   -- ****f* Mobs/Mobs.GenerateMob
   -- FUNCTION
   -- Generate mob from selected prototype and faction.
   -- PARAMETERS
   -- MobIndex     - Prototype index from ProtoMobs_List from which the mob
   --                will be generated
   -- FactionIndex - Faction index from Factions_List to which the generated
   --                mob will be belong
   -- RESULT
   -- Newly generated mob
   -- SOURCE
   function GenerateMob
     (MobIndex, FactionIndex: Unbounded_String) return Member_Data with
      Pre =>
      (ProtoMobs_List.Contains(MobIndex) and
       Factions_List.Contains(FactionIndex)),
      Test_Case => (Name => "Test_GenearateMob", Mode => Nominal);
      -- ****

      -- ****f* Mobs/Mobs.GetRandomItem
      -- FUNCTION
      -- Get random item from the list based on mob skills and faction
      -- PARAMETERS
      -- ItemsIndexes     - List of items from which item will be get
      -- EquipIndex       - Index of equipment for selected item: 1 - weapon,
      --                    2 - shield, 3 - helmet, 4 - torso, 5 - arms, 6 -
      --                    legs, 7 - tool
      -- HighestLevel     - Highest skill level for selected mob
      -- WeaponSkillLevel - Weapon skill level for selected mob
      -- FactionIndex     - Faction index to which selected mob belongs
      -- SOURCE
   function GetRandomItem
     (ItemsIndexes: UnboundedString_Container.Vector;
      EquipIndex, HighestLevel, WeaponSkillLevel: Positive;
      FactionIndex: Unbounded_String) return Unbounded_String with
      Pre =>
      (EquipIndex < 8 and
       HighestLevel < 101 and
       WeaponSkillLevel < 101 and
       Factions_List.Contains(FactionIndex)),
      Test_Case => (Name => "Test_GetRandomItem", Mode => Nominal);
      -- ****

end Mobs;
