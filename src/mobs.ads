--    Copyright 2017-2023 Bartek thindil Jasicki
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
with Crew; use Crew;
with Game; use Game;

-- ****h* Mobs/Mobs
-- FUNCTION
-- Provides code for manipulate mobiles
-- SOURCE
package Mobs is
-- ****

   -- ****s* Mobs/Mobs.Mob_Inventory_Record
   -- FUNCTION
   -- Data structure for mobs inventory
   -- PARAMETERS
   -- Proto_Index - Proto index of item in mob inventory
   -- Min_Amount  - Minimal amount of item in mob inventory
   -- Max_Amount  - Maximum amount of item in mob inventory
   -- SOURCE
   type Mob_Inventory_Record is record
      Proto_Index: Natural := 0;
      Min_Amount: Natural range 0 .. 100_000 := 0;
      Max_Amount: Natural range 0 .. 100_000 := 0;
   end record;
   -- ****

   --## rule off REDUCEABLE_SCOPE
   -- ****d* Mobs/Mobs.Empty_Mob_Item
   -- FUNCTION
   -- Empty item for mobs prototype
   -- SOURCE
   Empty_Mob_Item: constant Mob_Inventory_Record :=
     (Proto_Index => 0, Min_Amount => 0, Max_Amount => 0);
   -- ****
   --## rule on REDUCEABLE_SCOPE

     -- ****t* Mobs/Mobs.Inventory_Amount_Range
     -- FUNCTION
     -- Used to set the amount of items in mobs inventories
     -- HISTORY
     -- 7.0 - Added
     -- SOURCE
   subtype Inventory_Amount_Range is Count_Type range 1 .. 32;
   -- ****

   -- ****d* Mobs/Mobs.Default_Inventory_Amount
   -- FUNCTION
   -- The default amount of the items in mobs inventories
   -- HISTORY
   -- 7.0 -  Added
   -- SOURCE
   Default_Inventory_Amount: constant Inventory_Amount_Range := 32;
   -- ****

   -- ****t* Mobs/Mobs.MobInventory_Container
   -- FUNCTION
   -- Used for store mobiles inventories
   -- SOURCE
   package MobInventory_Container is new Formal_Vectors
     (Index_Type => Inventory_Amount_Range,
      Element_Type => Mob_Inventory_Record);
   -- ****

   -- ****s* Mobs/Mobs.Proto_Mob_Record
   -- FUNCTION
   -- Data structure for mobs prototypes
   -- PARAMETERS
   -- Skills     - Names indexes, levels and experience in skills of mob
   -- Attributes - Levels and experience in attributes of mob
   -- Order      - Current order for mob
   -- Priorities - Priority of orders of mob
   -- Inventory  - List of mob inventory
   -- Equipment  - Items indexes from inventory used by mob.
   -- SOURCE
   type Proto_Mob_Record is new Mob_Record with record
      Order: Crew_Orders;
      Priorities: Natural_Array(1 .. 12);
      Inventory: MobInventory_Container.Vector
        (Capacity => Default_Inventory_Amount);
      Equipment: Equipment_Array;
   end record;
   -- ****

   function Get_Proto_Mob(Index: Positive) return Proto_Mob_Record;

   --## rule off REDUCEABLE_SCOPE
   -- ****d* Mobs/Mobs.Empty_Mob
   -- FUNCTION
   -- Empty prototype of mob
   -- SOURCE
   Empty_Mob: constant Proto_Mob_Record :=
     (Amount_Of_Attributes => 1, Amount_Of_Skills => 1, others => <>);
   -- ****
   --## rule on REDUCEABLE_SCOPE

   -- ****t* Mobs/Mobs.Proto_Mobs_Amount_Range
   -- FUNCTION
   -- Used to set the amount of mobiles' prototypes available in the game
   -- HISTORY
   -- 7.0 - Added
   -- SOURCE
   subtype Proto_Mobs_Amount_Range is Positive range 1 .. 256;
   -- ****

   --## rule off REDUCEABLE_SCOPE
   -- ****d* Mobs/Mobs.Default_Proto_Mobs_Amount
   -- FUNCTION
   -- The default amount of mobiles' prototypes in the game
   -- HISTORY
   -- 7.0 - Added
   -- SOURCE
   Default_Proto_Mobs_Amount: constant Proto_Mobs_Amount_Range := 113;
   -- ****
   --## rule on REDUCEABLE_SCOPE

   -- ****f* Mobs/Mobs.Generate_Mob
   -- FUNCTION
   -- Generate mob from selected prototype and faction.
   -- PARAMETERS
   -- Mob_Index     - Prototype index from ProtoMobs_List from which the mob
   --                 will be generated
   -- Faction_Index - Faction index from Factions_List to which the generated
   --                 mob will be belong
   -- RESULT
   -- Newly generated mob
   -- SOURCE
   function Generate_Mob
     (Mob_Index: Positive; Faction_Index: Tiny_String.Bounded_String)
      return Member_Data with
      Pre => Mob_Index > 0 and Mob_Index < Get_Proto_Mobs_Amount,
      Post => Tiny_String.Length(Source => Generate_Mob'Result.Name) > 0;
      -- ****

-- Temporary code to interact with Nim

   function Get_Proto_Mobs_Amount return Positive with
      Import => True,
      Convention => C,
      External_Name => "adaGetProtoMobsAmount";

end Mobs;
