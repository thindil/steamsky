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

with Ada.Containers.Formal_Indefinite_Vectors; use Ada.Containers;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Game; use Game;
with Crew; use Crew;
with Items; use Items;
with Missions; use Missions;

-- ****h* Bases/Bases
-- FUNCTION
-- Provide code for manipulate sky bases
-- SOURCE
package Bases is
-- ****

   --## rule off TYPE_INITIAL_VALUES
   -- ****s* Bases/Bases.Recruit_Data
   -- FUNCTION
   -- Data structure for recruits
   -- PARAMETERS
   -- Name       - Name of recruit
   -- Gender     - Gender of recruit
   -- Skills     - Names indexes, levels and experience in skills of recruit
   -- Cost       - Cost of enlist of recruit
   -- Attributes - Names indexes, levels and experience in attributes of
   --              recruit
   -- Inventory  - Owned items by recruit
   -- Equipment  - Items indexes from inventory used by recruit.
   -- Payment    - How much money recruit will take as payment each day.
   -- Home_Base  - Index of base from which recruit is
   -- Faction    - Index of faction to which recruit belongs
   -- SOURCE
   type Recruit_Data is new Mob_Record with record
      Name: Tiny_String.Bounded_String;
      Gender: Character;
      Price: Positive;
      Inventory: Positive_Formal_Container.Vector (Capacity => 7);
      Equipment: Equipment_Array;
      Payment: Positive;
      Home_Base: Bases_Range;
      Faction: Tiny_String.Bounded_String;
   end record;
   -- ****

   type Nim_Recruit_Data is record
      Attributes: Nim_Attributes_Array;
      Skills: Nim_Skills_Array;
      Name: chars_ptr;
      Gender: Character;
      Equipment: Nim_Equipment_Array;
      Payment: Integer;
      Home_Base: Integer;
      Faction: chars_ptr;
      Price: Integer;
      Inventory: Nim_Equipment_Array;
   end record;
   --## rule on TYPE_INITIAL_VALUES

   function Recruit_To_Nim(Recruit: Recruit_Data) return Nim_Recruit_Data;
   procedure Recruit_From_Nim
     (Recruit: Nim_Recruit_Data; Ada_Recruit: in out Recruit_Data);

   -- ****t* Bases/Bases.Recruit_Amount_Range
   -- FUNCTION
   -- Used to set the amount of available recruits in bases
   -- HISTORY
   -- 7.5 - Added
   -- SOURCE
   subtype Recruit_Amount_Range is Positive range 1 .. 60;
   -- ****

   -- ****t* Bases/Bases.Recruit_Container
   -- FUNCTION
   -- Used to store sky bases recruits data
   -- SOURCE
   package Recruit_Container is new Formal_Indefinite_Vectors
     (Index_Type => Recruit_Amount_Range, Element_Type => Recruit_Data,
      Max_Size_In_Storage_Elements => Recruit_Data'Size, Bounded => False);
   -- ****

   -- ****s* Bases/Bases.Base_Cargo
   -- FUNCTION
   -- Data structure for bases cargo
   -- PARAMETERS
   -- Proto_Index - Index of item prototype
   -- Amount      - Amount of items
   -- Durability  - Durability of items
   -- Price       - Current price of item
   -- SOURCE
   type Base_Cargo is record
      Proto_Index: Natural;
      Amount: Natural;
      Durability: Items_Durability;
      Price: Natural;
   end record;
   -- ****

   --## rule off REDUCEABLE_SCOPE
   -- ****d* Bases/Bases.Empty_Base_Cargo
   -- FUNCTION
   -- Empty base cargo
   -- SOURCE
   Empty_Base_Cargo: constant Base_Cargo :=
     (Proto_Index => 0, Amount => 0, Durability => Default_Item_Durability,
      Price => 0);
   -- ****
   --## rule on REDUCEABLE_SCOPE

   -- ****t* Bases/Bases.BaseCargo_Container
   -- FUNCTION
   -- Used to store sky bases cargos
   -- SOURCE
   package BaseCargo_Container is new Formal_Indefinite_Vectors
     (Index_Type => Positive, Element_Type => Base_Cargo,
      Max_Size_In_Storage_Elements => Base_Cargo'Size, Bounded => False);
   -- ****

   -- ****t* Bases/Bases.Bases_Size
   -- FUNCTION
   -- Bases sizes
   -- SOURCE
   type Bases_Size is (SMALL, MEDIUM, BIG, UNKNOWN) with
      Default_Value => MEDIUM;
      -- ****

      --## rule off REDUCEABLE_SCOPE
      -- ****d* Bases/Bases.Default_Base_Size
      -- FUNCTION
      -- The default size of a base
      -- SOURCE
   Default_Base_Size: constant Bases_Size := MEDIUM;
   -- ****
   --## rule on REDUCEABLE_SCOPE

   --## rule off TYPE_INITIAL_VALUES
   -- ****s* Bases/Bases.Base_Record
   -- FUNCTION
   -- Data structure for bases
   -- PARAMETERS
   -- Name             - Base name
   -- Visited          - Time when player last visited base
   -- Sky_X            - X coordinate on sky map
   -- Sky_Y            - Y coordinate on sky map
   -- Base_Type        - Type of base
   -- Population       - Amount of people in base
   -- Recruit_Date     - Time when recruits was generated
   -- Recruits         - List of available recruits
   -- Known            - Did base is know to player
   -- Asked_For_Bases  - Did player asked for bases in this base
   -- Asked_For_Events - Time when players asked for events in this base
   -- Reputation       - Reputation level and progress of player
   -- Missions_Date    - Time when missions was generated
   -- Missions         - List of available missions
   -- Owner            - Index of faction which own base
   -- Cargo            - List of all cargo in base
   -- Size             - Size of base
   -- SOURCE
   type Base_Record is record
      Name: Tiny_String.Bounded_String;
      Visited: Date_Record;
      Sky_X: Map_X_Range;
      Sky_Y: Map_Y_Range;
      Base_Type: Tiny_String.Bounded_String;
      Population: Natural;
      Recruit_Date: Date_Record;
      Recruits: Recruit_Container.Vector (Capacity => 5);
      Known: Boolean;
      Asked_For_Bases: Boolean;
      Asked_For_Events: Date_Record;
      Reputation: Reputation_Data;
      Missions_Date: Date_Record;
      Missions: Mission_Container.Vector;
      Owner: Tiny_String.Bounded_String;
      Cargo: BaseCargo_Container.Vector (Capacity => 32);
      Size: Bases_Size;
   end record;
   -- ****
   --## rule on TYPE_INITIAL_VALUES

   -- ****v* Bases/Bases.SkyBases
   -- FUNCTION
   -- List of sky bases
   -- SOURCE
   Sky_Bases: array(Bases_Range) of Base_Record;
   -- ****

      -- ****f* Bases/Bases.Generate_Base_Name
      -- FUNCTION
      -- Generate random name for base based on faction
      -- PARAMETERS
      -- Faction_Index - Index of faction to which base belong
      -- RESULT
      -- Random name for the sky base
      -- SOURCE
   function Generate_Base_Name
     (Faction_Index: Tiny_String.Bounded_String)
      return Tiny_String.Bounded_String with
      Post => Tiny_String.Length(Source => Generate_Base_Name'Result) > 0;
      -- ****

-- Temporary code to interact with Nim

   procedure Get_Ada_Base_Location
     (Base_Index: Bases_Range; X: Map_X_Range; Y: Map_Y_Range) with
      Import => True,
      Convention => C,
      External_Name => "getAdaBaseLocation";

   procedure Set_Base_In_Nim(Base_Index: Bases_Range);

   procedure Get_Base_From_Nim(Base_Index: Bases_Range);

end Bases;
