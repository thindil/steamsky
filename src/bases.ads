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
with Ada.Containers.Vectors; use Ada.Containers;
with Game; use Game;
with Crew; use Crew;
with Missions; use Missions;
with Ships; use Ships;
with Factions; use Factions;

-- ****h* Steamsky/Bases
-- FUNCTION
-- Provide code for manipulate sky bases
-- SOURCE
package Bases is
-- ****

   -- ****t* Bases/Bases_Types
   -- FUNCTION
   -- Types of bases
   -- SOURCE
   type Bases_Types is
     (Industrial, Agricultural, Refinery, Shipyard, Military, Any);
   -- ****
   -- ****t* Bases/Recruit_Data
   -- FUNCTION
   -- Data structure for recruits
   -- SOURCE
   type Recruit_Data is record
      Name: Unbounded_String; -- Name of recruit
      Gender: Character; -- Gender of recruit
      Skills: Skills_Container
        .Vector; -- Names indexes, levels and experience in skills of recruit
      Price: Positive; -- Cost of enlist of recruit
      Attributes: Attributes_Container
        .Vector; -- Names indexes, levels and experience in attributes of recruit
      Inventory: UnboundedString_Container.Vector; -- Owned items by recruit
      Equipment: Equipment_Array; -- Items indexes from inventory used by recruit: 1 - weapon, 2 - shield, 3 - helmet, 4 - torso, 5 - arms, 6 - legs, 7 - tool
      Payment: Positive; -- How much money recruit will take as payment each day.
      HomeBase: Positive; -- Index of base from which recruit is
      Faction: Unbounded_String; -- Index of faction to which recruit belongs
   end record;
   -- ****

   -- ****t* Bases/Recruit_Container
   -- SOURCE
   package Recruit_Container is new Vectors(Positive, Recruit_Data);
   -- ****

   -- ****t* Bases/Reputation_Array
   -- FUNCTION
   -- Data structure for reputation, 1 = level, 2 = points to next level
   -- SOURCE
   type Reputation_Array is array(1 .. 2) of Integer;
   -- ****
   -- ****t* Bases/Base_Cargo
   -- FUNCTION
   -- Data structure for bases cargo
   -- SOURCE
   type Base_Cargo is record
      ProtoIndex: Unbounded_String; -- Index of item prototype
      Amount: Natural; -- Amount of items
      Durability: Positive; -- Durability of items
      Price: Natural; -- Current price of item
   end record;
   -- ****
   -- ****t* Bases/BaseCargo_Container
   -- SOURCE
   package BaseCargo_Container is new Vectors(Positive, Base_Cargo);
   -- ****
   -- ****t* Bases/Bases_Size
   -- FUNCTION
   -- Bases sizes
   -- SOURCE
   type Bases_Size is (Small, Medium, Big);
   -- ****
   -- ****t* Bases/BaseRecord
   -- FUNCTION
   -- Data structure for bases
   -- SOURCE
   type BaseRecord is record
      Name: Unbounded_String; -- Base name
      Visited: Date_Record; -- Time when player last visited base
      SkyX: Integer; -- X coordinate on sky map
      SkyY: Integer; -- Y coordinate on sky map
      BaseType: Bases_Types; -- Type of base
      Population: Natural; -- Amount of people in base
      RecruitDate: Date_Record; -- Time when recruits was generated
      Recruits: Recruit_Container.Vector; -- List of available recruits
      Known: Boolean; -- Did base is know to player
      AskedForBases: Boolean; -- Did player asked for bases in this base
      AskedForEvents: Date_Record; -- Time when players asked for events in this base
      Reputation: Reputation_Array; -- Reputation level and progress of player
      MissionsDate: Date_Record; -- Time when missions was generated
      Missions: Mission_Container.Vector; -- List of available missions
      Owner: Unbounded_String; -- Index of faction which own base
      Cargo: BaseCargo_Container.Vector; -- List of all cargo in base
      Size: Bases_Size; -- Size of base
   end record;
   -- ****
   -- ****t* Bases/BasesRange
   -- FUNCTION
   -- Amount of sky bases
   -- SOURCE
   subtype BasesRange is Positive range 1 .. 1024;
   -- ****
   -- ****v* Bases/SkyBases
   -- FUNCTION
   -- List of sky bases
   -- SOURCE
   SkyBases: array(BasesRange) of BaseRecord;
   -- ****
   -- ****v* Bases/BaseSyllablesPre
   -- FUNCTION
   -- List of pre syllables for generating bases names
   -- SOURCE
   BaseSyllablesPre: UnboundedString_Container.Vector;
   -- ****
   -- ****v* Bases/BaseSyllablesStart
   -- FUNCTION
   -- List of first syllables for generating bases names
   -- SOURCE
   BaseSyllablesStart: UnboundedString_Container.Vector;
   -- ****
   -- ****v* Bases/BaseSyllablesEnd
   -- FUNCTION
   -- List of second syllables for generating bases names
   -- SOURCE
   BaseSyllablesEnd: UnboundedString_Container.Vector;
   -- ****
   -- ****v* Bases/BaseSyllablesPost
   -- FUNCTION
   -- List of post syllables for generating bases names
   -- SOURCE
   BaseSyllablesPost: UnboundedString_Container.Vector;
   -- ****

   -- ****f* Bases/GainRep
   -- FUNCTION
   -- Gain reputation in selected base
   -- SOURCE
   procedure GainRep(BaseIndex: BasesRange; Points: Integer);
   -- ****
   -- ****f* Bases/CountPrice
   -- FUNCTION
   -- Count price for actions with bases (buying/selling/docking/ect)
   -- SOURCE
   procedure CountPrice
     (Price: in out Positive; TraderIndex: Crew_Container.Extended_Index;
      Reduce: Boolean := True) with
      Pre => TraderIndex <= PlayerShip.Crew.Last_Index;
      -- ****
      -- ****f* Bases/GenerateBaseName
      -- FUNCTION
      -- Generate random name for base based on faction
      -- SOURCE
   function GenerateBaseName
     (FactionIndex: Unbounded_String) return Unbounded_String with
      Pre => Factions_Container.Contains(Factions_List, FactionIndex);
      -- ****
      -- ****f* Bases/GenerateRecruits;
      -- FUNCTION
      -- Generate if needed new recruits in base
      -- SOURCE
   procedure GenerateRecruits;
   -- ****
   -- ****f* Bases/AskForBases;
   -- FUNCTION
   -- Ask in base for direction for other bases
   -- SOURCE
   procedure AskForBases;
   -- ****
   -- ****f* Bases/AskForEvents;
   -- FUNCTION
   -- Ask in base for direction for random events
   -- SOURCE
   procedure AskForEvents;
   -- ****
   -- ****f* Bases/UpdatePopulation;
   -- FUNCTION
   -- Update base population if needed
   -- SOURCE
   procedure UpdatePopulation;
   -- ****
   -- ****f* Bases/UpdatePrices;
   -- FUNCTION
   -- Random changes of items prices in base
   -- SOURCE
   procedure UpdatePrices;
   -- ****

end Bases;
