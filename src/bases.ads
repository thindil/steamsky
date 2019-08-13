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
   -- PARAMETERS
   -- Name       - Name of recruit
   -- Gender     - Gender of recruit
   -- Skills     - Names indexes, levels and experience in skills of recruit
   -- Cost       - Cost of enlist of recruit
   -- Attributes - Names indexes, levels and experience in attributes of
   --              recruit
   -- Inventory  - Owned items by recruit
   -- Equipment  - Items indexes from inventory used by recruit: 1 - weapon,
   --              2 - shield, 3 - helmet, 4 - torso, 5 - arms, 6 - legs,
   --              7 - tool
   -- Payment    - How much money recruit will take as payment each day.
   -- HomeBase   - Index of base from which recruit is
   -- Faction    - Index of faction to which recruit belongs
   -- SOURCE
   type Recruit_Data is record
      Name: Unbounded_String;
      Gender: Character;
      Skills: Skills_Container.Vector;
      Price: Positive;
      Attributes: Attributes_Container.Vector;
      Inventory: UnboundedString_Container.Vector;
      Equipment: Natural_Array(1 .. 7);
      Payment: Positive;
      HomeBase: Positive;
      Faction: Unbounded_String;
   end record;
   -- ****
   -- ****t* Bases/Recruit_Container
   -- FUNCTION
   -- Used to store sky bases recruits data
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
   -- PARAMETERS
   -- ProtoIndex - Index of item prototype
   -- Amount     - Amount of items
   -- Durability - Durability of items
   -- Price      - Current price of item
   -- SOURCE
   type Base_Cargo is record
      ProtoIndex: Unbounded_String;
      Amount: Natural;
      Durability: Positive;
      Price: Natural;
   end record;
   -- ****
   -- ****t* Bases/BaseCargo_Container
   -- FUNCTION
   -- Used to store sky bases cargos
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
   -- PARAMETERS
   -- Name           - Base name
   -- Visited        - Time when player last visited base
   -- SkyX           - X coordinate on sky map
   -- SkyY           - Y coordinate on sky map
   -- BaseType       - Type of base
   -- Population     - Amount of people in base
   -- RecruitDate    - Time when recruits was generated
   -- Recruits       - List of available recruits
   -- Known          - Did base is know to player
   -- AskedForBases  - Did player asked for bases in this base
   -- AskedForEvents - Time when players asked for events in this base
   -- Reputation     - Reputation level and progress of player
   -- MissionsDate   - Time when missions was generated
   -- Missions       - List of available missions
   -- Owner          - Index of faction which own base
   -- Cargo          - List of all cargo in base
   -- Size           - Size of base
   -- SOURCE
   type BaseRecord is record
      Name: Unbounded_String;
      Visited: Date_Record;
      SkyX: Integer;
      SkyY: Integer;
      BaseType: Bases_Types;
      Population: Natural;
      RecruitDate: Date_Record;
      Recruits: Recruit_Container.Vector;
      Known: Boolean;
      AskedForBases: Boolean;
      AskedForEvents: Date_Record;
      Reputation: Reputation_Array;
      MissionsDate: Date_Record;
      Missions: Mission_Container.Vector;
      Owner: Unbounded_String;
      Cargo: BaseCargo_Container.Vector;
      Size: Bases_Size;
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
   -- PARAMETERS
   -- BaseIndex - Index of the base in which player gained or lose reputation
   -- Points    - Amount of reputation points to gain or lose
   -- SOURCE
   procedure GainRep(BaseIndex: BasesRange; Points: Integer);
   -- ****
   -- ****f* Bases/CountPrice
   -- FUNCTION
   -- Count price for actions with bases (buying/selling/docking/ect)
   -- PARAMETERS
   -- Price       - Cost of action with the base
   -- TraderIndex - Index of crew member assigned as trader or 0 if noone is
   --               assigned
   -- Reduce      - If true, reduce cost of action, otherwise raise. Default
   --               is true
   -- RESULT
   -- Parameter Cost
   -- SOURCE
   procedure CountPrice
     (Price: in out Natural; TraderIndex: Crew_Container.Extended_Index;
      Reduce: Boolean := True) with
      Pre => TraderIndex <= PlayerShip.Crew.Last_Index;
      -- ****
      -- ****f* Bases/GenerateBaseName
      -- FUNCTION
      -- Generate random name for base based on faction
      -- PARAMETERS
      -- FactionIndex - Index of faction to which base belong
      -- RESULT
      -- Random name for the sky base
      -- SOURCE
   function GenerateBaseName
     (FactionIndex: Unbounded_String) return Unbounded_String with
      Pre => Factions_Container.Contains(Factions_List, FactionIndex);
      -- ****
      -- ****f* Bases/GenerateRecruits
      -- FUNCTION
      -- Generate if needed new recruits in base
      -- SOURCE
   procedure GenerateRecruits;
   -- ****
   -- ****f* Bases/AskForBases
   -- FUNCTION
   -- Ask in base for direction for other bases
   -- SOURCE
   procedure AskForBases;
   -- ****
   -- ****f* Bases/AskForEvents
   -- FUNCTION
   -- Ask in base for direction for random events
   -- SOURCE
   procedure AskForEvents;
   -- ****
   -- ****f* Bases/UpdatePopulation
   -- FUNCTION
   -- Update base population if needed
   -- SOURCE
   procedure UpdatePopulation;
   -- ****
   -- ****f* Bases/UpdatePrices
   -- FUNCTION
   -- Random changes of items prices in base
   -- SOURCE
   procedure UpdatePrices;
   -- ****

end Bases;
