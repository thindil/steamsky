--    Copyright 2016-2018 Bartek thindil Jasicki
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

package Bases is

   type Bases_Types is
     (Industrial, Agricultural, Refinery, Shipyard, Military, Any);
   type Recruit_Data is -- Data structure for recruits
   record
      Name: Unbounded_String; -- Name of recruit
      Gender: Character; -- Gender of recruit
      Skills: Skills_Container
        .Vector; -- Names indexes, levels and experience in skills of recruit
      Price: Positive; -- Cost of enlist of recruit
      Attributes: Attributes_Container
        .Vector; -- Names indexes, levels and experience in attributes of recruit
      Inventory: Positive_Container.Vector; -- Owned items by recruit
      Equipment: Equipment_Array; -- Items indexes from inventory used by recruit: 1 - weapon, 2 - shield, 3 - helmet, 4 - torso, 5 - arms, 6 - legs, 7 - tool
      Payment: Positive; -- How much money recruit will take as payment each day.
      HomeBase: Positive; -- Index of base from which recruit is
      Faction: Positive; -- Index of faction to which recruit belongs
   end record;
   package Recruit_Container is new Vectors(Positive, Recruit_Data);
   type Reputation_Array is
     array
       (1 ..
            2) of Integer; -- Data structure for reputation, 1 = level, 2 = points to next level
   type Base_Cargo is -- Data structure for bases cargo
   record
      ProtoIndex: Positive; -- Index of item prototype
      Amount: Natural; -- Amount of items
      Durability: Positive; -- Durability of items
      Price: Natural; -- Current price of item
   end record;
   package BaseCargo_Container is new Vectors(Positive, Base_Cargo);
   type Bases_Size is (Small, Medium, Big);
   type BaseRecord is -- Data structure for bases
   record
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
      Owner: Positive; -- Index of faction which own base
      Cargo: BaseCargo_Container.Vector; -- List of all cargo in base
      Size: Bases_Size; -- Size of base
   end record;
   SkyBases: array(1 .. 1024) of BaseRecord; -- List of sky bases
   BaseSyllablesPre: UnboundedString_Container.Vector;
   BaseSyllablesStart: UnboundedString_Container.Vector;
   BaseSyllablesEnd: UnboundedString_Container.Vector;
   BaseSyllablesPost: UnboundedString_Container.Vector;

   procedure GainRep
     (BaseIndex: Positive;
      Points: Integer); -- Gain reputation in selected base
   procedure CountPrice
     (Price: in out Positive; TraderIndex: Natural;
      Reduce: Boolean :=
        True); -- Count price for actions with bases (buying/selling/docking/ect)
   function GenerateBaseName
     (FactionIndex: Positive)
      return Unbounded_String; -- Generate random name for base based on faction
   procedure GenerateRecruits; -- Generate if needed new recruits in base
   procedure AskForBases; -- Ask in base for direction for other bases
   procedure AskForEvents; -- Ask in base for direction for random events
   procedure UpdatePopulation; -- Update base population if needed
   procedure UpdatePrices; -- Random changes of items prices in base

end Bases;
