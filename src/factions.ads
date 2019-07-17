--    Copyright 2018-2019 Bartek thindil Jasicki
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
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with DOM.Readers; use DOM.Readers;
with Crew; use Crew;
with Game; use Game;

package Factions is

-- ****t* Factions/NamesTypes
-- FUNCTION
-- Types of names of members and bases factions
-- SOURCE
   type NamesTypes is (STANDARD, ROBOTIC);
-- ****

-- ****t* Factions/Reputation_Array
-- SOURCE
   type Reputation_Array is array(1 .. 2) of Integer;
-- ****

-- ****t* Factions/RelationsRecord
-- FUNCTION
-- Data structure for relations between factions
-- SOURCE
   type RelationsRecord is record
      Reputation: Reputation_Array; -- Min and max value for starting reputation in bases owned by target faction
      Friendly: Boolean; -- Did target faction is friendly or enemy to this faction
   end record;
-- ****

-- ****t* Factions/Relations_Container
-- SOURCE
   package Relations_Container is new Hashed_Maps(Unbounded_String,
      RelationsRecord, Ada.Strings.Unbounded.Hash, "=");
-- ****

-- ****t* Factions/CareerRecord
-- FUNCTION
-- Data structure for player career in faction
-- SOURCE
   type CareerRecord is record
      ShipIndex: Unbounded_String; -- Index of proto ship which will be used as starting ship for player
      PlayerIndex: Unbounded_String; -- Index of mobile which will be used as starting character for player
      Description: Unbounded_String; -- Description of career, displayed to player
      Name: Unbounded_String; -- Name of career, may be different for each faction
   end record;
-- ****

-- ****t* Factions/Careers_Container
-- SOURCE
   package Careers_Container is new Hashed_Maps(Unbounded_String, CareerRecord,
      Ada.Strings.Unbounded.Hash, "=");
-- ****

-- ****t* Factions/FactionRecord
-- FUNCTION
-- Data structure for faction
-- SOURCE
   type FactionRecord is record
      Name: Unbounded_String; -- Name of faction, displayed to player
      MemberName: Unbounded_String; -- Name of single member of faction
      PluralMemberName: Unbounded_String; -- Plural name of members of faction
      SpawnChance: Natural; -- Chance that created at new game base will be owned by this faction
      Population: Attributes_Array; -- Min and max population for new bases with this faction as owner
      NamesType: NamesTypes; -- Type of names of members of faction (used in generating names of ships)
      Relations: Relations_Container
        .Map; -- Relations of this faction with others factions
      Description: Unbounded_String; -- Description on faction, displayed to player
      FoodTypes: UnboundedString_Container
        .Vector; -- Types of items used as food for members of this faction
      DrinksTypes: UnboundedString_Container
        .Vector; -- Types of items used as drinks for members of this faction
      HealingTools: Unbounded_String; -- Name of item type used as tool in healing members of this faction
      HealingSkill: Positive; -- Vector index of skill used in healing members of this faction
      Flags: UnboundedString_Container
        .Vector; -- Various flags for faction (no gender, etc)
      Careers: Careers_Container
        .Map; -- List of possible careers for that faction
      BaseIcon: Wide_Character; -- Character used as base icon on map for this faction
   end record;
-- ****

-- ****t* Factions/Factions_Container
-- SOURCE
   package Factions_Container is new Hashed_Maps(Unbounded_String,
      FactionRecord, Ada.Strings.Unbounded.Hash, "=");
-- ****

-- ****v* Factions/Factions_List
-- SOURCE
   Factions_List: Factions_Container.Map;
-- ****

-- ****f* Factions/LoadFactions
-- FUNCTION
-- Load NPC factions from file
-- SOURCE
   procedure LoadFactions(Reader: Tree_Reader);
-- ****
-- ****f* Factions/GetReputation
-- FUNCTION
-- Get reputation between SourceFaction and TargetFaction
-- SOURCE
   function GetReputation
     (SourceFaction, TargetFaction: Unbounded_String) return Integer with
      Pre =>
      (Factions_Container.Contains(Factions_List, SourceFaction) and
       Factions_Container.Contains(Factions_List, TargetFaction));
-- ****
-- ****f* Factions/IsFriendly
-- FUNCTION
-- Check if TargetFaction is friendly for SourceFaction. Returns true if yes, otherwise false.
-- SOURCE
   function IsFriendly
     (SourceFaction, TargetFaction: Unbounded_String) return Boolean with
      Pre =>
      (Factions_Container.Contains(Factions_List, SourceFaction) and
       Factions_Container.Contains(Factions_List, TargetFaction));
-- ****
-- ****f* Factions/GetRandomFaction
-- FUNCTION
-- Select random faction from list
-- SOURCE
   function GetRandomFaction return Unbounded_String;
-- ****

end Factions;
