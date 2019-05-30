--    Copyright 2018 Bartek thindil Jasicki
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

with Ada.Containers.Vectors; use Ada.Containers;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with DOM.Readers; use DOM.Readers;
with Crew; use Crew;
with Game; use Game;

package Factions is

   type NamesTypes is
     (STANDARD, ROBOTIC); -- Types of names of members and bases factions
   type Reputation_Array is array(1 .. 2) of Integer;
   type RelationsRecord is -- Data structure for relations between factions
   record
      TargetFaction: Unbounded_String; -- Index of faction to which relation is described
      Reputation: Reputation_Array; -- Min and max value for starting reputation in bases owned by target faction
      Friendly: Boolean; -- Did target faction is friendly or enemy to this faction
   end record;
   package Relations_Container is new Vectors(Positive, RelationsRecord);
   type CareerRecord is -- Data structure for player career in faction
   record
      Index: Unbounded_String; -- Index of career
      ShipIndex: Unbounded_String; -- Index of proto ship which will be used as starting ship for player
      PlayerIndex: Unbounded_String; -- Index of mobile which will be used as starting character for player
      Description: Unbounded_String; -- Description of career, displayed to player
      Name: Unbounded_String; -- Name of career, may be different for each faction
   end record;
   package Careers_Container is new Vectors(Positive, CareerRecord);
   type FactionRecord is -- Data structure for faction
   record
      Index: Unbounded_String; -- Index of faction, used in code
      Name: Unbounded_String; -- Name of faction, displayed to player
      MemberName: Unbounded_String; -- Name of single member of faction
      PluralMemberName: Unbounded_String; -- Plural name of members of faction
      SpawnChance: Natural; -- Chance that created at new game base will be owned by this faction
      Population: Attributes_Array; -- Min and max population for new bases with this faction as owner
      NamesType: NamesTypes; -- Type of names of members of faction (used in generating names of ships)
      Relations: Relations_Container
        .Vector; -- Relations of this faction with others factions
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
        .Vector; -- List of possible careers for that faction
   end record;
   package Factions_Container is new Vectors(Positive, FactionRecord);
   Factions_List: Factions_Container.Vector;
   Factions_Remove_Error: exception; -- Raised when can't remove selected faction
   Factions_Adding_Error: exception; -- Raised when can't add selected faction

   procedure LoadFactions(Reader: Tree_Reader); -- Load NPC factions from file
   function GetReputation
     (SourceFaction, TargetFaction: Unbounded_String)
      return Integer; -- Get reputation between SourceFaction and TargetFaction
   function IsFriendly
     (SourceFaction, TargetFaction: Unbounded_String)
      return Boolean; -- Check if TargetFaction is friendly for SourceFaction. Returns true if yes, otherwise false.
end Factions;
