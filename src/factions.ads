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
with Crew; use Crew;
with Game; use Game;

package Factions is

   type Reputation_Array is array(1 .. 2) of Integer;
   type RelationsRecord is -- Data structure for relations between factions
   record
      TargetFaction: Unbounded_String; -- Index of faction to which relation is described
      Reputation: Reputation_Array; -- Min and max value for starting reputation in bases owned by target faction
      Friendly: Boolean; -- Did target faction is friendly or enemy to this faction
   end record;
   package Relations_Container is new Vectors(Positive, RelationsRecord);
   type FactionRecord is -- Data structure for faction
   record
      Index: Unbounded_String; -- Index of faction, used in code
      Name: Unbounded_String; -- Name of faction, displayed to player
      MemberName: Unbounded_String; -- Name of single member of faction
      PluralMemberName: Unbounded_String; -- Plural name of members of faction
      SpawnChance: Attributes_Array; -- Chance that created at new game base will be owned by this faction
      Population: Attributes_Array; -- Min and max population for new bases with this faction as owner
      NamesType: Unbounded_String; -- Type of names of members of faction (used in generating names of ships)
      Relations: Relations_Container
        .Vector; -- Relations of this faction with others factions
      PlayerIndex: Unbounded_String; -- Index of mobile which will be used as starting character for player
      PlayerShipIndex: Unbounded_String; -- Index of proto ship which will be used as starting ship for player
      Description: Unbounded_String; -- Description on faction, displayed to player
      FoodTypes: UnboundedString_Container
        .Vector; -- Types of items used as food for members of this faction
      DrinksTypes: UnboundedString_Container
        .Vector; -- Types of items used as drinks for members of this faction
   end record;
   package Factions_Container is new Vectors(Positive, FactionRecord);
   Factions_List: Factions_Container.Vector;
   Factions_Directory_Not_Found: exception; -- Raised when no directory with factions files
   Factions_Files_Not_Found: exception; -- Raised when no files with factions

   procedure LoadFactions; -- Load NPC factions from file
   function GetReputation
     (SourceFaction,
      TargetFaction: Unbounded_String)
     return Integer; -- Get reputation between SourceFaction and TargetFaction
   function IsFriendly
     (SourceFaction,
      TargetFaction: Unbounded_String)
     return Boolean; -- Check if TargetFaction is friendly for SourceFaction. Returns true if yes, otherwise false.

end Factions;
