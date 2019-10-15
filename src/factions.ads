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

-- ****h* Steamsky/Factions
-- FUNCTION
-- Provide code for factions
-- SOURCE
package Factions is
-- ****

   -- ****t* Factions/NamesTypes
   -- FUNCTION
   -- Types of names of members and bases factions
   -- SOURCE
   type NamesTypes is (STANDARD, ROBOTIC);
   -- ****

   -- ****t* Factions/Reputation_Array
   -- FUNCTION
   -- Minium and maximum reputation values
   -- SOURCE
   type Reputation_Array is array(1 .. 2) of Integer;
   -- ****

   -- ****t* Factions/RelationsRecord
   -- FUNCTION
   -- Data structure for relations between factions
   -- PARAMETERS
   -- Reputation - Min and max value for starting reputation in bases owned
   --              by target faction
   -- Friendly   - Did target faction is friendly or enemy to this faction
   -- SOURCE
   type RelationsRecord is record
      Reputation: Reputation_Array;
      Friendly: Boolean;
   end record;
   -- ****

   -- ****t* Factions/Relations_Container
   -- FUNCTION
   -- Used to store relations data in faction
   -- SOURCE
   package Relations_Container is new Hashed_Maps(Unbounded_String,
      RelationsRecord, Ada.Strings.Unbounded.Hash, "=");
   -- ****

   -- ****t* Factions/CareerRecord
   -- FUNCTION
   -- Data structure for player career in faction
   -- PARAMETERS
   -- ShipIndex   - Index of proto ship which will be used as starting ship
   --               for player
   -- PlayerIndex - Index of mobile which will be used as starting character
   --               for player
   -- Description - Description of career, displayed to player
   -- Name        - Name of career, may be different for each faction
   -- SOURCE
   type CareerRecord is record
      ShipIndex: Unbounded_String;
      PlayerIndex: Unbounded_String;
      Description: Unbounded_String;
      Name: Unbounded_String;
   end record;
   -- ****

   -- ****t* Factions/Careers_Container
   -- FUNCTION
   -- Used to store careers data in faction
   -- SOURCE
   package Careers_Container is new Hashed_Maps(Unbounded_String, CareerRecord,
      Ada.Strings.Unbounded.Hash, "=");
   -- ****

   -- ****t* Factions/BaseType_Container
   -- FUNCTION
   -- Used to store bases types data in faction
   -- SOURCE
   package BaseType_Container is new Hashed_Maps(Unbounded_String, Positive,
      Ada.Strings.Unbounded.Hash, "=");
   -- ****

   -- ****
   -- ****t* Factions/FactionRecord
   -- FUNCTION
   -- Data structure for faction
   -- PARAMETERS
   -- Name             - Name of faction, displayed to player
   -- MemberName       - Name of single member of faction
   -- PluralMemberName - Plural name of members of faction
   -- SpawnChance      - Chance that created at new game base will be owned by
   --                    this faction
   -- Population       - Min and max population for new bases with this
   --                    faction as owner
   -- NamesTypes       - Type of names of members of faction (used in
   --                    generating names of ships)
   -- Relations        - Relations of this faction with others factions
   -- Description      - Description on faction, displayed to player
   -- FoodTypes        - Types of items used as food for members of this
   --                    faction
   -- DrinksTypes      - Types of items used as drinks for members of this
   --                    faction
   -- HealingTools     - Name of item type used as tool in healing members of
   --                    this faction
   -- HealingSkill     - Vector index of skill used in healing members of this
   --                    faction
   -- Flags            - Various flags for faction (no gender, etc)
   -- Careers          - List of possible careers for that faction
   -- BaseIcon         - Character used as base icon on map for this faction
   -- BasesTypes       - List of available base types (with chances to spawn)
   --                    for this faction. If it is empty then all bases types
   --                    are available for this faction
   -- SOURCE
   type FactionRecord is record
      Name: Unbounded_String;
      MemberName: Unbounded_String;
      PluralMemberName: Unbounded_String;
      SpawnChance: Natural;
      Population: Attributes_Array;
      NamesType: NamesTypes;
      Relations: Relations_Container.Map;
      Description: Unbounded_String;
      FoodTypes: UnboundedString_Container.Vector;
      DrinksTypes: UnboundedString_Container.Vector;
      HealingTools: Unbounded_String;
      HealingSkill: Positive;
      Flags: UnboundedString_Container.Vector;
      Careers: Careers_Container.Map;
      BaseIcon: Wide_Character;
      BasesTypes: BaseType_Container.Map;
   end record;
   -- ****

   -- ****t* Factions/Factions_Container
   -- FUNCTION
   -- Used to store factions data
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
   -- PARAMETERS
   -- Reader - XML Reader from which factions will be read
   -- SOURCE
   procedure LoadFactions(Reader: Tree_Reader);
   -- ****

   -- ****f* Factions/GetReputation
   -- FUNCTION
   -- Get reputation between SourceFaction and TargetFaction
   -- PARAMETERS
   -- SourceFaction - Index of first faction which reputation will be check
   -- TargetFaction - Index of second faction which reputation will be check
   -- RESULT
   -- Numeric reputation level between both factions
   -- SOURCE
   function GetReputation
     (SourceFaction, TargetFaction: Unbounded_String) return Integer with
      Pre =>
      (Factions_Container.Contains(Factions_List, SourceFaction) and
       Factions_Container.Contains(Factions_List, TargetFaction)),
      Test_Case => ("Test_GetReputation", Nominal);
      -- ****

      -- ****f* Factions/IsFriendly
      -- FUNCTION
      -- Check if TargetFaction is friendly for SourceFaction. Returns true if yes, otherwise false.
      -- PARAMETERS
      -- SourceFaction - Index of base faction to which TargetFaction will be checked
      -- TargetFaction - Index of faction to check
      -- RESULT
      -- True if factions are friendly between self, otherwise false
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
      -- RESULT
      -- Random index of faction
      -- SOURCE
   function GetRandomFaction return Unbounded_String;
   -- ****

end Factions;
