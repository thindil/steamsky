--    Copyright 2018-2021 Bartek thindil Jasicki
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
with Game; use Game;

-- ****h* Factions/Factions
-- FUNCTION
-- Provide code for factions
-- SOURCE
package Factions is
-- ****

   -- ****t* Factions/Factions.Names_Types
   -- FUNCTION
   -- Types of names of members and bases factions
   -- SOURCE
   type Names_Types is (STANDARD, ROBOTIC) with
      Default_Value => STANDARD;
   -- ****

   -- ****s* Factions/Factions.Relations_Record
   -- FUNCTION
   -- Data structure for relations between factions
   -- PARAMETERS
   -- Reputation - Min and max value for starting reputation in bases owned
   --              by target faction
   -- Friendly   - Did target faction is friendly or enemy to this faction
   -- SOURCE
   type Relations_Record is record
      Reputation: Reputation_Array;
      Friendly: Boolean;
   end record;
   -- ****

   -- ****t* Factions/Factions.Relations_Container
   -- FUNCTION
   -- Used to store relations data in faction
   -- SOURCE
   package Relations_Container is new Hashed_Maps
     (Key_Type => Unbounded_String, Element_Type => Relations_Record,
      Hash => Ada.Strings.Unbounded.Hash, Equivalent_Keys => "=");
   -- ****

   -- ****s* Factions/Factions.Career_Record
   -- FUNCTION
   -- Data structure for player career in faction
   -- PARAMETERS
   -- Ship_Index   - Index of proto ship which will be used as starting ship
   --                for player
   -- Player_Index - Index of mobile which will be used as starting character
   --                for player
   -- Description  - Description of career, displayed to player
   -- Name         - Name of career, may be different for each faction
   -- SOURCE
   type Career_Record is record
      Ship_Index: Unbounded_String;
      Player_Index: Unbounded_String;
      Description: Unbounded_String;
      Name: Unbounded_String;
   end record;
   -- ****

   -- ****t* Factions/Factions.Careers_Container
   -- FUNCTION
   -- Used to store careers data in faction
   -- SOURCE
   package Careers_Container is new Hashed_Maps
     (Key_Type => Unbounded_String, Element_Type => Career_Record,
      Hash => Ada.Strings.Unbounded.Hash, Equivalent_Keys => "=");
   -- ****

   -- ****t* Factions/Factions.BaseType_Container
   -- FUNCTION
   -- Used to store bases types data in faction
   -- SOURCE
   package BaseType_Container is new Hashed_Maps
     (Key_Type => Unbounded_String, Element_Type => Positive,
      Hash => Ada.Strings.Unbounded.Hash, Equivalent_Keys => "=");
   -- ****

   -- ****s* Factions/Factions.Faction_Record
   -- FUNCTION
   -- Data structure for faction
   -- PARAMETERS
   -- Name               - Name of faction, displayed to player
   -- Member_Name        - Name of single member of faction
   -- Plural_Member_Name - Plural name of members of faction
   -- Spawn_Chance       - Chance that created at new game base will be owned by
   --                      this faction
   -- Population         - Min and max population for new bases with this
   --                      faction as owner
   -- Names_Types        - Type of names of members of faction (used in
   --                      generating names of ships)
   -- Relations          - Relations of this faction with others factions
   -- Description        - Description on faction, displayed to player
   -- Food_Types         - Types of items used as food for members of this
   --                      faction
   -- Drinks_Types       - Types of items used as drinks for members of this
   --                      faction
   -- Healing_Tools      - Name of item type used as tool in healing members of
   --                      this faction
   -- Healing_Skill      - Vector index of skill used in healing members of this
   --                      faction
   -- Flags              - Various flags for faction (no gender, etc)
   -- Careers            - List of possible careers for that faction
   -- Base_Icon          - Character used as base icon on map for this faction
   -- Bases_Types        - List of available base types (with chances to spawn)
   --                      for this faction. If it is empty then all bases types
   --                      are available for this faction
   -- Weapon_Skill       - Vector index of skill used by prefered weapon of
   --                      members of this faction
   -- SOURCE
   type Faction_Record is record
      Name: Unbounded_String;
      Member_Name: Unbounded_String;
      Plural_Member_Name: Unbounded_String;
      Spawn_Chance: Natural := 0;
      Population: Attributes_Array;
      Names_Type: Names_Types;
      Relations: Relations_Container.Map;
      Description: Unbounded_String;
      Food_Types: UnboundedString_Container.Vector;
      Drinks_Types: UnboundedString_Container.Vector;
      Healing_Tools: Unbounded_String;
      Healing_Skill: SkillsData_Container.Extended_Index;
      Flags: UnboundedString_Container.Vector;
      Careers: Careers_Container.Map;
      Base_Icon: Wide_Character;
      Bases_Types: BaseType_Container.Map;
      Weapon_Skill: SkillsData_Container.Extended_Index;
   end record;
   -- ****

   -- ****t* Factions/Factions.Factions_Container
   -- FUNCTION
   -- Used to store factions data
   -- SOURCE
   package Factions_Container is new Hashed_Maps
     (Key_Type => Unbounded_String, Element_Type => Faction_Record,
      Hash => Ada.Strings.Unbounded.Hash, Equivalent_Keys => "=");
   -- ****

   -- ****v* Factions/Factions.Factions_List
   -- SOURCE
   Factions_List: Factions_Container.Map;
   -- ****

   -- ****f* Factions/Factions.Load_Factions
   -- FUNCTION
   -- Load NPC factions from file
   -- PARAMETERS
   -- Reader - XML Reader from which factions will be read
   -- SOURCE
   procedure Load_Factions(Reader: Tree_Reader);
   -- ****

   -- ****f* Factions/Factions.Get_Reputation
   -- FUNCTION
   -- Get reputation between Source_Faction and Target_Faction
   -- PARAMETERS
   -- Source_Faction - Index of first faction which reputation will be check
   -- Target_Faction - Index of second faction which reputation will be check
   -- RESULT
   -- Numeric reputation level between both factions
   -- SOURCE
   function Get_Reputation
     (Source_Faction, Target_Faction: Unbounded_String) return Integer with
      Pre =>
      (Factions_List.Contains(Key => Source_Faction) and
       Factions_List.Contains(Key => Target_Faction)),
      Test_Case => (Name => "Test_GetReputation", Mode => Nominal);
      -- ****

      -- ****f* Factions/Factions.IsFriendly
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
      (Factions_List.Contains(SourceFaction) and
       Factions_List.Contains(TargetFaction)),
      Test_Case => (Name => "Test_IsFriendly", Mode => Nominal);
      -- ****

      -- ****f* Factions/Factions.GetRandomFaction
      -- FUNCTION
      -- Select random faction from list
      -- RESULT
      -- Random index of faction
      -- SOURCE
   function GetRandomFaction return Unbounded_String with
      Test_Case => (Name => "Test_GetRandomFaction", Mode => Robustness);
      -- ****

end Factions;
