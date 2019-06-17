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
with GNAT.Directory_Operations; use GNAT.Directory_Operations;

package Game is

   type Date_Record is -- Data for game date/time
   record
      Year: Natural;
      Month: Natural;
      Day: Natural;
      Hour: Natural;
      Minutes: Natural;
   end record;
   GameDate: Date_Record;
   GameVersion: constant String := "Version: 3.9";
   package UnboundedString_Container is new Vectors(Positive,
      Unbounded_String);
   package Positive_Container is new Vectors(Positive, Positive);
   package Natural_Container is new Vectors(Positive, Natural);
   package Integer_Container is new Vectors(Positive, Integer);
   type Skill_Record is -- Data for skills
   record
      Name: Unbounded_String; -- Name of skill
      Attribute: Positive; -- Attribute used with that skill
      Description: Unbounded_String; -- Description of skill
      Tool: Unbounded_String; -- Item type used as tool for training that skill
   end record;
   package SkillsData_Container is new Vectors(Positive, Skill_Record);
   type Attribute_Record is -- Data for attributes
   record
      Name: Unbounded_String; -- Name of attribute
      Description: Unbounded_String; -- Description of attribute
   end record;
   package AttributesData_Container is new Vectors(Positive, Attribute_Record);
   Skills_List: SkillsData_Container.Vector; -- Contains data for all skills
   RepairTools: Unbounded_String; -- Name of item type used as tool in repairing/upgrading ship
   CleaningTools: Unbounded_String; -- Name of item type used as tool in cleaning ship
   AlchemyTools: Unbounded_String; -- Name of item type used as alchemy tools (mainly in deconstructing orders)
   CorpseIndex: Unbounded_String; -- Index of item used to create mobs corpses
   MissionItemsType: Unbounded_String; -- Name of item type used for delivery missions
   FuelType: Unbounded_String; -- Name of item type used as fuel for ships
   MoneyIndex: Unbounded_String; -- Index of item used as money
   MoneyName: Unbounded_String; -- Name of money (taken from MoneyIndex)
   SaveDirectory: Unbounded_String :=
     To_Unbounded_String
       ("data" & Dir_Separator & "saves" &
        Dir_Separator); -- Path to directory where are savegame and logs
   DataDirectory: Unbounded_String :=
     To_Unbounded_String
       ("data" &
        Dir_Separator); -- Path to directory where are game data files
   TradersName: Unbounded_String; -- Word used in ships names for traders ships (for events)
   Attributes_List: AttributesData_Container
     .Vector; -- Contains data for all characters attributes
   ConditionIndex: Positive; -- Index of attribute used as bonus to character condition
   StrengthIndex: Positive; -- Index of attribute used to count max character encumbrance
   PilotingSkill: Positive; -- Index of skill used to piloting ship
   EngineeringSkill: Positive; -- Index of skill used by engineer on ship
   GunnerySkill: Positive; -- Index of skill used by gunners
   TalkingSkill: Positive; -- Index of skill used for talk in bases or with other ships
   PerceptionSkill: Positive; -- Index of skill used for spoting
   DodgeSkill: Positive; -- Index of skill used for dodge in character's combat
   UnarmedSkill: Positive; -- Index of skill used for unarmed attacks in character's combat
   HeadArmor: Unbounded_String; -- Name of item type used as characters head armor
   ChestArmor: Unbounded_String; -- Name of item type used as characters torso armor
   ArmsArmor: Unbounded_String; -- Name of item type used as characters arms armor
   LegsArmor: Unbounded_String; -- Name of item type used as characters legs armor
   ShieldType: Unbounded_String; -- Name of item type used as characters shield
   WeaponType: Unbounded_String; -- Name of item type used as characters weapon
   DocDirectory: Unbounded_String :=
     To_Unbounded_String
       ("doc" & Dir_Separator); -- Path to directory where documentation is
   ModsDirectory: Unbounded_String :=
     To_Unbounded_String
       ("data" & Dir_Separator & "mods" &
        Dir_Separator); -- Path to directory where are game modifications
   PlayerCareer: Unbounded_String; -- Index of career selected by player during starting game
   ThemesDirectory: Unbounded_String :=
     To_Unbounded_String
       ("data" & Dir_Separator & "themes" &
        Dir_Separator); -- Path to directory where are ui themes
   type DataAction is
     (ADD, UPDATE, REMOVE); -- Possible actions to do when loading game data
   Data_Loading_Error: exception; -- Raised when error occurs during loading any game data

   procedure NewGame; -- Start new game: create map, place ship, crew, etc
   procedure UpdateGame
     (Minutes: Positive); -- Game ticks (update time, crew, ship, etc)
   procedure EndGame
     (Save: Boolean); -- Save (or not) game and clear all temporary data
   function FindSkillIndex(SkillName: Unbounded_String) return Natural with
      Pre => SkillName /=
      Null_Unbounded_String; -- Return vector index of selected skill
   function LoadGameData return String; -- Load game data from files

end Game;
