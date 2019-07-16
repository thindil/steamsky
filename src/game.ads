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

   -- Data for game date/time
   type Date_Record is record
      Year: Natural;
      Month: Natural;
      Day: Natural;
      Hour: Natural;
      Minutes: Natural;
   end record;
   GameDate: Date_Record;
   GameVersion: constant String := "Version: 4.0";
   package UnboundedString_Container is new Vectors(Positive,
      Unbounded_String);
   package Positive_Container is new Vectors(Positive, Positive);
   package Natural_Container is new Vectors(Positive, Natural);
   package Integer_Container is new Vectors(Positive, Integer);
   -- Data for skills
   type Skill_Record is record
      Name: Unbounded_String; -- Name of skill
      Attribute: Positive; -- Attribute used with that skill
      Description: Unbounded_String; -- Description of skill
      Tool: Unbounded_String; -- Item type used as tool for training that skill
   end record;
   package SkillsData_Container is new Vectors(Positive, Skill_Record);
   -- Data for attributes
   type Attribute_Record is record
      Name: Unbounded_String; -- Name of attribute
      Description: Unbounded_String; -- Description of attribute
   end record;
   package AttributesData_Container is new Vectors(Positive, Attribute_Record);
   -- Contains data for all skills
   Skills_List: SkillsData_Container.Vector;
   -- Name of item type used as tool in repairing/upgrading ship
   RepairTools: Unbounded_String;
   -- Name of item type used as tool in cleaning ship
   CleaningTools: Unbounded_String;
   -- Name of item type used as alchemy tools (mainly in deconstructing orders)
   AlchemyTools: Unbounded_String;
   -- Index of item used to create mobs corpses
   CorpseIndex: Unbounded_String;
   -- Name of item type used for delivery missions
   MissionItemsType: Unbounded_String;
   -- Name of item type used as fuel for ships
   FuelType: Unbounded_String;
   -- Index of item used as money
   MoneyIndex: Unbounded_String;
   -- Name of money (taken from MoneyIndex)
   MoneyName: Unbounded_String;
   -- Path to directory where are savegame and logs
   SaveDirectory: Unbounded_String :=
     To_Unbounded_String("data" & Dir_Separator & "saves" & Dir_Separator);
   -- Path to directory where are game data files
   DataDirectory: Unbounded_String :=
     To_Unbounded_String("data" & Dir_Separator);
   -- Word used in ships names for traders ships (for events)
   TradersName: Unbounded_String;
   -- Contains data for all characters attributes
   Attributes_List: AttributesData_Container.Vector;
   -- Index of attribute used as bonus to character condition
   ConditionIndex: Positive;
   -- Index of attribute used to count max character encumbrance
   StrengthIndex: Positive;
   -- Index of skill used to piloting ship
   PilotingSkill: Positive;
   -- Index of skill used by engineer on ship
   EngineeringSkill: Positive;
   -- Index of skill used by gunners
   GunnerySkill: Positive;
   -- Index of skill used for talk in bases or with other ships
   TalkingSkill: Positive;
   -- Index of skill used for spoting
   PerceptionSkill: Positive;
   -- Index of skill used for dodge in character's combat
   DodgeSkill: Positive;
   -- Index of skill used for unarmed attacks in character's combat
   UnarmedSkill: Positive;
   -- Name of item type used as characters head armor
   HeadArmor: Unbounded_String;
   -- Name of item type used as characters torso armor
   ChestArmor: Unbounded_String;
   -- Name of item type used as characters arms armor
   ArmsArmor: Unbounded_String;
   -- Name of item type used as characters legs armor
   LegsArmor: Unbounded_String;
   -- Name of item type used as characters shield
   ShieldType: Unbounded_String;
   -- Name of item type used as characters weapon
   WeaponType: Unbounded_String;
   -- Path to directory where documentation is
   DocDirectory: Unbounded_String :=
     To_Unbounded_String("doc" & Dir_Separator);
   -- Path to directory where are game modifications
   ModsDirectory: Unbounded_String :=
     To_Unbounded_String("data" & Dir_Separator & "mods" & Dir_Separator);
   -- Index of career selected by player during starting game
   PlayerCareer: Unbounded_String;
   -- Path to directory where are ui themes
   ThemesDirectory: Unbounded_String :=
     To_Unbounded_String("data" & Dir_Separator & "themes" & Dir_Separator);
   -- Possible actions to do when loading game data
   type DataAction is (ADD, UPDATE, REMOVE);
   -- Raised when error occurs during loading any game data
   Data_Loading_Error: exception;

   -- Start new game: create map, place ship, crew, etc
   procedure NewGame;
   -- Game ticks (update time, crew, ship, etc)
   procedure UpdateGame(Minutes: Positive; InCombat: Boolean := False);
   -- Save (or not) game and clear all temporary data
   procedure EndGame(Save: Boolean);
   -- Return vector index of selected skill
   function FindSkillIndex(SkillName: Unbounded_String) return Natural with
      Pre => SkillName /= Null_Unbounded_String;
      -- Load game data from files
   function LoadGameData return String;

end Game;
