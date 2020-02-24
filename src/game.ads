--    Copyright 2016-2020 Bartek thindil Jasicki
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

-- ****h* Steamsky/Game
-- FUNCTION
-- Main the game structures and data
-- SOURCE
package Game is
-- ****

   -- ****t* Game/Date_Record
   -- FUNCTION
   -- Data for game date/time
   -- Year    - Current in game year
   -- Month   - Current in game month
   -- Day     - Current in game day
   -- Hour    - Current in game hour
   -- Minutes - Current in game minutes
   -- SOURCE
   type Date_Record is record
      Year: Natural;
      Month: Natural;
      Day: Natural;
      Hour: Natural;
      Minutes: Natural;
   end record;
   -- ****

   -- ****v* Game/GameDate
   -- FUNCTION
   -- Current in game date
   -- SOURCE
   GameDate: Date_Record;
   -- ****

   -- ****v* Game/GameVersion
   -- FUNCTION
   -- Current the game version
   -- SOURCE
   GameVersion: constant String := "Version: 4.8";
   -- ****

   -- ****t* Game/UnboundedString_Container
   -- FUNCTION
   -- Used to store Unbounded_String values as list
   -- SOURCE
   package UnboundedString_Container is new Vectors(Positive,
      Unbounded_String);
   -- ****

   -- ****t* Game/Positive_Container
   -- FUNCTION
   -- Used to store Positive values as list
   -- SOURCE
   package Positive_Container is new Vectors(Positive, Positive);
   -- ****

   -- ****t* Game/Natural_Container
   -- FUNCTION
   -- Used to store Natural values as list
   -- SOURCE
   package Natural_Container is new Vectors(Positive, Natural);
   -- ****

   -- ****t* Game/Integer_Container
   -- FUNCTION
   -- Used to store Integer values as list
   -- SOURCE
   package Integer_Container is new Vectors(Positive, Integer);
   -- ****

   -- ****t* Game/Skill_Record
   -- FUNCTION
   -- Data for skills
   -- PARAMETERS
   -- Name        - Name of skill
   -- Attribute   - Attribute used with that skill
   -- Description - Description of skill
   -- Tool        - Item type used as tool for training that skill
   -- SOURCE
   type Skill_Record is record
      Name: Unbounded_String;
      Attribute: Positive;
      Description: Unbounded_String;
      Tool: Unbounded_String;
   end record;
   -- ****

   -- ****t* Game/SkillsData_Container
   -- FUNCTION
   -- Used to store skills data
   -- SOURCE
   package SkillsData_Container is new Vectors(Positive, Skill_Record);
   -- ****

   -- ****t* Game/Attribute_Record
   -- FUNCTION
   -- Data for attributes
   -- PARAMETERS
   -- Name        - Name of attribute
   -- Description - Description of attribute
   -- SOURCE
   type Attribute_Record is record
      Name: Unbounded_String;
      Description: Unbounded_String;
   end record;
   -- ****

   -- ****t* Game/AttributesData_Container
   -- FUNCTION
   -- Used to store attributes data
   -- SOURCE
   package AttributesData_Container is new Vectors(Positive, Attribute_Record);
   -- ****

   -- ****v* Game/Skills_List
   -- FUNCTION
   -- Contains data for all skills
   -- SOURCE
   Skills_List: SkillsData_Container.Vector;
   -- ****

   -- ****v* Game/RepairTools
   -- FUNCTION
   -- Name of item type used as tool in repairing/upgrading ship
   -- SOURCE
   RepairTools: Unbounded_String;
   -- ****

   -- ****v* Game/CleaningTools
   -- FUNCTION
   -- Name of item type used as tool in cleaning ship
   -- SOURCE
   CleaningTools: Unbounded_String;
   -- ****

   -- ****v* Game/AlchemyTools
   -- FUNCTION
   -- Name of item type used as alchemy tools (mainly in deconstructing orders)
   -- SOURCE
   AlchemyTools: Unbounded_String;
   -- ****

   -- ****v* Game/CorpseIndex
   -- FUNCTION
   -- Index of item used to create mobs corpses
   -- SOURCE
   CorpseIndex: Unbounded_String;
   -- ****

   -- ****v* Game/MissionItemsType
   -- FUNCTION
   -- Name of item type used for delivery missions
   -- SOURCE
   MissionItemsType: Unbounded_String;
   -- ****

   -- ****v* Game/FuelType
   -- FUNCTION
   -- Name of item type used as fuel for ships
   -- SOURCE
   FuelType: Unbounded_String;
   -- ****

   -- ****v* Game/MoneyIndex
   -- FUNCTION
   -- Index of item used as money
   -- SOURCE
   MoneyIndex: Unbounded_String;
   -- ****

   -- ****v* Game/MoneyName
   -- FUNCTION
   -- Name of money (taken from MoneyIndex)
   -- SOURCE
   MoneyName: Unbounded_String;
   -- ****

   -- ****v* Game/SaveDirectory
   -- FUNCTION
   -- Path to directory where are savegame and logs
   -- SOURCE
   SaveDirectory: Unbounded_String :=
     To_Unbounded_String("data" & Dir_Separator & "saves" & Dir_Separator);
   -- ****

   -- ****v* Game/DataDirectory
   -- FUNCTION
   -- Path to directory where are game data files
   -- SOURCE
   DataDirectory: Unbounded_String :=
     To_Unbounded_String("data" & Dir_Separator);
   -- ****

   -- ****v* Game/TradersName
   -- FUNCTION
   -- Word used in ships names for traders ships (for events)
   -- SOURCE
   TradersName: Unbounded_String;
   -- ****

   -- ****v* Game/Attributes_List
   -- FUNCTION
   -- Contains data for all characters attributes
   -- SOURCE
   Attributes_List: AttributesData_Container.Vector;
   -- ****

   -- ****v* Game/ConditionIndex
   -- FUNCTION
   -- Index of attribute used as bonus to character condition
   -- SOURCE
   ConditionIndex: Positive;
   -- ****

   -- ****v* Game/StrengthIndex
   -- FUNCTION
   -- Index of attribute used to count max character encumbrance
   -- SOURCE
   StrengthIndex: Positive;
   -- ****

   -- ****v* Game/PilotingSkill
   -- FUNCTION
   -- Index of skill used to piloting ship
   -- SOURCE
   PilotingSkill: Positive;
   -- ****

   -- ****v* Game/EngineeringSkill
   -- FUNCTION
   -- Index of skill used by engineer on ship
   -- SOURCE
   EngineeringSkill: Positive;
   -- ****

   -- ****v* Game/GunnerySkill
   -- FUNCTION
   -- Index of skill used by gunners
   -- SOURCE
   GunnerySkill: Positive;
   -- ****

   -- ****v* Game/TalkingSkill
   -- FUNCTION
   -- Index of skill used for talk in bases or with other ships
   -- SOURCE
   TalkingSkill: Positive;
   -- ****

   -- ****v* Game/PerceptionSkill
   -- FUNCTION
   -- Index of skill used for spoting
   -- SOURCE
   PerceptionSkill: Positive;
   -- ****

   -- ****v* Game/DodgeSkill
   -- FUNCTION
   -- Index of skill used for dodge in character's combat
   -- SOURCE
   DodgeSkill: Positive;
   -- ****

   -- ****v* Game/UnarmedSkill
   -- FUNCTION
   -- Index of skill used for unarmed attacks in character's combat
   -- SOURCE
   UnarmedSkill: Positive;
   -- ****

   -- ****v* Game/HeadArmor
   -- FUNCTION
   -- Name of item type used as characters head armor
   -- SOURCE
   HeadArmor: Unbounded_String;
   -- ****

   -- ****v* Game/ChestArmor
   -- FUNCTION
   -- Name of item type used as characters torso armor
   -- SOURCE
   ChestArmor: Unbounded_String;
   -- ****

   -- ****v* Game/ArmsArmor
   -- FUNCTION
   -- Name of item type used as characters arms armor
   -- SOURCE
   ArmsArmor: Unbounded_String;
   -- ****

   -- ****v* Game/LegsArmor
   -- FUNCTION
   -- Name of item type used as characters legs armor
   -- SOURCE
   LegsArmor: Unbounded_String;
   -- ****

   -- ****v* Game/ShieldType
   -- FUNCTION
   -- Name of item type used as characters shield
   -- SOURCE
   ShieldType: Unbounded_String;
   -- ****

   -- ****v* Game/WeaponType
   -- FUNCTION
   -- Name of item type used as characters weapon
   -- SOURCE
   WeaponType: Unbounded_String;
   -- ****

   -- ****v* Game/DocDirectory
   -- FUNCTION
   -- Path to directory where documentation is
   -- SOURCE
   DocDirectory: Unbounded_String :=
     To_Unbounded_String("doc" & Dir_Separator);
   -- ****

   -- ****v* Game/ModsDirectory
   -- FUNCTION
   -- Path to directory where are game modifications
   -- SOURCE
   ModsDirectory: Unbounded_String :=
     To_Unbounded_String("data" & Dir_Separator & "mods" & Dir_Separator);
   -- ****

   -- ****v* Game/PlayerCareer
   -- FUNCTION
   -- Index of career selected by player during starting game
   -- SOURCE
   PlayerCareer: Unbounded_String;
   -- ****

   -- ****v* Game/ThemesDirectory
   -- FUNCTION
   -- Path to directory where are ui themes
   -- SOURCE
   ThemesDirectory: Unbounded_String :=
     To_Unbounded_String("data" & Dir_Separator & "themes" & Dir_Separator);
   -- ****

   -- ****t* Game/DataAction
   -- FUNCTION
   -- Possible actions to do when loading game data
   -- SOURCE
   type DataAction is (ADD, UPDATE, REMOVE);
   -- ****

   -- ****t* Game/Natural_Array
   -- FUNCTION
   -- General purpose array of Natural
   -- SOURCE
   type Natural_Array is array(Positive range <>) of Natural;
   -- ****

   -- ****t* Game/DamageFactor
   -- FUNCTION
   -- Used mostly for count percentage of damage of modules
   -- SOURCE
   type DamageFactor is digits 2 range 0.0 .. 1.0;
   -- ****

   -- ****e* Game/Data_Loading_Error
   -- FUNCTION
   -- Raised when error occurs during loading any game data
   -- SOURCE
   Data_Loading_Error: exception;
   -- ****

   -- ****f* Game/NewGame
   -- FUNCTION
   -- Start new game: create map, place ship, crew, etc
   -- SOURCE
   procedure NewGame;
   -- ****

   -- ****f* Game/UpdateGame
   -- FUNCTION
   -- Game ticks (update time, crew, ship, etc)
   -- PARAMETERS
   -- Minutes  - Amount of in-game minutes passed
   -- InCombat - Did player is in combat currently. Default false
   -- SOURCE
   procedure UpdateGame(Minutes: Positive; InCombat: Boolean := False) with
      Test_Case => ("Test_UpdateGame", Robustness);
      -- ****

      -- ****f* Game/EndGame
      -- FUNCTION
      -- Save (or not) game and clear all temporary data
      -- PARAMETERS
      -- Save - Did game should be saved to file or not
      -- SOURCE
   procedure EndGame(Save: Boolean) with
      Test_Case => ("Test_EndGame", Robustness);
      -- ****

      -- ****f* Game/FindSkillIndex
      -- FUNCTION
      -- Find index of selected skill
      -- PARAMETERS
      -- SkillName - Name of the skill to search
      -- RESULT
      -- Index of selected skill or 0 if skill was not found
      -- SOURCE
   function FindSkillIndex(SkillName: Unbounded_String) return Natural with
      Pre => SkillName /= Null_Unbounded_String,
      Test_Case => ("Test_FindSkillIndex", Nominal);
      -- ****

      -- ****f* Game/LoadGameData
      -- FUNCTION
      -- Load game data from files
      -- RESULT
      -- Empty string if everything was ok, otherwise message with info what
      -- goes wrong
      -- SOURCE
   function LoadGameData return String;
   -- ****

end Game;
