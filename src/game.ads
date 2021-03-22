--    Copyright 2016-2021 Bartek thindil Jasicki
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

-- ****h* Game/Game
-- FUNCTION
-- Main the game structures and data
-- SOURCE
package Game is
-- ****

   -- ****s* Game/Game.Date_Record
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

   -- ****d* Game/Game.Start_Date
   -- FUNCTION
   -- The default start date for in-game time
   -- SOURCE
   Start_Date: constant Date_Record :=
     (Year => 1600, Month => 3, Day => 1, Hour => 8, Minutes => 1);
   -- ****

   -- ****v* Game/Game.Game_Date
   -- FUNCTION
   -- Current in game date
   -- SOURCE
   Game_Date: Date_Record;
   -- ****

   -- ****d* Game/Game.Game_Version
   -- FUNCTION
   -- Current the game version
   -- SOURCE
   Game_Version: constant String := "Version: 6.0";
   -- ****

   -- ****t* Game/Game.UnboundedString_Container
   -- FUNCTION
   -- Used to store Unbounded_String values as list
   -- SOURCE
   package UnboundedString_Container is new Vectors(Index_Type => Positive,
      Element_Type => Unbounded_String);
   -- ****

   -- ****t* Game/Game.Positive_Container
   -- FUNCTION
   -- Used to store Positive values as list
   -- SOURCE
   package Positive_Container is new Vectors(Index_Type => Positive,
      Element_Type => Positive);
   -- ****

   -- ****t* Game/Game.Natural_Container
   -- FUNCTION
   -- Used to store Natural values as list
   -- SOURCE
   package Natural_Container is new Vectors(Index_Type => Positive,
      Element_Type => Natural);
   -- ****

   -- ****t* Game/Game.Integer_Container
   -- FUNCTION
   -- Used to store Integer values as list
   -- SOURCE
   package Integer_Container is new Vectors(Index_Type => Positive,
      Element_Type => Integer);
   -- ****

   -- ****t* Game/Game.Attributes_Array
   -- FUNCTION
   -- Data structure for attributes: 1 - Attribute level, 2 - current experience in attribute
   -- SOURCE
   type Attributes_Array is array(1 .. 2) of Natural;
   -- ****

   -- ****d* Game/Game.Empty_Attributes_Array
   -- FUNCTION
   -- Empty attributes array constant
   -- SOURCE
   Empty_Attributes_Array: constant Attributes_Array := (others => 0);
   -- ****

   -- ****t* Game/Game.Attributes_Container
   -- Used to store attributes data
   -- SOURCE
   package Attributes_Container is new Vectors(Index_Type => Positive,
      Element_Type => Attributes_Array);
   -- ****

   -- ****s* Game/Game.Attribute_Record
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

   -- ****d* Game/Game.Empty_Atribute_Record
   -- FUNCTION
   -- Empty attributes record constant
   -- SOURCE
   Empty_Atribute_Record: constant Attribute_Record :=
     Attribute_Record'(others => <>);
   -- ****

   -- ****t* Game/Game.AttributesData_Container
   -- FUNCTION
   -- Used to store attributes data
   -- SOURCE
   package AttributesData_Container is new Vectors(Index_Type => Positive,
      Element_Type => Attribute_Record);
   -- ****

   -- ****s* Game/Game.Skill_Record
   -- FUNCTION
   -- Data for skills
   -- PARAMETERS
   -- Name          - Name of skill
   -- Attribute     - Attribute used with that skill
   -- Description   - Description of skill
   -- Tool          - Item type used as tool for training that skill
   -- Tools_Quality - Required tools quality for training that skill at the
   --                 selected level. First value minimal level of skill,
   --                 second minimum quality of tool
   -- SOURCE
   type Skill_Record is record
      Name: Unbounded_String;
      Attribute: AttributesData_Container.Extended_Index;
      Description: Unbounded_String;
      Tool: Unbounded_String;
      Tools_Quality: Attributes_Container.Vector;
   end record;
   -- ****

   -- ****d* Game/Game.Empty_Skill
   -- FUNCTION
   -- Empty skill data constant
   -- SOURCE
   Empty_Skill: constant Skill_Record :=
     (Name => Null_Unbounded_String, Attribute => 0,
      Description => Null_Unbounded_String, Tool => Null_Unbounded_String,
      Tools_Quality => Attributes_Container.Empty_Vector);
   -- ****

   -- ****t* Game/Game.SkillsData_Container
   -- FUNCTION
   -- Used to store skills data
   -- SOURCE
   package SkillsData_Container is new Vectors(Index_Type => Positive,
      Element_Type => Skill_Record);
   -- ****

   -- ****v* Game/Game.Skills_List
   -- FUNCTION
   -- Contains data for all skills
   -- SOURCE
   Skills_List: SkillsData_Container.Vector;
   -- ****

   -- ****v* Game/Game.Repair_Tools
   -- FUNCTION
   -- Name of item type used as tool in repairing/upgrading ship
   -- SOURCE
   Repair_Tools: Unbounded_String;
   -- ****

   -- ****v* Game/Game.Cleaning_Tools
   -- FUNCTION
   -- Name of item type used as tool in cleaning ship
   -- SOURCE
   Cleaning_Tools: Unbounded_String;
   -- ****

   -- ****v* Game/Game.Alchemy_Tools
   -- FUNCTION
   -- Name of item type used as alchemy tools (mainly in deconstructing orders)
   -- SOURCE
   Alchemy_Tools: Unbounded_String;
   -- ****

   -- ****v* Game/Game.Corpse_Index
   -- FUNCTION
   -- Index of item used to create mobs corpses
   -- SOURCE
   Corpse_Index: Unbounded_String;
   -- ****

   -- ****v* Game/Game.Mission_Items_Type
   -- FUNCTION
   -- Name of item type used for delivery missions
   -- SOURCE
   Mission_Items_Type: Unbounded_String;
   -- ****

   -- ****v* Game/Game.Fuel_Type
   -- FUNCTION
   -- Name of item type used as fuel for ships
   -- SOURCE
   Fuel_Type: Unbounded_String;
   -- ****

   -- ****v* Game/Game.Money_Index
   -- FUNCTION
   -- Index of item used as money
   -- SOURCE
   Money_Index: Unbounded_String;
   -- ****

   -- ****v* Game/Game.Money_Name
   -- FUNCTION
   -- Name of money (taken from MoneyIndex)
   -- SOURCE
   Money_Name: Unbounded_String;
   -- ****

   -- ****v* Game/Game.Save_Directory
   -- FUNCTION
   -- Path to directory where are savegame and logs
   -- SOURCE
   Save_Directory: Unbounded_String :=
     To_Unbounded_String
       (Source => "data" & Dir_Separator & "saves" & Dir_Separator);
   -- ****

   -- ****v* Game/Game.Data_Directory
   -- FUNCTION
   -- Path to directory where are game data files
   -- SOURCE
   Data_Directory: Unbounded_String :=
     To_Unbounded_String(Source => "data" & Dir_Separator);
   -- ****

   -- ****v* Game/Game.Traders_Name
   -- FUNCTION
   -- Word used in ships names for traders ships (for events)
   -- SOURCE
   Traders_Name: Unbounded_String;
   -- ****

   -- ****v* Game/Game.Attributes_List
   -- FUNCTION
   -- Contains data for all characters attributes
   -- SOURCE
   Attributes_List: AttributesData_Container.Vector;
   -- ****

   -- ****v* Game/Game.Condition_Index
   -- FUNCTION
   -- Index of attribute used as bonus to character condition
   -- SOURCE
   Condition_Index: AttributesData_Container.Extended_Index;
   -- ****

   -- ****v* Game/Game.Strength_Index
   -- FUNCTION
   -- Index of attribute used to count max character encumbrance
   -- SOURCE
   Strength_Index: AttributesData_Container.Extended_Index;
   -- ****

   -- ****v* Game/Game.Piloting_Skill
   -- FUNCTION
   -- Index of skill used to piloting ship
   -- SOURCE
   Piloting_Skill: SkillsData_Container.Extended_Index;
   -- ****

   -- ****v* Game/Game.Engineering_Skill
   -- FUNCTION
   -- Index of skill used by engineer on ship
   -- SOURCE
   Engineering_Skill: SkillsData_Container.Extended_Index;
   -- ****

   -- ****v* Game/Game.Gunnery_Skill
   -- FUNCTION
   -- Index of skill used by gunners
   -- SOURCE
   Gunnery_Skill: SkillsData_Container.Extended_Index;
   -- ****

   -- ****v* Game/Game.Talking_Skill
   -- FUNCTION
   -- Index of skill used for talk in bases or with other ships
   -- SOURCE
   Talking_Skill: SkillsData_Container.Extended_Index;
   -- ****

   -- ****v* Game/Game.Perception_Skill
   -- FUNCTION
   -- Index of skill used for spoting
   -- SOURCE
   Perception_Skill: SkillsData_Container.Extended_Index;
   -- ****

   -- ****v* Game/Game.Dodge_Skill
   -- FUNCTION
   -- Index of skill used for dodge in character's combat
   -- SOURCE
   Dodge_Skill: SkillsData_Container.Extended_Index;
   -- ****

   -- ****v* Game/Game.Unarmed_Skill
   -- FUNCTION
   -- Index of skill used for unarmed attacks in character's combat
   -- SOURCE
   Unarmed_Skill: SkillsData_Container.Extended_Index;
   -- ****

   -- ****v* Game/Game.Head_Armor
   -- FUNCTION
   -- Name of item type used as characters head armor
   -- SOURCE
   Head_Armor: Unbounded_String;
   -- ****

   -- ****v* Game/Game.Chest_Armor
   -- FUNCTION
   -- Name of item type used as characters torso armor
   -- SOURCE
   Chest_Armor: Unbounded_String;
   -- ****

   -- ****v* Game/Game.Arms_Armor
   -- FUNCTION
   -- Name of item type used as characters arms armor
   -- SOURCE
   Arms_Armor: Unbounded_String;
   -- ****

   -- ****v* Game/Game.Legs_Armor
   -- FUNCTION
   -- Name of item type used as characters legs armor
   -- SOURCE
   Legs_Armor: Unbounded_String;
   -- ****

   -- ****v* Game/Game.Shield_Type
   -- FUNCTION
   -- Name of item type used as characters shield
   -- SOURCE
   Shield_Type: Unbounded_String;
   -- ****

   -- ****v* Game/Game.Weapon_Type
   -- FUNCTION
   -- Name of item type used as characters weapon
   -- SOURCE
   Weapon_Type: Unbounded_String;
   -- ****

   -- ****v* Game/Game.Doc_Directory
   -- FUNCTION
   -- Path to directory where documentation is
   -- SOURCE
   Doc_Directory: Unbounded_String :=
     To_Unbounded_String(Source => "doc" & Dir_Separator);
   -- ****

   -- ****v* Game/Game.Mods_Directory
   -- FUNCTION
   -- Path to directory where are game modifications
   -- SOURCE
   Mods_Directory: Unbounded_String :=
     To_Unbounded_String
       (Source => "data" & Dir_Separator & "mods" & Dir_Separator);
   -- ****

   -- ****v* Game/Game.Player_Career
   -- FUNCTION
   -- Index of career selected by player during starting game
   -- SOURCE
   Player_Career: Unbounded_String;
   -- ****

   -- ****v* Game/Game.Themes_Directory
   -- FUNCTION
   -- Path to directory where are ui themes
   -- SOURCE
   Themes_Directory: Unbounded_String :=
     To_Unbounded_String
       (Source => "data" & Dir_Separator & "themes" & Dir_Separator);
   -- ****

   -- ****t* Game/Game.Data_Action
   -- FUNCTION
   -- Possible actions to do when loading game data
   -- SOURCE
   type Data_Action is (ADD, UPDATE, REMOVE);
   -- ****

   -- ****d* Game/Game.Default_Data_Action
   -- FUNCTION
   -- Default data action when loading the game data
   -- SOURCE
   Default_Data_Action: constant Data_Action := ADD;
   -- ****

   --## rule off TYPE_INITIAL_VALUES
   -- ****t* Game/Game.Natural_Array
   -- FUNCTION
   -- General purpose array of Natural
   -- SOURCE
   type Natural_Array is array(Positive range <>) of Natural;
   -- ****
   --## rule on TYPE_INITIAL_VALUES

   -- ****t* Game/Game.Damage_Factor
   -- FUNCTION
   -- Used mostly for count percentage of damage of modules
   -- SOURCE
   type Damage_Factor is digits 2 range 0.0 .. 1.0;
   -- ****

   -- ****d* Game/Game.No_Damage
   -- FUNCTION
   -- Constant for no damage for Damage_Factor type
   -- SOURCE
   No_Damage: constant Damage_Factor := 0.0;
   -- ****

   -- ****t* Game/Game.Reputation_Array
   -- FUNCTION
   -- Data structure for reputation, 1 = level, 2 = points to next level
   -- SOURCE
   type Reputation_Array is array(1 .. 2) of Integer;
   -- ****

   -- ****d* Game/Default_Reputation
   -- FUNCTION
   -- Default reputation values
   -- SOURCE
   Default_Reputation: constant Reputation_Array := (others => 0);
   -- ****

   -- ****t* Game/Game.Bases_Range
   -- FUNCTION
   -- Amount of sky bases
   -- SOURCE
   subtype Bases_Range is Positive range 1 .. 1024;
   -- ****

   -- ****t* Game/Game.Extended_Base_Range
   -- FUNCTION
   -- Amount of sky bases starting from 0
   -- SOURCE
   subtype Extended_Base_Range is Natural range 0 .. 1024;
   -- ****

   -- ****t* Game/Game.Map_X_Range
   -- FUNCTION
   -- X axis size of the game map
   -- SOURCE
   subtype Map_X_Range is Positive range 1 .. 1024;
   -- ****

   -- ****t* Game/Game.Map_Y_Range
   -- FUNCTION
   -- Y axis size of the game map
   -- SOURCE
   subtype Map_Y_Range is Positive range 1 .. 1024;
   -- ****

   -- ****t* Game/Game.Reputation_Range
   -- FUNCTION
   -- Range of the player's reputation level in bases
   -- SOURCE
   subtype Reputation_Range is Integer range -100 .. 100;
   -- ****

   -- ****e* Game/Game.Data_Loading_Error
   -- FUNCTION
   -- Raised when error occurs during loading any game data
   -- SOURCE
   Data_Loading_Error: exception;
   -- ****

   -- ****f* Game/Game.New_Game
   -- FUNCTION
   -- Start new game: create map, place ship, crew, etc
   -- SOURCE
   procedure New_Game;
   -- ****

   -- ****f* Game/Game.Update_Game
   -- FUNCTION
   -- Game ticks (update time, crew, ship, etc)
   -- PARAMETERS
   -- Minutes  - Amount of in-game minutes passed
   -- InCombat - Did player is in combat currently. Default false
   -- SOURCE
   procedure Update_Game(Minutes: Positive; In_Combat: Boolean := False) with
      Test_Case => (Name => "Test_UpdateGame", Mode => Robustness);
      -- ****

      -- ****f* Game/Game.End_Game
      -- FUNCTION
      -- Save (or not) game and clear all temporary data
      -- PARAMETERS
      -- Save - Did game should be saved to file or not
      -- SOURCE
   procedure End_Game(Save: Boolean) with
      Test_Case => (Name => "Test_EndGame", Mode => Robustness);
      -- ****

      -- ****f* Game/Game.Find_Skill_Index
      -- FUNCTION
      -- Find index of selected skill
      -- PARAMETERS
      -- SkillName - Name of the skill to search
      -- RESULT
      -- Index of selected skill or 0 if skill was not found
      -- SOURCE
   function Find_Skill_Index(Skill_Name: Unbounded_String) return Natural with
      Pre => Skill_Name /= Null_Unbounded_String,
      Test_Case => (Name => "Test_FindSkillIndex", Mode => Nominal);
      -- ****

      -- ****f* Game/Game.Load_Game_Data
      -- FUNCTION
      -- Load game data from files
      -- RESULT
      -- Empty string if everything was ok, otherwise message with info what
      -- goes wrong
      -- SOURCE
   function Load_Game_Data return String;
   -- ****

end Game;
