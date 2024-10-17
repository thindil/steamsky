--    Copyright 2016-2024 Bartek thindil Jasicki
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

with Ada.Strings.Bounded; use Ada.Strings.Bounded;
with Ada.Strings.Bounded.Hash;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors; use Ada.Containers;
with Ada.Containers.Formal_Indefinite_Vectors;
with Ada.Containers.Formal_Vectors;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;

-- ****h* Game/Game
-- FUNCTION
-- Main the game structures and data
-- SOURCE
package Game is
-- ****

   --## rule off TYPE_INITIAL_VALUES
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
      Year: Natural range 0 .. 4_000_000;
      Month: Natural range 0 .. 24;
      Day: Natural range 0 .. 62;
      Hour: Natural range 0 .. 48;
      Minutes: Natural range 0 .. 120;
   end record;
   -- ****
   --## rule on TYPE_INITIAL_VALUES

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
   Game_Version: constant String := "Version: 10.0.4";
   -- ****

   -- ****t* Game/Game.UnboundedString_Container
   -- FUNCTION
   -- Used to store Unbounded_String values as list
   -- SOURCE
   package UnboundedString_Container is new Vectors
     (Index_Type => Positive, Element_Type => Unbounded_String);
   -- ****

   -- ****t* Game/Game.Positive_Container
   -- FUNCTION
   -- Used to store Positive values as list
   -- SOURCE
   package Positive_Container is new Vectors
     (Index_Type => Positive, Element_Type => Positive);
   -- ****

   -- ****t* Game/Game.Positive_Formal_Container
   -- FUNCTION
   -- Used to store Positive values as list
   -- HISTORY
   -- 7.4 - Added
   -- SOURCE
   package Positive_Formal_Container is new Formal_Vectors
     (Index_Type => Positive, Element_Type => Positive);
   -- ****

   -- ****t* Game/Game.Natural_Container
   -- FUNCTION
   -- Used to store Natural values as list
   -- SOURCE
   package Natural_Container is new Vectors
     (Index_Type => Positive, Element_Type => Natural);
   -- ****

   -- ****t* Game/Game.Attributes_Array
   -- FUNCTION
   -- Data structure for attributes: 1 - Attribute level, 2 - current experience in attribute
   -- SOURCE
   type Attributes_Array is array(1 .. 2) of Natural with
      Default_Component_Value => 0;
   -- ****

   --## rule off REDUCEABLE_SCOPE
   -- ****d* Game/Game.Empty_Attributes_Array
   -- FUNCTION
   -- Empty attributes array constant
   -- SOURCE
   Empty_Attributes_Array: constant Attributes_Array := (others => 0);
   -- ****
   --## rule on REDUCEABLE_SCOPE

   -- ****t* Game/Game.Short_String
   -- FUNCTION
   -- Used to store various texts, max length 512
   -- HISTORY
   -- 6.5 - Added
   -- SOURCE
   package Short_String is new Generic_Bounded_Length(Max => 512);
   -- ****

   -- ****t* Game/Game.Tiny_String
   -- FUNCTION
   -- Used to store various texts, max length 64
   -- HISTORY
   -- 6.5 - Added
   -- SOURCE
   package Tiny_String is new Generic_Bounded_Length(Max => 64);
   -- ****

   -- ****t* Game/Game.TinyString_Container
   -- FUNCTION
   -- Used to store Tiny_String values as list
   -- SOURCE
   package TinyString_Container is new Vectors
     (Index_Type => Positive, Element_Type => Tiny_String.Bounded_String,
      "=" => Tiny_String."=");
   -- ****

   -- ****t* Game/Game.TinyString_Formal_Container
   -- FUNCTION
   -- Used to store Tiny_String values as list
   -- HISTORY
   -- 7.1 - Added
   -- SOURCE
   package TinyString_Formal_Container is new Formal_Vectors
     (Index_Type => Positive, Element_Type => Tiny_String.Bounded_String,
      "=" => Tiny_String."=");
   -- ****

   -- ****s* Game/Game.Attribute_Record
   -- FUNCTION
   -- Data for attributes
   -- PARAMETERS
   -- Name        - Name of attribute
   -- Description - Description of attribute
   -- SOURCE
   type Attribute_Record is record
      Name: Tiny_String.Bounded_String;
      Description: Short_String.Bounded_String;
   end record;
   -- ****

   --## rule off REDUCEABLE_SCOPE
   -- ****d* Game/Game.Empty_Atribute_Record
   -- FUNCTION
   -- Empty attributes record constant
   -- SOURCE
   Empty_Atribute_Record: constant Attribute_Record :=
     Attribute_Record'(others => <>);
   -- ****
   --## rule on REDUCEABLE_SCOPE

     -- ****t* Game/Game.Attributes_Amount_Range
     -- FUNCTION
     -- Used to set the amount of available characters attributes
     -- HISTORY
     -- 6.6 - Added
     -- SOURCE
   subtype Attributes_Amount_Range is Positive range 1 .. 16;
   -- ****

   -- ****d* Game/Game.Default_Attributes_Amount
   -- FUNCTION
   -- The default amount of the attributes available in the game
   -- HISTORY
   -- 6.6 -  Added
   -- SOURCE
   Default_Attributes_Amount: constant Attributes_Amount_Range := 16;
   -- ****

   -- ****t* Game/Game.AttributesData_Container
   -- FUNCTION
   -- Used to store attributes data
   -- SOURCE
   package AttributesData_Container is new Formal_Vectors
     (Index_Type => Attributes_Amount_Range, Element_Type => Attribute_Record);
   -- ****

   -- ****t* Game/Game.Skill_Range
   -- FUNCTION
   -- Range used for skills but also for health, tiredness, hunger, thirst and
   -- loyalty
   -- SOURCE
   subtype Skill_Range is Natural range 0 .. 100;
   -- ****

   -- ****s* Game/Tool_Quality_Record
   -- FUNCTION
   -- Used to store information about required by skill training tool
   -- PARAMETERS
   -- Level   - The minimal level of the skill which requires that tool
   -- Quality - The minimal quality of the tool for use it for training
   -- HISTORY
   -- 6.6 - Added
   -- SOURCE
   type Tool_Quality_Record is record
      Level: Skill_Range := 100;
      Quality: Skill_Range := 100;
   end record;
   -- ****

   -- ****d* Game/Default_Tools_Quality
   -- FUNCTION
   -- Default values for tools qualities for skills
   -- SOURCE
   Default_Tools_Quality: constant Tool_Quality_Record :=
     Tool_Quality_Record'(others => <>);
   -- ****

   -- ****t* Game/Game.Tools_Quality_Range
   -- FUNCTION
   -- Used to set amount of tools qualities for skills
   -- SOURCE
   subtype Tools_Quality_Range is Positive range 1 .. 16;
   -- ****

   -- ****t* Game/Game.Tool_Quality_Array
   -- FUNCTION
   -- Used to store information about tools needed for training skills
   -- HISTORY
   -- 6.5 - Added
   -- SOURCE
   type Tool_Quality_Array is
     array(Tools_Quality_Range range <>) of Tool_Quality_Record;
   -- ****

   -- ****d* Game/Game.Empty_Tool_Quality_Array
   -- FUNCTION
   -- Default value for tools used to train the selected skill if nothig is
   -- set in the game data files
   -- HISTORY
   -- 6.5 -Added
   -- SOURCE
   Empty_Tool_Quality_Array: constant Tool_Quality_Array(1 .. 1) :=
     (1 => Default_Tools_Quality);
   -- ****

   -- ****s* Game/Game.Skill_Record
   -- FUNCTION
   -- Data for skills
   -- PARAMETERS
   -- Quality_Amount - The length of the array with information about tools
   --                  required to train the skill
   -- Name           - Name of skill
   -- Attribute      - Attribute used with that skill
   -- Description    - Description of skill
   -- Tool           - Item type used as tool for training that skill
   -- Tools_Quality  - Required tools quality for training that skill at the
   --                  selected level. First value minimal level of skill,
   --                  second minimum quality of tool
   -- SOURCE
   type Skill_Record(Quality_Amount: Tools_Quality_Range) is record
      Name: Tiny_String.Bounded_String;
      Attribute: Attributes_Amount_Range;
      Description: Short_String.Bounded_String;
      Tool: Tiny_String.Bounded_String;
      Tools_Quality: Tool_Quality_Array(1 .. Quality_Amount);
   end record;
   -- ****

   --## rule off REDUCEABLE_SCOPE
   -- ****d* Game/Game.Empty_Skill
   -- FUNCTION
   -- Empty skill data constant
   -- SOURCE
   Empty_Skill: constant Skill_Record :=
     (Quality_Amount => 1, Name => Tiny_String.Null_Bounded_String,
      Attribute => 1, Description => Short_String.Null_Bounded_String,
      Tool => Tiny_String.Null_Bounded_String,
      Tools_Quality => Empty_Tool_Quality_Array);
   -- ****
   --## rule on REDUCEABLE_SCOPE

     -- ****t* Game/Game.Skills_Amount_Range
     -- FUNCTION
     -- Used to set the amount of available characters skills
     -- HISTORY
     -- 6.6 - Added
     -- SOURCE
   subtype Skills_Amount_Range is Count_Type range 1 .. 64;
   -- ****

   -- ****d* Game/Game.Default_Skills_Amount
   -- FUNCTION
   -- The default amount of the skills available in the game
   -- HISTORY
   -- 6.6 -  Added
   -- SOURCE
   Default_Skills_Amount: constant Skills_Amount_Range := 26;
   -- ****

   -- ****t* Game/Game.SkillsData_Container
   -- FUNCTION
   -- Used to store skills data
   -- SOURCE
   package SkillsData_Container is new Formal_Indefinite_Vectors
     (Index_Type => Skills_Amount_Range, Element_Type => Skill_Record,
      Max_Size_In_Storage_Elements => Skill_Record'Size, Bounded => False);
   -- ****

   -- ****v* Game/Game.Skills_List
   -- FUNCTION
   -- Contains data for all skills
   -- SOURCE
   Skills_List: SkillsData_Container.Vector
     (Capacity => Default_Skills_Amount);
   -- ****

   -- ****v* Game/Game.Skills_Amount
   -- FUNCTION
   -- The amount of skills in the game
   -- HISTORY
   -- 6.6 - Added
   -- SOURCE
   Skills_Amount: SkillsData_Container.Extended_Index := 0;
   -- ****

   -- ****v* Game/Game.Fuel_Type
   -- FUNCTION
   -- Name of item type used as fuel for ships
   -- SOURCE
   Fuel_Type: Tiny_String.Bounded_String;
   -- ****

   -- ****v* Game/Game.Money_Index
   -- FUNCTION
   -- Index of item used as money
   -- SOURCE
   Money_Index: Positive;
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

   -- ****v* Game/Game.Attributes_List
   -- FUNCTION
   -- Contains data for all characters attributes
   -- SOURCE
   Attributes_List: AttributesData_Container.Vector
     (Capacity => Count_Type(Default_Attributes_Amount));
   -- ****

   -- ****v* Game/Game.Attributes_Amount
   -- FUNCTION
   -- The amount of attributes in the game
   -- HISTORY
   -- 6.6 - Added
   -- SOURCE
   Attributes_Amount: Natural := 0;
   -- ****

   -- ****v* Game/Game.Talking_Skill
   -- FUNCTION
   -- Index of skill used for talk in bases or with other ships
   -- SOURCE
   Talking_Skill: Skills_Amount_Range;
   -- ****

   -- ****v* Game/Game.Head_Armor
   -- FUNCTION
   -- Name of item type used as characters head armor
   -- SOURCE
   Head_Armor: Tiny_String.Bounded_String;
   -- ****

   -- ****v* Game/Game.Chest_Armor
   -- FUNCTION
   -- Name of item type used as characters torso armor
   -- SOURCE
   Chest_Armor: Tiny_String.Bounded_String;
   -- ****

   -- ****v* Game/Game.Arms_Armor
   -- FUNCTION
   -- Name of item type used as characters arms armor
   -- SOURCE
   Arms_Armor: Tiny_String.Bounded_String;
   -- ****

   -- ****v* Game/Game.Legs_Armor
   -- FUNCTION
   -- Name of item type used as characters legs armor
   -- SOURCE
   Legs_Armor: Tiny_String.Bounded_String;
   -- ****

   -- ****v* Game/Game.Shield_Type
   -- FUNCTION
   -- Name of item type used as characters shield
   -- SOURCE
   Shield_Type: Tiny_String.Bounded_String;
   -- ****

   -- ****v* Game/Game.Weapon_Type
   -- FUNCTION
   -- Name of item type used as characters weapon
   -- SOURCE
   Weapon_Type: Tiny_String.Bounded_String;
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
   type Data_Action is (ADD, UPDATE, REMOVE) with
      Default_Value => ADD;
   -- ****

   --## rule off REDUCEABLE_SCOPE
   -- ****d* Game/Game.Default_Data_Action
   -- FUNCTION
   -- Default data action when loading the game data
   -- SOURCE
   Default_Data_Action: constant Data_Action := ADD;
   -- ****
   --## rule on REDUCEABLE_SCOPE

   --## rule off TYPE_INITIAL_VALUES
   -- ****t* Game/Game.Natural_Array
   -- FUNCTION
   -- General purpose array of Natural
   -- SOURCE
   type Natural_Array is array(Positive range <>) of Natural with
      Default_Component_Value => 0;
      -- ****

      -- ****t* Game/Game.Integer_Array
      -- FUNCTION
      -- General purpose array of Integer
      -- HISTORY
      -- 7.4 - Added
      -- SOURCE
   type Integer_Array is array(Positive range <>) of Integer with
      Default_Component_Value => 0;
   -- ****
   --## rule on TYPE_INITIAL_VALUES

   -- ****t* Game/Game.Damage_Factor
   -- FUNCTION
   -- Used mostly for count percentage of damage of modules
   -- SOURCE
   type Damage_Factor is digits 2 range 0.0 .. 1.0 with
      Default_Value => 0.0;
   -- ****

   --## rule off REDUCEABLE_SCOPE
   -- ****d* Game/Game.No_Damage
   -- FUNCTION
   -- Constant for no damage for Damage_Factor type
   -- SOURCE
   No_Damage: constant Damage_Factor := 0.0;
   -- ****
   --## rule on REDUCEABLE_SCOPE

   -- ****t* Game/Game.Reputation_Range
   -- FUNCTION
   -- Range of the player's reputation level in bases
   -- SOURCE
   subtype Reputation_Range is Integer range -100 .. 100;
   -- ****

   -- ****s* Game/Game.Reputation_Data
   -- FUNCTION
   -- Data for reputation
   -- PARAMETERS
   -- Level      - The level of the reputation
   -- Experience - The current experience in the reputation
   -- HISTORY
   -- 7.1 - Added
   -- SOURCE
   type Reputation_Data is record
      Level: Reputation_Range := 0;
      Experience: Natural := 0;
   end record;
   -- ****

   --## rule off REDUCEABLE_SCOPE
   -- ****d* Game/Game.Default_Reputation
   -- FUNCTION
   -- Default reputation values
   -- SOURCE
   Default_Reputation: constant Reputation_Data :=
     (Level => 0, Experience => 0);
   -- ****
   --## rule on REDUCEABLE_SCOPE

   -- ****t* Game/Game.Bases_Range
   -- FUNCTION
   -- Amount of sky bases
   -- SOURCE
   subtype Bases_Range is Positive range 1 .. 1_024;
   -- ****

   -- ****t* Game/Game.Extended_Base_Range
   -- FUNCTION
   -- Amount of sky bases starting from 0
   -- SOURCE
   subtype Extended_Base_Range is Natural range 0 .. 1_024;
   -- ****

   -- ****t* Game/Game.Map_X_Range
   -- FUNCTION
   -- X axis size of the game map
   -- SOURCE
   subtype Map_X_Range is Positive range 1 .. 1_024;
   -- ****

   -- ****t* Game/Game.Map_Y_Range
   -- FUNCTION
   -- Y axis size of the game map
   -- SOURCE
   subtype Map_Y_Range is Positive range 1 .. 1_024;
   -- ****

   -- ****e* Game/Game.Data_Loading_Error
   -- FUNCTION
   -- Raised when error occurs during loading any game data
   -- SOURCE
   Data_Loading_Error: exception;
   -- ****

   -- ****f* Game/Game.Tiny_String_Hash
   -- FUNCTION
   -- Compute the hash of the selected Tiny_String
   -- PARAMETERS
   -- Key - The Tiny_String which hash will be computed
   -- RESULT
   -- The String with hash of the selected Tiny_String
   -- HISTORY
   -- 6.8 - Added
   -- SOURCE
   function Tiny_String_Hash is new Ada.Strings.Bounded.Hash(Tiny_String);
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
   procedure Update_Game(Minutes: Positive; In_Combat: Boolean := False);
   -- ****

   -- ****f* Game/Game.End_Game
   -- FUNCTION
   -- Save (or not) game and clear all temporary data
   -- PARAMETERS
   -- Save - Did game should be saved to file or not
   -- SOURCE
   procedure End_Game(Save: Boolean);
   -- ****

   -- ****f* Game/Game.Find_Skill_Index
   -- FUNCTION
   -- Find index of selected skill
   -- PARAMETERS
   -- SkillName - Name of the skill to search
   -- RESULT
   -- Index of selected skill or 0 if skill was not found
   -- SOURCE
   function Find_Skill_Index
     (Skill_Name: String) return SkillsData_Container.Extended_Index with
      Pre => Skill_Name'Length > 0,
      Post => Find_Skill_Index'Result <=
      SkillsData_Container.Length(Container => Skills_List);
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

-- Temporary code to interact with Nim

   procedure Get_Game_Date;

   procedure Set_Game_Date;

end Game;
