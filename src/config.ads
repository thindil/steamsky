--    Copyright 2016-2023 Bartek thindil Jasicki
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
with Game; use Game;
with Ships; use Ships;

-- ****h* Config/Config
-- FUNCTION
-- Provide code for load and save the game configuration
-- SOURCE
package Config is
-- ****

   -- ****t* Config/Config.Bonus_Type
   -- FUNCTION
   -- Used to store the game difficulty settings
   -- SOURCE
   subtype Bonus_Type is Float range 0.0 .. 5.0;
   -- ****

   -- ****t* Config/Config.Difficulty_Type
   -- FUNCTION
   -- Used to set the game difficulty level
   -- SOURCE
   type Difficulty_Type is
     (VERY_EASY, EASY, NORMAL, HARD, VERY_HARD, CUSTOM) with
      Default_Value => NORMAL;
   -- ****

   -- ****d* Config/Config.Default_Difficulty_Type
   -- FUNCTION
   -- Default difficulty level for the game
   -- SOURCE
   Default_Difficulty_Type: constant Difficulty_Type := NORMAL;
   -- ****

   -- ****s* Config/Config.New_Game_Record
   -- FUNCTION
   -- Data for new game settings
   -- PARAMETERS
   -- Player_Name               - Default player name
   -- Player_Gender             - Default player gender
   -- Ship_Name                 - Default ship name
   -- Player_Faction            - Default player faction index
   -- Player_Career             - Default player career index
   -- Starting_Base             - Default starting base type
   -- Enemy_Damage_Bonus        - Default bonus for enemy ship to damage
   -- Player_Damage_Bonus       - Default bonus for player ship to damage
   -- Enemy_Melee_Damage_Bonus  - Default bonus for enemy to damage in melee
   --                             combat
   -- Player_Melee_Damage_Bonus - Default bonus for player and player's ship
   --                             crew to damage in melee combat
   -- Experience_Bonus          - Default bonus to gained experience
   -- Reputation_Bonus          - Default bonus to gained or lost reputation in
   --                             bases
   -- Upgrade_Cost_Bonus        - Default bonus to amount of materials needed for
   --                             player's ship upgrades.
   -- Prices_Bonus              - Default bonus to prices for services in bases
   -- Difficulty_Level          - Default the game difficulty level
   -- SOURCE
   type New_Game_Record is record
      Player_Name: Unbounded_String;
      Player_Gender: Character;
      Ship_Name: Unbounded_String;
      Player_Faction: Tiny_String.Bounded_String;
      Player_Career: Unbounded_String;
      Starting_Base: Tiny_String.Bounded_String;
      Enemy_Damage_Bonus: Bonus_Type;
      Player_Damage_Bonus: Bonus_Type;
      Enemy_Melee_Damage_Bonus: Bonus_Type;
      Player_Melee_Damage_Bonus: Bonus_Type;
      Experience_Bonus: Bonus_Type;
      Reputation_Bonus: Bonus_Type;
      Upgrade_Cost_Bonus: Bonus_Type;
      Prices_Bonus: Bonus_Type;
      Difficulty_Level: Difficulty_Type;
   end record;
   -- ****

   -- ****d* Config/Config.Default_New_Game_Settings
   -- FUNCTION
   -- Default settings for the new game
   -- SOURCE
   Default_New_Game_Settings: constant New_Game_Record :=
     (Player_Name => To_Unbounded_String(Source => "Laeran"),
      Player_Gender => 'M',
      Ship_Name => To_Unbounded_String(Source => "Anaria"),
      Player_Faction => Tiny_String.To_Bounded_String(Source => "POLEIS"),
      Player_Career => To_Unbounded_String(Source => "general"),
      Starting_Base => Tiny_String.To_Bounded_String(Source => "Any"),
      Enemy_Damage_Bonus => 1.0, Player_Damage_Bonus => 1.0,
      Enemy_Melee_Damage_Bonus => 1.0, Player_Melee_Damage_Bonus => 1.0,
      Experience_Bonus => 1.0, Reputation_Bonus => 1.0,
      Upgrade_Cost_Bonus => 1.0, Prices_Bonus => 1.0,
      Difficulty_Level => Default_Difficulty_Type);
   -- ****

   -- ****t* Config/Config.Auto_Move_Break
   -- FUNCTION
   -- Options when stop auto move of player ship
   -- SOURCE
   type Auto_Move_Break is (NEVER, ANY, FRIENDLY, ENEMY) with
      Default_Value => NEVER;
   -- ****

   -- ****d* Config/Config.Default_Auto_Move_Stop
   -- FUNCTION
   -- Default setting for stop automovement of the player ship
   -- SOURCE
   Default_Auto_Move_Stop: constant Auto_Move_Break := NEVER;
   -- ****

   -- ****t* Config/Config.Messages_Order_Type
   -- FUNCTION
   -- Options to set showing messages order
   -- SOURCE
   type Messages_Order_Type is (OLDER_FIRST, NEWER_FIRST) with
      Default_Value => OLDER_FIRST;
   -- ****

   -- ****d* Config/Config.Default_Messages_Order
   -- FUNCTION
   -- Default order of show the last messages
   -- SOURCE
   Default_Messages_Order: constant Messages_Order_Type := OLDER_FIRST;
   -- ****

   -- ****t* Config/Config.Auto_Save_Type
   -- FUNCTION
   -- Type used to set how often autosave is done
   -- SOURCE
   type Auto_Save_Type is (NONE, DOCK, UNDOCK, DAILY, MONTHLY, YEARLY) with
      Default_Value => NONE;
   -- ****

   -- ****d* Config/Config.Default_Auto_Save_Time
   -- FUNCTION
   -- Default time when to auto save the game
   -- SOURCE
   Default_Auto_Save_Time: constant Auto_Save_Type := NONE;
   -- ****

   -- ****v* Config/Config.New_Game_Settings
   -- FUNCTION
   -- Settings for the new game
   -- SOURCE
   New_Game_Settings: New_Game_Record;
   -- ****

   -- ****t* Config/Config.Font_Types
   -- FUNCTION
   -- Types of fonts, used ot set their size
   -- HISTORY
   -- 7.1 - Added
   -- SOURCE
   type Font_Types is (HELPFONT, INTERFACEFONT, MAPFONT) with
      Default_Value => HELPFONT;
      -- ****

      -- ****d* Config/Config.Help_Font_Type
      -- FUNCTION
      -- Default type of font used when setting them
      -- HISTORY
      -- 7.8 - Added
      -- SOURCE
   Help_Font_Type: constant Font_Types := HELPFONT;
   -- ****

      -- ****f* Config/Config.Load_Config
      -- FUNCTION
      -- Load game configuration from file
      -- SOURCE
   procedure Load_Config;
   -- ****

   -- ****f* Config/Config.SaveConfig
   -- FUNCTION
   -- Save game configuration to file
   -- SOURCE
   procedure Save_Config;
   -- ****

-- Temporary code to interact with Nim

   procedure Get_New_Game_Settings;
   function Get_Boolean_Setting(Name: String) return Boolean;
   procedure Set_Boolean_Setting(Name: String; Value: Boolean);
   function Get_Integer_Setting(Name: String) return Integer;
   procedure Set_Integer_Setting(Name: String; Value: Integer);
   function Get_Undock_Speed return Ship_Speed;
   procedure Set_Undock_Speed(Value: Ship_Speed);
   function Get_Auto_Move_Stop return Auto_Move_Break;
   procedure Set_Auto_Move_Stop(Value: Auto_Move_Break);
   function Get_Messages_Order return Messages_Order_Type;
   procedure Set_Messages_Order(Value: Messages_Order_Type);
   function Get_Auto_Save return Auto_Save_Type;
   procedure Set_Auto_Save(Value: Auto_Save_Type);
   function Get_Interface_Theme return Unbounded_String;
   procedure Set_Interface_Theme(Value: Unbounded_String);
   function Get_String_Setting(Name: String) return String;
   procedure Set_String_Setting(Name, Value: String);
   function Get_Float_Setting(Name: String) return Bonus_Type;
   procedure Set_Float_Setting(Name: String; Value: Bonus_Type);
   function Get_Difficulty return Difficulty_Type;
   procedure Set_Difficulty(Value: Difficulty_Type);

end Config;
