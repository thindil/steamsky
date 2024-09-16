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

--## rule off REDUCEABLE_SCOPE
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
--## rule on REDUCEABLE_SCOPE

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

   --## rule off REDUCEABLE_SCOPE
   -- ****d* Config/Config.Default_Difficulty_Type
   -- FUNCTION
   -- Default difficulty level for the game
   -- SOURCE
   Default_Difficulty_Type: constant Difficulty_Type := NORMAL;
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
   --## rule on REDUCEABLE_SCOPE

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

-- Temporary code to interact with Nim

   function Get_Boolean_Setting(Name: String) return Boolean;
   function Get_Integer_Setting(Name: String) return Integer;
   procedure Set_Integer_Setting(Name: String; Value: Integer);
   function Get_Interface_Theme return Unbounded_String;
   function Get_String_Setting(Name: String) return String;
   function Get_Float_Setting(Name: String) return Bonus_Type;
   function Get_Difficulty return Difficulty_Type;
   function Get_Gender return Character with
      Import => True,
      Convention => C,
      External_Name => "getAdaGender";

end Config;
