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

-- ****h* Config/Config
-- FUNCTION
-- Provide code for load and save the game configuration
-- SOURCE
package Config is
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

   --## rule off TYPE_INITIAL_VALUES
   -- ****t* Config/Config.Font_Types
   -- FUNCTION
   -- Types of fonts, used ot set their size
   -- HISTORY
   -- 7.1 - Added
   -- SOURCE
   type Font_Types is (HELPFONT, INTERFACEFONT, MAPFONT) with
      Default_Value => HELPFONT;
      -- ****
   --## rule on TYPE_INITIAL_VALUES

      -- ****f* Config/Config.Load_Config
      -- FUNCTION
      -- Load game configuration from file
      -- SOURCE
   procedure Load_Config;
   -- ****

end Config;
