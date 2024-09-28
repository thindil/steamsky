--    Copyright 2017-2024 Bartek thindil Jasicki
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

-- ****h* Log/Log
-- FUNCTION
-- Provides code for manipulate debug log
-- SOURCE
package Log is
-- ****

   -- ****t* Log/Log.Debug_Types
   -- FUNCTION
   -- Types of debug mode, which messages log to file
   -- SOURCE
   type Debug_Types is (NONE, EVERYTHING, COMBAT, MENU) with
      Default_Value => NONE;
   -- ****

   -- ****d* Log/Log.Default_Debug_Mode
   -- FUNCTION
   -- Default type of debug (disabled)
   -- HISTORY
   -- 6.0 - Added
   -- SOURCE
   Default_Debug_Mode: constant Debug_Types := NONE;
   -- ****

   -- ****v* Log/Log.Debug_Mode
   -- FUNCTION
   -- Did game is run in debug mode
   -- SOURCE
   Debug_Mode: Debug_Types := Default_Debug_Mode;
   -- ****

end Log;
