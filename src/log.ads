--    Copyright 2017-2019 Bartek thindil Jasicki
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

-- ****h* Steamsky/Log
-- FUNCTION
--  Provides code for manipulate debug log
-- SOURCE
package Log is
-- ****

   -- ****t* Log/Debug_Types
   -- FUNCTION
   -- Types of debug mode, which messages log to file
   -- SOURCE
   type Debug_Types is (None, Everything, Combat, Menu);
   -- ****
   -- ****v* Log/DebugMode
   -- FUNCTION
   -- Did game is run in debug mode
   -- SOURCE
   DebugMode: Debug_Types := None;
   -- ****

   -- ****f* Log/StartLogging
   -- FUNCTION
   -- Open/create debug.log file
   -- SOURCE
   procedure StartLogging;
   -- ****
   -- ****f* Log/LogMessage
   -- FUNCTION
   -- Log message (if proper type) to file in debug mode
   -- PARAMETERS
   -- Message     - Message to write to debug log file
   -- MessageType - Type of message to write to debug log file
   -- NewLine     - If true, add new line character after message. Default is
   --               true
   -- TimeStamp   - If true, add timestamp before message. Default is true
   -- SOURCE
   procedure LogMessage
     (Message: String; MessageType: Debug_Types;
      NewLine, TimeStamp: Boolean := True) with
      Pre => Message'Length > 0;
   -- ****
   -- ****f* Log/EndLogging
   -- FUNCTION
   -- Close debug.file
   -- SOURCE
   procedure EndLogging;
   -- ****

end Log;
