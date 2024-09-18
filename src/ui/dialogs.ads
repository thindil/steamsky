-- Copyright (c) 2021-2024 Bartek thindil Jasicki
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

-- ****h* Dialogs/Dialogs
-- FUNCTION
-- Provide code to show various in game dialogs (messages, questins, etc)
-- SOURCE
package Dialogs is
-- ****

   -- ****f* Dialogs/Dialogs.Add_Commands
   -- FUNCTION
   -- Add Tcl commands related to dialogs
   -- SOURCE
   procedure Add_Commands;
   -- ****

      -- ****f* Dialogs/Dialogs.Show_Message
      -- FUNCTION
      -- Show the selected message to a player
      -- PARAMETERS
      -- Text         - Text of message to show
      -- Parent_Frame - The parent frame of the message dialog. Default is
      --                the game frame. Can be empty
      -- Title        - The text show in the dialog header.
      -- SOURCE
   procedure Show_Message
     (Text: String; Parent_Frame: String := ".gameframe"; Title: String) with
      Pre => Text'Length > 0 and Parent_Frame'Length > 0;
      -- ****

      -- ****f* Dialogs/Dialogs.Show_Question
      -- FUNCTION
      -- Show the dialog with question to the player
      -- PARAMETERS
      -- Question - The question which will be the player asked for
      -- Result   - The value set for Ok button
      -- In_Game  - The question in show during game
      -- HISTORY
      -- 5.9 - Added
      -- SOURCE
   procedure Show_Question
     (Question, Result: String; In_Game: Boolean := True) with
      Pre => Question'Length > 0;
      -- ****

-- Temporary code to interact with Nim

end Dialogs;
