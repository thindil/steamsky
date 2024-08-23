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

with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Game; use Game;

-- ****h* Dialogs/Dialogs
-- FUNCTION
-- Provide code to show various in game dialogs (messages, questins, etc)
-- SOURCE
package Dialogs is
-- ****

   -- ****f* Dialogs/Dialogs.Create_Dialog
   -- FUNCTION
   -- Create a new dialog with the selected title
   -- PARAMETERS
   -- Name        - The Tk path name of the new dialog
   -- Title       - The title of the new dialog
   -- Title_Width - The maximum width of the title. Used to set wrapping for
   --               it. Default value is 275 pixels. Can be empty.
   -- Columns     - The amount of columns which dialog will have. Used to set
   --               the title. Default value is 1 column. Can be empty.
   -- RESULT
   -- The newly created Dialog as Ttk_Frame
   -- SOURCE
   function Create_Dialog
     (Name, Title: String; Title_Width: Positive := 275;
      Columns: Positive := 1; Parent_Name: String := ".gameframe")
      return Ttk_Frame;
      -- ****

   -- ****f* Dialogs/Dialogs.Show_Dialog
   -- FUNCTION
   -- Show the selected dialog to the player
   -- PARAMETERS
   -- Dialog       - The dialog which will be shown
   -- Parent_Frame - The parent frame name for the dialog. Can be empty.
   --                Default value is .gameframe
   -- With_Timer   - If True, add timer to the dialog. Can be empty. Default
   --                value is False
   -- Relative_X   - Relative X coordinate inside of parent frame for the
   --                dialog. 0.0 is left border. Can be empty. Default value
   --                is 0.3
   -- Relative_Y   - Relative Y coordinate inside of parent frame for the
   --                dialog. 0.0 is top border. Can be empty. Default value is
   --                0.3
   -- SOURCE
   procedure Show_Dialog
     (Dialog: Ttk_Frame; Parent_Frame: String := ".gameframe";
      With_Timer: Boolean := False;
      Relative_X, Relative_Y: Damage_Factor := 0.3);
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
