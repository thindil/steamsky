-- Copyright (c) 2021 Bartek thindil Jasicki <thindil@laeran.pl>
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

with Interfaces.C; use Interfaces.C;
with CArgv;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Items; use Items;

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

      -- ****f* Dialogs/Dialogs.Add_Close_Button
      -- FUNCTION
      -- Add button to close the selected dialog and set proper bindings for
      -- it.
      -- PARAMETERS
      -- Name    - The Tk path name for the button
      -- Text    - The text to display on the button
      -- Command - The Tcl command to run when the button was clicked
      -- SOURCE
   procedure Add_Close_Button(Name, Text, Command: String);
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
   -- SOURCE
   procedure Show_Dialog
     (Dialog: Ttk_Frame; Parent_Frame: String := ".gameframe";
      With_Timer: Boolean := False);
   -- ****

   -- ****o* Dialogs/Dialogs.Close_Dialog_Command
   -- FUNCTION
   -- Close the selected dialog
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- CloseDialog dialogname
   -- Dialogname is name of the dialog to close
   -- SOURCE
   function Close_Dialog_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

      -- ****f* Dialogs/Dialogs.Add_Commands
      -- FUNCTION
      -- Add Tcl commands related to dialogs
      -- SOURCE
   procedure Add_Commands;
   -- ****

      -- ****f* Dialogs/Dialogs.ShowMessage
      -- FUNCTION
      -- Show the selected message to a player
      -- PARAMETERS
      -- Text        - Text of message to show
      -- ParentFrame - The parent frame of the message dialog. Default is
      --               the game frame. Can be empty
      -- Title       - The text show in the dialog header.
      -- SOURCE
   procedure ShowMessage
     (Text: String; ParentFrame: String := ".gameframe"; Title: String) with
      Pre => Text'Length > 0 and ParentFrame'Length > 0;
   -- ****

      -- ****f* Dialogs/Dialogs.ShowInfo
      -- FUNCTION
      -- Show the selected info to a player
      -- PARAMETERS
      -- Text       - Text of info to show
      -- ParentName - Name of the parent widget. If empty, then the main game
      --              window will be used as parent for widget. Default value
      --              is .gameframe
      -- Title      - The text show in the dialog header.
      -- SOURCE
   procedure ShowInfo
     (Text: String; ParentName: String := ".gameframe"; Title: String) with
      Pre => Text'Length > 0 and ParentName'Length > 0;
      -- ****

      -- ****f* Dialogs/Dialogs.ShowManipulateItem
      -- FUNCTION
      -- Show the dialog for manipulate items amount in cargo (like selling,
      -- dropping, etc).
      -- PARAMETERS
      -- Title     - Title of the dialog
      -- Command   - Tcl command which will be executed when the player hit
      --             the button Ok
      -- Action    - The name of action which the player is doing (like drop,
      --             sell, ect)
      -- ItemIndex - The index of the item which will be manipulated
      -- MaxAmount - Max amount of the items to manipualate. If zero, use max
      --             amount of items from player ship cargo. Default value is
      --             zero.
      -- SOURCE
   procedure ShowManipulateItem
     (Title, Command, Action: String;
      ItemIndex: Inventory_Container.Extended_Index;
      MaxAmount: Natural := 0) with
      Pre => Title'Length > 0 and Command'Length > 0;
      -- ****

      -- ****f* Dialogs/Dialogs.ShowQuestion
      -- FUNCTION
      -- Show the dialog with question to the player
      -- PARAMETERS
      -- Question - The question which will be the player asked for
      -- Result   - The value set for Ok button
      -- In_Game  - The question in show during game
      -- HISTORY
      -- 5.9 - Added
      -- SOURCE
   procedure ShowQuestion
     (Question, Result: String; In_Game: Boolean := True) with
      Pre => Question'Length > 0;
      -- ****

end Dialogs;
