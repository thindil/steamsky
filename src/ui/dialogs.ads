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

with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;

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
   -- SOURCE
   procedure Show_Dialog
     (Dialog: Ttk_Frame; Parent_Frame: String := ".gameframe");
   -- ****

end Dialogs;
