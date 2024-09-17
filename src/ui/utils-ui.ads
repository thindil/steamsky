-- Copyright (c) 2020-2024 Bartek thindil Jasicki
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

with Tcl.Ada;
with Config; use Config;

-- ****h* Utils/UUI
-- FUNCTION
-- Provide various code for GUI
-- SOURCE
package Utils.UI is
-- ****

   -- ****t* UUI/UUI.CreateCommands
   -- FUNCTION
   -- Used to add new Tcl commands to interpreter
   -- SOURCE
   package CreateCommands is new Tcl.Ada.Generic_Command
     (ClientData => Integer);
   -- ****

   -- ****f* UUI/UUI.Add_Command
   -- FUNCTION
   -- Add the selected command to Tcl interpreter
   -- PARAMETERS
   -- Name        - The name of the command which will be used in Tcl
   -- Ada_Command - Ada function which will be invoked
   -- SOURCE
   procedure Add_Command
     (Name: String; Ada_Command: not null CreateCommands.Tcl_CmdProc) with
      Pre => Name'Length > 0;
      -- ****

      -- ****f* UUI/UUI.Add_Commands
      -- FUNCTION
      -- Add various, UI related Tcl commands
      -- SOURCE
   procedure Add_Commands;
   -- ****

   -- ****f* UUI/UUI.Set_Fonts
   -- FUNCTION
   -- Set all the game fonts to the selected size
   -- PARAMETERS
   -- New_Size  - The new size of the selected font's type
   -- Font_Type - The type of the font
   -- HISTORY
   -- 7.1 - Added
   -- SOURCE
   procedure Set_Fonts(New_Size: Positive; Font_Type: Font_Types);
   -- ****

end Utils.UI;
