-- Copyright (c) 2020 Bartek thindil Jasicki <thindil@laeran.pl>
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
with Tcl; use Tcl;

-- ****h* MainMenu/MCommands
-- FUNCTION
-- Provide code of Tcl commands related to MainMenu
-- SOURCE
package MainMenu.Commands is
-- ****

   -- ****o* MCommands/Open_Link_Command
   -- FUNCTION
   -- Open the selected link in the proper program
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- OpenLink url
   -- Url is link which will be opened
   -- SOURCE
   function Open_Link_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

      -- ****f* MCommands/AddCommands
      -- FUNCTION
      -- Add Tcl commands related to main menu to the game
      -- SOURCE
   procedure AddCommands;
   -- ****

end MainMenu.Commands;
