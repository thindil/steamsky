-- Copyright (c) 2020-2021 Bartek thindil Jasicki <thindil@laeran.pl>
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

with Interfaces.C;
with CArgv;
with Tcl; use Tcl;

-- ****h* WaitMenu/WaitMenu
-- FUNCTION
-- Provide code for wait orders
-- SOURCE
package WaitMenu is
-- ****

   -- ****o* WaitMenu/WaitMenu.Show_Wait_Command
   -- FUNCTION
   -- Show available wait orders to the player
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowWait
   -- SOURCE
   function Show_Wait_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp;
      Argc: Interfaces.C.int; Argv: CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   -- ****f* WaitMenu/WaitMenu.AddCommands
   -- FUNCTION
   -- Add Tcl commands related to the waiting
   -- SOURCE
   procedure AddCommands;
   -- ****

end WaitMenu;
