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

with Tcl;
with Tcl.Tk.Ada;

package body Utils.UI is

   procedure Add_Command
     (Name: String; Ada_Command: not null CreateCommands.Tcl_CmdProc) is
      use Tcl;
      use Tcl.Ada;
      use Tcl.Tk.Ada;

      Command: Tcl.Tcl_Command;
      Steam_Sky_Add_Command_Error: exception;
   begin
      Tcl_Eval(interp => Get_Context, strng => "info commands " & Name);
      if Tcl_GetResult(interp => Get_Context) /= "" then
         raise Steam_Sky_Add_Command_Error
           with "Command with name " & Name & " exists";
      end if;
      Command :=
        CreateCommands.Tcl_CreateCommand
          (interp => Get_Context, cmdName => Name, proc => Ada_Command,
           data => 0, deleteProc => null);
      if Command = null then
         raise Steam_Sky_Add_Command_Error with "Can't add command " & Name;
      end if;
   end Add_Command;

end Utils.UI;
