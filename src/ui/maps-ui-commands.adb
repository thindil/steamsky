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

with Interfaces.C; use Interfaces.C;
with CArgv; use CArgv;
with Tcl; use Tcl;
with Dialogs; use Dialogs;
with Ships;
with Ships.Movement;
with Utils.UI;

package body Maps.UI.Commands is

   -- ****o* MapCommands/MapCommands.Set_Ship_Speed_Command
   -- FUNCTION
   -- Set the new speed for the player's ship
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetShipSpeed speed
   -- Speed is the new speed order for the player's ship.
   -- SOURCE
   function Set_Ship_Speed_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Ship_Speed_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc);
      use Ships;
      use Ships.Movement;

      Message: constant String :=
        Change_Ship_Speed
          (Speed_Value =>
             Ship_Speed'Val
               (Natural'Value(CArgv.Arg(Argv => Argv, N => 1)) + 1));
   begin
      if Message'Length > 0 then
         Show_Message(Text => Message, Title => "Changing the ship's speed.");
      end if;
      return TCL_OK;
   end Set_Ship_Speed_Command;

   procedure Add_Commands is
      use Utils.UI;
      procedure Add_Ada_Commands with
         Import => True,
         Convention => C,
         External_Name => "addAdaMapsCommands";
   begin
      Add_Ada_Commands;
      Add_Command
        (Name => "SetShipSpeed", Ada_Command => Set_Ship_Speed_Command'Access);
   end Add_Commands;

end Maps.UI.Commands;
