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

with Interfaces.C.Strings;
with Interfaces.C; use Interfaces.C;
with CArgv;
with Tcl;
with Utils.UI;

package body Dialogs is

   -- ****io* Dialogs/Dialogs.Close_Dialog_Command
   -- FUNCTION
   -- Close the selected dialog
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command.
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- CloseDialog dialogname
   -- Dialogname is name of the dialog to close
   -- SOURCE
   function Close_Dialog_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C,
      Import => True,
      External_Name => "closeDialogCommand";
      -- ****

   -- ****o* Dialogs/Dialogs.Update_Dialog_Command
   -- FUNCTION
   -- Update countdown timer on the selected dialog. If timer reach 0, close
   -- dialog
   -- PARAMETERS
   -- Client_Data - Custom data send to the command.
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command.
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- UpdateDialog dialogname
   -- Dialogname is name of the dialog to update
   -- SOURCE
   function Update_Dialog_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C,
      Import => True,
      External_Name => "updateDialogCommand";
      -- ****

   -- ****o* UUI/UUI.Get_String_Command
   -- FUNCTION
   -- Get string value from the player, like new ship or module name
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- GetString caption closeaction title okbutton
   -- Caption is the text showed above entry field in the dialog, variable
   -- is the variable which will be set, title is the title of the dialog and
   -- okbutton is the text which will be displayed on the confirmation
   -- button
   -- SOURCE
   function Get_String_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C,
      Import => True,
      External_Name => "getStringCommand";
      -- ****

   -- ****o* Dialogs/Dialogs.Set_Mouse_Position_Command
   -- FUNCTION
   -- Set the mouse position
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetMousePosition x y
   -- X and Y are current position of the mouse
   -- SOURCE
   function Set_Mouse_Position_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C,
      Import => True,
      External_Name => "setMousePositionCommand";
      -- ****

   -- ****o* Dialogs/Dialogs.Move_Dialog_Command
   -- FUNCTION
   -- Move the selected dialog around
   -- PARAMETERS
   -- Client_Data - Custom data send to the command.
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command.
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- MoveDialog dialogname x y
   -- Dialogname is name of the dialog to move, x and y are the current
   -- position of the mouse to count where to move the dialog
   -- SOURCE
   function Move_Dialog_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C,
      Import => True,
      External_Name => "moveDialogCommand";
      -- ****

   procedure Add_Commands is
      use Utils.UI;
   begin
      Add_Command
        (Name => "CloseDialog", Ada_Command => Close_Dialog_Command'Access);
      Add_Command
        (Name => "UpdateDialog", Ada_Command => Update_Dialog_Command'Access);
      Add_Command
        (Name => "GetString", Ada_Command => Get_String_Command'Access);
      Add_Command
        (Name => "SetMousePosition",
         Ada_Command => Set_Mouse_Position_Command'Access);
      Add_Command
        (Name => "MoveDialog", Ada_Command => Move_Dialog_Command'Access);
   end Add_Commands;

   procedure Show_Message
     (Text: String; Parent_Frame: String := ".gameframe"; Title: String) is
      use Interfaces.C.Strings;

      function Show_Ada_Message
        (Te, P_Frame, Ti: chars_ptr) return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "showAdaMessage";
   begin
      if Strlen
          (Item =>
             Show_Ada_Message
               (Te => New_String(Str => Text),
                P_Frame => New_String(Str => Parent_Frame),
                Ti => New_String(Str => Title))) =
        0 then
         return;
      end if;
   end Show_Message;

end Dialogs;
