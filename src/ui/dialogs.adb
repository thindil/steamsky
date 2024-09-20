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

-- with Ada.Strings;
-- with Ada.Strings.Fixed;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Interfaces.C; use Interfaces.C;
with CArgv;
with Tcl; -- use Tcl;
-- with Tcl.Ada;
-- with Tcl.Tk.Ada; use Tcl.Tk.Ada;
-- with Tcl.Tk.Ada.Place;
-- with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
-- with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
-- with Tcl.Tk.Ada.Widgets.TtkFrame;
-- with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
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

   -- ****iv* Dialogs/Dialogs.Mouse_X_Position
   -- FUNCTION
   -- The current mouse position in X coordinates
   -- SOURCE
--   Mouse_X_Position: Natural := 0;
   -- ****

   --## rule off REDUCEABLE_SCOPE
   -- ****if* Dialogs/Dialogs.Get_Mouse_X_Position
   -- FUNCTION
   -- Get the X position of the mouse pointer
   -- RESULT
   -- The X axis position of the mouse pointer
   -- SOURCE
--   function Get_Mouse_X_Position return Natural is
--      -- ****
--   begin
--      return Mouse_X_Position;
--   end Get_Mouse_X_Position;

   -- ****if* Dialogs/Set_Mouse_X_Position
   -- FUNCTION
   -- Set the X position of the mouse pointer
   -- PARAMETERS
   -- New_Value - the new value for mouse pointer X position
   -- SOURCE
--   procedure Set_Mouse_X_Position(New_Value: Natural) is
--      -- ****
--   begin
--      Mouse_X_Position := New_Value;
--   end Set_Mouse_X_Position;

   -- ****if* Dialogs/Dialogs.Mouse_Y_Position
   -- FUNCTION
   -- The current mouse position in Y coordinates
   -- SOURCE
--   Mouse_Y_Position: Natural := 0;
   -- ****

   -- ****if* Dialogs/Dialogs.Get_Mouse_Y_Position
   -- FUNCTION
   -- Get the Y position of the mouse pointer
   -- RESULT
   -- The Y axis position of the mouse pointer
   -- SOURCE
--   function Get_Mouse_Y_Position return Natural is
--      -- ****
--   begin
--      return Mouse_Y_Position;
--   end Get_Mouse_Y_Position;

   -- ****if* Dialogs/Set_Mouse_Y_Position
   -- FUNCTION
   -- Set the Y position of the mouse pointer
   -- PARAMETERS
   -- New_Value - the new value for mouse pointer X position
   -- SOURCE
--   procedure Set_Mouse_Y_Position(New_Value: Natural) is
--      -- ****
--   begin
--      Mouse_Y_Position := New_Value;
--   end Set_Mouse_Y_Position;
   --## rule on REDUCEABLE_SCOPE

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

--   function Move_Dialog_Command
--     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
--      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
--      pragma Unreferenced(Client_Data, Argc);
--      use Ada.Strings;
--      use Ada.Strings.Fixed;
--      use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
--      use Tcl.Tk.Ada.Widgets.TtkFrame;
--
--      Dialog: constant Ttk_Frame :=
--        Get_Widget
--          (pathName => CArgv.Arg(Argv => Argv, N => 1), Interp => Interp);
--      New_X, New_Y, Current_X_Mouse, Current_Y_Mouse: Integer;
--      Header: constant Ttk_Frame := Get_Widget(pathName => Dialog & ".header");
--      function Get_Coordinate(Name: String) return Integer is
--         use Tcl.Ada;
--      begin
--         Tcl_Eval
--           (interp => Interp,
--            strng =>
--              "lindex [place configure " & Dialog & " -" & Name & "] 4");
--         if Tcl_GetResult(interp => Interp) = "" then
--            return 0;
--         end if;
--         return Integer'Value(Tcl_GetResult(interp => Interp));
--      end Get_Coordinate;
--   begin
--      if Get_Mouse_X_Position = 0 and Get_Mouse_Y_Position = 0 then
--         return TCL_OK;
--      end if;
--      Current_X_Mouse := Integer'Value(CArgv.Arg(Argv => Argv, N => 2));
--      Current_Y_Mouse := Integer'Value(CArgv.Arg(Argv => Argv, N => 3));
--      if Get_Mouse_X_Position > Current_X_Mouse
--        and then Integer'Value(Winfo_Get(Widgt => Dialog, Info => "x")) <
--          5 then
--         return TCL_OK;
--      end if;
--      if Get_Mouse_Y_Position > Current_Y_Mouse
--        and then Integer'Value(Winfo_Get(Widgt => Dialog, Info => "y")) <
--          5 then
--         return TCL_OK;
--      end if;
--      if Get_Mouse_X_Position < Current_X_Mouse
--        and then
--          Integer'Value(Winfo_Get(Widgt => Dialog, Info => "x")) +
--            Integer'Value(Winfo_Get(Widgt => Dialog, Info => "width")) >
--          Integer'Value
--            (Winfo_Get
--               (Widgt => Get_Main_Window(Interp => Interp),
--                Info => "width")) then
--         return TCL_OK;
--      end if;
--      if Get_Mouse_Y_Position < Current_Y_Mouse
--        and then
--          Integer'Value(Winfo_Get(Widgt => Dialog, Info => "y")) +
--            Integer'Value(Winfo_Get(Widgt => Header, Info => "height")) + 5 >
--          Integer'Value
--            (Winfo_Get
--               (Widgt => Get_Main_Window(Interp => Interp),
--                Info => "height")) then
--         return TCL_OK;
--      end if;
--      New_X :=
--        Get_Coordinate(Name => "x") - (Get_Mouse_X_Position - Current_X_Mouse);
--      New_Y :=
--        Get_Coordinate(Name => "y") - (Get_Mouse_Y_Position - Current_Y_Mouse);
--      Tcl.Tk.Ada.Place.Place_Configure
--        (Slave => Dialog,
--         Options =>
--           "-x " & Trim(Source => Integer'Image(New_X), Side => Left) &
--           " -y " & Trim(Source => Integer'Image(New_Y), Side => Left));
--      Set_Mouse_X_Position(New_Value => Current_X_Mouse);
--      Set_Mouse_Y_Position(New_Value => Current_Y_Mouse);
--      return TCL_OK;
--   end Move_Dialog_Command;

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

   procedure Show_Question
     (Question, Result: String; In_Game: Boolean := True) is
      procedure Show_Ada_Question(Q, R: chars_ptr; I_Game: Integer) with
         Import => True,
         Convention => C,
         External_Name => "showAdaQuestion";
   begin
      Show_Ada_Question
        (Q => New_String(Str => Question), R => New_String(Str => Result),
         I_Game => (if In_Game then 1 else 0));
   end Show_Question;

end Dialogs;
