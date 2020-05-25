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

with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with CArgv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Busy; use Tcl.Tk.Ada.Busy;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Pack;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Toplevel; use Tcl.Tk.Ada.Widgets.Toplevel;
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Tcl.Tk.Ada.Wm; use Tcl.Tk.Ada.Wm;
with Config; use Config;

package body Utils.UI is

   -- ****iv* UI/Timer_Token
   -- FUNCTION
   -- Identifier for the timer for close message dialog
   -- SOURCE
   Timer_Token: Tcl_TimerToken;
   -- ****

   -- ****if* UI/Close_Dialog_Command
   -- FUNCTION
   -- Close the selected dialog
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- SOURCE
   function Close_Dialog_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Close_Dialog_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      Dialog: Tk_Toplevel;
      MainWindow: constant Tk_Toplevel := Get_Main_Window(Interp);
   begin
      Dialog.Interp := Interp;
      Dialog.Name := New_String(CArgv.Arg(Argv, 1));
      Destroy(Dialog);
      if Winfo_Get(MainWindow, "exists") = "1"
        and then Status(MainWindow) = "1" then
         Forget(MainWindow);
      end if;
      return TCL_OK;
   end Close_Dialog_Command;

   -- ****if* UI/CloseMessage
   -- FUNCTION
   -- Auto close the message dialog
   -- PARAMETERS
   -- data - Custom data sent to the procedure. Unused
   -- SOURCE
   procedure CloseMessage(data: ClientData) with
      Convention => C;
      -- ****

   procedure CloseMessage(data: ClientData) is
      pragma Unreferenced(data);
   begin
      Tcl_Eval(Get_Context, "CloseDialog .message");
   end CloseMessage;

   procedure ShowMessage(Text: String) is
      MessageDialog: constant Tk_Toplevel :=
        Create(".message", "-class Dialog");
      MainWindow: constant Tk_Toplevel := Get_Main_Window(Get_Context);
      X, Y: Integer;
      MessageFrame: constant Ttk_Frame := Create(".message.frame");
      MessageLabel: constant Ttk_Label :=
        Create
          (Widget_Image(MessageFrame) & ".text",
           "-text {" & Text & "} -wraplength 300");
      MessageButton: constant Ttk_Button :=
        Create
          (Widget_Image(MessageFrame) & ".button",
           "-text X -command {CloseDialog .message} -style Toolbutton");
   begin
      Tcl.Tk.Ada.Busy.Busy(MainWindow);
      if Timer_Token /= null then
         Tcl_DeleteTimerHandler(Timer_Token);
      end if;
      Wm_Set(MessageDialog, "title", "{Steam Sky - Message}");
      Wm_Set(MessageDialog, "transient", ".");
      if Tcl_GetVar(Get_Context, "tcl_platform(os)") = "Linux" then
         Wm_Set(MessageDialog, "attributes", "-type dialog");
      end if;
      Tcl.Tk.Ada.Grid.Grid(MessageLabel, "-sticky we");
      Tcl.Tk.Ada.Grid.Grid(MessageButton, "-row 0 -column 1");
      Tcl.Tk.Ada.Pack.Pack(MessageFrame, "-expand true -fill both");
      X := (Positive'Value(Winfo_Get(MessageDialog, "vrootwidth")) - 450) / 2;
      if X < 0 then
         X := 0;
      end if;
      Y := (Positive'Value(Winfo_Get(MessageDialog, "vrootheight")) - 200) / 2;
      if Y < 0 then
         Y := 0;
      end if;
      Wm_Set
        (MessageDialog, "geometry",
         "400x200" & "+" & Trim(Positive'Image(X), Left) & "+" &
         Trim(Positive'Image(Y), Left));
      Focus(MessageButton);
      Bind
        (MessageDialog, "<Destroy>",
         "{CloseDialog " & Value(MessageDialog.Name) & "}");
      Timer_Token :=
        Tcl_CreateTimerHandler
          (int(GameSettings.AutoCloseMessagesTime) * 1_000,
           CloseMessage'Access, Null_ClientData);
   end ShowMessage;

   package CreateCommands is new Tcl.Ada.Generic_Command(Integer);

   -- ****if* UI/AddCommand
   -- FUNCTION
   -- Add the selected command to Tcl interpreter
   -- PARAMETERS
   -- Name       - The name of the command which will be used in Tcl
   -- AdaCommand - Ada function which will be invoked
   -- SOURCE
   procedure AddCommand
     (Name: String; AdaCommand: not null CreateCommands.Tcl_CmdProc) is
     -- ****
      Command: Tcl.Tcl_Command;
      Aziptk_Add_Command_Error: exception;
   begin
      Command :=
        CreateCommands.Tcl_CreateCommand
          (Get_Context, Name, AdaCommand, 0, null);
      if Command = null then
         raise Aziptk_Add_Command_Error with "Can't add command " & Name;
      end if;
   end AddCommand;

   procedure AddCommands is
   begin
      AddCommand("CloseDialog", Close_Dialog_Command'Access);
   end AddCommands;

end Utils.UI;
