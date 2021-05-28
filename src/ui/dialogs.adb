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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Busy;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Place;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Config; use Config;
with CoreUI; use CoreUI;
with Utils.UI; use Utils.UI;

package body Dialogs is

   -- ****iv* Dialogs/Dialogs.TimerId
   -- FUNCTION
   -- Id of timer for auto close command
   -- SOURCE
   TimerId: Unbounded_String := Null_Unbounded_String;
   -- ****

   function Create_Dialog
     (Name, Title: String; Title_Width: Positive := 275;
      Columns: Positive := 1; Parent_Name: String := ".gameframe")
      return Ttk_Frame is
      New_Dialog: constant Ttk_Frame := Create(Name, "-style Dialog.TFrame");
      Dialog_Header: constant Ttk_Label :=
        Create
          (New_Dialog & ".header",
           "-text {" & Title & "} -wraplength" & Positive'Image(Title_Width) &
           " -style Header.TLabel");
   begin
      if Parent_Name = ".gameframe" then
         Tcl.Tk.Ada.Busy.Busy(Game_Header);
         Tcl.Tk.Ada.Busy.Busy(Main_Paned);
      else
         Tcl.Tk.Ada.Busy.Busy(Ttk_Frame'(Get_Widget(Parent_Name)));
      end if;
      if TimerId /= Null_Unbounded_String then
         Cancel(To_String(TimerId));
         TimerId := Null_Unbounded_String;
      end if;
      Tcl_Eval(Get_Context, "update");
      Tcl.Tk.Ada.Grid.Grid
        (Dialog_Header,
         "-sticky we -padx 2 -pady {2 0}" &
         (if Columns > 1 then " -columnspan" & Positive'Image(Columns)
          else ""));
      return New_Dialog;
   end Create_Dialog;

   procedure Add_Close_Button(Name, Text, Command: String) is
      Button: constant Ttk_Button :=
        Create(Name, "-text {" & Text & "} -command {" & Command & "}");
   begin
      Tcl.Tk.Ada.Grid.Grid(Button, "-pady 5");
      Focus(Button);
      Bind(Button, "<Tab>", "{break}");
      Bind(Button, "<Escape>", "{" & Button & " invoke;break}");
   end Add_Close_Button;

   procedure Show_Dialog
     (Dialog: Ttk_Frame; Parent_Frame: String := ".gameframe";
      With_Timer: Boolean := False) is
   begin
      Tcl.Tk.Ada.Place.Place
        (Dialog, "-in " & Parent_Frame & " -relx 0.3 -rely 0.3");
      Widget_Raise(Dialog);
      if With_Timer then
         TimerId :=
           To_Unbounded_String(After(1_000, "UpdateDialog " & Dialog));
      end if;
   end Show_Dialog;

   function Close_Dialog_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData);
      Dialog: Ttk_Frame := Get_Widget(CArgv.Arg(Argv, 1), Interp);
      Frame: Ttk_Frame := Get_Widget(".gameframe.header", Interp);
   begin
      if TimerId /= Null_Unbounded_String then
         Cancel(To_String(TimerId));
         TimerId := Null_Unbounded_String;
      end if;
      if Argc = 3 then
         Frame := Get_Widget(CArgv.Arg(Argv, 2), Interp);
         Tcl.Tk.Ada.Busy.Forget(Frame);
         Focus(Frame);
         Destroy(Dialog);
         return TCL_OK;
      end if;
      if Tcl.Tk.Ada.Busy.Status(Frame) = "1" then
         Tcl.Tk.Ada.Busy.Forget(Frame);
         Frame := Get_Widget(".gameframe.paned");
         Tcl.Tk.Ada.Busy.Forget(Frame);
      end if;
      Destroy(Dialog);
      return TCL_OK;
   end Close_Dialog_Command;

   -- ****o* Dialogs/Dialogs.Update_Dialog_Command
   -- FUNCTION
   -- Update countdown timer on the selected dialog. If timer reach 0, close
   -- dialog
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- UpdateDialog dialogname
   -- Dialogname is name of the dialog to update
   -- SOURCE
   function Update_Dialog_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Update_Dialog_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      MessageButton: constant Ttk_Button :=
        Get_Widget(CArgv.Arg(Argv, 1) & ".button", Interp);
      Text: constant String := Widgets.cget(MessageButton, "-text");
      Seconds: constant Natural := Natural'Value(Text(6 .. Text'Last)) - 1;
   begin
      if Seconds = 0 then
         return Close_Dialog_Command(ClientData, Interp, Argc, Argv);
      end if;
      Widgets.configure
        (MessageButton, "-text {Close" & Positive'Image(Seconds) & "}");
      TimerId :=
        To_Unbounded_String
          (After(1_000, "UpdateDialog " & CArgv.Arg(Argv, 1)));
      return TCL_OK;
   end Update_Dialog_Command;

   procedure Add_Commands is
   begin
      AddCommand("CloseDialog", Close_Dialog_Command'Access);
      AddCommand("UpdateDialog", Update_Dialog_Command'Access);
   end Add_Commands;

   procedure ShowMessage
     (Text: String; ParentFrame: String := ".gameframe"; Title: String) is
      MessageDialog: constant Ttk_Frame :=
        Create_Dialog(ParentFrame & ".message", Title);
      MessageLabel: constant Ttk_Label :=
        Create
          (MessageDialog & ".text", "-text {" & Text & "} -wraplength 300");
   begin
      Tcl.Tk.Ada.Grid.Grid(MessageLabel, "-sticky we -padx 5 -pady 5");
      Add_Close_Button
        (MessageDialog & ".button",
         "Close" & Positive'Image(Game_Settings.Auto_Close_Messages_Time),
         "CloseDialog " & MessageDialog);
      Show_Dialog(MessageDialog, ParentFrame);
   end ShowMessage;

   procedure ShowInfo
     (Text: String; ParentName: String := ".gameframe"; Title: String) is
      InfoDialog: constant Ttk_Frame :=
        Create_Dialog(".info", Title, 275, 1, ParentName);
      InfoLabel: constant Ttk_Label :=
        Create(InfoDialog & ".text", "-text {" & Text & "} -wraplength 300");
   begin
      Tcl.Tk.Ada.Grid.Grid(InfoLabel, "-sticky we -padx 5 -pady {5 0}");
      if ParentName = ".gameframe" then
         Add_Close_Button
           (InfoDialog & ".button", "Close", "CloseDialog " & InfoDialog);
      else
         Add_Close_Button
           (InfoDialog & ".button", "Close",
            "CloseDialog " & InfoDialog & " " & ParentName);
      end if;
      Show_Dialog(InfoDialog);
   end ShowInfo;

end Dialogs;
