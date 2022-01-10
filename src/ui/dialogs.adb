-- Copyright (c) 2021-2022 Bartek thindil Jasicki <thindil@laeran.pl>
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
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Busy;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Place;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkEntry; use Tcl.Tk.Ada.Widgets.TtkEntry;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkWidget; use Tcl.Tk.Ada.Widgets.TtkWidget;
with Config; use Config;
with CoreUI; use CoreUI;
with Ships; use Ships;
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
           " -style Header.TLabel -cursor hand1");
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
      Bind
        (Dialog_Header,
         "<ButtonPress-" & (if Game_Settings.Right_Button then "3" else "1") &
         ">",
         "{SetMousePosition " & Dialog_Header & " %X %Y}");
      Bind(Dialog_Header, "<Motion>", "{MoveDialog " & New_Dialog & " %X %Y}");
      Bind
        (Dialog_Header,
         "<ButtonRelease-" &
         (if Game_Settings.Right_Button then "3" else "1") & ">",
         "{SetMousePosition " & Dialog_Header & " 0 0}");
      return New_Dialog;
   end Create_Dialog;

   procedure Add_Close_Button
     (Name, Text, Command: String; Column_Span: Positive := 1;
      Row: Natural := 0) is
      Button: constant Ttk_Button :=
        Create(Name, "-text {" & Text & "} -command {" & Command & "}");
   begin
      Tcl.Tk.Ada.Grid.Grid
        (Button,
         "-pady 5" &
         (if Column_Span > 1 then " -columnspan" & Positive'Image(Column_Span)
          else "") &
         (if Row > 0 then " -row" & Positive'Image(Row) else ""));
      Focus(Button);
      Bind(Button, "<Tab>", "{break}");
      Bind(Button, "<Escape>", "{" & Button & " invoke;break}");
   end Add_Close_Button;

   procedure Show_Dialog
     (Dialog: Ttk_Frame; Parent_Frame: String := ".gameframe";
      With_Timer: Boolean := False;
      Relative_X, Relative_Y: Damage_Factor := 0.3) is
   begin
      Tcl.Tk.Ada.Place.Place
        (Dialog,
         "-in " & Parent_Frame & " -relx" & Damage_Factor'Image(Relative_X) &
         " -rely" & Damage_Factor'Image(Relative_Y));
      Widget_Raise(Dialog);
      if With_Timer then
         TimerId :=
           To_Unbounded_String
             (After
                (1_000,
                 "UpdateDialog " & Dialog &
                 (if Parent_Frame = ".gameframe" then ""
                  else " " & Parent_Frame)));
      end if;
   end Show_Dialog;

   function Close_Dialog_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data);
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
         if CArgv.Arg(Argv, 2) = ".memberdialog" then
            Frame := Get_Widget(Frame & ".button", Interp);
         end if;
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

   procedure Change_Title(Dialog: Ttk_Frame; New_Title: String) is
      Dialog_Header: constant Ttk_Label := Get_Widget(Dialog & ".header");
   begin
      configure(Dialog_Header, "-text {" & New_Title & "}");
   end Change_Title;

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
          (After
             (1_000,
              "UpdateDialog " & CArgv.Arg(Argv, 1) &
              (if Argc = 3 then " " & CArgv.Arg(Argv, 2) else "")));
      return TCL_OK;
   end Update_Dialog_Command;

   -- ****o* UUI/UUI.Get_String_Command
   -- FUNCTION
   -- Get string value from the player, like new ship or module name
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- GetString caption closeaction title
   -- Caption is the text showed above entry field in the dialog, variable
   -- is the variable which will be set and title is the title of the dialog
   -- SOURCE
   function Get_String_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Get_String_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
      StringDialog: constant Ttk_Frame :=
        Create_Dialog(".getstring", CArgv.Arg(Argv, 3), 275, 2);
      StringLabel: constant Ttk_Label :=
        Create
          (StringDialog & ".text",
           "-text {" & CArgv.Arg(Argv, 1) & "} -wraplength 300");
      StringEntry: constant Ttk_Entry :=
        Create
          (StringDialog & ".entry",
           "-validate key -validatecommand {set value %P;if {$value == {}} {.getstring.okbutton state disabled; return 1} else {.getstring.okbutton state !disabled; return 1}}");
      OkButton: constant Ttk_Button :=
        Create
          (StringDialog & ".okbutton",
           "-text {Ok} -command {SetTextVariable " & CArgv.Arg(Argv, 2) &
           "; CloseDialog " & StringDialog & "}");
      CancelButton: constant Ttk_Button :=
        Create
          (StringDialog & ".closebutton",
           "-text {Cancel} -command {CloseDialog " & StringDialog & "}");
   begin
      Tcl.Tk.Ada.Grid.Grid(StringLabel, "-padx 5 -pady {5 0} -columnspan 2");
      Tcl.Tk.Ada.Grid.Grid(StringEntry, "-sticky we -padx 5 -columnspan 2");
      Tcl.Tk.Ada.Grid.Grid(OkButton, "-row 3 -pady 5 -padx 5");
      State(OkButton, "disabled");
      Tcl.Tk.Ada.Grid.Grid(CancelButton, "-row 3 -column 1 -pady 5 -padx 5");
      Bind(CancelButton, "<Tab>", "{focus .getstring.entry;break}");
      Bind(CancelButton, "<Escape>", "{" & CancelButton & " invoke;break}");
      Bind(OkButton, "<Escape>", "{" & CancelButton & " invoke;break}");
      Bind(StringEntry, "<Escape>", "{" & CancelButton & " invoke;break}");
      Bind(StringEntry, "<Return>", "{" & OkButton & " invoke;break}");
      Focus(StringEntry);
      Show_Dialog(StringDialog);
      return TCL_OK;
   end Get_String_Command;

   -- ****iv* Dialogs/Dialogs.Mouse_X_Position
   -- FUNCTION
   -- The current mouse position in X coordinates
   -- SOURCE
   Mouse_X_Position: Natural := 0;
   -- ****

   -- ****if* Dialogs/Dialogs.Mouse_Y_Position
   -- FUNCTION
   -- The current mouse position in Y coordinates
   -- SOURCE
   Mouse_Y_Position: Natural := 0;
   -- ****

   -- ****o* Dialogs/Dialogs.Set_Mouse_Position_Command
   -- FUNCTION
   -- Set the mouse position
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetMousePosition x y
   -- X and Y are current position of the mouse
   -- SOURCE
   function Set_Mouse_Position_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Mouse_Position_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      Dialog_Header: constant Ttk_Label :=
        Get_Widget(CArgv.Arg(Argv, 1), Interp);
   begin
      Mouse_X_Position := Natural'Value(CArgv.Arg(Argv, 2));
      Mouse_Y_Position := Natural'Value(CArgv.Arg(Argv, 3));
      if Mouse_X_Position > 0 and Mouse_Y_Position > 0 then
         configure(Dialog_Header, "-cursor fleur");
      else
         configure(Dialog_Header, "-cursor hand1");
      end if;
      return TCL_OK;
   end Set_Mouse_Position_Command;

   -- ****o* Dialogs/Dialogs.Move_Dialog_Command
   -- FUNCTION
   -- Move the selected dialog around
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- MoveDialog dialogname x y
   -- Dialogname is name of the dialog to move, x and y are the current
   -- position of the mouse to count where to move the dialog
   -- SOURCE
   function Move_Dialog_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Move_Dialog_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      Dialog: constant Ttk_Frame := Get_Widget(CArgv.Arg(Argv, 1), Interp);
      New_X, New_Y: Integer;
      function Get_Coordinate(Name: String) return Integer is
      begin
         Tcl_Eval
           (Interp, "lindex [place configure " & Dialog & " -" & Name & "] 4");
         if Tcl_GetResult(Interp) = "" then
            return 0;
         end if;
         return Integer'Value(Tcl_GetResult(Interp));
      end Get_Coordinate;
   begin
      if Mouse_X_Position = 0 and Mouse_Y_Position = 0 then
         return TCL_OK;
      end if;
      New_X :=
        Get_Coordinate("x") -
        (Mouse_X_Position - Integer'Value(CArgv.Arg(Argv, 2)));
      New_Y :=
        Get_Coordinate("y") -
        (Mouse_Y_Position - Integer'Value(CArgv.Arg(Argv, 3)));
      Tcl.Tk.Ada.Place.Place_Configure
        (Dialog,
         "-x " & Trim(Integer'Image(New_X), Left) & " -y " &
         Trim(Integer'Image(New_Y), Left));
      Mouse_X_Position := Integer'Value(CArgv.Arg(Argv, 2));
      Mouse_Y_Position := Integer'Value(CArgv.Arg(Argv, 3));
      return TCL_OK;
   end Move_Dialog_Command;

   procedure Add_Commands is
   begin
      Add_Command("CloseDialog", Close_Dialog_Command'Access);
      Add_Command("UpdateDialog", Update_Dialog_Command'Access);
      Add_Command("GetString", Get_String_Command'Access);
      Add_Command("SetMousePosition", Set_Mouse_Position_Command'Access);
      Add_Command("MoveDialog", Move_Dialog_Command'Access);
   end Add_Commands;

   procedure Show_Message
     (Text: String; Parent_Frame: String := ".gameframe"; Title: String) is
      MessageDialog: constant Ttk_Frame :=
        Create_Dialog
          (Name =>
             (if Parent_Frame = "." then "" else Parent_Frame) & ".message",
           Title => Title, Parent_Name => Parent_Frame);
      MessageLabel: constant Ttk_Label :=
        Create
          (MessageDialog & ".text", "-text {" & Text & "} -wraplength 300");
   begin
      Tcl.Tk.Ada.Grid.Grid(MessageLabel, "-sticky we -padx 5 -pady 5");
      Add_Close_Button
        (MessageDialog & ".button",
         "Close" & Positive'Image(Game_Settings.Auto_Close_Messages_Time),
         "CloseDialog " & MessageDialog &
         (if Parent_Frame = ".gameframe" then "" else " " & Parent_Frame));
      Show_Dialog(MessageDialog, Parent_Frame, True);
   end Show_Message;

   procedure Show_Info
     (Text: String; Parent_Name: String := ".gameframe"; Title: String) is
      InfoDialog: constant Ttk_Frame :=
        Create_Dialog(".info", Title, 275, 1, Parent_Name);
      InfoLabel: constant Ttk_Label :=
        Create(InfoDialog & ".text", "-text {" & Text & "} -wraplength 300");
   begin
      Tcl.Tk.Ada.Grid.Grid(InfoLabel, "-sticky we -padx 5 -pady {5 0}");
      if Parent_Name = ".gameframe" then
         Add_Close_Button
           (InfoDialog & ".button", "Close", "CloseDialog " & InfoDialog);
      else
         Add_Close_Button
           (InfoDialog & ".button", "Close",
            "CloseDialog " & InfoDialog & " " & Parent_Name);
      end if;
      Show_Dialog(InfoDialog);
   end Show_Info;

   procedure Show_Manipulate_Item
     (Title, Command, Action: String;
      Item_Index: Inventory_Container.Extended_Index;
      Max_Amount, Cost: Natural := 0) is
      ItemDialog: constant Ttk_Frame :=
        Create_Dialog(".itemdialog", Title, 275, 2);
      Button: Ttk_Button :=
        Create
          (ItemDialog & ".dropbutton", "-text Ok -command {" & Command & "}");
      Label: Ttk_Label;
      AmountBox: Ttk_SpinBox;
   begin
      if Max_Amount = 0 then
         AmountBox :=
           Create
             (ItemDialog & ".amount",
              "-width 10 -from 1 -to" &
              Positive'Image(Player_Ship.Cargo(Item_Index).Amount) &
              " -validate key -validatecommand {CheckAmount " & ItemDialog &
              ".amount" & Positive'Image(Item_Index) & " %P " & Action &
              (if Cost > 0 then Positive'Image(Cost) else "") &
              "} -command {ValidateAmount " & ItemDialog & ".amount" &
              Positive'Image(Item_Index) & " " & Action &
              (if Cost > 0 then Positive'Image(Cost) else "") & "}");
      else
         AmountBox :=
           Create
             (ItemDialog & ".amount",
              "-width 10 -from 1 -to" & Positive'Image(Max_Amount) &
              " -validate key -validatecommand {CheckAmount " & ItemDialog &
              ".amount" & Positive'Image(Item_Index) & " %P " & Action &
              (if Cost > 0 then Positive'Image(Cost) else "") &
              "} -command {ValidateAmount " & ItemDialog & ".amount" &
              Positive'Image(Item_Index) & " " & Action &
              (if Cost > 0 then Positive'Image(Cost) else "") & "}");
      end if;
      if Max_Amount = 0 then
         Label :=
           Create
             (ItemDialog & ".amountlbl",
              "-text {Amount (max:" &
              Positive'Image(Player_Ship.Cargo(Item_Index).Amount) &
              "):} -takefocus 0");
      else
         Label :=
           Create
             (ItemDialog & ".amountlbl",
              "-text {Amount (max:" & Positive'Image(Max_Amount) &
              "):} -takefocus 0");
      end if;
      Tcl.Tk.Ada.Grid.Grid(Label, "-padx {5 0}");
      Set(AmountBox, "1");
      Tcl.Tk.Ada.Grid.Grid(AmountBox, "-column 1 -row 1");
      Bind
        (AmountBox, "<Escape>",
         "{" & ItemDialog & ".cancelbutton invoke;break}");
      if Cost > 0 then
         Label :=
           Create
             (ItemDialog & ".costlbl",
              "-wraplength 370 -text {" &
              (if Action = "buy" then "Cost:" else "Gain:") &
              Natural'Image(Cost) & " " & To_String(Money_Name) & "}");
         Tcl.Tk.Ada.Grid.Grid(Label, "-columnspan 2 -padx 5 -sticky w");
      end if;
      Label :=
        Create
          (ItemDialog & ".errorlbl",
           "-style Headerred.TLabel -wraplength 370");
      Tcl.Tk.Ada.Grid.Grid(Label, "-columnspan 2 -padx 5");
      Tcl.Tk.Ada.Grid.Grid_Remove(Label);
      Tcl.Tk.Ada.Grid.Grid(Button, "-column 0 -row 4 -pady {0 5}");
      Bind
        (Button, "<Escape>", "{" & ItemDialog & ".cancelbutton invoke;break}");
      Button :=
        Create
          (ItemDialog & ".cancelbutton",
           "-text Cancel -command {CloseDialog " & ItemDialog & "}");
      Tcl.Tk.Ada.Grid.Grid(Button, "-column 1 -row 4 -pady {0 5}");
      Focus(Button);
      Bind(Button, "<Tab>", "{focus .itemdialog.dropbutton;break}");
      Bind(Button, "<Escape>", "{" & Button & " invoke;break}");
      Show_Dialog(ItemDialog);
   end Show_Manipulate_Item;

   procedure Show_Question
     (Question, Result: String; In_Game: Boolean := True) is
      QuestionDialog: constant Ttk_Frame :=
        Create_Dialog
          (".questiondialog",
           (if Result = "showstats" then "Question" else "Confirmation"), 275,
           2, (if In_Game then ".gameframe" else "."));
      Label: constant Ttk_Label :=
        Create
          (QuestionDialog & ".question",
           "-text {" & Question & "} -wraplength 370 -takefocus 0");
      Button: Ttk_Button :=
        Create
          (QuestionDialog & ".yesbutton",
           "-text Yes -command {.questiondialog.nobutton invoke; ProcessQuestion " &
           Result & "}");
   begin
      Tcl.Tk.Ada.Grid.Grid(Label, "-columnspan 2 -padx 5 -pady {5 0}");
      Tcl.Tk.Ada.Grid.Grid(Button, "-column 0 -row 2 -pady {0 5} -padx 5");
      Bind
        (Button, "<Escape>", "{" & QuestionDialog & ".nobutton invoke;break}");
      if not In_Game then
         Button :=
           Create
             (QuestionDialog & ".nobutton",
              "-text No -command {CloseDialog " & QuestionDialog & " .}");
      else
         Button :=
           Create
             (QuestionDialog & ".nobutton",
              "-text No -command {CloseDialog " & QuestionDialog & "}");
      end if;
      Tcl.Tk.Ada.Grid.Grid(Button, "-column 1 -row 2 -pady {0 5} -padx 5");
      Focus(Button);
      if In_Game then
         Show_Dialog(QuestionDialog);
      else
         Show_Dialog(QuestionDialog, ".");
      end if;
      Bind(Button, "<Tab>", "{focus .questiondialog.yesbutton;break}");
      Bind(Button, "<Escape>", "{" & Button & " invoke;break}");
      if Result = "showstats" then
         Widgets.configure
           (Button,
            "-command {CloseDialog " & QuestionDialog &
            "; ProcessQuestion mainmenu}");
         Button := Get_Widget(QuestionDialog & ".yesbutton");
         Widgets.configure
           (Button,
            "-command {CloseDialog " & QuestionDialog &
            "; ProcessQuestion showstats}");
      end if;
   end Show_Question;

end Dialogs;
