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

with Ada.Characters.Handling; use Ada.Characters.Handling;
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
with Tcl.Tklib.Ada.Tooltip; use Tcl.Tklib.Ada.Tooltip;
with Config; use Config;
with CoreUI; use CoreUI;
with Ships; use Ships;
with Utils.UI; use Utils.UI;

package body Dialogs is

   -- ****iv* Dialogs/Dialogs.Timer_Id
   -- FUNCTION
   -- Id of timer for auto close command
   -- SOURCE
   Timer_Id: Unbounded_String := Null_Unbounded_String;
   -- ****

   function Create_Dialog
     (Name, Title: String; Title_Width: Positive := 275;
      Columns: Positive := 1; Parent_Name: String := ".gameframe")
      return Ttk_Frame is
      New_Dialog: constant Ttk_Frame :=
        Create(pathName => Name, options => "-style Dialog.TFrame");
      Dialog_Header: constant Ttk_Label :=
        Create
          (pathName => New_Dialog & ".header",
           options =>
             "-text {" & Title & "} -wraplength" &
             Positive'Image(Title_Width) &
             " -style Header.TLabel -cursor hand1");
   begin
      if Parent_Name = ".gameframe" then
         Tcl.Tk.Ada.Busy.Busy(Window => Game_Header);
         Tcl.Tk.Ada.Busy.Busy(Window => Main_Paned);
      else
         Tcl.Tk.Ada.Busy.Busy
           (Window => Ttk_Frame'(Get_Widget(pathName => Parent_Name)));
      end if;
      if Timer_Id /= Null_Unbounded_String then
         Cancel(id_or_script => To_String(Source => Timer_Id));
         Timer_Id := Null_Unbounded_String;
      end if;
      Tcl_Eval(interp => Get_Context, strng => "update");
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Dialog_Header,
         Options =>
           "-sticky we -padx 2 -pady {2 0}" &
           (if Columns > 1 then " -columnspan" & Positive'Image(Columns)
            else ""));
      Bind
        (Widgt => Dialog_Header,
         Sequence =>
           "<ButtonPress-" &
           (if Game_Settings.Right_Button then "3" else "1") & ">",
         Script => "{SetMousePosition " & Dialog_Header & " %X %Y}");
      Bind
        (Widgt => Dialog_Header, Sequence => "<Motion>",
         Script => "{MoveDialog " & New_Dialog & " %X %Y}");
      Bind
        (Widgt => Dialog_Header,
         Sequence =>
           "<ButtonRelease-" &
           (if Game_Settings.Right_Button then "3" else "1") & ">",
         Script => "{SetMousePosition " & Dialog_Header & " 0 0}");
      return New_Dialog;
   end Create_Dialog;

   procedure Add_Close_Button
     (Name, Text, Command: String; Column_Span: Positive := 1;
      Row, Column: Natural := 0; Icon: String := "") is
      Button: constant Ttk_Button :=
        Create
          (pathName => Name,
           options =>
             "-command {" & Command & "}" &
             (if Icon'Length > 1 then
                " -image {" & Icon & "} -style Dialog.TButton"
              else " -text {" & Text & "}"));
   begin
      if Icon'Length > 0 then
         Add(Widget => Button, Message => Text);
      end if;
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Button,
         Options =>
           "-pady 5" &
           (if Column_Span > 1 then
              " -columnspan" & Positive'Image(Column_Span)
            else "") &
           (if Row > 0 then " -row" & Positive'Image(Row) else "") &
           (if Column > 0 then " -column" & Positive'Image(Column) else ""));
      Focus(Widgt => Button);
      Bind(Widgt => Button, Sequence => "<Tab>", Script => "{break}");
      Bind
        (Widgt => Button, Sequence => "<Escape>",
         Script => "{" & Button & " invoke;break}");
   end Add_Close_Button;

   procedure Show_Dialog
     (Dialog: Ttk_Frame; Parent_Frame: String := ".gameframe";
      With_Timer: Boolean := False;
      Relative_X, Relative_Y: Damage_Factor := 0.3) is
   begin
      Tcl.Tk.Ada.Place.Place
        (Slave => Dialog,
         Options =>
           "-in " & Parent_Frame & " -relx" & Damage_Factor'Image(Relative_X) &
           " -rely" & Damage_Factor'Image(Relative_Y));
      Widget_Raise(Widgt => Dialog);
      if With_Timer then
         Timer_Id :=
           To_Unbounded_String
             (Source =>
                After
                  (Ms => 1_000,
                   Script =>
                     "UpdateDialog " & Dialog &
                     (if Parent_Frame = ".gameframe" then ""
                      else " " & Parent_Frame)));
      end if;
   end Show_Dialog;

   function Close_Dialog_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data);
      Dialog: Ttk_Frame :=
        Get_Widget
          (pathName => CArgv.Arg(Argv => Argv, N => 1), Interp => Interp);
      Frame: Ttk_Frame :=
        Get_Widget(pathName => ".gameframe.header", Interp => Interp);
   begin
      if Timer_Id /= Null_Unbounded_String then
         Cancel(id_or_script => To_String(Source => Timer_Id));
         Timer_Id := Null_Unbounded_String;
      end if;
      if Argc = 3 then
         Frame :=
           Get_Widget
             (pathName => CArgv.Arg(Argv => Argv, N => 2), Interp => Interp);
         Tcl.Tk.Ada.Busy.Forget(Window => Frame);
         if CArgv.Arg(Argv => Argv, N => 2) = ".memberdialog" then
            Frame :=
              Get_Widget(pathName => Frame & ".button", Interp => Interp);
         end if;
         Focus(Widgt => Frame);
         Destroy(Widgt => Dialog);
         return TCL_OK;
      end if;
      if Tcl.Tk.Ada.Busy.Status(Window => Frame) = "1" then
         Tcl.Tk.Ada.Busy.Forget(Window => Frame);
         Frame := Get_Widget(pathName => ".gameframe.paned");
         Tcl.Tk.Ada.Busy.Forget(Window => Frame);
      end if;
      Destroy(Widgt => Dialog);
      return TCL_OK;
   end Close_Dialog_Command;

   procedure Change_Title(Dialog: Ttk_Frame; New_Title: String) is
      Dialog_Header: constant Ttk_Label :=
        Get_Widget(pathName => Dialog & ".header");
   begin
      configure
        (Widgt => Dialog_Header, options => "-text {" & New_Title & "}");
   end Change_Title;

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
      Convention => C;
      -- ****

   function Update_Dialog_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      Message_Button: constant Ttk_Button :=
        Get_Widget
          (pathName => CArgv.Arg(Argv => Argv, N => 1) & ".button",
           Interp => Interp);
      Text: constant String :=
        Widgets.cget(Widgt => Message_Button, option => "-text");
      Seconds: constant Natural := Natural'Value(Text(6 .. Text'Last)) - 1;
   begin
      if Seconds = 0 then
         return
           Close_Dialog_Command
             (Client_Data => Client_Data, Interp => Interp, Argc => Argc,
              Argv => Argv);
      end if;
      Widgets.configure
        (Widgt => Message_Button,
         options => "-text {Close" & Positive'Image(Seconds) & "}");
      Timer_Id :=
        To_Unbounded_String
          (Source =>
             After
               (Ms => 1_000,
                Script =>
                  "UpdateDialog " & CArgv.Arg(Argv => Argv, N => 1) &
                  (if Argc = 3 then " " & CArgv.Arg(Argv => Argv, N => 2)
                   else "")));
      return TCL_OK;
   end Update_Dialog_Command;

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
   -- GetString caption closeaction title
   -- Caption is the text showed above entry field in the dialog, variable
   -- is the variable which will be set and title is the title of the dialog
   -- SOURCE
   function Get_String_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Get_String_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc);
      String_Dialog: constant Ttk_Frame :=
        Create_Dialog
          (Name => ".getstring", Title => CArgv.Arg(Argv => Argv, N => 3),
           Title_Width => 275, Columns => 2);
      String_Label: constant Ttk_Label :=
        Create
          (pathName => String_Dialog & ".text",
           options =>
             "-text {" & CArgv.Arg(Argv => Argv, N => 1) &
             "} -wraplength 300");
      String_Entry: constant Ttk_Entry :=
        Create
          (pathName => String_Dialog & ".entry",
           options =>
             "-validate key -validatecommand {set value %P;if {$value == {} || [string length $value] > 64} {.getstring.okbutton state disabled; return 1} else {.getstring.okbutton state !disabled; return 1}}");
      Ok_Button: constant Ttk_Button :=
        Create
          (pathName => String_Dialog & ".okbutton",
           options =>
             "-text {Ok} -command {SetTextVariable " &
             CArgv.Arg(Argv => Argv, N => 2) & "; CloseDialog " &
             String_Dialog & "}");
      Cancel_Button: constant Ttk_Button :=
        Create
          (pathName => String_Dialog & ".closebutton",
           options =>
             "-text {Cancel} -command {CloseDialog " & String_Dialog & "}");
   begin
      Tcl.Tk.Ada.Grid.Grid
        (Slave => String_Label,
         Options => "-padx 5 -pady {5 0} -columnspan 2");
      Tcl.Tk.Ada.Grid.Grid
        (Slave => String_Entry, Options => "-sticky we -padx 5 -columnspan 2");
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Ok_Button, Options => "-row 3 -pady 5 -padx 5");
      State(Widget => Ok_Button, StateSpec => "disabled");
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Cancel_Button,
         Options => "-row 3 -column 1 -pady 5 -padx 5");
      Bind
        (Widgt => Cancel_Button, Sequence => "<Tab>",
         Script => "{focus .getstring.entry;break}");
      Bind
        (Widgt => Cancel_Button, Sequence => "<Escape>",
         Script => "{" & Cancel_Button & " invoke;break}");
      Bind
        (Widgt => Ok_Button, Sequence => "<Escape>",
         Script => "{" & Cancel_Button & " invoke;break}");
      Bind
        (Widgt => String_Entry, Sequence => "<Escape>",
         Script => "{" & Cancel_Button & " invoke;break}");
      Bind
        (Widgt => String_Entry, Sequence => "<Return>",
         Script => "{" & Ok_Button & " invoke;break}");
      Focus(Widgt => String_Entry);
      Show_Dialog(Dialog => String_Dialog);
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
      Convention => C;
      -- ****

   function Set_Mouse_Position_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc);
      Dialog_Header: constant Ttk_Label :=
        Get_Widget
          (pathName => CArgv.Arg(Argv => Argv, N => 1), Interp => Interp);
   begin
      Mouse_X_Position := Natural'Value(CArgv.Arg(Argv => Argv, N => 2));
      Mouse_Y_Position := Natural'Value(CArgv.Arg(Argv => Argv, N => 3));
      if Mouse_X_Position > 0 and Mouse_Y_Position > 0 then
         configure(Widgt => Dialog_Header, options => "-cursor fleur");
      else
         configure(Widgt => Dialog_Header, options => "-cursor hand1");
      end if;
      return TCL_OK;
   end Set_Mouse_Position_Command;

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
      Convention => C;
      -- ****

   function Move_Dialog_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc);
      Dialog: constant Ttk_Frame :=
        Get_Widget
          (pathName => CArgv.Arg(Argv => Argv, N => 1), Interp => Interp);
      New_X, New_Y: Integer;
      function Get_Coordinate(Name: String) return Integer is
      begin
         Tcl_Eval
           (interp => Interp,
            strng =>
              "lindex [place configure " & Dialog & " -" & Name & "] 4");
         if Tcl_GetResult(interp => Interp) = "" then
            return 0;
         end if;
         return Integer'Value(Tcl_GetResult(interp => Interp));
      end Get_Coordinate;
   begin
      if Mouse_X_Position = 0 and Mouse_Y_Position = 0 then
         return TCL_OK;
      end if;
      New_X :=
        Get_Coordinate(Name => "x") -
        (Mouse_X_Position - Integer'Value(CArgv.Arg(Argv => Argv, N => 2)));
      New_Y :=
        Get_Coordinate(Name => "y") -
        (Mouse_Y_Position - Integer'Value(CArgv.Arg(Argv => Argv, N => 3)));
      Tcl.Tk.Ada.Place.Place_Configure
        (Slave => Dialog,
         Options =>
           "-x " & Trim(Source => Integer'Image(New_X), Side => Left) &
           " -y " & Trim(Source => Integer'Image(New_Y), Side => Left));
      Mouse_X_Position := Integer'Value(CArgv.Arg(Argv => Argv, N => 2));
      Mouse_Y_Position := Integer'Value(CArgv.Arg(Argv => Argv, N => 3));
      return TCL_OK;
   end Move_Dialog_Command;

   procedure Add_Commands is
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
      Message_Dialog: constant Ttk_Frame :=
        Create_Dialog
          (Name =>
             (if Parent_Frame = "." then "" else Parent_Frame) & ".message",
           Title => Title, Parent_Name => Parent_Frame);
      Message_Label: constant Ttk_Label :=
        Create
          (pathName => Message_Dialog & ".text",
           options => "-text {" & Text & "} -wraplength 300");
   begin
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Message_Label, Options => "-sticky we -padx 5 -pady 5");
      Add_Close_Button
        (Name => Message_Dialog & ".button",
         Text =>
           "Close" & Positive'Image(Game_Settings.Auto_Close_Messages_Time),
         Command =>
           "CloseDialog " & Message_Dialog &
           (if Parent_Frame = ".gameframe" then "" else " " & Parent_Frame));
      Show_Dialog
        (Dialog => Message_Dialog, Parent_Frame => Parent_Frame,
         With_Timer => True);
   end Show_Message;

   procedure Show_Info
     (Text: String; Parent_Name: String := ".gameframe"; Title: String;
      Button_1_Text, Button_1_Command, Button_1_Icon, Button_2_Text,
      Button_2_Command, Button_2_Icon: String := "") is
      Info_Dialog: constant Ttk_Frame :=
        Create_Dialog
          (Name => ".info", Title => Title, Title_Width => 275, Columns => 3,
           Parent_Name => Parent_Name);
      Info_Label: constant Ttk_Label :=
        Create
          (pathName => Info_Dialog & ".text",
           options => "-text {" & Text & "} -wraplength 300");
      Button: Ttk_Button;
      Close_Command: constant String :=
        "CloseDialog " & Info_Dialog &
        (if Parent_Name = ".gameframe" then "" else " " & Parent_Name);
   begin
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Info_Label,
         Options => "-sticky we -padx 5 -pady {5 0} -columnspan 3");
      if Button_1_Text'Length > 0 and Button_1_Command'Length > 0 then
         if Button_1_Icon'Length < 2 then
            Button :=
              Create
                (pathName => Info_Dialog & ".button1",
                 options =>
                   "-text {" & Button_1_Text & "} -command {" & Close_Command &
                   ";" & Button_1_Command & "}");
         else
            Button :=
              Create
                (pathName => Info_Dialog & ".button1",
                 options =>
                   "-image {" & Button_1_Icon & "} -command {" &
                   Close_Command & ";" & Button_1_Command &
                   "} -style Dialog.TButton");
            Add(Widget => Button, Message => Button_1_Text);
         end if;
         Tcl.Tk.Ada.Grid.Grid(Slave => Button, Options => "-padx {5 10}");
         Bind
           (Widgt => Button, Sequence => "<Tab>",
            Script => "{focus " & Info_Dialog & ".button;break}");
         Bind
           (Widgt => Button, Sequence => "<Escape>",
            Script => "{" & Info_Dialog & ".button invoke;break}");
      end if;
      Add_Close_Button
        (Name => Info_Dialog & ".button",
         Text => "Close dialog \[Escape key\]", Command => Close_Command,
         Row => 2, Column => (if Button_1_Text'Length > 0 then 1 else 0),
         Column_Span => (if Button_1_Text'Length > 0 then 1 else 3),
         Icon => "exiticon");
      Button := Get_Widget(pathName => Info_Dialog & ".button");
      if Button_2_Text'Length > 0 and Button_2_Command'Length > 1 then
         Bind
           (Widgt => Button, Sequence => "<Tab>",
            Script => "{focus " & Info_Dialog & ".button2;break}");
         if Button_2_Icon'Length < 2 then
            Button :=
              Create
                (pathName => Info_Dialog & ".button2",
                 options =>
                   "-text {" & Button_2_Text & "} -command {" & Close_Command &
                   ";" & Button_2_Command & "}");
         else
            Button :=
              Create
                (pathName => Info_Dialog & ".button2",
                 options =>
                   "-image {" & Button_2_Icon & "} -command {" &
                   Close_Command & ";" & Button_2_Command &
                   "} -style Dialog.TButton");
            Add(Widget => Button, Message => Button_2_Text);
         end if;
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Button, Options => "-row 2 -column 2 -padx {10 5}");
         if Button_1_Text'Length > 0 then
            Bind
              (Widgt => Button, Sequence => "<Tab>",
               Script => "{focus " & Info_Dialog & ".button1;break}");
         else
            Bind
              (Widgt => Button, Sequence => "<Tab>",
               Script => "{focus " & Info_Dialog & ".button;break}");
         end if;
         Bind
           (Widgt => Button, Sequence => "<Escape>",
            Script => "{" & Info_Dialog & ".button invoke;break}");
      elsif Button_1_Text'Length > 0 and Button_1_Command'Length > 0 then
         Bind
           (Widgt => Button, Sequence => "<Tab>",
            Script => "{focus " & Info_Dialog & ".button1;break}");
      end if;
      Show_Dialog(Dialog => Info_Dialog);
   end Show_Info;

   procedure Show_Manipulate_Item
     (Title, Command, Action: String;
      Item_Index: Inventory_Container.Extended_Index;
      Max_Amount, Cost: Natural := 0) is
      Item_Dialog: constant Ttk_Frame :=
        Create_Dialog
          (Name => ".itemdialog", Title => Title, Title_Width => 275,
           Columns => 2);
      Button: Ttk_Button :=
        Create
          (pathName => Item_Dialog & ".dropbutton",
           options =>
             "-text {" & To_Upper(Item => Action(Action'First)) &
             Action(Action'First + 1 .. Action'Last) & "} -command {" &
             Command & "}");
      Label: Ttk_Label;
      Amount_Box: Ttk_SpinBox;
      Max_Button: Ttk_Button;
   begin
      if Max_Amount = 0 then
         Amount_Box :=
           Create
             (pathName => Item_Dialog & ".amount",
              options =>
                "-width 10 -from 1 -to" &
                Positive'Image
                  (Inventory_Container.Element
                     (Container => Player_Ship.Cargo, Index => Item_Index)
                     .Amount) &
                " -validate key -validatecommand {CheckAmount " & Item_Dialog &
                ".amount" & Positive'Image(Item_Index) & " %P " & Action &
                (if Cost > 0 then Positive'Image(Cost) else "") &
                "} -command {ValidateAmount " & Item_Dialog & ".amount" &
                Positive'Image(Item_Index) & " " & Action &
                (if Cost > 0 then Positive'Image(Cost) else "") & "}");
      else
         Amount_Box :=
           Create
             (pathName => Item_Dialog & ".amount",
              options =>
                "-width 10 -from 1 -to" & Positive'Image(Max_Amount) &
                " -validate key -validatecommand {CheckAmount " & Item_Dialog &
                ".amount" & Positive'Image(Item_Index) & " %P " & Action &
                (if Cost > 0 then Positive'Image(Cost) else "") &
                "} -command {ValidateAmount " & Item_Dialog & ".amount" &
                Positive'Image(Item_Index) & " " & Action &
                (if Cost > 0 then Positive'Image(Cost) else "") & "}");
      end if;
      if Max_Amount = 0 then
         Max_Button :=
           Create
             (pathName => Item_Dialog & ".amountlbl",
              options =>
                "-text {Amount (max:" &
                Positive'Image
                  (Inventory_Container.Element
                     (Container => Player_Ship.Cargo, Index => Item_Index)
                     .Amount) &
                "):} -command {" & Amount_Box & " set" &
                Positive'Image
                  (Inventory_Container.Element
                     (Container => Player_Ship.Cargo, Index => Item_Index)
                     .Amount) &
                "}");
         Tcl.Tk.Ada.Grid.Grid(Slave => Max_Button, Options => "-padx {5 0}");
         Add
           (Widget => Max_Button,
            Message => "Max amount of items to " & Action & ".");
         Bind
           (Widgt => Max_Button, Sequence => "<Escape>",
            Script => "{" & Item_Dialog & ".cancelbutton invoke;break}");
      else
         Max_Button :=
           Create
             (pathName => Item_Dialog & ".amountlbl",
              options =>
                "-text {Amount (max:" & Positive'Image(Max_Amount) &
                "):} -command {" & Amount_Box & " set" &
                Positive'Image(Max_Amount) & "}");
         Tcl.Tk.Ada.Grid.Grid(Slave => Max_Button, Options => "-padx {5 0}");
         Add
           (Widget => Max_Button,
            Message =>
              "Max amount of items to " & Action & "." &
              (if Action in "buy" | "sell" then
                 " It depends on bonuses\nfrom the base's reputation and trader's skill level too."
               else ""));
         Bind
           (Widgt => Max_Button, Sequence => "<Escape>",
            Script => "{" & Item_Dialog & ".cancelbutton invoke;break}");
      end if;
      Set(SpinBox => Amount_Box, Value => "1");
      Tcl.Tk.Ada.Grid.Grid(Slave => Amount_Box, Options => "-column 1 -row 1");
      Bind
        (Widgt => Amount_Box, Sequence => "<Escape>",
         Script => "{" & Item_Dialog & ".cancelbutton invoke;break}");
      if Cost > 0 then
         Label :=
           Create
             (pathName => Item_Dialog & ".costlbl",
              options =>
                "-wraplength 370 -text {" &
                (if Action = "buy" then "Cost:" else "Gain:") &
                Natural'Image(Cost) & " " & To_String(Source => Money_Name) &
                "}");
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Label, Options => "-columnspan 2 -padx 5 -sticky w");
      end if;
      Label :=
        Create
          (pathName => Item_Dialog & ".errorlbl",
           options => "-style Headerred.TLabel -wraplength 370");
      Tcl.Tk.Ada.Grid.Grid(Slave => Label, Options => "-columnspan 2 -padx 5");
      Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Label);
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Button, Options => "-column 0 -row 4 -pady {0 5}");
      Bind
        (Widgt => Button, Sequence => "<Escape>",
         Script => "{" & Item_Dialog & ".cancelbutton invoke;break}");
      Button :=
        Create
          (pathName => Item_Dialog & ".cancelbutton",
           options =>
             "-text Cancel -command {CloseDialog " & Item_Dialog & "}");
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Button, Options => "-column 1 -row 4 -pady {0 5}");
      Focus(Widgt => Button);
      Bind
        (Widgt => Button, Sequence => "<Tab>",
         Script => "{focus .itemdialog.dropbutton;break}");
      Bind
        (Widgt => Button, Sequence => "<Escape>",
         Script => "{" & Button & " invoke;break}");
      Show_Dialog(Dialog => Item_Dialog);
   end Show_Manipulate_Item;

   procedure Show_Question
     (Question, Result: String; In_Game: Boolean := True) is
      Question_Dialog: constant Ttk_Frame :=
        Create_Dialog
          (Name => ".questiondialog",
           Title =>
             (if Result = "showstats" then "Question" else "Confirmation"),
           Title_Width => 275, Columns => 2,
           Parent_Name => (if In_Game then ".gameframe" else "."));
      Label: constant Ttk_Label :=
        Create
          (pathName => Question_Dialog & ".question",
           options => "-text {" & Question & "} -wraplength 370 -takefocus 0");
      Button: Ttk_Button :=
        Create
          (pathName => Question_Dialog & ".yesbutton",
           options =>
             "-text Yes -command {.questiondialog.nobutton invoke; ProcessQuestion " &
             Result & "}");
   begin
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Label, Options => "-columnspan 2 -padx 5 -pady {5 0}");
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Button, Options => "-column 0 -row 2 -pady {0 5} -padx 5");
      Bind
        (Widgt => Button, Sequence => "<Escape>",
         Script => "{" & Question_Dialog & ".nobutton invoke;break}");
      if not In_Game then
         Button :=
           Create
             (pathName => Question_Dialog & ".nobutton",
              options =>
                "-text No -command {CloseDialog " & Question_Dialog & " .}");
      else
         Button :=
           Create
             (pathName => Question_Dialog & ".nobutton",
              options =>
                "-text No -command {CloseDialog " & Question_Dialog & "}");
      end if;
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Button, Options => "-column 1 -row 2 -pady {0 5} -padx 5");
      Focus(Widgt => Button);
      if In_Game then
         Show_Dialog(Dialog => Question_Dialog);
      else
         Show_Dialog(Dialog => Question_Dialog, Parent_Frame => ".");
      end if;
      Bind
        (Widgt => Button, Sequence => "<Tab>",
         Script => "{focus .questiondialog.yesbutton;break}");
      Bind
        (Widgt => Button, Sequence => "<Escape>",
         Script => "{" & Button & " invoke;break}");
      if Result = "showstats" then
         Widgets.configure
           (Widgt => Button,
            options =>
              "-command {CloseDialog " & Question_Dialog &
              "; ProcessQuestion mainmenu}");
         Button := Get_Widget(pathName => Question_Dialog & ".yesbutton");
         Widgets.configure
           (Widgt => Button,
            options =>
              "-command {CloseDialog " & Question_Dialog &
              "; ProcessQuestion showstats}");
      end if;
   end Show_Question;

end Dialogs;
