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

with Ada.Strings;
with Ada.Strings.Fixed;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Tcl; use Tcl;
with Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Busy;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Place;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkEntry;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkWidget;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Utils.UI;

package body Dialogs is

   -- ****iv* Dialogs/Dialogs.Timer_Id
   -- FUNCTION
   -- Id of timer for auto close command
   -- SOURCE
   Timer_Id: Unbounded_String := Null_Unbounded_String;
   -- ****

   -- ****if* Dialogs/Dialogs.Get_Timer_Id
   -- FUNCTION
   -- Get the Id of the auto close timer
   -- RESULT
   -- The Id of the auto close timer
   -- SOURCE
   function Get_Timer_Id return Unbounded_String is
      -- ****
   begin
      return Timer_Id;
   end Get_Timer_Id;

   -- ****if* Dialogs/Dialogs.Set_Timer_Id
   -- FUNCTION
   -- Set the Id of the auto close timer
   -- PARAMETERS
   -- New_Value - the new Id for the auto close timer
   -- SOURCE
   procedure Set_Timer_Id(New_Value: Unbounded_String) is
      -- ****
   begin
      Timer_Id := New_Value;
   end Set_Timer_Id;

   function Create_Dialog
     (Name, Title: String; Title_Width: Positive := 275;
      Columns: Positive := 1; Parent_Name: String := ".gameframe")
      return Ttk_Frame is

      New_Dialog: Ttk_Frame;
      Temp_Timer_Id: chars_ptr :=
        New_String(Str => To_String(Source => Get_Timer_Id));
      function Create_Ada_Dialog
        (N, T: chars_ptr; T_Width, Cols: Positive; P_Name: chars_ptr;
         Timer_Name: out chars_ptr) return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "createAdaDialog";
   begin
      New_Dialog :=
        Get_Widget
          (pathName =>
             Value
               (Item =>
                  Create_Ada_Dialog
                    (N => New_String(Str => Name),
                     T => New_String(Str => Title), T_Width => Title_Width,
                     Cols => Columns, P_Name => New_String(Str => Parent_Name),
                     Timer_Name => Temp_Timer_Id)));
      Set_Timer_Id
        (New_Value =>
           To_Unbounded_String(Source => Value(Item => Temp_Timer_Id)));
      return New_Dialog;
   end Create_Dialog;

   --## rule off LOCAL_HIDING
   procedure Add_Close_Button
     (Name, Text, Command: String; Column_Span: Positive := 1;
      Row, Column: Natural := 0; Icon: String := "exiticon";
      Color: String := "") is

      procedure Add_Ada_Close_Button
        (N, T, Com: chars_ptr; Col_Span, R, Col: Integer;
         I, Colo: chars_ptr) with
         Import => True,
         Convention => C,
         External_Name => "addAdaCloseButton";
   begin
      Add_Ada_Close_Button
        (N => New_String(Str => Name), T => New_String(Str => Text),
         Com => New_String(Str => Command), Col_Span => Column_Span, R => Row,
         Col => Column, I => New_String(Str => Icon),
         Colo => New_String(Str => Color));
   end Add_Close_Button;

   procedure Show_Dialog
     (Dialog: Ttk_Frame; Parent_Frame: String := ".gameframe";
      With_Timer: Boolean := False;
      Relative_X, Relative_Y: Damage_Factor := 0.3) is

      Local_Timer: chars_ptr;
      function Show_Ada_Dialog
        (D, P_Frame: chars_ptr; W_Timer: Integer; Rel_X, Rel_Y: Damage_Factor)
         return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "showAdaDialog";
   begin
      Local_Timer :=
        Show_Ada_Dialog
          (D => New_String(Str => Widget_Image(Win => Dialog)),
           P_Frame => New_String(Str => Parent_Frame),
           W_Timer => (if With_Timer then 1 else 0), Rel_X => Relative_X,
           Rel_Y => Relative_Y);
      Set_Timer_Id
        (New_Value =>
           To_Unbounded_String(Source => Value(Item => Local_Timer)));
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
      if Get_Timer_Id /= Null_Unbounded_String then
         Cancel(id_or_script => To_String(Source => Get_Timer_Id));
         Set_Timer_Id(New_Value => Null_Unbounded_String);
      end if;
      if Argc = 3 then
         Frame :=
           Get_Widget
             (pathName => CArgv.Arg(Argv => Argv, N => 2), Interp => Interp);
         if Tcl.Tk.Ada.Busy.Status(Window => Frame) = "1" then
            Tcl.Tk.Ada.Busy.Forget(Window => Frame);
         end if;
         if CArgv.Arg(Argv => Argv, N => 2) = ".memberdialog" then
            Frame :=
              Get_Widget
                (pathName => Frame & ".buttons.button", Interp => Interp);
            if Winfo_Get(Widgt => Frame, Info => "exists") = "0" then
               Frame :=
                 Get_Widget
                   (pathName => ".memberdialog.button", Interp => Interp);
            end if;
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
   begin
      if Winfo_Get(Widgt => Message_Button, Info => "exists") = "0" then
         return
           Close_Dialog_Command
             (Client_Data => Client_Data, Interp => Interp, Argc => Argc,
              Argv => Argv);
      end if;
      Update_Timer_Block:
      declare
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
         Set_Timer_Id
           (New_Value =>
              To_Unbounded_String
                (Source =>
                   After
                     (Ms => 1_000,
                      Script =>
                        "UpdateDialog " & CArgv.Arg(Argv => Argv, N => 1) &
                        (if Argc = 3 then " " & CArgv.Arg(Argv => Argv, N => 2)
                         else ""))));
      end Update_Timer_Block;
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
   -- GetString caption closeaction title okbutton
   -- Caption is the text showed above entry field in the dialog, variable
   -- is the variable which will be set, title is the title of the dialog and
   -- okbutton is the text which will be displayed on the confirmation
   -- button
   -- SOURCE
   function Get_String_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Get_String_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      use Tcl.Tk.Ada.Widgets.TtkEntry;
      use Tcl.Tk.Ada.Widgets.TtkWidget;

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
             "-text {" & CArgv.Arg(Argv => Argv, N => 4) &
             "} -command {SetTextVariable " & CArgv.Arg(Argv => Argv, N => 2) &
             "; CloseDialog " & String_Dialog &
             "} -image edit2icon -style Dialoggreen.TButton");
      Cancel_Button: constant Ttk_Button :=
        Create
          (pathName => String_Dialog & ".closebutton",
           options =>
             "-text Cancel -command {CloseDialog " & String_Dialog &
             "} -image cancelicon -style Dialogred.TButton");
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

   -- ****if* Dialogs/Dialogs.Get_Mouse_X_Position
   -- FUNCTION
   -- Get the X position of the mouse pointer
   -- RESULT
   -- The X axis position of the mouse pointer
   -- SOURCE
   function Get_Mouse_X_Position return Natural is
      -- ****
   begin
      return Mouse_X_Position;
   end Get_Mouse_X_Position;

   -- ****if* Dialogs/Set_Mouse_X_Position
   -- FUNCTION
   -- Set the X position of the mouse pointer
   -- PARAMETERS
   -- New_Value - the new value for mouse pointer X position
   -- SOURCE
   procedure Set_Mouse_X_Position(New_Value: Natural) is
      -- ****
   begin
      Mouse_X_Position := New_Value;
   end Set_Mouse_X_Position;

   -- ****if* Dialogs/Dialogs.Mouse_Y_Position
   -- FUNCTION
   -- The current mouse position in Y coordinates
   -- SOURCE
   Mouse_Y_Position: Natural := 0;
   -- ****

   -- ****if* Dialogs/Dialogs.Get_Mouse_Y_Position
   -- FUNCTION
   -- Get the Y position of the mouse pointer
   -- RESULT
   -- The Y axis position of the mouse pointer
   -- SOURCE
   function Get_Mouse_Y_Position return Natural is
      -- ****
   begin
      return Mouse_Y_Position;
   end Get_Mouse_Y_Position;

   -- ****if* Dialogs/Set_Mouse_Y_Position
   -- FUNCTION
   -- Set the Y position of the mouse pointer
   -- PARAMETERS
   -- New_Value - the new value for mouse pointer X position
   -- SOURCE
   procedure Set_Mouse_Y_Position(New_Value: Natural) is
      -- ****
   begin
      Mouse_Y_Position := New_Value;
   end Set_Mouse_Y_Position;

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
      Assign_Mouse_Position_Block :
      begin
         Set_Mouse_X_Position
           (New_Value => Natural'Value(CArgv.Arg(Argv => Argv, N => 2)));
         Set_Mouse_Y_Position
           (New_Value => Natural'Value(CArgv.Arg(Argv => Argv, N => 3)));
      exception
         when Constraint_Error =>
            Set_Mouse_X_Position(New_Value => 0);
            Set_Mouse_Y_Position(New_Value => 0);
      end Assign_Mouse_Position_Block;
      if Get_Mouse_X_Position > 0 and Get_Mouse_Y_Position > 0 then
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
      use Ada.Strings;
      use Ada.Strings.Fixed;
      use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;

      Dialog: constant Ttk_Frame :=
        Get_Widget
          (pathName => CArgv.Arg(Argv => Argv, N => 1), Interp => Interp);
      New_X, New_Y, Current_X_Mouse, Current_Y_Mouse: Integer;
      Header: constant Ttk_Frame := Get_Widget(pathName => Dialog & ".header");
      function Get_Coordinate(Name: String) return Integer is
         use Tcl.Ada;
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
      if Get_Mouse_X_Position = 0 and Get_Mouse_Y_Position = 0 then
         return TCL_OK;
      end if;
      Current_X_Mouse := Integer'Value(CArgv.Arg(Argv => Argv, N => 2));
      Current_Y_Mouse := Integer'Value(CArgv.Arg(Argv => Argv, N => 3));
      if Get_Mouse_X_Position > Current_X_Mouse
        and then Integer'Value(Winfo_Get(Widgt => Dialog, Info => "x")) <
          5 then
         return TCL_OK;
      end if;
      if Get_Mouse_Y_Position > Current_Y_Mouse
        and then Integer'Value(Winfo_Get(Widgt => Dialog, Info => "y")) <
          5 then
         return TCL_OK;
      end if;
      if Get_Mouse_X_Position < Current_X_Mouse
        and then
          Integer'Value(Winfo_Get(Widgt => Dialog, Info => "x")) +
            Integer'Value(Winfo_Get(Widgt => Dialog, Info => "width")) >
          Integer'Value
            (Winfo_Get
               (Widgt => Get_Main_Window(Interp => Interp),
                Info => "width")) then
         return TCL_OK;
      end if;
      if Get_Mouse_Y_Position < Current_Y_Mouse
        and then
          Integer'Value(Winfo_Get(Widgt => Dialog, Info => "y")) +
            Integer'Value(Winfo_Get(Widgt => Header, Info => "height")) + 5 >
          Integer'Value
            (Winfo_Get
               (Widgt => Get_Main_Window(Interp => Interp),
                Info => "height")) then
         return TCL_OK;
      end if;
      New_X :=
        Get_Coordinate(Name => "x") - (Get_Mouse_X_Position - Current_X_Mouse);
      New_Y :=
        Get_Coordinate(Name => "y") - (Get_Mouse_Y_Position - Current_Y_Mouse);
      Tcl.Tk.Ada.Place.Place_Configure
        (Slave => Dialog,
         Options =>
           "-x " & Trim(Source => Integer'Image(New_X), Side => Left) &
           " -y " & Trim(Source => Integer'Image(New_Y), Side => Left));
      Set_Mouse_X_Position(New_Value => Current_X_Mouse);
      Set_Mouse_Y_Position(New_Value => Current_Y_Mouse);
      return TCL_OK;
   end Move_Dialog_Command;

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

      Local_Timer: chars_ptr;
      function Show_Ada_Message
        (Te, P_Frame, Ti: chars_ptr) return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "showAdaMessage";
   begin
      Local_Timer :=
        Show_Ada_Message
          (Te => New_String(Str => Text),
           P_Frame => New_String(Str => Parent_Frame),
           Ti => New_String(Str => Title));
      Set_Timer_Id
        (New_Value =>
           To_Unbounded_String(Source => Value(Item => Local_Timer)));
   end Show_Message;

   procedure Show_Info
     (Text: String; Parent_Name: String := ".gameframe"; Title: String;
      Button_1, Button_2: Button_Settings := Empty_Button_Settings) is

      --## rule off TYPE_INITIAL_VALUES
      type Nim_Button_Settings is record
         Text: chars_ptr;
         Command: chars_ptr;
         Icon: chars_ptr;
         Tooltip: chars_ptr;
         Color: chars_ptr;
      end record;
      --## rule on TYPE_INITIAL_VALUES
      Nim_Button_1: constant Nim_Button_Settings :=
        (Text => New_String(Str => To_String(Source => Button_1.Text)),
         Command => New_String(Str => To_String(Source => Button_1.Command)),
         Icon => New_String(Str => To_String(Source => Button_1.Icon)),
         Tooltip => New_String(Str => To_String(Source => Button_1.Tooltip)),
         Color => New_String(Str => To_String(Source => Button_1.Color)));
      Nim_Button_2: constant Nim_Button_Settings :=
        (Text => New_String(Str => To_String(Source => Button_2.Text)),
         Command => New_String(Str => To_String(Source => Button_2.Command)),
         Icon => New_String(Str => To_String(Source => Button_2.Icon)),
         Tooltip => New_String(Str => To_String(Source => Button_2.Tooltip)),
         Color => New_String(Str => To_String(Source => Button_2.Color)));
      procedure Show_Ada_Info
        (Te, P_Name, Ti: chars_ptr; B_1, B_2: Nim_Button_Settings) with
         Import => True,
         Convention => C,
         External_Name => "showAdaInfo";
   begin
      Show_Ada_Info
        (Te => New_String(Str => Text),
         P_Name => New_String(Str => Parent_Name),
         Ti => New_String(Str => Title), B_1 => Nim_Button_1,
         B_2 => Nim_Button_2);
   end Show_Info;

   procedure Show_Manipulate_Item
     (Title, Command, Action: String;
      Item_Index: Inventory_Container.Extended_Index;
      Max_Amount, Cost: Natural := 0) is
      procedure Show_Ada_Manipulate_Item
        (Ti, Com, Act: chars_ptr; I_Index, M_Amount, C: Integer) with
         Import => True,
         Convention => C,
         External_Name => "showAdaManipulateItem";
   begin
      Show_Ada_Manipulate_Item
        (Ti => New_String(Str => Title), Com => New_String(Str => Command),
         Act => New_String(Str => Action), I_Index => Item_Index,
         M_Amount => Max_Amount, C => Cost);
   end Show_Manipulate_Item;

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
