-- Copyright (c) 2020-2022 Bartek thindil Jasicki <thindil@laeran.pl>
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
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Directories; use Ada.Directories;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.String_Split; use GNAT.String_Split;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets.Text; use Tcl.Tk.Ada.Widgets.Text;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkEntry; use Tcl.Tk.Ada.Widgets.TtkEntry;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkPanedWindow; use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
with Tcl.Tk.Ada.Widgets.TtkScrollbar; use Tcl.Tk.Ada.Widgets.TtkScrollbar;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Bases; use Bases;
with Combat.UI; use Combat.UI;
with Config; use Config;
with CoreUI; use CoreUI;
with Crew; use Crew;
with Dialogs; use Dialogs;
with Events; use Events;
with Factions; use Factions;
with Maps; use Maps;
with Maps.UI; use Maps.UI;
with MainMenu; use MainMenu;
with Messages; use Messages;
with Missions; use Missions;
with Ships.Cargo; use Ships.Cargo;
with Ships.Crew; use Ships.Crew;
with Ships.Movement; use Ships.Movement;
with Ships.UI.Crew; use Ships.UI.Crew;
with Ships.UI.Modules; use Ships.UI.Modules;
with Statistics.UI; use Statistics.UI;

package body Utils.UI is

   procedure Add_Command
     (Name: String; Ada_Command: not null CreateCommands.Tcl_CmdProc) is
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

   -- ****o* UUI/UUI.Resize_Canvas_Command
   -- PARAMETERS
   -- Resize the selected canvas
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ResizeCanvas name width height
   -- Name is the name of the canvas to resize, width it a new width, height
   -- is a new height
   -- SOURCE
   function Resize_Canvas_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Resize_Canvas_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc);
      Canvas: constant Ttk_Frame :=
        Get_Widget
          (pathName => CArgv.Arg(Argv => Argv, N => 1), Interp => Interp);
      Parent_Frame: Ttk_Frame;
   begin
      if Winfo_Get(Widgt => Canvas, Info => "exists") = "0" then
         return TCL_OK;
      end if;
      Parent_Frame :=
        Get_Widget
          (pathName => Winfo_Get(Widgt => Canvas, Info => "parent"),
           Interp => Interp);
      Unbind(Widgt => Parent_Frame, Sequence => "<Configure>");
      Widgets.configure
        (Widgt => Canvas,
         options =>
           "-width " & CArgv.Arg(Argv => Argv, N => 2) & " -height [expr " &
           CArgv.Arg(Argv => Argv, N => 3) & " - 20]");
      Bind
        (Widgt => Parent_Frame, Sequence => "<Configure>",
         Script => "{ResizeCanvas %W.canvas %w %h}");
      return TCL_OK;
   end Resize_Canvas_Command;

   -- ****o* UUI/UUI.Check_Amount_Command
   -- PARAMETERS
   -- Check amount of the item, if it is not below low level warning or if
   -- entered amount is a proper number
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- CheckAmount name cargoindex value
   -- Name is the name of spinbox which value will be checked, cargoindex is
   -- the index of the item in the cargo
   -- SOURCE
   function Check_Amount_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Check_Amount_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data);
      Cargo_Index: constant Natural :=
        Natural'Value(CArgv.Arg(Argv => Argv, N => 2));
      Warning_Text: Unbounded_String := Null_Unbounded_String;
      Amount: Integer := 0;
      Label: Ttk_Label :=
        Get_Widget(pathName => ".itemdialog.errorlbl", Interp => Interp);
      Value: Integer := 0;
      Spin_Box: constant Ttk_SpinBox :=
        Get_Widget
          (pathName => CArgv.Arg(Argv => Argv, N => 1), Interp => Interp);
      Max_Value: constant Positive :=
        Positive'Value(Widgets.cget(Widgt => Spin_Box, option => "-to"));
   begin
      if CArgv.Arg(Argv => Argv, N => 3)'Length > 0 then
         Check_Argument_Loop :
         for Char of CArgv.Arg(Argv => Argv, N => 3) loop
            if not Is_Decimal_Digit(Item => Char) then
               Tcl_SetResult(interp => Interp, str => "0");
               return TCL_OK;
            end if;
         end loop Check_Argument_Loop;
         Value := Integer'Value(CArgv.Arg(Argv => Argv, N => 3));
      end if;
      if CArgv.Arg(Argv => Argv, N => 1) = ".itemdialog.giveamount" then
         Warning_Text :=
           To_Unbounded_String
             (Source => "You will give amount below low level of ");
      else
         Warning_Text :=
           To_Unbounded_String
             (Source =>
                "You will " & CArgv.Arg(Argv => Argv, N => 4) &
                " amount below low level of ");
      end if;
      if Value < 1 then
         Set(SpinBox => Spin_Box, Value => "1");
         Value := 1;
      elsif Value > Max_Value then
         Set(SpinBox => Spin_Box, Value => Positive'Image(Max_Value));
         Value := Max_Value;
      end if;
      if Argc > 4 then
         if CArgv.Arg(Argv => Argv, N => 4) = "take" then
            Tcl_SetResult(interp => Interp, str => "1");
            return TCL_OK;
         elsif CArgv.Arg(Argv => Argv, N => 4) in "buy" | "sell" then
            Set_Price_Info_Block :
            declare
               Cost: Natural :=
                 Value * Positive'Value(CArgv.Arg(Argv => Argv, N => 5));
            begin
               Label :=
                 Get_Widget
                   (pathName => ".itemdialog.costlbl", Interp => Interp);
               Count_Price
                 (Price => Cost, Trader_Index => Find_Member(Order => TALK),
                  Reduce =>
                    (if CArgv.Arg(Argv => Argv, N => 4) = "buy" then True
                     else False));
               configure
                 (Widgt => Label,
                  options =>
                    "-text {" &
                    (if CArgv.Arg(Argv => Argv, N => 4) = "buy" then "Cost:"
                     else "Gain:") &
                    Natural'Image(Cost) & " " &
                    To_String(Source => Money_Name) & "}");
               if CArgv.Arg(Argv => Argv, N => 4) = "buy" then
                  Tcl_SetResult(interp => Interp, str => "1");
                  return TCL_OK;
               end if;
            end Set_Price_Info_Block;
         end if;
      end if;
      Label :=
        Get_Widget(pathName => ".itemdialog.errorlbl", Interp => Interp);
      if Items_List(Player_Ship.Cargo(Cargo_Index).Proto_Index).I_Type =
        Fuel_Type then
         Amount := GetItemAmount(ItemType => Fuel_Type) - Value;
         if Amount <= Game_Settings.Low_Fuel then
            Widgets.configure
              (Widgt => Label,
               options =>
                 "-text {" & To_String(Source => Warning_Text) & "fuel.}");
            Tcl.Tk.Ada.Grid.Grid(Slave => Label);
            Tcl_SetResult(interp => Interp, str => "1");
            return TCL_OK;
         end if;
      end if;
      Check_Food_And_Drinks_Loop :
      for Member of Player_Ship.Crew loop
         if Factions_List(Member.Faction).Drinks_Types.Contains
             (Item =>
                Items_List(Player_Ship.Cargo(Cargo_Index).Proto_Index)
                  .I_Type) then
            Amount := GetItemsAmount(IType => "Drinks") - Value;
            if Amount <= Game_Settings.Low_Drinks then
               Widgets.configure
                 (Widgt => Label,
                  options =>
                    "-text {" & To_String(Source => Warning_Text) &
                    "drinks.}");
               Tcl.Tk.Ada.Grid.Grid(Slave => Label);
               Tcl_SetResult(interp => Interp, str => "1");
               return TCL_OK;
            end if;
            exit Check_Food_And_Drinks_Loop;
         elsif Factions_List(Member.Faction).Food_Types.Contains
             (Item =>
                Items_List(Player_Ship.Cargo(Cargo_Index).Proto_Index)
                  .I_Type) then
            Amount := GetItemsAmount(IType => "Food") - Value;
            if Amount <= Game_Settings.Low_Food then
               Widgets.configure
                 (Widgt => Label,
                  options =>
                    "-text {" & To_String(Source => Warning_Text) & "food.}");
               Tcl.Tk.Ada.Grid.Grid(Slave => Label);
               Tcl_SetResult(interp => Interp, str => "1");
               return TCL_OK;
            end if;
            exit Check_Food_And_Drinks_Loop;
         end if;
      end loop Check_Food_And_Drinks_Loop;
      Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Label);
      Tcl_SetResult(interp => Interp, str => "1");
      return TCL_OK;
   exception
      when Constraint_Error =>
         Tcl_SetResult(interp => Interp, str => "0");
         return TCL_OK;
   end Check_Amount_Command;

   -- ****o* UUI/UUI.Validate_Amount_Command
   -- PARAMETERS
   -- Validate amount of the item when button to increase or decrease the
   -- amount was pressed
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ValidateAmount name
   -- Name is the name of spinbox which value will be validated
   -- SOURCE
   function Validate_Amount_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Validate_Amount_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      Spin_Box: constant Ttk_SpinBox :=
        Get_Widget
          (pathName => CArgv.Arg(Argv => Argv, N => 1), Interp => Interp);
      New_Argv: constant CArgv.Chars_Ptr_Ptr :=
        (if Argc < 4 then Argv & Get(Widgt => Spin_Box)
         elsif Argc = 4 then
           CArgv.Empty & CArgv.Arg(Argv => Argv, N => 0) &
           CArgv.Arg(Argv => Argv, N => 1) & CArgv.Arg(Argv => Argv, N => 2) &
           Get(Widgt => Spin_Box) & CArgv.Arg(Argv => Argv, N => 3)
         else CArgv.Empty & CArgv.Arg(Argv => Argv, N => 0) &
           CArgv.Arg(Argv => Argv, N => 1) & CArgv.Arg(Argv => Argv, N => 2) &
           Get(Widgt => Spin_Box) & CArgv.Arg(Argv => Argv, N => 3) &
           CArgv.Arg(Argv => Argv, N => 4));
   begin
      return
        Check_Amount_Command
          (Client_Data => Client_Data, Interp => Interp,
           Argc => CArgv.Argc(Argv => New_Argv), Argv => New_Argv);
   end Validate_Amount_Command;

   -- ****o* UUI/UUI.Set_Text_Variable_Command
   -- FUNCTION
   -- Set the selected Tcl text variable and the proper the Ada its equivalent
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetTextVariable variablename
   -- Variablename is the name of variable to set
   -- SOURCE
   function Set_Text_Variable_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Text_Variable_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc);
      T_Entry: constant Ttk_Entry :=
        Get_Widget(pathName => ".getstring.entry", Interp => Interp);
      Value: constant String := Get(Widgt => T_Entry);
      Var_Name: constant String := CArgv.Arg(Argv => Argv, N => 1);
   begin
      Tcl_SetVar(interp => Interp, varName => Var_Name, newValue => Value);
      if Var_Name = "shipname" then
         Player_Ship.Name := To_Unbounded_String(Source => Value);
      elsif Var_Name'Length > 10 and then Var_Name(1 .. 10) = "modulename" then
         Rename_Module_Block :
         declare
            Module_Index: constant Positive :=
              Positive'Value(Var_Name(11 .. Var_Name'Last));
         begin
            Player_Ship.Modules(Module_Index).Name :=
              To_Unbounded_String(Source => Value);
            Tcl_UnsetVar(interp => Interp, varName => Var_Name);
            UpdateModulesInfo;
         end Rename_Module_Block;
      elsif Var_Name'Length > 8 and then Var_Name(1 .. 8) = "crewname" then
         Rename_Crew_Member_Block :
         declare
            Crew_Index: constant Positive :=
              Positive'Value(Var_Name(9 .. Var_Name'Last));
         begin
            Player_Ship.Crew(Crew_Index).Name :=
              To_Unbounded_String(Source => Value);
            Tcl_UnsetVar(interp => Interp, varName => Var_Name);
            UpdateCrewInfo;
         end Rename_Crew_Member_Block;
      end if;
      return TCL_OK;
   end Set_Text_Variable_Command;

   -- ****o* UUI/UUI.Process_Question_Command
   -- FUNCTION
   -- Process question from dialog when the player answer Yes there
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ProcessQuestion answer
   -- Answer is the answer set for the selected question
   -- SOURCE
   function Process_Question_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Process_Question_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc);
      Result: constant String := CArgv.Arg(Argv => Argv, N => 1);
   begin
      if Result = "deletesave" then
         Delete_File
           (Name =>
              To_String
                (Source =>
                   Save_Directory &
                   Tcl_GetVar(interp => Interp, varName => "deletesave")));
         Tcl_UnsetVar(interp => Interp, varName => "deletesave");
         Tcl_Eval(interp => Interp, strng => "ShowLoadGame");
      elsif Result = "sethomebase" then
         Set_Home_Base_Block :
         declare
            Trader_Index: constant Natural := Find_Member(Order => TALK);
            Price: Positive := 1_000;
            Money_Index2: constant Natural :=
              Find_Item
                (Inventory => Player_Ship.Cargo, Proto_Index => Money_Index);
         begin
            if Money_Index2 = 0 then
               ShowMessage
                 (Text =>
                    "You don't have any " & To_String(Source => Money_Name) &
                    " for change ship home base.",
                  Title => "No money");
               return TCL_OK;
            end if;
            Count_Price(Price => Price, Trader_Index => Trader_Index);
            if Player_Ship.Cargo(Money_Index2).Amount < Price then
               ShowMessage
                 (Text =>
                    "You don't have enough " &
                    To_String(Source => Money_Name) &
                    " for change ship home base.",
                  Title => "No money");
               return TCL_OK;
            end if;
            Player_Ship.Home_Base :=
              Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
            UpdateCargo
              (Ship => Player_Ship, CargoIndex => Money_Index2,
               Amount => -Price);
            Add_Message
              (Message =>
                 "You changed your ship home base to: " &
                 To_String(Source => Sky_Bases(Player_Ship.Home_Base).Name),
               M_Type => OTHERMESSAGE);
            Gain_Exp
              (Amount => 1, Skill_Number => Natural(Talking_Skill),
               Crew_Index => Trader_Index);
            Update_Game(Minutes => 10);
            ShowSkyMap;
         end Set_Home_Base_Block;
      elsif Result = "nopilot" then
         Wait_For_Rest;
         Check_For_Combat_Block :
         declare
            Starts_Combat: constant Boolean := Check_For_Event;
            Message: Unbounded_String := Null_Unbounded_String;
         begin
            if not Starts_Combat and Game_Settings.Auto_Finish then
               Message := To_Unbounded_String(Source => Auto_Finish_Missions);
            end if;
            if Message /= Null_Unbounded_String then
               ShowMessage
                 (Text => To_String(Source => Message), Title => "Error");
            end if;
            CenterX := Player_Ship.Sky_X;
            CenterY := Player_Ship.Sky_Y;
            if Starts_Combat then
               ShowCombatUI;
            else
               ShowSkyMap;
            end if;
         end Check_For_Combat_Block;
      elsif Result = "quit" then
         Game_Settings.Messages_Position :=
           Game_Settings.Window_Height -
           Natural'Value(SashPos(Paned => Main_Paned, Index => "0"));
         End_Game(Save => True);
         Show_Main_Menu;
      elsif Result = "resign" then
         Death
           (Member_Index => 1,
            Reason => To_Unbounded_String(Source => "resignation"),
            Ship => Player_Ship);
         ShowQuestion
           (Question =>
              "You are dead. Would you like to see your game statistics?",
            Result => "showstats");
      elsif Result = "showstats" then
         Show_Game_Stats_Block :
         declare
            Button: constant Ttk_Button :=
              Get_Widget(pathName => Game_Header & ".menubutton");
         begin
            Tcl.Tk.Ada.Grid.Grid(Slave => Button);
            Widgets.configure
              (Widgt => Close_Button, options => "-command ShowMainMenu");
            Tcl.Tk.Ada.Grid.Grid
              (Slave => Close_Button, Options => "-row 0 -column 1");
            Tcl_SetVar
              (interp => Interp, varName => "gamestate", newValue => "dead");
            ShowStatistics;
            End_Game(Save => False);
         end Show_Game_Stats_Block;
      elsif Result = "mainmenu" then
         Game_Settings.Messages_Position :=
           Game_Settings.Window_Height -
           Natural'Value(SashPos(Paned => Main_Paned, Index => "0"));
         End_Game(Save => False);
         Show_Main_Menu;
      elsif Result = "messages" then
         Show_Last_Messages_Block :
         declare
            Type_Box: constant Ttk_ComboBox :=
              Get_Widget
                (pathName =>
                   Main_Paned & ".messagesframe.canvas.messages.options.types",
                 Interp => Get_Context);
         begin
            Clear_Messages;
            Current(ComboBox => Type_Box, NewIndex => "0");
            Tcl_Eval(interp => Get_Context, strng => "ShowLastMessages");
         end Show_Last_Messages_Block;
      elsif Result = "retire" then
         Death
           (Member_Index => 1,
            Reason =>
              To_Unbounded_String(Source => "retired after finished the game"),
            Ship => Player_Ship);
         ShowQuestion
           (Question =>
              "You are dead. Would you like to see your game statistics?",
            Result => "showstats");
      else
         Dismiss_Member_Block :
         declare
            Base_Index: constant Positive :=
              Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
            Member_Index: constant Positive :=
              Positive'Value(CArgv.Arg(Argv => Argv, N => 1));
         begin
            Add_Message
              (Message =>
                 "You dismissed " &
                 To_String(Source => Player_Ship.Crew(Member_Index).Name) &
                 ".",
               M_Type => ORDERMESSAGE);
            Delete_Member(Member_Index => Member_Index, Ship => Player_Ship);
            Sky_Bases(Base_Index).Population :=
              Sky_Bases(Base_Index).Population + 1;
            Update_Morale_Loop :
            for I in Player_Ship.Crew.Iterate loop
               Update_Morale
                 (Ship => Player_Ship,
                  Member_Index => Crew_Container.To_Index(Position => I),
                  Value => Get_Random(Min => -5, Max => -1));
            end loop Update_Morale_Loop;
            UpdateCrewInfo;
            UpdateHeader;
            Update_Messages;
         end Dismiss_Member_Block;
      end if;
      return TCL_OK;
   end Process_Question_Command;

   -- ****o* UUI/UUI.Set_Scrollbar_Bindings_Command
   -- FUNCTION
   -- Assign scrolling events with mouse wheel to the selected vertical
   -- scrollbar from the selected widget
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetScrollbarBindings widget scrollbar
   -- Widget is the widget from which events will be fired, scrollbar is
   -- Ttk::scrollbar which to which bindings will be added
   -- SOURCE
   function Set_Scrollbar_Bindings_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Scrollbar_Bindings_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc);
      Widget: constant Ttk_Frame :=
        Get_Widget
          (pathName => CArgv.Arg(Argv => Argv, N => 1), Interp => Interp);
      Scrollbar: constant Ttk_Scrollbar :=
        Get_Widget
          (pathName => CArgv.Arg(Argv => Argv, N => 2), Interp => Interp);
   begin
      Bind
        (Widgt => Widget, Sequence => "<Button-4>",
         Script =>
           "{if {[winfo ismapped " & Scrollbar & "]} {event generate " &
           Scrollbar & " <Button-4>}}");
      Bind
        (Widgt => Widget, Sequence => "<Key-Prior>",
         Script =>
           "{if {[winfo ismapped " & Scrollbar & "]} {event generate " &
           Scrollbar & " <Button-4>}}");
      Bind
        (Widgt => Widget, Sequence => "<Button-5>",
         Script =>
           "{if {[winfo ismapped " & Scrollbar & "]} {event generate " &
           Scrollbar & " <Button-5>}}");
      Bind
        (Widgt => Widget, Sequence => "<Key-Next>",
         Script =>
           "{if {[winfo ismapped " & Scrollbar & "]} {event generate " &
           Scrollbar & " <Button-5>}}");
      Bind
        (Widgt => Widget, Sequence => "<MouseWheel>",
         Script =>
           "{if {[winfo ismapped " & Scrollbar & "]} {event generate " &
           Scrollbar & " <MouseWheel> -delta %D}}");
      return TCL_OK;
   end Set_Scrollbar_Bindings_Command;

   function Show_On_Map_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc);
   begin
      CenterX := Positive'Value(CArgv.Arg(Argv => Argv, N => 1));
      CenterY := Positive'Value(CArgv.Arg(Argv => Argv, N => 2));
      Tcl_Eval(interp => Interp, strng => "InvokeButton " & Close_Button);
      Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Close_Button);
      return TCL_OK;
   end Show_On_Map_Command;

   function Set_Destination_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc);
   begin
      if Positive'Value(CArgv.Arg(Argv => Argv, N => 1)) =
        Player_Ship.Sky_X and
        Positive'Value(CArgv.Arg(Argv => Argv, N => 2)) =
          Player_Ship.Sky_Y then
         ShowMessage
           (Text => "You are at this location now.",
            Title => "Can't set destination");
         return TCL_OK;
      end if;
      Player_Ship.Destination_X :=
        Positive'Value(CArgv.Arg(Argv => Argv, N => 1));
      Player_Ship.Destination_Y :=
        Positive'Value(CArgv.Arg(Argv => Argv, N => 2));
      Add_Message
        (Message => "You set the travel destination for your ship.",
         M_Type => ORDERMESSAGE);
      Tcl_Eval(interp => Interp, strng => "InvokeButton " & Close_Button);
      Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Close_Button);
      return TCL_OK;
   end Set_Destination_Command;

   procedure Add_Commands is
   begin
      Add_Command
        (Name => "ResizeCanvas", Ada_Command => Resize_Canvas_Command'Access);
      Add_Command
        (Name => "CheckAmount", Ada_Command => Check_Amount_Command'Access);
      Add_Command
        (Name => "ValidateAmount",
         Ada_Command => Validate_Amount_Command'Access);
      Add_Command
        (Name => "SetTextVariable",
         Ada_Command => Set_Text_Variable_Command'Access);
      Add_Command
        (Name => "ProcessQuestion",
         Ada_Command => Process_Question_Command'Access);
      Add_Command
        (Name => "SetScrollbarBindings",
         Ada_Command => Set_Scrollbar_Bindings_Command'Access);
      Add_Command
        (Name => "ShowOnMap", Ada_Command => Show_On_Map_Command'Access);
      Add_Command
        (Name => "SetDestination2",
         Ada_Command => Set_Destination_Command'Access);
   end Add_Commands;

   procedure Minutes_To_Date
     (Minutes: Natural; Info_Text: in out Unbounded_String) with
      SPARK_Mode
   is
      Travel_Time: Date_Record := (others => 0);
      Minutes_Diff: Integer := Minutes;
   begin
      Count_Time_Loop :
      while Minutes_Diff > 0 loop
         pragma Loop_Invariant
           (Travel_Time.Year < 4_000_000 and Travel_Time.Month < 13 and
            Travel_Time.Day < 32 and Travel_Time.Hour < 24);
         case Minutes_Diff is
            when 518_401 .. Integer'Last =>
               Travel_Time.Year := Travel_Time.Year + 1;
               Minutes_Diff := Minutes_Diff - 518_400;
            when 43_201 .. 518_400 =>
               Travel_Time.Month := Travel_Time.Month + 1;
               if Travel_Time.Month > 12 then
                  Travel_Time.Month := 1;
                  Travel_Time.Year := Travel_Time.Year + 1;
               end if;
               Minutes_Diff := Minutes_Diff - 43_200;
            when 1_441 .. 43_200 =>
               Travel_Time.Day := Travel_Time.Day + 1;
               if Travel_Time.Day > 31 then
                  Travel_Time.Day := 1;
                  Travel_Time.Month := Travel_Time.Month + 1;
                  if Travel_Time.Month > 12 then
                     Travel_Time.Month := 1;
                     Travel_Time.Year := Travel_Time.Year + 1;
                  end if;
               end if;
               Minutes_Diff := Minutes_Diff - 1_440;
            when 61 .. 1_440 =>
               Travel_Time.Hour := Travel_Time.Hour + 1;
               if Travel_Time.Hour > 23 then
                  Travel_Time.Hour := 0;
                  Travel_Time.Day := Travel_Time.Day + 1;
                  if Travel_Time.Day > 31 then
                     Travel_Time.Day := 1;
                     Travel_Time.Month := Travel_Time.Month + 1;
                     if Travel_Time.Month > 12 then
                        Travel_Time.Month := 1;
                        Travel_Time.Year := Travel_Time.Year + 1;
                     end if;
                  end if;
               end if;
               Minutes_Diff := Minutes_Diff - 60;
            when others =>
               Travel_Time.Minutes := Minutes_Diff;
               Minutes_Diff := 0;
         end case;
         exit Count_Time_Loop when Travel_Time.Year = 4_000_000;
      end loop Count_Time_Loop;
      if Travel_Time.Year > 0
        and then Length(Source => Info_Text) <
          Natural'Last - (Positive'Image(Travel_Time.Year)'Length + 1) then
         Append
           (Source => Info_Text,
            New_Item => Positive'Image(Travel_Time.Year) & "y");
      end if;
      if Travel_Time.Month > 0
        and then Length(Source => Info_Text) <
          Natural'Last - (Positive'Image(Travel_Time.Month)'Length + 1) then
         Append
           (Source => Info_Text,
            New_Item => Positive'Image(Travel_Time.Month) & "m");
      end if;
      if Travel_Time.Day > 0
        and then Length(Source => Info_Text) <
          Natural'Last - (Positive'Image(Travel_Time.Day)'Length + 1) then
         Append
           (Source => Info_Text,
            New_Item => Positive'Image(Travel_Time.Day) & "d");
      end if;
      if Travel_Time.Hour > 0
        and then Length(Source => Info_Text) <
          Natural'Last - (Positive'Image(Travel_Time.Hour)'Length + 1) then
         Append
           (Source => Info_Text,
            New_Item => Positive'Image(Travel_Time.Hour) & "h");
      end if;
      if Travel_Time.Minutes > 0
        and then Length(Source => Info_Text) <
          Natural'Last - (Positive'Image(Travel_Time.Minutes)'Length + 4) then
         Append
           (Source => Info_Text,
            New_Item => Positive'Image(Travel_Time.Minutes) & "mins");
      end if;
   end Minutes_To_Date;

   procedure Travel_Info
     (Info_Text: in out Unbounded_String; Distance: Positive;
      Show_Fuel_Name: Boolean := False) is
      type Speed_Type is digits 2;
      Speed: constant Speed_Type :=
        Speed_Type(RealSpeed(Ship => Player_Ship, InfoOnly => True)) / 1_000.0;
      Minutes_Diff: Integer;
      Rests, Cabin_Index, Rest_Time, Tired, Cabin_Bonus, Temp_Time: Natural :=
        0;
      Damage: Damage_Factor := 0.0;
   begin
      if Speed = 0.0 then
         Append(Source => Info_Text, New_Item => LF & "ETA: Never");
         return;
      end if;
      Minutes_Diff := Integer(100.0 / Speed);
      case Player_Ship.Speed is
         when QUARTER_SPEED =>
            if Minutes_Diff < 60 then
               Minutes_Diff := 60;
            end if;
         when HALF_SPEED =>
            if Minutes_Diff < 30 then
               Minutes_Diff := 30;
            end if;
         when FULL_SPEED =>
            if Minutes_Diff < 15 then
               Minutes_Diff := 15;
            end if;
         when others =>
            null;
      end case;
      Append(Source => Info_Text, New_Item => LF & "ETA:");
      Minutes_Diff := Minutes_Diff * Distance;
      Count_Rest_Time_Loop :
      for I in Player_Ship.Crew.Iterate loop
         if Player_Ship.Crew(I).Order not in PILOT | ENGINEER then
            goto End_Of_Count_Loop;
         end if;
         Tired := (Minutes_Diff / 15) + Player_Ship.Crew(I).Tired;
         if
           (Tired /
            (80 +
             Player_Ship.Crew(I).Attributes(Integer(Condition_Index)).Level)) >
           Rests then
            Rests :=
              (Tired /
               (80 +
                Player_Ship.Crew(I).Attributes(Integer(Condition_Index))
                  .Level));
         end if;
         if Rests > 0 then
            Cabin_Index :=
              Find_Cabin
                (Member_Index => Crew_Container.To_Index(Position => I));
            if Cabin_Index > 0 then
               Damage :=
                 1.0 -
                 Damage_Factor
                   (Float(Player_Ship.Modules(Cabin_Index).Durability) /
                    Float(Player_Ship.Modules(Cabin_Index).Max_Durability));
               Cabin_Bonus :=
                 Player_Ship.Modules(Cabin_Index).Cleanliness -
                 Natural
                   (Float(Player_Ship.Modules(Cabin_Index).Cleanliness) *
                    Float(Damage));
               if Cabin_Bonus = 0 then
                  Cabin_Bonus := 1;
               end if;
               Temp_Time :=
                 ((80 +
                   Player_Ship.Crew(I).Attributes(Integer(Condition_Index))
                     .Level) /
                  Cabin_Bonus) *
                 15;
               if Temp_Time = 0 then
                  Temp_Time := 15;
               end if;
            else
               Temp_Time :=
                 (80 +
                  Player_Ship.Crew(I).Attributes(Integer(Condition_Index))
                    .Level) *
                 15;
            end if;
            Temp_Time := Temp_Time + 15;
            if Temp_Time > Rest_Time then
               Rest_Time := Temp_Time;
            end if;
         end if;
         <<End_Of_Count_Loop>>
      end loop Count_Rest_Time_Loop;
      Minutes_Diff := Minutes_Diff + (Rests * Rest_Time);
      Minutes_To_Date(Minutes => Minutes_Diff, Info_Text => Info_Text);
      Append
        (Source => Info_Text,
         New_Item =>
           LF & "Approx fuel usage:" &
           Natural'Image
             (abs (Distance * CountFuelNeeded) + (Rests * (Rest_Time / 10))) &
           " ");
      if Show_Fuel_Name then
         Append
           (Source => Info_Text,
            New_Item =>
              Items_List(Find_Proto_Item(Item_Type => Fuel_Type)).Name);
      end if;
   end Travel_Info;

   procedure Update_Messages with
      SPARK_Mode
   is
      Loop_Start: Integer := 0 - Messages_Amount;
      Message: Message_Data;
      Tag_Names: constant array(1 .. 5) of Unbounded_String :=
        (1 => To_Unbounded_String(Source => "yellow"),
         2 => To_Unbounded_String(Source => "green"),
         3 => To_Unbounded_String(Source => "red"),
         4 => To_Unbounded_String(Source => "blue"),
         5 => To_Unbounded_String(Source => "cyan"));
      Messages_View: constant Tk_Text :=
        Get_Widget(pathName => ".gameframe.paned.controls.messages.view");
      procedure Show_Message is
      begin
         if Message.Color = WHITE then
            Insert
              (TextWidget => Messages_View, Index => "end",
               Text => "{" & To_String(Source => Message.Message) & "}");
         else
            Insert
              (TextWidget => Messages_View, Index => "end",
               Text =>
                 "{" & To_String(Source => Message.Message) & "} [list " &
                 To_String
                   (Source => Tag_Names(Message_Color'Pos(Message.Color))) &
                 "]");
         end if;
      end Show_Message;
   begin
      Tcl.Tk.Ada.Widgets.configure
        (Widgt => Messages_View, options => "-state normal");
      Delete
        (TextWidget => Messages_View, StartIndex => "1.0", Indexes => "end");
      if Loop_Start = 0 then
         return;
      end if;
      if Loop_Start < -10 then
         Loop_Start := -10;
      end if;
      if Game_Settings.Messages_Order = OLDER_FIRST then
         Show_Older_First_Loop :
         for I in Loop_Start .. -1 loop
            Message := Get_Message(Message_Index => I + 1);
            Show_Message;
            if I < -1 then
               Insert
                 (TextWidget => Messages_View, Index => "end",
                  Text => "{" & LF & "}");
            end if;
         end loop Show_Older_First_Loop;
         Tcl_Eval(interp => Get_Context, strng => "update");
         See(TextWidget => Messages_View, Index => "end");
      else
         Show_Newer_First_Loop :
         for I in reverse Loop_Start .. -1 loop
            Message := Get_Message(Message_Index => I + 1);
            Show_Message;
            if I > Loop_Start then
               Insert
                 (TextWidget => Messages_View, Index => "end",
                  Text => "{" & LF & "}");
            end if;
         end loop Show_Newer_First_Loop;
      end if;
      Tcl.Tk.Ada.Widgets.configure
        (Widgt => Messages_View, options => "-state disable");
   end Update_Messages;

   procedure Show_Screen(New_Screen_Name: String) with
      SPARK_Mode
   is
      Sub_Window, Old_Sub_Window: Ttk_Frame;
      Sub_Windows: Unbounded_String;
      Messages_Frame: constant Ttk_Frame :=
        Get_Widget(pathName => Main_Paned & ".controls.messages");
      Paned: constant Ttk_PanedWindow :=
        Get_Widget(pathName => Main_Paned & ".controls.buttons");
   begin
      Sub_Windows := To_Unbounded_String(Source => Panes(Paned => Main_Paned));
      Old_Sub_Window :=
        (if Index(Source => Sub_Windows, Pattern => " ") = 0 then
           Get_Widget(pathName => To_String(Source => Sub_Windows))
         else Get_Widget
             (pathName =>
                Slice
                  (Source => Sub_Windows, Low => 1,
                   High => Index(Source => Sub_Windows, Pattern => " "))));
      Forget(Paned => Main_Paned, SubWindow => Old_Sub_Window);
      Sub_Window.Name :=
        New_String(Str => ".gameframe.paned." & New_Screen_Name);
      Insert
        (Paned => Main_Paned, Position => "0", SubWindow => Sub_Window,
         Options => "-weight 1");
      if New_Screen_Name in "optionsframe" | "messagesframe" or
        not Game_Settings.Show_Last_Messages then
         Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Messages_Frame);
         if New_Screen_Name /= "mapframe" then
            SashPos
              (Paned => Main_Paned, Index => "0",
               NewPos => Winfo_Get(Widgt => Main_Paned, Info => "height"));
         end if;
      else
         if Trim
             (Source => Widget_Image(Win => Old_Sub_Window), Side => Both) in
             Main_Paned & ".messagesframe" | Main_Paned & ".optionsframe" then
            SashPos
              (Paned => Main_Paned, Index => "0",
               NewPos =>
                 Natural'Image
                   (Game_Settings.Window_Height -
                    Game_Settings.Messages_Position));
         end if;
         Tcl.Tk.Ada.Grid.Grid(Slave => Messages_Frame);
      end if;
      if New_Screen_Name = "mapframe" then
         Tcl.Tk.Ada.Grid.Grid(Slave => Paned);
      else
         Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Paned);
      end if;
   end Show_Screen;

   procedure Show_Inventory_Item_Info
     (Parent: String; Item_Index: Positive; Member_Index: Natural) is
      use Tiny_String;
      Proto_Index: Tiny_String.Bounded_String;
      Item_Info: Unbounded_String;
      Item_Types: constant array(1 .. 6) of Unbounded_String :=
        (1 => Weapon_Type, 2 => Chest_Armor, 3 => Head_Armor, 4 => Arms_Armor,
         5 => Legs_Armor, 6 => Shield_Type);
   begin
      if Member_Index > 0 then
         Proto_Index :=
           Player_Ship.Crew(Member_Index).Inventory(Item_Index).Proto_Index;
         if Player_Ship.Crew(Member_Index).Inventory(Item_Index).Durability <
           Default_Item_Durability then
            Append
              (Source => Item_Info,
               New_Item =>
                 Get_Item_Damage
                   (Item_Durability =>
                      Player_Ship.Crew(Member_Index).Inventory(Item_Index)
                        .Durability) &
                 LF);
         end if;
      else
         Proto_Index := Player_Ship.Cargo(Item_Index).Proto_Index;
         if Player_Ship.Cargo(Item_Index).Durability <
           Default_Item_Durability then
            Append
              (Source => Item_Info,
               New_Item =>
                 Get_Item_Damage
                   (Item_Durability =>
                      Player_Ship.Cargo(Item_Index).Durability) &
                 LF);
         end if;
      end if;
      Append
        (Source => Item_Info,
         New_Item =>
           "Weight:" & Positive'Image(Items_List(Proto_Index).Weight) & " kg");
      if Items_List(Proto_Index).I_Type = Weapon_Type then
         Append
           (Source => Item_Info,
            New_Item =>
              LF & "Skill: " &
              To_String
                (Source =>
                   SkillsData_Container.Element
                     (Container => Skills_List,
                      Index => Skills_Amount_Range(Items_List(Proto_Index).Value.Element(3)))
                     .Name) &
              "/" &
              To_String
                (Source =>
                   AttributesData_Container.Element
                     (Container => Attributes_List,
                      Index =>
                        (SkillsData_Container.Element
                           (Container => Skills_List,
                            Index => Skills_Amount_Range(Items_List(Proto_Index).Value.Element(3)))
                           .Attribute))
                     .Name));
         if Items_List(Proto_Index).Value(4) = 1 then
            Append
              (Source => Item_Info,
               New_Item => LF & "Can be used with shield.");
         else
            Append
              (Source => Item_Info,
               New_Item =>
                 LF & "Can't be used with shield (two-handed weapon).");
         end if;
         Append
           (Source => Item_Info,
            New_Item =>
              LF & "Damage type: " &
              (case Items_List(Proto_Index).Value(5) is when 1 => "cutting",
                 when 2 => "impaling", when 3 => "blunt", when others => ""));
      end if;
      Show_More_Item_Info_Loop :
      for ItemType of Item_Types loop
         if Items_List(Proto_Index).I_Type = ItemType then
            Append
              (Source => Item_Info,
               New_Item =>
                 LF & "Damage chance: " & LF & "Strength:" &
                 Integer'Image(Items_List(Proto_Index).Value(2)));
            exit Show_More_Item_Info_Loop;
         end if;
      end loop Show_More_Item_Info_Loop;
      if Tools_List.Contains(Item => Items_List(Proto_Index).I_Type) then
         Append
           (Source => Item_Info,
            New_Item =>
              LF & "Damage chance: " &
              Get_Item_Chance_To_Damage
                (Item_Data => Items_List(Proto_Index).Value(1)));
      end if;
      if Length(Source => Items_List(Proto_Index).I_Type) > 4
        and then
        (Slice(Source => Items_List(Proto_Index).I_Type, Low => 1, High => 4) =
         "Ammo" or
         Items_List(Proto_Index).I_Type =
           To_Unbounded_String(Source => "Harpoon")) then
         Append
           (Source => Item_Info,
            New_Item =>
              LF & "Strength:" &
              Integer'Image(Items_List(Proto_Index).Value(1)));
      end if;
      if Items_List(Proto_Index).Description /= Null_Unbounded_String then
         Append
           (Source => Item_Info,
            New_Item =>
              LF & LF &
              To_String(Source => Items_List(Proto_Index).Description));
      end if;
      if Parent = "." then
         ShowInfo
           (Text => To_String(Source => Item_Info),
            Title =>
              (if Member_Index > 0 then
                 Get_Item_Name
                   (Item =>
                      Player_Ship.Crew(Member_Index).Inventory(Item_Index),
                    Damage_Info => False, To_Lower => False)
               else Get_Item_Name
                   (Item => Player_Ship.Cargo(Item_Index),
                    Damage_Info => False, To_Lower => False)));
      else
         ShowInfo
           (Text => To_String(Source => Item_Info), ParentName => Parent,
            Title =>
              (if Member_Index > 0 then
                 Get_Item_Name
                   (Item =>
                      Player_Ship.Crew(Member_Index).Inventory(Item_Index),
                    Damage_Info => False, To_Lower => False)
               else Get_Item_Name
                   (Item => Player_Ship.Cargo(Item_Index),
                    Damage_Info => False, To_Lower => False)));
      end if;
   end Show_Inventory_Item_Info;

   procedure Delete_Widgets
     (Start_Index, End_Index: Integer; Frame: Tk_Widget'Class) with
      SPARK_Mode
   is
      Tokens: Slice_Set;
      Item: Ttk_Frame;
   begin
      if End_Index < Start_Index then
         return;
      end if;
      Delete_Widgets_Loop :
      for I in Start_Index .. End_Index loop
         Create
           (S => Tokens,
            From =>
              Tcl.Tk.Ada.Grid.Grid_Slaves
                (Master => Frame, Option => "-row" & Positive'Image(I)),
            Separators => " ");
         Delete_Row_Loop :
         for J in 1 .. Slice_Count(S => Tokens) loop
            Item := Get_Widget(pathName => Slice(S => Tokens, Index => J));
            Destroy(Widgt => Item);
         end loop Delete_Row_Loop;
      end loop Delete_Widgets_Loop;
   end Delete_Widgets;

   function Get_Skill_Marks
     (Skill_Index: Skills_Amount_Range; Member_Index: Positive)
      return String is
      Skill_Value, Crew_Index: Natural := 0;
      Skill_String: Unbounded_String := Null_Unbounded_String;
   begin
      Get_Highest_Skills_Loop :
      for I in Player_Ship.Crew.First_Index .. Player_Ship.Crew.Last_Index loop
         if Get_Skill_Level
             (Member => Player_Ship.Crew(I), Skill_Index => Skill_Index) >
           Skill_Value then
            Crew_Index := I;
            Skill_Value :=
              Get_Skill_Level
                (Member => Player_Ship.Crew(I), Skill_Index => Skill_Index);
         end if;
      end loop Get_Highest_Skills_Loop;
      if Get_Skill_Level
          (Member => Player_Ship.Crew(Member_Index),
           Skill_Index => Skill_Index) >
        0 then
         Skill_String := To_Unbounded_String(Source => " +");
      end if;
      if Member_Index = Crew_Index then
         Skill_String := Skill_String & To_Unbounded_String(Source => "+");
      end if;
      return To_String(Source => Skill_String);
   end Get_Skill_Marks;

end Utils.UI;
