-- Copyright (c) 2020-2023 Bartek thindil Jasicki <thindil@laeran.pl>
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

with Ada.Characters.Handling;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Directories;
with Ada.Strings;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Font;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets.Text;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkEntry;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkPanedWindow;
with Tcl.Tk.Ada.Widgets.TtkScrollbar;
with Tcl.Tk.Ada.Winfo;
with Bases; use Bases;
with Combat.UI;
with CoreUI; use CoreUI;
with Crew; use Crew;
with Events;
with Factions;
with Items; use Items;
with Maps;
with Maps.UI; use Maps.UI;
with MainMenu;
with Messages; use Messages;
with Missions;
with Ships.Cargo; use Ships.Cargo;
with Ships.Crew; use Ships.Crew;
with Ships.Movement;
with Ships.UI.Crew; use Ships.UI.Crew;
with Ships.UI.Modules;
with Statistics.UI;

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
      use Tcl.Tk.Ada.Winfo;

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
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- CheckAmount name cargoindex value action button
   -- Name is the name of spinbox which value will be checked, cargoindex is
   -- the index of the item in the cargo, value is the value entered by the
   -- player, action is the action performed by the player and button is
   -- the button which accept the action
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
      use Ada.Characters.Handling;
      use Tcl.Tk.Ada.Widgets.TtkLabel;
      use Tiny_String;
      use Factions;

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
      Button: constant Ttk_Button :=
        Get_Widget(pathName => CArgv.Arg(Argv => Argv, N => Argc - 1));
      Faction: Faction_Record; --## rule line off IMPROPER_INITIALIZATION
   begin
      if CArgv.Arg(Argv => Argv, N => 3)'Length > 0 then
         Check_Argument_Loop :
         for Char of CArgv.Arg(Argv => Argv, N => 3) loop
            if not Is_Decimal_Digit(Item => Char) then
               Tcl_SetResult(interp => Interp, str => "0");
               return TCL_OK;
            end if;
         end loop Check_Argument_Loop;
         if CArgv.Arg(Argv => Argv, N => 3)'Length > 0 then
            Value := Integer'Value(CArgv.Arg(Argv => Argv, N => 3));
         else
            Value := 0;
         end if;
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
         if Strlen(Item => Button.Name) > 0 then
            Widgets.configure(Widgt => Button, options => "-state disabled");
         end if;
         Tcl_SetResult(interp => Interp, str => "1");
         return TCL_OK;
      elsif Value > Max_Value then
         Set(SpinBox => Spin_Box, Value => Positive'Image(Max_Value));
         Value := Max_Value;
      end if;
      if Strlen(Item => Button.Name) > 0 then
         Widgets.configure(Widgt => Button, options => "-state normal");
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
      if To_String
          (Source =>
             Get_Proto_Item
               (Index =>
                  Inventory_Container.Element
                    (Container => Player_Ship.Cargo, Index => Cargo_Index)
                    .Proto_Index)
               .I_Type) =
        To_String(Source => Fuel_Type) then
         Amount := Get_Item_Amount(Item_Type => Fuel_Type) - Value;
         if Amount <= Get_Integer_Setting(Name => "lowFuel") then
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
         Faction := Get_Faction(Index => Member.Faction);
         if Faction.Drinks_Types.Contains
             (Item =>
                Get_Proto_Item
                  (Index =>
                     Inventory_Container.Element
                       (Container => Player_Ship.Cargo, Index => Cargo_Index)
                       .Proto_Index)
                  .I_Type) then
            Amount := Get_Items_Amount(I_Type => "Drinks") - Value;
            if Amount <= Get_Integer_Setting(Name => "lowDrinks") then
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
         elsif Faction.Food_Types.Contains
             (Item =>
                Get_Proto_Item
                  (Index =>
                     Inventory_Container.Element
                       (Container => Player_Ship.Cargo, Index => Cargo_Index)
                       .Proto_Index)
                  .I_Type) then
            Amount := Get_Items_Amount(I_Type => "Food") - Value;
            if Amount <= Get_Integer_Setting(Name => "lowFood") then
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
           CArgv.Arg(Argv => Argv, N => 4) & CArgv.Arg(Argv => Argv, N => 5));
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
      use Tcl.Tk.Ada.Widgets.TtkEntry;
      use Tiny_String;

      T_Entry: constant Ttk_Entry :=
        Get_Widget(pathName => ".getstring.entry", Interp => Interp);
      Value: constant String := Get(Widgt => T_Entry);
      Var_Name: constant String := CArgv.Arg(Argv => Argv, N => 1);
   begin
      Tcl_SetVar(interp => Interp, varName => Var_Name, newValue => Value);
      if Var_Name = "shipname" then
         Player_Ship.Name := To_Bounded_String(Source => Value);
      elsif Var_Name'Length > 10 and then Var_Name(1 .. 10) = "modulename" then
         Rename_Module_Block :
         declare
            use Ships.UI.Modules;

            Module_Index: constant Positive :=
              Positive'Value(Var_Name(11 .. Var_Name'Last));
         begin
            Player_Ship.Modules(Module_Index).Name :=
              To_Bounded_String(Source => Value);
            Tcl_UnsetVar(interp => Interp, varName => Var_Name);
            Update_Modules_Info;
         end Rename_Module_Block;
      elsif Var_Name'Length > 8 and then Var_Name(1 .. 8) = "crewname" then
         Rename_Crew_Member_Block :
         declare
            Crew_Index: constant Positive :=
              Positive'Value(Var_Name(9 .. Var_Name'Last));
         begin
            Player_Ship.Crew(Crew_Index).Name :=
              To_Bounded_String(Source => Value);
            Tcl_UnsetVar(interp => Interp, varName => Var_Name);
            Update_Crew_Info;
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
      use Ada.Directories;
      use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
      use Maps;
      use MainMenu;
      use Tiny_String;

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
               Show_Message
                 (Text =>
                    "You don't have any " & To_String(Source => Money_Name) &
                    " for change ship home base.",
                  Title => "No money");
               return TCL_OK;
            end if;
            Count_Price(Price => Price, Trader_Index => Trader_Index);
            if Inventory_Container.Element
                (Container => Player_Ship.Cargo, Index => Money_Index2)
                .Amount <
              Price then
               Show_Message
                 (Text =>
                    "You don't have enough " &
                    To_String(Source => Money_Name) &
                    " for change ship home base.",
                  Title => "No money");
               return TCL_OK;
            end if;
            Player_Ship.Home_Base :=
              Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
            Update_Cargo
              (Ship => Player_Ship, Cargo_Index => Money_Index2,
               Amount => -Price);
            Add_Message
              (Message =>
                 "You changed your ship home base to: " &
                 To_String(Source => Sky_Bases(Player_Ship.Home_Base).Name),
               M_Type => OTHERMESSAGE);
            Gain_Exp
              (Amount => 1, Skill_Number => Talking_Skill,
               Crew_Index => Trader_Index);
            Update_Game(Minutes => 10);
            Show_Sky_Map;
         end Set_Home_Base_Block;
      elsif Result = "nopilot" then
         Wait_For_Rest;
         Check_For_Combat_Block :
         declare
            use Combat.UI;
            use Events;
            use Missions;

            Starts_Combat: constant Boolean := Check_For_Event;
            Message: Unbounded_String := Null_Unbounded_String;
         begin
            if not Starts_Combat and
              Get_Boolean_Setting(Name => "autoFinish") then
               Message := To_Unbounded_String(Source => Auto_Finish_Missions);
            end if;
            if Message /= Null_Unbounded_String then
               Show_Message
                 (Text => To_String(Source => Message), Title => "Error");
            end if;
            Center_X := Player_Ship.Sky_X;
            Center_Y := Player_Ship.Sky_Y;
            if Starts_Combat then
               Show_Combat_Ui;
            else
               Show_Sky_Map;
            end if;
         end Check_For_Combat_Block;
      elsif Result = "quit" then
         Game_Settings.Messages_Position :=
           Get_Integer_Setting(Name => "windowHeight") -
           Natural'Value(SashPos(Paned => Main_Paned, Index => "0"));
         End_Game(Save => True);
         Show_Main_Menu;
      elsif Result = "resign" then
         Death
           (Member_Index => 1,
            Reason => To_Unbounded_String(Source => "resignation"),
            Ship => Player_Ship);
         Show_Question
           (Question =>
              "You are dead. Would you like to see your game statistics?",
            Result => "showstats");
      elsif Result = "showstats" then
         Show_Game_Stats_Block :
         declare
            use Statistics.UI;

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
            Show_Statistics;
            End_Game(Save => False);
         end Show_Game_Stats_Block;
      elsif Result = "mainmenu" then
         Game_Settings.Messages_Position :=
           Get_Integer_Setting(Name => "windowHeight") -
           Natural'Value(SashPos(Paned => Main_Paned, Index => "0"));
         End_Game(Save => False);
         Show_Main_Menu;
      elsif Result = "messages" then
         Show_Last_Messages_Block :
         declare
            use Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;

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
         Show_Question
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
                  Amount => Get_Random(Min => -5, Max => -1));
            end loop Update_Morale_Loop;
            Update_Crew_Info;
            Update_Header;
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
      use Tcl.Tk.Ada.Widgets.TtkScrollbar;

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
      Center_X := Positive'Value(CArgv.Arg(Argv => Argv, N => 1));
      Center_Y := Positive'Value(CArgv.Arg(Argv => Argv, N => 2));
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
         Show_Message
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
     (Minutes: Natural; Info_Text: in out Unbounded_String) is
      procedure Min_To_Date(Mins: Natural; Info: in out chars_ptr) with
         Import => True,
         Convention => C,
         External_Name => "minutesToDate";
      New_Text: chars_ptr := New_String(Str => To_String(Source => Info_Text));
   begin
      Min_To_Date(Mins => Minutes, Info => New_Text);
      Info_Text := To_Unbounded_String(Source => Value(Item => New_Text));
   end Minutes_To_Date;

   function Travel_Info(Distance: Positive) return Travel_Array is
      use Ships.Movement;

      type Speed_Type is digits 2;
      Speed: constant Speed_Type :=
        Speed_Type(Real_Speed(Ship => Player_Ship, Info_Only => True)) /
        1_000.0;
      Minutes_Diff: Integer;
      Rests, Cabin_Index, Rest_Time, Tired, Cabin_Bonus, Temp_Time: Natural :=
        0;
      Damage: Damage_Factor := 0.0;
      Result: Travel_Array := (1 => 0, 2 => 0);
   begin
      if Speed = 0.0 then
         return Result;
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
      Minutes_Diff := Minutes_Diff * Distance;
      Count_Rest_Time_Loop :
      for I in Player_Ship.Crew.Iterate loop
         if Player_Ship.Crew(I).Order not in PILOT | ENGINEER then
            goto End_Of_Count_Loop;
         end if;
         --## rule off SIMPLIFIABLE_EXPRESSIONS
         Tired := (Minutes_Diff / 15) + Player_Ship.Crew(I).Tired;
         if
           (Tired /
            (80 + Player_Ship.Crew(I).Attributes(Condition_Index).Level)) >
           Rests then
            Rests :=
              (Tired /
               (80 + Player_Ship.Crew(I).Attributes(Condition_Index).Level));
         end if;
         --## rule on SIMPLIFIABLE_EXPRESSIONS
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
                   Player_Ship.Crew(I).Attributes(Condition_Index).Level) /
                  Cabin_Bonus) *
                 15;
               if Temp_Time = 0 then
                  Temp_Time := 15;
               end if;
            else
               Temp_Time :=
                 (80 + Player_Ship.Crew(I).Attributes(Condition_Index).Level) *
                 15;
            end if;
            Temp_Time := Temp_Time + 15;
            if Temp_Time > Rest_Time then
               Rest_Time := Temp_Time;
            end if;
         end if;
         <<End_Of_Count_Loop>>
      end loop Count_Rest_Time_Loop;
      --## rule off SIMPLIFIABLE_EXPRESSIONS
      Result(1) := Minutes_Diff + (Rests * Rest_Time);
      Result(2) :=
        (abs (Distance * Count_Fuel_Needed) + (Rests * (Rest_Time / 10)));
      --## rule on SIMPLIFIABLE_EXPRESSIONS
      return Result;
   end Travel_Info;

   procedure Update_Messages is
      use Tcl.Tk.Ada.Widgets.Text;

      Loop_Start: Integer := 0 - Messages_Amount;
      Message: Message_Data; --## rule line off IMPROPER_INITIALIZATION
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

   procedure Show_Screen(New_Screen_Name: String) is
      procedure Nim_Show_Screen(Screen_Name: chars_ptr) with
         Import => True,
         Convention => C,
         External_Name => "showScreen";
   begin
      if Tcl_GetVar(interp => Get_Context, varName => "mappreview") = "1" and
        New_Screen_Name /= "mapframe" then
         Tcl_UnsetVar(interp => Get_Context, varName => "mappreview");
      end if;
      Nim_Show_Screen(Screen_Name => New_String(Str => New_Screen_Name));
   end Show_Screen;

   procedure Show_Inventory_Item_Info
     (Parent: String; Item_Index: Positive; Member_Index: Natural;
      Button_1, Button_2: Button_Settings := Empty_Button_Settings) is
      use Short_String;
      use Tiny_String;

      Proto_Index: Natural;
      Item_Info: Unbounded_String := Null_Unbounded_String;
      Item_Types: constant array(1 .. 6) of Tiny_String.Bounded_String :=
        (1 => Weapon_Type, 2 => Chest_Armor, 3 => Head_Armor, 4 => Arms_Armor,
         5 => Legs_Armor, 6 => Shield_Type);
   begin
      if Member_Index > 0 then
         Proto_Index :=
           Inventory_Container.Element
             (Container => Player_Ship.Crew(Member_Index).Inventory,
              Index => Item_Index)
             .Proto_Index;
         if Inventory_Container.Element
             (Container => Player_Ship.Crew(Member_Index).Inventory,
              Index => Item_Index)
             .Durability <
           Default_Item_Durability then
            Append
              (Source => Item_Info,
               New_Item =>
                 Get_Item_Damage
                   (Item_Durability =>
                      Inventory_Container.Element
                        (Container => Player_Ship.Crew(Member_Index).Inventory,
                         Index => Item_Index)
                        .Durability) &
                 LF);
         end if;
      else
         Proto_Index :=
           Inventory_Container.Element
             (Container => Player_Ship.Cargo, Index => Item_Index)
             .Proto_Index;
         if Inventory_Container.Element
             (Container => Player_Ship.Cargo, Index => Item_Index)
             .Durability <
           Default_Item_Durability then
            Append
              (Source => Item_Info,
               New_Item =>
                 Get_Item_Damage
                   (Item_Durability =>
                      Inventory_Container.Element
                        (Container => Player_Ship.Cargo, Index => Item_Index)
                        .Durability) &
                 LF);
         end if;
      end if;
      Append
        (Source => Item_Info,
         New_Item =>
           "Weight:" &
           Positive'Image(Get_Proto_Item(Index => Proto_Index).Weight) &
           " kg");
      if Get_Proto_Item(Index => Proto_Index).I_Type = Weapon_Type then
         Append
           (Source => Item_Info,
            New_Item =>
              LF & "Skill: " &
              To_String
                (Source =>
                   SkillsData_Container.Element
                     (Container => Skills_List,
                      Index =>
                        Skills_Amount_Range
                          (Get_Proto_Item(Index => Proto_Index).Value(3)))
                     .Name) &
              "/" &
              To_String
                (Source =>
                   AttributesData_Container.Element
                     (Container => Attributes_List,
                      Index =>
                        SkillsData_Container.Element
                          (Container => Skills_List,
                           Index =>
                             Skills_Amount_Range
                               (Get_Proto_Item(Index => Proto_Index).Value(3)))
                          .Attribute)
                     .Name));
         if Get_Proto_Item(Index => Proto_Index).Value(4) = 1 then
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
              (case Get_Proto_Item(Index => Proto_Index).Value(5) is
                 when 1 => "cutting", when 2 => "impaling", when 3 => "blunt",
                 when others => ""));
      end if;
      Show_More_Item_Info_Loop :
      for ItemType of Item_Types loop
         if Get_Proto_Item(Index => Proto_Index).I_Type = ItemType then
            Append
              (Source => Item_Info,
               New_Item =>
                 LF & "Damage chance: " &
                 Get_Item_Chance_To_Damage
                   (Item_Data =>
                      Get_Proto_Item(Index => Proto_Index).Value(1)) &
                 LF & "Strength:" &
                 Integer'Image(Get_Proto_Item(Index => Proto_Index).Value(2)));
            exit Show_More_Item_Info_Loop;
         end if;
      end loop Show_More_Item_Info_Loop;
      if Is_Tool(Item_Type => Get_Proto_Item(Index => Proto_Index).I_Type) then
         Append
           (Source => Item_Info,
            New_Item =>
              LF & "Damage chance: " &
              Get_Item_Chance_To_Damage
                (Item_Data => Get_Proto_Item(Index => Proto_Index).Value(1)));
      end if;
      if Length(Source => Get_Proto_Item(Index => Proto_Index).I_Type) > 4
        and then
        (Slice
           (Source => Get_Proto_Item(Index => Proto_Index).I_Type, Low => 1,
            High => 4) =
         "Ammo" or
         Get_Proto_Item(Index => Proto_Index).I_Type =
           To_Bounded_String(Source => "Harpoon")) then
         Append
           (Source => Item_Info,
            New_Item =>
              LF & "Strength:" &
              Integer'Image(Get_Proto_Item(Index => Proto_Index).Value(1)));
      end if;
      if Get_Proto_Item(Index => Proto_Index).Description /=
        Short_String.Null_Bounded_String then
         Append
           (Source => Item_Info,
            New_Item =>
              LF & LF &
              Short_String.To_String
                (Source => Get_Proto_Item(Index => Proto_Index).Description));
      end if;
      if Parent = "." then
         Show_Info
           (Text => To_String(Source => Item_Info),
            Title =>
              (if Member_Index > 0 then
                 Get_Item_Name
                   (Item =>
                      Inventory_Container.Element
                        (Container => Player_Ship.Crew(Member_Index).Inventory,
                         Index => Item_Index),
                    Damage_Info => False, To_Lower => False)
               else Get_Item_Name
                   (Item =>
                      Inventory_Container.Element
                        (Container => Player_Ship.Cargo, Index => Item_Index),
                    Damage_Info => False, To_Lower => False)),
            Button_1 => Button_1, Button_2 => Button_2);
      else
         Show_Info
           (Text => To_String(Source => Item_Info), Parent_Name => Parent,
            Title =>
              (if Member_Index > 0 then
                 Get_Item_Name
                   (Item =>
                      Inventory_Container.Element
                        (Container => Player_Ship.Crew(Member_Index).Inventory,
                         Index => Item_Index),
                    Damage_Info => False, To_Lower => False)
               else Get_Item_Name
                   (Item =>
                      Inventory_Container.Element
                        (Container => Player_Ship.Cargo, Index => Item_Index),
                    Damage_Info => False, To_Lower => False)),
            Button_1 => Button_1, Button_2 => Button_2);
      end if;
   end Show_Inventory_Item_Info;

   procedure Delete_Widgets
     (Start_Index, End_Index: Integer; Frame: Tk_Widget'Class) is
      procedure Delete_Widgts
        (S_Index, E_Index: Integer; Parent: chars_ptr) with
         Import => True,
         Convention => C,
         External_Name => "deleteWidgets";
   begin
      Delete_Widgts
        (S_Index => Start_Index, E_Index => End_Index,
         Parent => New_String(Str => Widget_Image(Win => Frame)));
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

   procedure Set_Fonts(New_Size: Positive; Font_Type: Font_Types) is
      Help_Fonts: constant array(1 .. 4) of Unbounded_String :=
        (1 => To_Unbounded_String(Source => "HelpFont"),
         2 => To_Unbounded_String(Source => "BoldHelpFont"),
         3 => To_Unbounded_String(Source => "UnderlineHelpFont"),
         4 => To_Unbounded_String(Source => "ItalicHelpFont"));
      Interface_Fonts: constant array(1 .. 4) of Unbounded_String :=
        (1 => To_Unbounded_String(Source => "InterfaceFont"),
         2 => To_Unbounded_String(Source => "OverstrikedFont"),
         3 => To_Unbounded_String(Source => "UnderlineFont"),
         4 => To_Unbounded_String(Source => "BoldFont"));
   begin
      case Font_Type is
         when MAPFONT =>
            Set_Integer_Setting(Name => "mapFontSize", Value => New_Size);
            Font.Configure
              (FontName => "MapFont",
               Options =>
                 "-size" &
                 Positive'Image(Get_Integer_Setting(Name => "mapFontSize")));
         when Help_Font_Type =>
            Set_Integer_Setting(Name => "helpFontSize", Value => New_Size);
            Set_Fonts_Loop :
            for FontName of Help_Fonts loop
               Font.Configure
                 (FontName => To_String(Source => FontName),
                  Options =>
                    "-size" &
                    Positive'Image
                      (Get_Integer_Setting(Name => "helpFontSize")));
            end loop Set_Fonts_Loop;
         when INTERFACEFONT =>
            Game_Settings.Interface_Font_Size := New_Size;
            Set_Interface_Fonts_Loop :
            for FontName of Interface_Fonts loop
               Font.Configure
                 (FontName => To_String(Source => FontName),
                  Options =>
                    "-size" &
                    Positive'Image(Game_Settings.Interface_Font_Size));
            end loop Set_Interface_Fonts_Loop;
      end case;
   end Set_Fonts;

end Utils.UI;
