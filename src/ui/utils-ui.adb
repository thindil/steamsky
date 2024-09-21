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

with Ada.Directories;
with Ada.Strings;
with Ada.Strings.Unbounded;
with Interfaces.C; use Interfaces.C;
with CArgv; use CArgv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkEntry;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
-- with Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkPanedWindow;
-- with Tcl.Tk.Ada.Widgets.TtkScrollbar;
with Bases;
with Combat.UI;
with CoreUI; use CoreUI;
with Crew;
with Dialogs; use Dialogs;
with Events;
with Game;
with Items;
with Maps;
with Maps.UI;
with MainMenu;
with Messages; use Messages;
with Missions;
with Ships; use Ships;
with Ships.Cargo;
with Ships.Crew;
with Ships.UI.Crew;
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
      Import => True,
      Convention => C,
      External_Name => "setTextVariableCommand";
      -- ****

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
      use Ada.Strings.Unbounded;
      use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
      use Bases;
      use Crew;
      use Game;
      use Maps;
      use Maps.UI;
      use MainMenu;
      use Ships.Crew;
      use Tiny_String;

      Result: constant String := CArgv.Arg(Argv => Argv, N => 1);
   begin
      if Result = "deletesave" then
         Delete_File
           (Name => Tcl_GetVar(interp => Interp, varName => "deletesave"));
         Tcl_UnsetVar(interp => Interp, varName => "deletesave");
         Tcl_Eval(interp => Interp, strng => "ShowLoadGame");
      elsif Result = "sethomebase" then
         Set_Home_Base_Block :
         declare
            use Items;
            use Ships.Cargo;

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
            Set_Center_Point(X => Player_Ship.Sky_X, Y => Player_Ship.Sky_Y);
            if Starts_Combat then
               Show_Combat_Ui;
            else
               Show_Sky_Map;
            end if;
         end Check_For_Combat_Block;
      elsif Result = "quit" then
         Set_Integer_Setting
           (Name => "messagesPosition",
            Value =>
              Get_Integer_Setting(Name => "windowHeight") -
              Natural'Value(SashPos(Paned => Main_Paned, Index => "0")));
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
            use Tcl.Tk.Ada.Widgets.TtkButton;
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
         Set_Integer_Setting
           (Name => "messagesPosition",
            Value =>
              Get_Integer_Setting(Name => "windowHeight") -
              Natural'Value(SashPos(Paned => Main_Paned, Index => "0")));
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
            use Ships.UI.Crew;
            Base_Index: constant Positive :=
              Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
            Member_Index: constant Positive :=
              Positive'Value(CArgv.Arg(Argv => Argv, N => 1));
            procedure Update_Messages is
               procedure Update_Ada_Messages with
                  Import => True,
                  Convention => C,
                  External_Name => "updateAdaMessages";
            begin
               Update_Ada_Messages;
            end Update_Messages;
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
      Import => True,
      Convention => C,
      External_Name => "setScrollbarBindingsCommand";
      -- ****

   -- ****io* UUI/UUI.Set_Destination_Command
   -- FUNCTION
   -- Set the selected map point as the player's ship destination
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetDestination X Y
   -- X is the x coordinate of point to set, Y is the y coordinate of point
   -- to set
   -- SOURCE
   function Set_Destination_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Destination_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc);
   begin
      Set_Ada_Ship(Ship => Player_Ship);
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
      Get_Ada_Ship;
      Add_Message
        (Message => "You set the travel destination for your ship.",
         M_Type => ORDERMESSAGE);
      Tcl_Eval(interp => Interp, strng => "InvokeButton " & Close_Button);
      Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Close_Button);
      return TCL_OK;
   end Set_Destination_Command;

   function Resize_Canvas_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "resizeCanvasCommand";

   function Check_Amount_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "checkAmountCommand";

   function Validate_Amount_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "validateAmountCommand";

   -- ****o* UUI/UUI.Show_On_Map_Command
   -- FUNCTION
   -- Show the selected point on map
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowOnMap X Y
   -- X is the x coordinate of point to show, Y is the y coordinate of point
   -- to show
   -- SOURCE
   function Show_On_Map_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "showOnMapCommand";
      -- ****

   procedure Add_Commands is
      procedure Add_Ada_Commands with
         Import => True,
         Convention => C,
         External_Name => "addAdaUtilsCommands";
   begin
      Add_Ada_Commands;
      Add_Command
        (Name => "SetTextVariable",
         Ada_Command => Set_Text_Variable_Command'Access);
      Add_Command
        (Name => "ResizeCanvas", Ada_Command => Resize_Canvas_Command'Access);
      Add_Command
        (Name => "CheckAmount", Ada_Command => Check_Amount_Command'Access);
      Add_Command
        (Name => "ValidateAmount",
         Ada_Command => Validate_Amount_Command'Access);
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

   procedure Set_Fonts(New_Size: Positive; Font_Type: Font_Types) is
      procedure Set_Ada_Fonts(N_Size, F_Type: Integer) with
         Import => True,
         Convention => C,
         External_Name => "setAdaFonts";
   begin
      Set_Ada_Fonts(N_Size => New_Size, F_Type => Font_Types'Pos(Font_Type));
   end Set_Fonts;

end Utils.UI;
