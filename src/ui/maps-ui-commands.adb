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

with Ada.Containers.Vectors;
with Interfaces.C; use Interfaces.C;
with CArgv; use CArgv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada.Busy;
with Tcl.Tk.Ada.Event;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Text;
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkPanedWindow;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Tcl.Tk.Ada.Wm;
with Config; use Config;
with CoreUI; use CoreUI;
with Dialogs; use Dialogs;
with Ships;
with Ships.Movement; use Ships.Movement;
with Statistics.UI;
with Utils.UI;

package body Maps.UI.Commands is

   -- ****o* MapCommands/MapCommands.Quit_Game_Command
   -- FUNCTION
   -- Ask player if he/she wants to quit from the game and if yes, save it and
   -- show main menu
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- QuitGame
   -- SOURCE
   function Quit_Game_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Quit_Game_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc, Argv);
   begin
      Show_Question
        (Question => "Are you sure want to quit?", Result => "quit");
      return TCL_OK;
   end Quit_Game_Command;

   -- ****o* MapCommands/MapCommands.Resign_Game_Command
   -- FUNCTION
   -- Resing from the game - if player resigned, kill he/she character and
   -- follow as for death of the player's character
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ResignGame
   -- SOURCE
   function Resign_Game_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Resign_Game_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc, Argv);
   begin
      Show_Question
        (Question => "Are you sure want to resign from game?",
         Result => "resign");
      return TCL_OK;
   end Resign_Game_Command;

   -- ****o* MapCommands/MapCommands.Show_Stats_Command
   -- FUNCTION
   -- Show the player's game statistics
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowStats
   -- SOURCE
   function Show_Stats_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Stats_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc, Argv);
      use Statistics.UI;

   begin
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Close_Button, Options => "-row 0 -column 1");
      Show_Statistics;
      return TCL_OK;
   end Show_Stats_Command;

   -- ****o* MapCommands/MapCommands.Show_Sky_Map_Command
   -- FUNCTION
   -- Show sky map
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowSkyMap ?previouscommand?
   -- Previouscommand is command to show previous screen. Some screens require
   -- to do special actions when closing them
   -- SOURCE
   function Show_Sky_Map_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Sky_Map_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data);
   begin
      if Argc = 1 then
         Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Close_Button);
         Show_Sky_Map(Clear => True);
      else
         Tcl_Eval(interp => Interp, strng => CArgv.Arg(Argv => Argv, N => 1));
      end if;
      Focus(Widgt => Get_Main_Window(Interp => Interp));
      return TCL_OK;
   end Show_Sky_Map_Command;

   -- ****o* MapCommands/MapCommands.Move_Mouse_Command
   -- FUNCTION
   -- Move mouse cursor with keyboard
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- MoveCursor direction
   -- Direction is the direction in which the mouse cursor should be moves or
   -- click if emulate clicking with the left or right button
   -- SOURCE
   function Move_Mouse_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Move_Mouse_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc);
      use Tcl.Tk.Ada.Event;
      use Tcl.Tk.Ada.Widgets.Text;

      Map_View: constant Tk_Text :=
        Get_Widget(pathName => Main_Paned & ".mapframe.map", Interp => Interp);
   begin
      if Focus /= Widget_Image(Win => Map_View) then
         Focus(Widgt => Map_View, Option => "-force");
         return TCL_OK;
      end if;
      if CArgv.Arg(Argv => Argv, N => 1) = "click" then
         Generate
           (Window => Map_View,
            EventName =>
              "<Button-" &
              (if Get_Boolean_Setting(Name => "rightButton") then "3"
               else "1") &
              ">",
            Options =>
              "-x " & CArgv.Arg(Argv => Argv, N => 2) & " -y " &
              CArgv.Arg(Argv => Argv, N => 3));
      elsif CArgv.Arg(Argv => Argv, N => 1) = "nw" then
         Generate
           (Window => Map_View, EventName => "<Motion>",
            Options =>
              "-warp 1 -x [expr " & CArgv.Arg(Argv => Argv, N => 2) &
              "-5] -y [expr " & CArgv.Arg(Argv => Argv, N => 3) & "-5]");
      elsif CArgv.Arg(Argv => Argv, N => 1) = "n" then
         Generate
           (Window => Map_View, EventName => "<Motion>",
            Options =>
              "-warp 1 -x " & CArgv.Arg(Argv => Argv, N => 2) & " -y [expr " &
              CArgv.Arg(Argv => Argv, N => 3) & "-5]");
      elsif CArgv.Arg(Argv => Argv, N => 1) = "ne" then
         Generate
           (Window => Map_View, EventName => "<Motion>",
            Options =>
              "-warp 1 -x [expr " & CArgv.Arg(Argv => Argv, N => 2) &
              "+5] -y [expr " & CArgv.Arg(Argv => Argv, N => 3) & "-5]");
      elsif CArgv.Arg(Argv => Argv, N => 1) = "w" then
         Generate
           (Window => Map_View, EventName => "<Motion>",
            Options =>
              "-warp 1 -x [expr " & CArgv.Arg(Argv => Argv, N => 2) &
              "-5] -y " & CArgv.Arg(Argv => Argv, N => 3));
      elsif CArgv.Arg(Argv => Argv, N => 1) = "e" then
         Generate
           (Window => Map_View, EventName => "<Motion>",
            Options =>
              "-warp 1 -x [expr " & CArgv.Arg(Argv => Argv, N => 2) &
              "+5] -y " & CArgv.Arg(Argv => Argv, N => 3));
      elsif CArgv.Arg(Argv => Argv, N => 1) = "sw" then
         Generate
           (Window => Map_View, EventName => "<Motion>",
            Options =>
              "-warp 1 -x [expr " & CArgv.Arg(Argv => Argv, N => 2) &
              "-5] -y [expr " & CArgv.Arg(Argv => Argv, N => 3) & "+5]");
      elsif CArgv.Arg(Argv => Argv, N => 1) = "s" then
         Generate
           (Window => Map_View, EventName => "<Motion>",
            Options =>
              "-warp 1 -x " & CArgv.Arg(Argv => Argv, N => 2) & " -y [expr " &
              CArgv.Arg(Argv => Argv, N => 3) & "+5]");
      elsif CArgv.Arg(Argv => Argv, N => 1) = "se" then
         Generate
           (Window => Map_View, EventName => "<Motion>",
            Options =>
              "-warp 1 -x [expr " & CArgv.Arg(Argv => Argv, N => 2) &
              "+5] -y [expr " & CArgv.Arg(Argv => Argv, N => 3) & "+5]");
      end if;
      return TCL_OK;
   end Move_Mouse_Command;

   -- ****o* MapCommands/MapCommands.Toggle_Full_Screen_Command
   -- FUNCTION
   -- Toggle the game full screen mode
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ToggleFullScreen
   -- SOURCE
   function Toggle_Full_Screen_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Toggle_Full_Screen_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc, Argv);
      use Tcl.Tk.Ada.Wm;

   begin
      Tcl_Eval(interp => Interp, strng => "wm attributes . -fullscreen");
      if Tcl_GetResult(interp => Interp) = "0" then
         Wm_Set
           (Widgt => Get_Main_Window(Interp => Interp), Action => "attributes",
            Options => "-fullscreen 1");
         Set_Boolean_Setting(Name => "fullScreen", Value => True);
      else
         Wm_Set
           (Widgt => Get_Main_Window(Interp => Interp), Action => "attributes",
            Options => "-fullscreen 0");
         Set_Boolean_Setting(Name => "fullScreen", Value => False);
      end if;
      return TCL_OK;
   end Toggle_Full_Screen_Command;

   -- ****o* MapCommands/MapCommands.Resize_Last_Messages_Command
   -- FUNCTION
   -- Resize the last messages window
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ResizeLastMessages
   -- SOURCE
   function Resize_Last_Messages_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Resize_Last_Messages_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc, Argv);
      use Tcl.Tk.Ada.Widgets.TtkPanedWindow;

      Paned_Position: Positive;
      Sash_Position: constant Natural :=
        Natural'Value(SashPos(Paned => Main_Paned, Index => "0"));
      procedure Set_Ada_Messages_Position(New_Value: Integer) with
         Import => True,
         Convention => C,
         External_Name => "setAdaMessagesPosition";
   begin
      Set_Integer_Setting
        (Name => "windowWidth",
         Value =>
           Positive'Value
             (Winfo_Get
                (Widgt => Get_Main_Window(Interp => Interp),
                 Info => "width")));
      Set_Integer_Setting
        (Name => "windowHeight",
         Value =>
           Positive'Value
             (Winfo_Get
                (Widgt => Get_Main_Window(Interp => Interp),
                 Info => "height")));
      Paned_Position :=
        (if
           Get_Integer_Setting(Name => "windowHeight") -
           Get_Integer_Setting(Name => "messagesPosition") <
           0
         then Get_Integer_Setting(Name => "windowHeight")
         else Get_Integer_Setting(Name => "windowHeight") -
           Get_Integer_Setting(Name => "messagesPosition"));
      if Sash_Position > 0 and then Sash_Position /= Paned_Position then
         if Get_Integer_Setting(Name => "windowHeight") - Sash_Position >
           -1 then
            Set_Integer_Setting
              (Name => "messagesPosition",
               Value =>
                 Get_Integer_Setting(Name => "windowHeight") - Sash_Position);
            Set_Ada_Messages_Position
              (New_Value => Get_Integer_Setting(Name => "messagesPosition"));
         end if;
         Paned_Position := Sash_Position;
      end if;
      return TCL_OK;
   end Resize_Last_Messages_Command;

   -- ****o* MapCommands/MapCommands.Show_Game_Menu_Command
   -- FUNCTION
   -- Show the main menu of the game
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowGameMenu
   -- SOURCE
   function Show_Game_Menu_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Game_Menu_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc, Argv);
      use Ada.Containers;
      use Tcl.Tk.Ada.Widgets.TtkButton;

      Row: Positive := 1;
      State: constant String :=
        Tcl_GetVar(interp => Interp, varName => "gamestate");
      --## rule off TYPE_INITIAL_VALUES
      type Menu_Shortcut is record
         Button_Name: Unbounded_String;
         Shortcut: Unbounded_String;
      end record;
      --## rule on TYPE_INITIAL_VALUES
      package Shortcuts_Container is new Vectors
        (Index_Type => Positive, Element_Type => Menu_Shortcut);
      --## rule off IMPROPER_INITIALIZATION
      Shortcuts: Shortcuts_Container.Vector;
      --## rule on IMPROPER_INITIALIZATION
      Game_Menu: Ttk_Frame := Get_Widget(pathName => ".gameframe.gamemenu");
      procedure Add_Button
        (Name, Label, Command: String; Shortcut: String;
         Last: Boolean := False) is
         Button: constant Ttk_Button :=
           Create
             (pathName => Game_Menu & Name,
              options =>
                "-text {" & Label & " [" & Shortcut &
                "]} -command {CloseDialog " & Game_Menu & ";" & Command & "}");
      begin
         if Last then
            Bind
              (Widgt => Button, Sequence => "<Tab>",
               Script =>
                 "{focus " &
                 To_String(Source => Shortcuts.First_Element.Button_Name) &
                 ";break}");
            Tcl.Tk.Ada.Grid.Grid
              (Slave => Button, Options => "-sticky we -padx 5 -pady {0 3}");
            Focus(Widgt => Button);
         else
            Tcl.Tk.Ada.Grid.Grid
              (Slave => Button, Options => "-sticky we -padx 5");
         end if;
         Shortcuts.Append
           (New_Item =>
              (Button_Name => To_Unbounded_String(Source => Game_Menu & Name),
               Shortcut => To_Unbounded_String(Source => Shortcut)));
         Row := Row + 1;
      end Add_Button;
   begin
      if Winfo_Get(Widgt => Game_Menu, Info => "exists") = "1" then
         Tcl_Eval(interp => Interp, strng => "CloseDialog " & Game_Menu);
         return TCL_OK;
      end if;
      Game_Menu :=
        Create_Dialog(Name => ".gameframe.gamemenu", Title => "Game menu");
      Add_Button
        (Name => ".shipinfo", Label => "Ship information",
         Command => "ShowShipInfo",
         Shortcut => Get_Menu_Accelerator(Index => 1));
      if State not in "combat" | "dead" then
         Add_Button
           (Name => ".shiporders", Label => "Ship orders",
            Command => "ShowOrders",
            Shortcut => Get_Menu_Accelerator(Index => 2));
      end if;
      if State /= "dead" then
         Add_Button
           (Name => ".crafting", Label => "Crafting",
            Command => "ShowCrafting",
            Shortcut => Get_Menu_Accelerator(Index => 3));
      end if;
      Add_Button
        (Name => ".messages", Label => "Last messages",
         Command => "ShowLastMessages",
         Shortcut => Get_Menu_Accelerator(Index => 4));
      Add_Button
        (Name => ".knowledge", Label => "Knowledge lists",
         Command => "ShowKnowledge",
         Shortcut => Get_Menu_Accelerator(Index => 5));
      if State not in "combat" | "dead" then
         Add_Button
           (Name => ".wait", Label => "Wait orders", Command => "ShowWait",
            Shortcut => Get_Menu_Accelerator(Index => 6));
      end if;
      Add_Button
        (Name => ".stats", Label => "Game statistics", Command => "ShowStats",
         Shortcut => Get_Menu_Accelerator(Index => 7));
      if State /= "dead" then
         Add_Button
           (Name => ".help", Label => "Help", Command => "ShowHelp " & State,
            Shortcut => Get_Menu_Accelerator(Index => 8));
         Add_Button
           (Name => ".options", Label => "Game options",
            Command => "ShowOptions",
            Shortcut => Get_Menu_Accelerator(Index => 9));
         Add_Button
           (Name => ".quit", Label => "Quit from game", Command => "QuitGame",
            Shortcut => Get_Menu_Accelerator(Index => 10));
         Add_Button
           (Name => ".resign", Label => "Resign from game",
            Command => "ResignGame",
            Shortcut => Get_Menu_Accelerator(Index => 11));
      end if;
      Add_Button
        (Name => ".close", Label => "Close",
         Command => "CloseDialog " & Game_Menu, Shortcut => "Escape",
         Last => True);
      Add_Bindings_Block :
      declare
         --## rule off IMPROPER_INITIALIZATION
         Menu_Button: Ttk_Button;
         --## rule on IMPROPER_INITIALIZATION
      begin
         Buttons_Loop :
         for Button of Shortcuts loop
            Menu_Button :=
              Get_Widget(pathName => To_String(Source => Button.Button_Name));
            Add_Bindings_Loop :
            for Shortcut of Shortcuts loop
               Bind
                 (Widgt => Menu_Button,
                  Sequence =>
                    "<KeyPress-" & To_String(Source => Shortcut.Shortcut) &
                    ">",
                  Script =>
                    "{" & To_String(Source => Shortcut.Button_Name) &
                    " invoke;break}");
            end loop Add_Bindings_Loop;
            Bind
              (Widgt => Menu_Button,
               Sequence =>
                 "<KeyPress-" & Get_Map_Accelerator(Index => 1) & ">",
               Script => "{ShowGameMenu;break}");
         end loop Buttons_Loop;
      end Add_Bindings_Block;
      Show_Dialog(Dialog => Game_Menu, Relative_X => 0.4, Relative_Y => 0.1);
      return TCL_OK;
   end Show_Game_Menu_Command;

   -- ****o* MapCommands/MapCommands.Invoke_Menu_Command
   -- FUNCTION
   -- Invoke the selected game menu option with the selected keyboard shortcut
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- InvokeMenu shortcut
   -- Shortcut, the keyboard shortcut which was pressed
   -- SOURCE
   function Invoke_Menu_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Invoke_Menu_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc);
      Focused_Widget: constant Ttk_Frame :=
        Get_Widget(pathName => Focus(Interp => Interp), Interp => Interp);
      Menu_Commands: constant array(1 .. 11) of Unbounded_String :=
        (1 => To_Unbounded_String(Source => "ShowShipInfo"),
         2 => To_Unbounded_String(Source => "ShowOrders"),
         3 => To_Unbounded_String(Source => "ShowCrafting"),
         4 => To_Unbounded_String(Source => "ShowLastMessages"),
         5 => To_Unbounded_String(Source => "ShowKnowledge"),
         6 => To_Unbounded_String(Source => "ShowWait"),
         7 => To_Unbounded_String(Source => "ShowStats"),
         8 => To_Unbounded_String(Source => "ShowHelp"),
         9 => To_Unbounded_String(Source => "ShowOptions"),
         10 => To_Unbounded_String(Source => "QuitGame"),
         11 => To_Unbounded_String(Source => "ResignGame"));
   begin
      if Winfo_Get(Widgt => Focused_Widget, Info => "class") = "TEntry" or
        Tcl.Tk.Ada.Busy.Status(Window => Game_Header) = "1" then
         return TCL_OK;
      end if;
      Invoke_Button_Loop :
      for I in 1 .. 11 loop
         if Get_Menu_Accelerator(Index => I) =
           CArgv.Arg(Argv => Argv, N => 1) then
            Tcl_Eval
              (interp => Interp,
               strng => To_String(Source => Menu_Commands(I)));
            return TCL_OK;
         end if;
      end loop Invoke_Button_Loop;
      return TCL_OK;
   end Invoke_Menu_Command;

   -- ****o* MapCommands/MapCommands.Set_Ship_Speed_Command
   -- FUNCTION
   -- Set the new speed for the player's ship
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetShipSpeed speed
   -- Speed is the new speed order for the player's ship.
   -- SOURCE
   function Set_Ship_Speed_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Ship_Speed_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc);
      use Ships;

      Message: constant String :=
        Change_Ship_Speed
          (Speed_Value =>
             Ship_Speed'Val
               (Natural'Value(CArgv.Arg(Argv => Argv, N => 1)) + 1));
   begin
      if Message'Length > 0 then
         Show_Message(Text => Message, Title => "Changing the ship's speed.");
      end if;
      return TCL_OK;
   end Set_Ship_Speed_Command;

   procedure Add_Commands is
      use Utils.UI;
      procedure Add_Ada_Commands with
         Import => True,
         Convention => C,
         External_Name => "addAdaMapsCommands";
   begin
      Add_Ada_Commands;
--      Add_Command(Name => "MoveMap", Ada_Command => Move_Map_Command'Access);
--      Add_Command(Name => "MoveShip", Ada_Command => Move_Ship_Command'Access);
      Add_Command(Name => "QuitGame", Ada_Command => Quit_Game_Command'Access);
      Add_Command
        (Name => "ResignGame", Ada_Command => Resign_Game_Command'Access);
      Add_Command
        (Name => "ShowStats", Ada_Command => Show_Stats_Command'Access);
      Add_Command
        (Name => "ShowSkyMap", Ada_Command => Show_Sky_Map_Command'Access);
      Add_Command
        (Name => "MoveCursor", Ada_Command => Move_Mouse_Command'Access);
      Add_Command
        (Name => "ToggleFullScreen",
         Ada_Command => Toggle_Full_Screen_Command'Access);
      Add_Command
        (Name => "ResizeLastMessages",
         Ada_Command => Resize_Last_Messages_Command'Access);
      Add_Command
        (Name => "ShowGameMenu", Ada_Command => Show_Game_Menu_Command'Access);
      Add_Command
        (Name => "InvokeMenu", Ada_Command => Invoke_Menu_Command'Access);
      Add_Command
        (Name => "SetShipSpeed", Ada_Command => Set_Ship_Speed_Command'Access);
   end Add_Commands;

end Maps.UI.Commands;
