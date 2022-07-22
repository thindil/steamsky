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

with Ada.Directories; use Ada.Directories;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with CArgv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.TtkStyle; use Tcl.Tk.Ada.TtkStyle;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Canvas; use Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.Text; use Tcl.Tk.Ada.Widgets.Text;
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with Tcl.Tk.Ada.Widgets.TtkEntry; use Tcl.Tk.Ada.Widgets.TtkEntry;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Tcl.Tk.Ada.Wm; use Tcl.Tk.Ada.Wm;
with Tcl.Tklib.Ada.Tooltip; use Tcl.Tklib.Ada.Tooltip;
with Config; use Config;
with CoreUI; use CoreUI;
with Combat.UI; use Combat.UI;
with Game; use Game;
with Maps.UI; use Maps.UI;
with Ships; use Ships;
with Themes; use Themes;
with Utils.UI; use Utils.UI;

package body GameOptions is

   -- ****it* GameOptions/GameOptions.Accel_Data
   -- FUNCTION
   -- Data for showing keyboard shortcuts
   -- PARAMETERS
   -- Shortcut    - Keyboard shortcut
   -- Entry_Name  - Name of the text entry which will be showing this shortcut
   -- Config_Name - The name of the entry in keyboard configuration file
   -- SOURCE
   type Accel_Data is record
      Shortcut: Unbounded_String;
      Entry_Name: Unbounded_String;
      Config_Name: Unbounded_String;
   end record;
   -- ****

   -- ****iv* GameOptions/GameOptions.Accels
   -- FUNCTION
   -- Array with data to show keyboard shortcuts
   -- SOURCE
   Accels: array(1 .. 53) of Accel_Data :=
     (1 =>
        (Shortcut => Menu_Accelerators(1),
         Entry_Name => To_Unbounded_String(Source => ".menu.shipinfo"),
         Config_Name => To_Unbounded_String(Source => "ShipInfo")),
      2 =>
        (Shortcut => Menu_Accelerators(2),
         Entry_Name => To_Unbounded_String(Source => ".menu.orders"),
         Config_Name => To_Unbounded_String(Source => "Orders")),
      3 =>
        (Shortcut => Menu_Accelerators(3),
         Entry_Name => To_Unbounded_String(Source => ".menu.crafts"),
         Config_Name => To_Unbounded_String(Source => "Crafting")),
      4 =>
        (Shortcut => Menu_Accelerators(4),
         Entry_Name => To_Unbounded_String(Source => ".menu.messages"),
         Config_Name => To_Unbounded_String(Source => "LastMessages")),
      5 =>
        (Shortcut => Menu_Accelerators(5),
         Entry_Name => To_Unbounded_String(Source => ".menu.knowledge"),
         Config_Name => To_Unbounded_String(Source => "Knowledge")),
      6 =>
        (Shortcut => Menu_Accelerators(6),
         Entry_Name => To_Unbounded_String(Source => ".menu.waitorders"),
         Config_Name => To_Unbounded_String(Source => "WaitOrders")),
      7 =>
        (Shortcut => Menu_Accelerators(7),
         Entry_Name => To_Unbounded_String(Source => ".menu.gamestats"),
         Config_Name => To_Unbounded_String(Source => "GameStats")),
      8 =>
        (Shortcut => Menu_Accelerators(8),
         Entry_Name => To_Unbounded_String(Source => ".menu.help"),
         Config_Name => To_Unbounded_String(Source => "Help")),
      9 =>
        (Shortcut => Menu_Accelerators(9),
         Entry_Name => To_Unbounded_String(Source => ".menu.gameoptions"),
         Config_Name => To_Unbounded_String(Source => "GameOptions")),
      10 =>
        (Shortcut => Menu_Accelerators(10),
         Entry_Name => To_Unbounded_String(Source => ".menu.quit"),
         Config_Name => To_Unbounded_String(Source => "Quit")),
      11 =>
        (Shortcut => Menu_Accelerators(11),
         Entry_Name => To_Unbounded_String(Source => ".menu.resign"),
         Config_Name => To_Unbounded_String(Source => "Resign")),
      12 =>
        (Shortcut => Map_Accelerators(1),
         Entry_Name => To_Unbounded_String(Source => ".menu.menu"),
         Config_Name => To_Unbounded_String(Source => "GameMenu")),
      13 =>
        (Shortcut => Map_Accelerators(2),
         Entry_Name => To_Unbounded_String(Source => ".map.mapoptions"),
         Config_Name => To_Unbounded_String(Source => "MapOptions")),
      14 =>
        (Shortcut => Map_Accelerators(3),
         Entry_Name => To_Unbounded_String(Source => ".map.zoomin"),
         Config_Name => To_Unbounded_String(Source => "ZoomInMap")),
      15 =>
        (Shortcut => Map_Accelerators(4),
         Entry_Name => To_Unbounded_String(Source => ".map.zoomout"),
         Config_Name => To_Unbounded_String(Source => "ZoomOutMap")),
      16 =>
        (Shortcut => Map_Accelerators(5),
         Entry_Name => To_Unbounded_String(Source => ".movement.upleft"),
         Config_Name => To_Unbounded_String(Source => "MoveUpLeft")),
      17 =>
        (Shortcut => Map_Accelerators(6),
         Entry_Name => To_Unbounded_String(Source => ".movement.up"),
         Config_Name => To_Unbounded_String(Source => "MoveUp")),
      18 =>
        (Shortcut => Map_Accelerators(7),
         Entry_Name => To_Unbounded_String(Source => ".movement.upright"),
         Config_Name => To_Unbounded_String(Source => "MoveUpRight")),
      19 =>
        (Shortcut => Map_Accelerators(8),
         Entry_Name => To_Unbounded_String(Source => ".movement.left"),
         Config_Name => To_Unbounded_String(Source => "MoveLeft")),
      20 =>
        (Shortcut => Map_Accelerators(10),
         Entry_Name => To_Unbounded_String(Source => ".movement.wait"),
         Config_Name => To_Unbounded_String(Source => "WaitInPlace")),
      21 =>
        (Shortcut => Map_Accelerators(9),
         Entry_Name => To_Unbounded_String(Source => ".movement.right"),
         Config_Name => To_Unbounded_String(Source => "MoveRight")),
      22 =>
        (Shortcut => Map_Accelerators(11),
         Entry_Name => To_Unbounded_String(Source => ".movement.downleft"),
         Config_Name => To_Unbounded_String(Source => "MoveDownRight")),
      23 =>
        (Shortcut => Map_Accelerators(12),
         Entry_Name => To_Unbounded_String(Source => ".movement.down"),
         Config_Name => To_Unbounded_String(Source => "MoveDown")),
      24 =>
        (Shortcut => Map_Accelerators(13),
         Entry_Name => To_Unbounded_String(Source => ".movement.downright"),
         Config_Name => To_Unbounded_String(Source => "MoveDownRight")),
      25 =>
        (Shortcut => Map_Accelerators(14),
         Entry_Name => To_Unbounded_String(Source => ".movement.moveto"),
         Config_Name => To_Unbounded_String(Source => "MoveTo")),
      26 =>
        (Shortcut => Map_Accelerators(15),
         Entry_Name => To_Unbounded_String(Source => ".map.center"),
         Config_Name => To_Unbounded_String(Source => "CenterMap")),
      27 =>
        (Shortcut => Map_Accelerators(16),
         Entry_Name => To_Unbounded_String(Source => ".map.centerhomebase"),
         Config_Name => To_Unbounded_String(Source => "CenterMapOnHomeBase")),
      28 =>
        (Shortcut => Map_Accelerators(17),
         Entry_Name => To_Unbounded_String(Source => ".map.mapupleft"),
         Config_Name => To_Unbounded_String(Source => "MoveMapUpLeft")),
      29 =>
        (Shortcut => Map_Accelerators(18),
         Entry_Name => To_Unbounded_String(Source => ".map.mapup"),
         Config_Name => To_Unbounded_String(Source => "MoveMapUp")),
      30 =>
        (Shortcut => Map_Accelerators(19),
         Entry_Name => To_Unbounded_String(Source => ".map.mapupright"),
         Config_Name => To_Unbounded_String(Source => "MoveMapUpRight")),
      31 =>
        (Shortcut => Map_Accelerators(20),
         Entry_Name => To_Unbounded_String(Source => ".map.mapleft"),
         Config_Name => To_Unbounded_String(Source => "MoveMapLeft")),
      32 =>
        (Shortcut => Map_Accelerators(21),
         Entry_Name => To_Unbounded_String(Source => ".map.mapright"),
         Config_Name => To_Unbounded_String(Source => "MoveMapRight")),
      33 =>
        (Shortcut => Map_Accelerators(22),
         Entry_Name => To_Unbounded_String(Source => ".map.mapdownleft"),
         Config_Name => To_Unbounded_String(Source => "MoveMapDownLeft")),
      34 =>
        (Shortcut => Map_Accelerators(23),
         Entry_Name => To_Unbounded_String(Source => ".map.mapdown"),
         Config_Name => To_Unbounded_String(Source => "MoveMapDown")),
      35 =>
        (Shortcut => Map_Accelerators(24),
         Entry_Name => To_Unbounded_String(Source => ".map.mapdownright"),
         Config_Name => To_Unbounded_String(Source => "MoveMapDownRight")),
      36 =>
        (Shortcut => Map_Accelerators(25),
         Entry_Name => To_Unbounded_String(Source => ".map.cursorupleft"),
         Config_Name => To_Unbounded_String(Source => "MoveCursorUpLeft")),
      37 =>
        (Shortcut => Map_Accelerators(26),
         Entry_Name => To_Unbounded_String(Source => ".map.cursorup"),
         Config_Name => To_Unbounded_String(Source => "MoveCursorUp")),
      38 =>
        (Shortcut => Map_Accelerators(27),
         Entry_Name => To_Unbounded_String(Source => ".map.cursorupright"),
         Config_Name => To_Unbounded_String(Source => "MoveCursorUpRight")),
      39 =>
        (Shortcut => Map_Accelerators(28),
         Entry_Name => To_Unbounded_String(Source => ".map.cursorleft"),
         Config_Name => To_Unbounded_String(Source => "MoveCursorLeft")),
      40 =>
        (Shortcut => Map_Accelerators(29),
         Entry_Name => To_Unbounded_String(Source => ".map.cursorright"),
         Config_Name => To_Unbounded_String(Source => "MoveCursorRight")),
      41 =>
        (Shortcut => Map_Accelerators(30),
         Entry_Name => To_Unbounded_String(Source => ".map.cursordownleft"),
         Config_Name => To_Unbounded_String(Source => "MoveCursorDownLeft")),
      42 =>
        (Shortcut => Map_Accelerators(31),
         Entry_Name => To_Unbounded_String(Source => ".map.cursordown"),
         Config_Name => To_Unbounded_String(Source => "MoveCursorDown")),
      43 =>
        (Shortcut => Map_Accelerators(32),
         Entry_Name => To_Unbounded_String(Source => ".map.cursordownright"),
         Config_Name => To_Unbounded_String(Source => "MoveCursorDownRight")),
      44 =>
        (Shortcut => Map_Accelerators(33),
         Entry_Name => To_Unbounded_String(Source => ".map.clickmouse"),
         Config_Name => To_Unbounded_String(Source => "LeftClickMouse")),
      45 =>
        (Shortcut => Map_Accelerators(34),
         Entry_Name => To_Unbounded_String(Source => ".movement.fullstop"),
         Config_Name => To_Unbounded_String(Source => "FullStop")),
      46 =>
        (Shortcut => Map_Accelerators(35),
         Entry_Name => To_Unbounded_String(Source => ".movement.quarterspeed"),
         Config_Name => To_Unbounded_String(Source => "QuarterSpeed")),
      47 =>
        (Shortcut => Map_Accelerators(36),
         Entry_Name => To_Unbounded_String(Source => ".movement.halfspeed"),
         Config_Name => To_Unbounded_String(Source => "HalfSpeed")),
      48 =>
        (Shortcut => Map_Accelerators(37),
         Entry_Name => To_Unbounded_String(Source => ".movement.fullspeed"),
         Config_Name => To_Unbounded_String(Source => "FullSpeed")),
      49 =>
        (Shortcut => Full_Screen_Accel,
         Entry_Name =>
           To_Unbounded_String(Source => ".interface.fullscreenkey"),
         Config_Name => To_Unbounded_String(Source => "FullScreen")),
      50 =>
        (Shortcut => General_Accelerators(1),
         Entry_Name => To_Unbounded_String(Source => ".ui.resizefirst"),
         Config_Name => To_Unbounded_String(Source => "ResizeFirst")),
      51 =>
        (Shortcut => General_Accelerators(2),
         Entry_Name => To_Unbounded_String(Source => ".ui.resizesecond"),
         Config_Name => To_Unbounded_String(Source => "ResizeSecond")),
      52 =>
        (Shortcut => General_Accelerators(3),
         Entry_Name => To_Unbounded_String(Source => ".ui.resizethird"),
         Config_Name => To_Unbounded_String(Source => "ResizeThird")),
      53 =>
        (Shortcut => General_Accelerators(4),
         Entry_Name => To_Unbounded_String(Source => ".ui.resizefourth"),
         Config_Name => To_Unbounded_String(Source => "ResizeFourth")));
   -- ****

   -- ****o* GameOptions/GameOptions.Show_Options_Tab_Command
   -- FUNCTION
   -- Show the selected options tab
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowOptionsTab
   -- SOURCE
   function Show_Options_Tab_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Options_Tab_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc, Argv);
      Options_Canvas: constant Tk_Canvas :=
        Get_Widget
          (pathName => Main_Paned & ".optionsframe.canvas", Interp => Interp);
      Options_Frame: constant Ttk_Frame :=
        Get_Widget(pathName => Options_Canvas & ".options", Interp => Interp);
      Frame: constant Ttk_Frame :=
        Get_Widget
          (pathName =>
             Options_Frame & "." &
             Tcl_GetVar(interp => Interp, varName => "newtab"));
      Old_Frame: constant Ttk_Frame :=
        Get_Widget
          (pathName =>
             Tcl.Tk.Ada.Grid.Grid_Slaves
               (Master => Options_Frame, Option => "-row 1"));
   begin
      Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Old_Frame);
      Tcl.Tk.Ada.Grid.Grid(Slave => Frame, Options => "-sticky nwes -padx 10");
      Tcl_Eval(interp => Interp, strng => "update");
      configure
        (Widgt => Options_Canvas,
         options =>
           "-scrollregion [list " &
           BBox(CanvasWidget => Options_Canvas, TagOrId => "all") & "]");
      return TCL_OK;
   end Show_Options_Tab_Command;

   -- ****o* GameOptions/GameOptions.Show_Options_Command
   -- FUNCTION
   -- Show the game options to the player
   -- PARAMETERS
   -- Client_Data - Custom data send to the command.
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command.
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowOptions
   -- SOURCE
   function Show_Options_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Options_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      Options_Frame: Ttk_Frame :=
        Get_Widget(pathName => Main_Paned & ".optionsframe", Interp => Interp);
      Options_Canvas: constant Tk_Canvas :=
        Get_Widget(pathName => Options_Frame & ".canvas", Interp => Interp);
      Label: Ttk_Label;
      Combo_Box_Widget: Ttk_ComboBox;
      Spin_Box_Widget: Ttk_SpinBox;
      Key_Entry: Ttk_Entry;
      Local_Themes_List: Unbounded_String;
      type Widget_Data is record
         Name: Unbounded_String;
         Value: Unbounded_String;
      end record;
      Labels_Array: constant array(1 .. 4) of Widget_Data :=
        (1 =>
           (Name => To_Unbounded_String(Source => "data"),
            Value => Data_Directory),
         2 =>
           (Name => To_Unbounded_String(Source => "save"),
            Value => Save_Directory),
         3 =>
           (Name => To_Unbounded_String(Source => "docs"),
            Value => Doc_Directory),
         4 =>
           (Name => To_Unbounded_String(Source => "mods"),
            Value => Mods_Directory));
      Checkbox_Array: constant array(1 .. 11) of Widget_Data :=
        (1 =>
           (Name =>
              To_Unbounded_String
                (Source => Options_Canvas & ".options.general.autorest"),
            Value =>
              To_Unbounded_String
                (Source => (if Game_Settings.Auto_Rest then "1" else "0"))),
         2 =>
           (Name =>
              To_Unbounded_String
                (Source => Options_Canvas & ".options.general.autocenter"),
            Value =>
              To_Unbounded_String
                (Source => (if Game_Settings.Auto_Center then "1" else "0"))),
         3 =>
           (Name =>
              To_Unbounded_String
                (Source => Options_Canvas & ".options.general.autoreturn"),
            Value =>
              To_Unbounded_String
                (Source => (if Game_Settings.Auto_Return then "1" else "0"))),
         4 =>
           (Name =>
              To_Unbounded_String
                (Source => Options_Canvas & ".options.general.autofinish"),
            Value =>
              To_Unbounded_String
                (Source => (if Game_Settings.Auto_Finish then "1" else "0"))),
         5 =>
           (Name =>
              To_Unbounded_String
                (Source =>
                   Options_Canvas & ".options.general.autoaskforbases"),
            Value =>
              To_Unbounded_String
                (Source =>
                   (if Game_Settings.Auto_Ask_For_Bases then "1" else "0"))),
         6 =>
           (Name =>
              To_Unbounded_String
                (Source =>
                   Options_Canvas & ".options.general.autoaskforevents"),
            Value =>
              To_Unbounded_String
                (Source =>
                   (if Game_Settings.Auto_Ask_For_Events then "1" else "0"))),
         7 =>
           (Name =>
              To_Unbounded_String
                (Source => Options_Canvas & ".options.interface.rightbutton"),
            Value =>
              To_Unbounded_String
                (Source => (if Game_Settings.Right_Button then "1" else "0"))),
         8 =>
           (Name =>
              To_Unbounded_String
                (Source => Options_Canvas & ".options.interface.showtooltips"),
            Value =>
              To_Unbounded_String
                (Source =>
                   (if Game_Settings.Show_Tooltips then "1" else "0"))),
         9 =>
           (Name =>
              To_Unbounded_String
                (Source => Options_Canvas & ".options.interface.showmessages"),
            Value =>
              To_Unbounded_String
                (Source =>
                   (if Game_Settings.Show_Last_Messages then "1" else "0"))),
         10 =>
           (Name =>
              To_Unbounded_String
                (Source => Options_Canvas & ".options.interface.fullscreen"),
            Value =>
              To_Unbounded_String
                (Source => (if Game_Settings.Full_Screen then "1" else "0"))),
         11 =>
           (Name =>
              To_Unbounded_String
                (Source => Options_Canvas & ".options.interface.shownumbers"),
            Value =>
              To_Unbounded_String
                (Source =>
                   (if Game_Settings.Show_Numbers then "1" else "0"))));
      Spin_Box_Array: constant array(1 .. 10) of Widget_Data :=
        (1 =>
           (Name =>
              To_Unbounded_String
                (Source => Options_Canvas & ".options.general.fuel"),
            Value =>
              To_Unbounded_String
                (Source => Natural'Image(Game_Settings.Low_Fuel))),
         2 =>
           (Name =>
              To_Unbounded_String
                (Source => Options_Canvas & ".options.general.drinks"),
            Value =>
              To_Unbounded_String
                (Source => Natural'Image(Game_Settings.Low_Drinks))),
         3 =>
           (Name =>
              To_Unbounded_String
                (Source => Options_Canvas & ".options.general.food"),
            Value =>
              To_Unbounded_String
                (Source => Natural'Image(Game_Settings.Low_Food))),
         4 =>
           (Name =>
              To_Unbounded_String
                (Source => Options_Canvas & ".options.general.messageslimit"),
            Value =>
              To_Unbounded_String
                (Source => Natural'Image(Game_Settings.Messages_Limit))),
         5 =>
           (Name =>
              To_Unbounded_String
                (Source => Options_Canvas & ".options.general.savedmessages"),
            Value =>
              To_Unbounded_String
                (Source => Natural'Image(Game_Settings.Saved_Messages))),
         6 =>
           (Name =>
              To_Unbounded_String
                (Source =>
                   Options_Canvas & ".options.interface.closemessages"),
            Value =>
              To_Unbounded_String
                (Source =>
                   Natural'Image(Game_Settings.Auto_Close_Messages_Time))),
         7 =>
           (Name =>
              To_Unbounded_String
                (Source => Options_Canvas & ".options.interface.mapfont"),
            Value =>
              To_Unbounded_String
                (Source => Natural'Image(Game_Settings.Map_Font_Size))),
         8 =>
           (Name =>
              To_Unbounded_String
                (Source =>
                   Options_Canvas & ".options.interface.interfacefont"),
            Value =>
              To_Unbounded_String
                (Source => Natural'Image(Game_Settings.Interface_Font_Size))),
         9 =>
           (Name =>
              To_Unbounded_String
                (Source => Options_Canvas & ".options.interface.helpfont"),
            Value =>
              To_Unbounded_String
                (Source => Natural'Image(Game_Settings.Help_Font_Size))),
         10 =>
           (Name =>
              To_Unbounded_String
                (Source => Options_Canvas & ".options.interface.listslimit"),
            Value =>
              To_Unbounded_String
                (Source => Natural'Image(Game_Settings.Lists_Limit))));
      Combo_Box_Array: constant array(1 .. 4) of Widget_Data :=
        (1 =>
           (Name =>
              To_Unbounded_String
                (Source => Options_Canvas & ".options.general.speed"),
            Value =>
              To_Unbounded_String
                (Source =>
                   Natural'Image
                     (Ship_Speed'Pos(Game_Settings.Undock_Speed) - 1))),
         2 =>
           (Name =>
              To_Unbounded_String
                (Source => Options_Canvas & ".options.general.automovestop"),
            Value =>
              To_Unbounded_String
                (Source =>
                   Natural'Image
                     (Auto_Move_Break'Pos(Game_Settings.Auto_Move_Stop)))),
         3 =>
           (Name =>
              To_Unbounded_String
                (Source => Options_Canvas & ".options.general.messagesorder"),
            Value =>
              To_Unbounded_String
                (Source =>
                   Natural'Image
                     (Messages_Order_Type'Pos(Game_Settings.Messages_Order)))),
         4 =>
           (Name =>
              To_Unbounded_String
                (Source => Options_Canvas & ".options.general.autosave"),
            Value =>
              To_Unbounded_String
                (Source =>
                   Natural'Image
                     (Auto_Save_Type'Pos(Game_Settings.Auto_Save)))));
   begin
      Label.Interp := Interp;
      Combo_Box_Widget.Interp := Interp;
      Tcl_SetVar(interp => Interp, varName => "newtab", newValue => "general");
      if Winfo_Get(Widgt => Options_Canvas, Info => "exists") = "0" then
         Tcl_EvalFile
           (interp => Get_Context,
            fileName =>
              To_String(Source => Data_Directory) & "ui" & Dir_Separator &
              "options.tcl");
         Bind
           (Widgt => Options_Frame, Sequence => "<Configure>",
            Script => "{ResizeCanvas %W.canvas %w %h}");
         Configure_Labels_Loop :
         for Path_Label of Labels_Array loop
            Label.Name :=
              New_String
                (Str =>
                   Widget_Image(Win => Options_Canvas) & ".options.info." &
                   To_String(Source => Path_Label.Name));
            configure
              (Widgt => Label,
               options =>
                 "-text {" &
                 Full_Name(Name => To_String(Source => Path_Label.Value)) &
                 " }");
         end loop Configure_Labels_Loop;
         Load_Themes_Loop :
         for Theme of Themes_List loop
            Append
              (Source => Local_Themes_List,
               New_Item => " {" & Theme.Name & "}");
         end loop Load_Themes_Loop;
         Combo_Box_Widget.Name :=
           New_String
             (Str => Options_Frame & ".canvas.options.interface.theme");
         configure
           (Widgt => Combo_Box_Widget,
            options =>
              "-values [list" & To_String(Source => Local_Themes_List) & "]");
      elsif Winfo_Get(Widgt => Options_Canvas, Info => "ismapped") = "1" then
         Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Close_Button);
         Show_Sky_Map(Clear => True);
         return TCL_OK;
      end if;
      Options_Frame.Name :=
        New_String(Str => Options_Canvas & ".options.general");
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Options_Frame, Options => "-sticky nwes -padx 10");
      Set_Checkboxes_Loop :
      for CheckBox of Checkbox_Array loop
         Tcl_SetVar
           (interp => Interp, varName => To_String(Source => CheckBox.Name),
            newValue => To_String(Source => CheckBox.Value));
      end loop Set_Checkboxes_Loop;
      Set_Spinboxes_Loop :
      for SpinBox of Spin_Box_Array loop
         Spin_Box_Widget :=
           Get_Widget
             (pathName => To_String(Source => SpinBox.Name), Interp => Interp);
         Set
           (SpinBox => Spin_Box_Widget,
            Value => To_String(Source => SpinBox.Value));
      end loop Set_Spinboxes_Loop;
      Set_Comboboxes_Loop :
      for ComboBox of Combo_Box_Array loop
         Combo_Box_Widget :=
           Get_Widget
             (pathName => To_String(Source => ComboBox.Name),
              Interp => Interp);
         Current
           (ComboBox => Combo_Box_Widget,
            NewIndex => To_String(Source => ComboBox.Value));
      end loop Set_Comboboxes_Loop;
      Options_Frame.Name :=
        New_String
          (Str => Widget_Image(Win => Options_Canvas) & ".options.interface");
      Combo_Box_Widget.Name :=
        New_String(Str => Widget_Image(Win => Options_Frame) & ".theme");
      Set
        (ComboBox => Combo_Box_Widget,
         Value =>
           "{" &
           To_String
             (Source =>
                Themes_List(To_String(Source => Game_Settings.Interface_Theme))
                  .Name) &
           "}");
      Key_Entry.Interp := Interp;
      Options_Frame.Name :=
        New_String(Str => Widget_Image(Win => Options_Canvas) & ".options");
      Load_Menu_Accelerators_Loop :
      for I in Menu_Accelerators'Range loop
         Accels(I).Shortcut := Menu_Accelerators(I);
      end loop Load_Menu_Accelerators_Loop;
      Load_Map_Accelerators_Loop :
      for I in Map_Accelerators'Range loop
         Accels(I + Menu_Accelerators'Last).Shortcut := Map_Accelerators(I);
      end loop Load_Map_Accelerators_Loop;
      Accels(Menu_Accelerators'Last + Map_Accelerators'Last + 1).Shortcut :=
        Full_Screen_Accel;
      Load_General_Accelerators_Loop :
      for I in General_Accelerators'Range loop
         Accels(I + Menu_Accelerators'Last + Map_Accelerators'Last + 1)
           .Shortcut :=
           General_Accelerators(I);
      end loop Load_General_Accelerators_Loop;
      Load_Accelerators_Loop :
      for Accel of Accels loop
         Key_Entry.Name :=
           New_String
             (Str =>
                Widget_Image(Win => Options_Frame) &
                To_String(Source => Accel.Entry_Name));
         Delete(TextEntry => Key_Entry, FirstIndex => "0", LastIndex => "end");
         Insert
           (TextEntry => Key_Entry, Index => "0",
            Text => To_String(Source => Accel.Shortcut));
      end loop Load_Accelerators_Loop;
      if cget(Widgt => Close_Button, option => "-command") =
        "ShowCombatUI" then
         configure
           (Widgt => Close_Button,
            options => "-command {CloseOptions combat}");
      else
         configure
           (Widgt => Close_Button, options => "-command {CloseOptions map}");
      end if;
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Close_Button, Options => "-row 0 -column 1");
      configure
        (Widgt => Options_Canvas,
         options =>
           "-height " & cget(Widgt => Main_Paned, option => "-height") &
           " -width " & cget(Widgt => Main_Paned, option => "-width"));
      Tcl_Eval(interp => Get_Context, strng => "update");
      Canvas_Create
        (Parent => Options_Canvas, Child_Type => "window",
         Options =>
           "0 0 -anchor nw -window " & Widget_Image(Win => Options_Frame));
      Tcl_Eval(interp => Get_Context, strng => "update");
      configure
        (Widgt => Options_Canvas,
         options =>
           "-scrollregion [list " &
           BBox(CanvasWidget => Options_Canvas, TagOrId => "all") & "]");
      Show_Screen(New_Screen_Name => "optionsframe");
      return
        Show_Options_Tab_Command
          (Client_Data => Client_Data, Interp => Interp, Argc => Argc,
           Argv => Argv);
   end Show_Options_Command;

   -- ****o* GameOptions/GameOptions.Set_Fonts_Command
   -- FUNCTION
   -- Set the selected font
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetFonts fontfield
   -- Fontfield is the name of the spinbox which value changed.
   -- SOURCE
   function Set_Fonts_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Fonts_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc);
      Frame_Name: constant String :=
        ".gameframe.paned.optionsframe.canvas.options.interface";
      Spin_Box: constant Ttk_SpinBox :=
        Get_Widget
          (pathName => CArgv.Arg(Argv => Argv, N => 1), Interp => Interp);
   begin
      if CArgv.Arg(Argv => Argv, N => 1) = Frame_Name & ".mapfont" then
         Set_Fonts
           (New_Size => Positive'Value(Get(Widgt => Spin_Box)),
            Font_Type => MAPFONT);
      elsif CArgv.Arg(Argv => Argv, N => 1) = Frame_Name & ".helpfont" then
         Set_Fonts
           (New_Size => Positive'Value(Get(Widgt => Spin_Box)),
            Font_Type => HELPFONT);
      else
         Set_Fonts
           (New_Size => Positive'Value(Get(Widgt => Spin_Box)),
            Font_Type => INTERFACEFONT);
      end if;
      Load_Theme_Images;
      return TCL_OK;
   end Set_Fonts_Command;

   -- ****o* GameOptions/GameOptions.Set_Default_Fonts_Command
   -- FUNCTION
   -- Set the default values for fonts
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetDefaultFonts
   -- SOURCE
   function Set_Default_Fonts_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Default_Fonts_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc, Argv);
      Spin_Box: Ttk_SpinBox;
      Spin_Box_Names: constant array(1 .. 3) of Unbounded_String :=
        (1 => To_Unbounded_String(Source => "map"),
         2 => To_Unbounded_String(Source => "interface"),
         3 => To_Unbounded_String(Source => "help"));
      Font_Types_Names: constant array(1 .. 3) of Config.Font_Types :=
        (1 => MAPFONT, 2 => INTERFACEFONT, 3 => HELPFONT);
   begin
      Spin_Box.Interp := Interp;
      Set_Default_Fonts_Loop :
      for I in Spin_Box_Names'Range loop
         Spin_Box.Name :=
           New_String
             (Str =>
                ".gameframe.paned.optionsframe.canvas.options.interface." &
                To_String(Source => Spin_Box_Names(I)) & "font");
         Set
           (SpinBox => Spin_Box,
            Value => Positive'Image(Default_Fonts_Sizes(I)));
         Set_Fonts
           (New_Size => Default_Fonts_Sizes(I),
            Font_Type => Font_Types_Names(I));
      end loop Set_Default_Fonts_Loop;
      Load_Theme_Images;
      return TCL_OK;
   end Set_Default_Fonts_Command;

   -- ****o* GameOptions/GameOptions.Close_Options_Command
   -- FUNCTION
   -- Save all options and back to the map
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- CloseOptions oldscreen
   -- Oldscreen is name of the screen to which the game should return.
   -- Can be 'map' or 'combat'.
   -- SOURCE
   function Close_Options_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Close_Options_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc);
      Root_Name: constant String :=
        ".gameframe.paned.optionsframe.canvas.options";
      Key_Entry: Ttk_Entry;
      Map_View: Tk_Text;
      Theme_Combo_Box: constant Ttk_ComboBox :=
        Get_Widget
          (pathName => Root_Name & ".interface.theme", Interp => Interp);
      function Get_Spinbox_Value(Spin_Box_Name: String) return Natural is
         Spin_Box: constant Ttk_SpinBox :=
           Get_Widget(pathName => Root_Name & Spin_Box_Name, Interp => Interp);
      begin
         return Natural'Value(Get(Widgt => Spin_Box));
      end Get_Spinbox_Value;
      function Get_Checkbox_Value(Check_Box_Name: String) return Boolean is
      begin
         if Tcl_GetVar
             (interp => Interp, varName => Root_Name & Check_Box_Name) =
           "1" then
            return True;
         end if;
         return False;
      end Get_Checkbox_Value;
      function Get_Combobox_Value(Combo_Box_Name: String) return Natural is
         Combo_Box: constant Ttk_ComboBox :=
           Get_Widget
             (pathName => Root_Name & Combo_Box_Name, Interp => Interp);
      begin
         return Natural'Value(Current(ComboBox => Combo_Box));
      end Get_Combobox_Value;
   begin
      configure(Widgt => Close_Button, options => "-command ShowSkyMap");
      Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Close_Button);
      Game_Settings.Auto_Rest :=
        Get_Checkbox_Value(Check_Box_Name => ".general.autorest");
      Game_Settings.Undock_Speed :=
        Ship_Speed'Val
          (Get_Combobox_Value(Combo_Box_Name => ".general.speed") + 1);
      Game_Settings.Auto_Center :=
        Get_Checkbox_Value(Check_Box_Name => ".general.autocenter");
      Game_Settings.Auto_Return :=
        Get_Checkbox_Value(Check_Box_Name => ".general.autoreturn");
      Game_Settings.Auto_Finish :=
        Get_Checkbox_Value(Check_Box_Name => ".general.autofinish");
      Game_Settings.Auto_Ask_For_Bases :=
        Get_Checkbox_Value(Check_Box_Name => ".general.autoaskforbases");
      Game_Settings.Auto_Ask_For_Events :=
        Get_Checkbox_Value(Check_Box_Name => ".general.autoaskforevents");
      Game_Settings.Low_Fuel :=
        Get_Spinbox_Value(Spin_Box_Name => ".general.fuel");
      Game_Settings.Low_Drinks :=
        Get_Spinbox_Value(Spin_Box_Name => ".general.drinks");
      Game_Settings.Low_Food :=
        Get_Spinbox_Value(Spin_Box_Name => ".general.food");
      Game_Settings.Auto_Move_Stop :=
        Auto_Move_Break'Val
          (Get_Combobox_Value(Combo_Box_Name => ".general.automovestop"));
      Game_Settings.Messages_Limit :=
        Get_Spinbox_Value(Spin_Box_Name => ".general.messageslimit");
      Game_Settings.Saved_Messages :=
        Get_Spinbox_Value(Spin_Box_Name => ".general.savedmessages");
      Game_Settings.Messages_Order :=
        Messages_Order_Type'Val
          (Get_Combobox_Value(Combo_Box_Name => ".general.messagesorder"));
      Game_Settings.Auto_Save :=
        Auto_Save_Type'Val
          (Get_Combobox_Value(Combo_Box_Name => ".general.autosave"));
      Set_Theme_Loop :
      for I in Themes_List.Iterate loop
         if Themes_List(I).Name = Get(Widgt => Theme_Combo_Box) then
            Game_Settings.Interface_Theme :=
              To_Unbounded_String
                (Source => Themes_Container.Key(Position => I));
            exit Set_Theme_Loop;
         end if;
      end loop Set_Theme_Loop;
      Theme_Use
        (ThemeName => To_String(Source => Game_Settings.Interface_Theme));
      Set_Theme;
      Map_View := Get_Widget(pathName => ".gameframe.paned.mapframe.map");
      if Tcl_GetVar
          (interp => Interp, varName => Root_Name & ".interface.rightbutton") =
        "1" then
         Game_Settings.Right_Button := True;
         Bind
           (Widgt => Map_View, Sequence => "<Button-3>",
            Script => "{ShowDestinationMenu %X %Y}");
         Unbind(Widgt => Map_View, Sequence => "<Button-1>");
      else
         Game_Settings.Right_Button := False;
         Bind
           (Widgt => Map_View, Sequence => "<Button-1>",
            Script => "{ShowDestinationMenu %X %Y}");
         Unbind(Widgt => Map_View, Sequence => "<Button-3>");
      end if;
      if Tcl_GetVar
          (interp => Interp,
           varName => Root_Name & ".interface.showtooltips") =
        "1" then
         Game_Settings.Show_Tooltips := True;
         Enable;
      else
         Game_Settings.Show_Tooltips := False;
         Disable;
      end if;
      Game_Settings.Show_Last_Messages :=
        (if
           Tcl_GetVar
             (interp => Interp,
              varName => Root_Name & ".interface.showmessages") =
           "1"
         then True
         else False);
      if Tcl_GetVar
          (interp => Interp, varName => Root_Name & ".interface.fullscreen") =
        "1" then
         Game_Settings.Full_Screen := True;
         Wm_Set
           (Widgt => Get_Main_Window(Interp => Interp), Action => "attributes",
            Options => "-fullscreen 1");
      else
         Game_Settings.Full_Screen := False;
         Wm_Set
           (Widgt => Get_Main_Window(Interp => Interp), Action => "attributes",
            Options => "-fullscreen 0");
      end if;
      Game_Settings.Auto_Close_Messages_Time :=
        Get_Spinbox_Value(Spin_Box_Name => ".interface.closemessages");
      Game_Settings.Show_Numbers :=
        Get_Checkbox_Value(Check_Box_Name => ".interface.shownumbers");
      Game_Settings.Map_Font_Size :=
        Get_Spinbox_Value(Spin_Box_Name => ".interface.mapfont");
      Game_Settings.Help_Font_Size :=
        Get_Spinbox_Value(Spin_Box_Name => ".interface.helpfont");
      Game_Settings.Interface_Font_Size :=
        Get_Spinbox_Value(Spin_Box_Name => ".interface.interfacefont");
      Game_Settings.Lists_Limit :=
        Get_Spinbox_Value(Spin_Box_Name => ".interface.listslimit");
      Save_Config;
      Key_Entry.Interp := Interp;
      Set_Accelerators_Loop :
      for I in Accels'Range loop
         Unbind_From_Main_Window
           (Interp => Interp,
            Sequence =>
              "<" &
              To_String
                (Source =>
                   Insert
                     (Source => Accels(I).Shortcut,
                      Before =>
                        Index
                          (Source => Accels(I).Shortcut, Pattern => "-",
                           Going => Backward) +
                        1,
                      New_Item => "KeyPress-")) &
              ">");
         Key_Entry.Name :=
           New_String
             (Str => Root_Name & To_String(Source => Accels(I).Entry_Name));
         if I < 12 then
            Menu_Accelerators(I) :=
              To_Unbounded_String(Source => Get(Widgt => Key_Entry));
            Bind_To_Main_Window
              (Interp => Get_Context,
               Sequence =>
                 "<" &
                 To_String
                   (Source =>
                      Insert
                        (Source =>
                           To_Unbounded_String
                             (Source => Get(Widgt => Key_Entry)),
                         Before =>
                           Index
                             (Source =>
                                To_Unbounded_String
                                  (Source => Get(Widgt => Key_Entry)),
                              Pattern => "-", Going => Backward) +
                           1,
                         New_Item => "KeyPress-")) &
                 ">",
               Script =>
                 "{InvokeMenu " & To_String(Source => Menu_Accelerators(I)) &
                 "}");
         elsif I < 49 then
            Map_Accelerators(I - 11) :=
              To_Unbounded_String(Source => Get(Widgt => Key_Entry));
         elsif I = 49 then
            null;
         else
            General_Accelerators(I - 49) :=
              To_Unbounded_String(Source => Get(Widgt => Key_Entry));
         end if;
         Accels(I).Shortcut :=
           To_Unbounded_String(Source => Get(Widgt => Key_Entry));
      end loop Set_Accelerators_Loop;
      Unbind_From_Main_Window
        (Interp => Interp,
         Sequence =>
           "<" &
           To_String
             (Source =>
                Insert
                  (Source => Accels(Accels'Last).Shortcut,
                   Before =>
                     Index
                       (Source => Accels(Accels'Last).Shortcut, Pattern => "-",
                        Going => Backward) +
                     1,
                   New_Item => "KeyPress-")) &
           ">");
      Key_Entry.Name :=
        New_String
          (Str =>
             Root_Name &
             To_String
               (Source =>
                  Accels(Menu_Accelerators'Last + Map_Accelerators'Last + 1)
                    .Entry_Name));
      Full_Screen_Accel :=
        To_Unbounded_String(Source => Get(Widgt => Key_Entry));
      Save_Keys_To_File_Block :
      declare
         Keys_File: File_Type;
      begin
         Create
           (File => Keys_File, Mode => Append_File,
            Name => To_String(Source => Save_Directory) & "keys.cfg");
         Save_Accelerators_Loop :
         for Accel of Accels loop
            Put_Line
              (File => Keys_File,
               Item =>
                 To_String(Source => Accel.Config_Name) & " = " &
                 To_String(Source => Accel.Shortcut));
         end loop Save_Accelerators_Loop;
         Close(File => Keys_File);
      end Save_Keys_To_File_Block;
      Set_Keys;
      if CArgv.Arg(Argv => Argv, N => 1) = "map" then
         Show_Sky_Map(Clear => True);
      else
         Show_Combat_Ui(New_Combat => False);
      end if;
      return TCL_OK;
   end Close_Options_Command;

   -- ****o* GameOptions/GameOptions.Reset_Keys_Command
   -- FUNCTION
   -- Reset the selected group of keys to their default values
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ResetKeys group
   -- Group is the group of keys which will be resetted. Possible values are
   -- movement, map, menu
   -- SOURCE
   function Reset_Keys_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Reset_Keys_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc);
      Default_Movement_Accels: constant array(1 .. 14) of Accel_Data :=
        (1 =>
           (Shortcut =>
              To_Unbounded_String
                (Source =>
                   (if Dir_Separator = '\' then "Home" else "KP_Home")),
            Entry_Name => To_Unbounded_String(Source => ".movement.upleft"),
            Config_Name => To_Unbounded_String(Source => "")),
         2 =>
           (Shortcut =>
              To_Unbounded_String
                (Source => (if Dir_Separator = '\' then "Up" else "KP_Up")),
            Entry_Name => To_Unbounded_String(Source => ".movement.up"),
            Config_Name => To_Unbounded_String(Source => "")),
         3 =>
           (Shortcut =>
              To_Unbounded_String
                (Source =>
                   (if Dir_Separator = '\' then "Prior" else "KP_Prior")),
            Entry_Name => To_Unbounded_String(Source => ".movement.upright"),
            Config_Name => To_Unbounded_String(Source => "")),
         4 =>
           (Shortcut =>
              To_Unbounded_String
                (Source =>
                   (if Dir_Separator = '\' then "Left" else "KP_Left")),
            Entry_Name => To_Unbounded_String(Source => ".movement.left"),
            Config_Name => To_Unbounded_String(Source => "")),
         5 =>
           (Shortcut =>
              To_Unbounded_String
                (Source =>
                   (if Dir_Separator = '\' then "Clear" else "KP_Begin")),
            Entry_Name => To_Unbounded_String(Source => ".movement.wait"),
            Config_Name => To_Unbounded_String(Source => "")),
         6 =>
           (Shortcut =>
              To_Unbounded_String
                (Source =>
                   (if Dir_Separator = '\' then "Right" else "KP_Right")),
            Entry_Name => To_Unbounded_String(Source => ".movement.right"),
            Config_Name => To_Unbounded_String(Source => "")),
         7 =>
           (Shortcut =>
              To_Unbounded_String
                (Source => (if Dir_Separator = '\' then "End" else "KP_End")),
            Entry_Name => To_Unbounded_String(Source => ".movement.downleft"),
            Config_Name => To_Unbounded_String(Source => "")),
         8 =>
           (Shortcut =>
              To_Unbounded_String
                (Source =>
                   (if Dir_Separator = '\' then "Down" else "KP_Down")),
            Entry_Name => To_Unbounded_String(Source => ".movement.down"),
            Config_Name => To_Unbounded_String(Source => "")),
         9 =>
           (Shortcut =>
              To_Unbounded_String
                (Source =>
                   (if Dir_Separator = '\' then "Next" else "KP_Next")),
            Entry_Name => To_Unbounded_String(Source => ".movement.downright"),
            Config_Name => To_Unbounded_String(Source => "")),
         10 =>
           (Shortcut =>
              To_Unbounded_String
                (Source =>
                   (if Dir_Separator = '\' then "slash" else "KP_Divide")),
            Entry_Name => To_Unbounded_String(Source => ".movement.moveto"),
            Config_Name => To_Unbounded_String(Source => "")),
         11 =>
           (Shortcut => To_Unbounded_String(Source => "Control-a"),
            Entry_Name => To_Unbounded_String(Source => ".movement.fullstop"),
            Config_Name => To_Unbounded_String(Source => "")),
         12 =>
           (Shortcut => To_Unbounded_String(Source => "Control-b"),
            Entry_Name =>
              To_Unbounded_String(Source => ".movement.quarterspeed"),
            Config_Name => To_Unbounded_String(Source => "")),
         13 =>
           (Shortcut => To_Unbounded_String(Source => "Control-c"),
            Entry_Name => To_Unbounded_String(Source => ".movement.halfspeed"),
            Config_Name => To_Unbounded_String(Source => "")),
         14 =>
           (Shortcut => To_Unbounded_String(Source => "Control-d"),
            Entry_Name => To_Unbounded_String(Source => ".movement.fullspeed"),
            Config_Name => To_Unbounded_String(Source => "")));
      Default_Menu_Accels: constant array(1 .. 12) of Accel_Data :=
        (1 =>
           (Shortcut => To_Unbounded_String(Source => "s"),
            Entry_Name => To_Unbounded_String(Source => ".menu.shipinfo"),
            Config_Name => To_Unbounded_String(Source => "")),
         2 =>
           (Shortcut => To_Unbounded_String(Source => "o"),
            Entry_Name => To_Unbounded_String(Source => ".menu.orders"),
            Config_Name => To_Unbounded_String(Source => "")),
         3 =>
           (Shortcut => To_Unbounded_String(Source => "r"),
            Entry_Name => To_Unbounded_String(Source => ".menu.crafts"),
            Config_Name => To_Unbounded_String(Source => "")),
         4 =>
           (Shortcut => To_Unbounded_String(Source => "m"),
            Entry_Name => To_Unbounded_String(Source => ".menu.messages"),
            Config_Name => To_Unbounded_String(Source => "")),
         5 =>
           (Shortcut => To_Unbounded_String(Source => "k"),
            Entry_Name => To_Unbounded_String(Source => ".menu.knowledge"),
            Config_Name => To_Unbounded_String(Source => "")),
         6 =>
           (Shortcut => To_Unbounded_String(Source => "w"),
            Entry_Name => To_Unbounded_String(Source => ".menu.waitorders"),
            Config_Name => To_Unbounded_String(Source => "")),
         7 =>
           (Shortcut => To_Unbounded_String(Source => "g"),
            Entry_Name => To_Unbounded_String(Source => ".menu.gamestats"),
            Config_Name => To_Unbounded_String(Source => "")),
         8 =>
           (Shortcut => To_Unbounded_String(Source => "F1"),
            Entry_Name => To_Unbounded_String(Source => ".menu.help"),
            Config_Name => To_Unbounded_String(Source => "")),
         9 =>
           (Shortcut => To_Unbounded_String(Source => "p"),
            Entry_Name => To_Unbounded_String(Source => ".menu.gameoptions"),
            Config_Name => To_Unbounded_String(Source => "")),
         10 =>
           (Shortcut => To_Unbounded_String(Source => "q"),
            Entry_Name => To_Unbounded_String(Source => ".menu.quit"),
            Config_Name => To_Unbounded_String(Source => "")),
         11 =>
           (Shortcut => To_Unbounded_String(Source => "x"),
            Entry_Name => To_Unbounded_String(Source => ".menu.resign"),
            Config_Name => To_Unbounded_String(Source => "")),
         12 =>
           (Shortcut => To_Unbounded_String(Source => "e"),
            Entry_Name => To_Unbounded_String(Source => ".menu.menu"),
            Config_Name => To_Unbounded_String(Source => "")));
      Default_Map_Accels: constant array(1 .. 23) of Accel_Data :=
        (1 =>
           (Shortcut => To_Unbounded_String(Source => "Shift-Return"),
            Entry_Name => To_Unbounded_String(Source => ".map.center"),
            Config_Name => To_Unbounded_String(Source => "")),
         2 =>
           (Shortcut => To_Unbounded_String(Source => "Shift-h"),
            Entry_Name => To_Unbounded_String(Source => ".map.centerhomebase"),
            Config_Name => To_Unbounded_String(Source => "")),
         3 =>
           (Shortcut =>
              To_Unbounded_String
                (Source =>
                   "Shift-" &
                   (if Dir_Separator = '\' then "Home" else "KP_7")),
            Entry_Name => To_Unbounded_String(Source => ".map.mapupleft"),
            Config_Name => To_Unbounded_String(Source => "")),
         4 =>
           (Shortcut =>
              To_Unbounded_String
                (Source =>
                   "Shift-" & (if Dir_Separator = '\' then "Up" else "KP_8")),
            Entry_Name => To_Unbounded_String(Source => ".map.mapup"),
            Config_Name => To_Unbounded_String(Source => "")),
         5 =>
           (Shortcut =>
              To_Unbounded_String
                (Source =>
                   "Shift-" &
                   (if Dir_Separator = '\' then "Prior" else "KP_9")),
            Entry_Name => To_Unbounded_String(Source => ".map.mapupright"),
            Config_Name => To_Unbounded_String(Source => "")),
         6 =>
           (Shortcut =>
              To_Unbounded_String
                (Source =>
                   "Shift-" &
                   (if Dir_Separator = '\' then "Left" else "KP_4")),
            Entry_Name => To_Unbounded_String(Source => ".map.mapleft"),
            Config_Name => To_Unbounded_String(Source => "")),
         7 =>
           (Shortcut =>
              To_Unbounded_String
                (Source =>
                   "Shift-" &
                   (if Dir_Separator = '\' then "Right" else "KP_6")),
            Entry_Name => To_Unbounded_String(Source => ".map.mapright"),
            Config_Name => To_Unbounded_String(Source => "")),
         8 =>
           (Shortcut =>
              To_Unbounded_String
                (Source =>
                   "Shift-" & (if Dir_Separator = '\' then "End" else "KP_1")),
            Entry_Name => To_Unbounded_String(Source => ".map.mapdownleft"),
            Config_Name => To_Unbounded_String(Source => "")),
         9 =>
           (Shortcut =>
              To_Unbounded_String
                (Source =>
                   "Shift-" &
                   (if Dir_Separator = '\' then "Down" else "KP_2")),
            Entry_Name => To_Unbounded_String(Source => ".map.mapdown"),
            Config_Name => To_Unbounded_String(Source => "")),
         10 =>
           (Shortcut =>
              To_Unbounded_String
                (Source =>
                   "Shift-" &
                   (if Dir_Separator = '\' then "Next" else "KP_3")),
            Entry_Name => To_Unbounded_String(Source => ".map.mapdownright"),
            Config_Name => To_Unbounded_String(Source => "")),
         11 =>
           (Shortcut =>
              To_Unbounded_String
                (Source =>
                   "Control-" &
                   (if Dir_Separator = '\' then "Home" else "KP_Home")),
            Entry_Name => To_Unbounded_String(Source => ".map.cursorupleft"),
            Config_Name => To_Unbounded_String(Source => "")),
         12 =>
           (Shortcut =>
              To_Unbounded_String
                (Source =>
                   "Control-" &
                   (if Dir_Separator = '\' then "Up" else "KP_Up")),
            Entry_Name => To_Unbounded_String(Source => ".map.cursorup"),
            Config_Name => To_Unbounded_String(Source => "")),
         13 =>
           (Shortcut =>
              To_Unbounded_String
                (Source =>
                   "Control-" &
                   (if Dir_Separator = '\' then "Prior" else "KP_Prior")),
            Entry_Name => To_Unbounded_String(Source => ".map.cursorupright"),
            Config_Name => To_Unbounded_String(Source => "")),
         14 =>
           (Shortcut =>
              To_Unbounded_String
                (Source =>
                   "Control-" &
                   (if Dir_Separator = '\' then "Left" else "KP_Left")),
            Entry_Name => To_Unbounded_String(Source => ".map.cursorleft"),
            Config_Name => To_Unbounded_String(Source => "")),
         15 =>
           (Shortcut =>
              To_Unbounded_String
                (Source =>
                   "Control-" &
                   (if Dir_Separator = '\' then "Right" else "KP_Right")),
            Entry_Name => To_Unbounded_String(Source => ".map.cursorright"),
            Config_Name => To_Unbounded_String(Source => "")),
         16 =>
           (Shortcut =>
              To_Unbounded_String
                (Source =>
                   "Control-" &
                   (if Dir_Separator = '\' then "End" else "KP_End")),
            Entry_Name => To_Unbounded_String(Source => ".map.cursordownleft"),
            Config_Name => To_Unbounded_String(Source => "")),
         17 =>
           (Shortcut =>
              To_Unbounded_String
                (Source =>
                   "Control-" &
                   (if Dir_Separator = '\' then "Down" else "KP_Down")),
            Entry_Name => To_Unbounded_String(Source => ".map.cursordown"),
            Config_Name => To_Unbounded_String(Source => "")),
         18 =>
           (Shortcut =>
              To_Unbounded_String
                (Source =>
                   "Control-" &
                   (if Dir_Separator = '\' then "Next" else "KP_Next")),
            Entry_Name =>
              To_Unbounded_String(Source => ".map.cursordownright"),
            Config_Name => To_Unbounded_String(Source => "")),
         19 =>
           (Shortcut =>
              To_Unbounded_String
                (Source =>
                   "Control-" &
                   (if Dir_Separator = '\' then "Begin" else "Return")),
            Entry_Name => To_Unbounded_String(Source => ".map.clickmouse"),
            Config_Name => To_Unbounded_String(Source => "")),
         20 =>
           (Shortcut => To_Unbounded_String(Source => "Control-a"),
            Entry_Name => To_Unbounded_String(Source => ".movement.fullstop"),
            Config_Name => To_Unbounded_String(Source => "")),
         21 =>
           (Shortcut => To_Unbounded_String(Source => "Control-b"),
            Entry_Name =>
              To_Unbounded_String(Source => ".movement.quarterspeed"),
            Config_Name => To_Unbounded_String(Source => "")),
         22 =>
           (Shortcut => To_Unbounded_String(Source => "Control-c"),
            Entry_Name => To_Unbounded_String(Source => ".movement.halfspeed"),
            Config_Name => To_Unbounded_String(Source => "")),
         23 =>
           (Shortcut => To_Unbounded_String(Source => "Control-d"),
            Entry_Name => To_Unbounded_String(Source => ".movement.fullspeed"),
            Config_Name => To_Unbounded_String(Source => "")));
      Default_General_Accels: constant array(1 .. 4) of Accel_Data :=
        (1 =>
           (Shortcut => To_Unbounded_String(Source => "Alt-a"),
            Entry_Name => To_Unbounded_String(Source => ".ui.resizefirst"), Config_Name => To_Unbounded_String(Source => "")),
         2 =>
           (Shortcut => To_Unbounded_String(Source => "Alt-b"),
            Entry_Name => To_Unbounded_String(Source => ".ui.resizesecond"), Config_Name => To_Unbounded_String(Source => "")),
         3 =>
           (Shortcut => To_Unbounded_String(Source => "Alt-c"),
            Entry_Name => To_Unbounded_String(Source => ".ui.resizethird"), Config_Name => To_Unbounded_String(Source => "")),
         4 =>
           (Shortcut => To_Unbounded_String(Source => "Alt-d"),
            Entry_Name => To_Unbounded_String(Source => ".ui.resizefourth"), Config_Name => To_Unbounded_String(Source => "")));
      Key_Entry: Ttk_Entry;
   begin
      Key_Entry.Interp := Interp;
      if CArgv.Arg(Argv => Argv, N => 1) = "movement" then
         Reset_Movement_Keys_Loop :
         for Accel of Default_Movement_Accels loop
            Key_Entry.Name :=
              New_String
                (Str => ".gameframe.paned.optionsframe.canvas.options" &
                 To_String(Source => Accel.Entry_Name));
            Delete(TextEntry => Key_Entry, FirstIndex => "0", LastIndex => "end");
            Insert(TextEntry => Key_Entry, Index => "0", Text => To_String(Source => Accel.Shortcut));
         end loop Reset_Movement_Keys_Loop;
      elsif CArgv.Arg(Argv => Argv, N => 1) = "menu" then
         Reset_Menu_Keys_Loop :
         for Accel of Default_Menu_Accels loop
            Key_Entry.Name :=
              New_String
                (Str => ".gameframe.paned.optionsframe.canvas.options" &
                 To_String(Source => Accel.Entry_Name));
            Delete(TextEntry => Key_Entry, FirstIndex => "0", LastIndex => "end");
            Insert(Key_Entry, "0", To_String(Accel.Shortcut));
         end loop Reset_Menu_Keys_Loop;
      elsif CArgv.Arg(Argv, 1) = "map" then
         Reset_Map_Keys_Loop :
         for Accel of Default_Map_Accels loop
            Key_Entry.Name :=
              New_String
                (".gameframe.paned.optionsframe.canvas.options" &
                 To_String(Accel.Entry_Name));
            Delete(Key_Entry, "0", "end");
            Insert(Key_Entry, "0", To_String(Accel.Shortcut));
         end loop Reset_Map_Keys_Loop;
      elsif CArgv.Arg(Argv, 1) = "general" then
         Reset_General_Keys_Loop :
         for Accel of Default_General_Accels loop
            Key_Entry.Name :=
              New_String
                (".gameframe.paned.optionsframe.canvas.options" &
                 To_String(Accel.Entry_Name));
            Delete(Key_Entry, "0", "end");
            Insert(Key_Entry, "0", To_String(Accel.Shortcut));
         end loop Reset_General_Keys_Loop;
      end if;
      return TCL_OK;
   end Reset_Keys_Command;

   procedure Add_Commands is
   begin
      Add_Command("ShowOptions", Show_Options_Command'Access);
      Add_Command("SetFonts", Set_Fonts_Command'Access);
      Add_Command("SetDefaultFonts", Set_Default_Fonts_Command'Access);
      Add_Command("CloseOptions", Close_Options_Command'Access);
      Add_Command("ShowOptionsTab", Show_Options_Tab_Command'Access);
      Add_Command("ResetKeys", Reset_Keys_Command'Access);
   end Add_Commands;

end GameOptions;
