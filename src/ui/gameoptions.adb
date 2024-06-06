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

-- with Ada.Directories;
with Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Directory_Operations;
with CArgv;
with Tcl; use Tcl;
with Tcl.Ada;
with Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.TtkStyle;
with Tcl.Tk.Ada.Widgets;
-- with Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.Text;
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with Tcl.Tk.Ada.Widgets.TtkEntry; use Tcl.Tk.Ada.Widgets.TtkEntry;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
-- with Tcl.Tk.Ada.Widgets.TtkFrame;
-- with Tcl.Tk.Ada.Widgets.TtkLabel;
-- with Tcl.Tk.Ada.Winfo;
with Tcl.Tk.Ada.Wm;
with Tcl.Tklib.Ada.Tooltip;
with Config; use Config;
with CoreUI;
with Combat.UI;
with Game;
with Maps.UI; use Maps.UI;
with Ships;
with Themes; use Themes;
with Utils.UI; use Utils.UI;

package body GameOptions is

   --## rule off TYPE_INITIAL_VALUES
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
   --## rule on TYPE_INITIAL_VALUES

   -- ****iv* GameOptions/GameOptions.Accels
   -- FUNCTION
   -- Array with data to show keyboard shortcuts
   -- SOURCE
   Accels: array(1 .. 53) of Accel_Data :=
     (1 =>
        (Shortcut =>
           To_Unbounded_String(Source => Get_Menu_Accelerator(Index => 1)),
         Entry_Name => To_Unbounded_String(Source => ".menu.shipinfo"),
         Config_Name => To_Unbounded_String(Source => "ShipInfo")),
      2 =>
        (Shortcut =>
           To_Unbounded_String(Source => Get_Menu_Accelerator(Index => 2)),
         Entry_Name => To_Unbounded_String(Source => ".menu.orders"),
         Config_Name => To_Unbounded_String(Source => "Orders")),
      3 =>
        (Shortcut =>
           To_Unbounded_String(Source => Get_Menu_Accelerator(Index => 3)),
         Entry_Name => To_Unbounded_String(Source => ".menu.crafts"),
         Config_Name => To_Unbounded_String(Source => "Crafting")),
      4 =>
        (Shortcut =>
           To_Unbounded_String(Source => Get_Menu_Accelerator(Index => 4)),
         Entry_Name => To_Unbounded_String(Source => ".menu.messages"),
         Config_Name => To_Unbounded_String(Source => "LastMessages")),
      5 =>
        (Shortcut =>
           To_Unbounded_String(Source => Get_Menu_Accelerator(Index => 5)),
         Entry_Name => To_Unbounded_String(Source => ".menu.knowledge"),
         Config_Name => To_Unbounded_String(Source => "Knowledge")),
      6 =>
        (Shortcut =>
           To_Unbounded_String(Source => Get_Menu_Accelerator(Index => 6)),
         Entry_Name => To_Unbounded_String(Source => ".menu.waitorders"),
         Config_Name => To_Unbounded_String(Source => "WaitOrders")),
      7 =>
        (Shortcut =>
           To_Unbounded_String(Source => Get_Menu_Accelerator(Index => 7)),
         Entry_Name => To_Unbounded_String(Source => ".menu.gamestats"),
         Config_Name => To_Unbounded_String(Source => "GameStats")),
      8 =>
        (Shortcut =>
           To_Unbounded_String(Source => Get_Menu_Accelerator(Index => 8)),
         Entry_Name => To_Unbounded_String(Source => ".menu.help"),
         Config_Name => To_Unbounded_String(Source => "Help")),
      9 =>
        (Shortcut =>
           To_Unbounded_String(Source => Get_Menu_Accelerator(Index => 9)),
         Entry_Name => To_Unbounded_String(Source => ".menu.gameoptions"),
         Config_Name => To_Unbounded_String(Source => "GameOptions")),
      10 =>
        (Shortcut =>
           To_Unbounded_String(Source => Get_Menu_Accelerator(Index => 10)),
         Entry_Name => To_Unbounded_String(Source => ".menu.quit"),
         Config_Name => To_Unbounded_String(Source => "Quit")),
      11 =>
        (Shortcut =>
           To_Unbounded_String(Source => Get_Menu_Accelerator(Index => 11)),
         Entry_Name => To_Unbounded_String(Source => ".menu.resign"),
         Config_Name => To_Unbounded_String(Source => "Resign")),
      12 =>
        (Shortcut =>
           To_Unbounded_String(Source => Get_Map_Accelerator(Index => 1)),
         Entry_Name => To_Unbounded_String(Source => ".menu.menu"),
         Config_Name => To_Unbounded_String(Source => "GameMenu")),
      13 =>
        (Shortcut =>
           To_Unbounded_String(Source => Get_Map_Accelerator(Index => 2)),
         Entry_Name => To_Unbounded_String(Source => ".map.mapoptions"),
         Config_Name => To_Unbounded_String(Source => "MapOptions")),
      14 =>
        (Shortcut =>
           To_Unbounded_String(Source => Get_Map_Accelerator(Index => 3)),
         Entry_Name => To_Unbounded_String(Source => ".map.zoomin"),
         Config_Name => To_Unbounded_String(Source => "ZoomInMap")),
      15 =>
        (Shortcut =>
           To_Unbounded_String(Source => Get_Map_Accelerator(Index => 4)),
         Entry_Name => To_Unbounded_String(Source => ".map.zoomout"),
         Config_Name => To_Unbounded_String(Source => "ZoomOutMap")),
      16 =>
        (Shortcut =>
           To_Unbounded_String(Source => Get_Map_Accelerator(Index => 5)),
         Entry_Name => To_Unbounded_String(Source => ".movement.upleft"),
         Config_Name => To_Unbounded_String(Source => "MoveUpLeft")),
      17 =>
        (Shortcut =>
           To_Unbounded_String(Source => Get_Map_Accelerator(Index => 6)),
         Entry_Name => To_Unbounded_String(Source => ".movement.up"),
         Config_Name => To_Unbounded_String(Source => "MoveUp")),
      18 =>
        (Shortcut =>
           To_Unbounded_String(Source => Get_Map_Accelerator(Index => 7)),
         Entry_Name => To_Unbounded_String(Source => ".movement.upright"),
         Config_Name => To_Unbounded_String(Source => "MoveUpRight")),
      19 =>
        (Shortcut =>
           To_Unbounded_String(Source => Get_Map_Accelerator(Index => 8)),
         Entry_Name => To_Unbounded_String(Source => ".movement.left"),
         Config_Name => To_Unbounded_String(Source => "MoveLeft")),
      20 =>
        (Shortcut =>
           To_Unbounded_String(Source => Get_Map_Accelerator(Index => 10)),
         Entry_Name => To_Unbounded_String(Source => ".movement.wait"),
         Config_Name => To_Unbounded_String(Source => "WaitInPlace")),
      21 =>
        (Shortcut =>
           To_Unbounded_String(Source => Get_Map_Accelerator(Index => 9)),
         Entry_Name => To_Unbounded_String(Source => ".movement.right"),
         Config_Name => To_Unbounded_String(Source => "MoveRight")),
      22 =>
        (Shortcut =>
           To_Unbounded_String(Source => Get_Map_Accelerator(Index => 11)),
         Entry_Name => To_Unbounded_String(Source => ".movement.downleft"),
         Config_Name => To_Unbounded_String(Source => "MoveDownRight")),
      23 =>
        (Shortcut =>
           To_Unbounded_String(Source => Get_Map_Accelerator(Index => 12)),
         Entry_Name => To_Unbounded_String(Source => ".movement.down"),
         Config_Name => To_Unbounded_String(Source => "MoveDown")),
      24 =>
        (Shortcut =>
           To_Unbounded_String(Source => Get_Map_Accelerator(Index => 13)),
         Entry_Name => To_Unbounded_String(Source => ".movement.downright"),
         Config_Name => To_Unbounded_String(Source => "MoveDownRight")),
      25 =>
        (Shortcut =>
           To_Unbounded_String(Source => Get_Map_Accelerator(Index => 14)),
         Entry_Name => To_Unbounded_String(Source => ".movement.moveto"),
         Config_Name => To_Unbounded_String(Source => "MoveTo")),
      26 =>
        (Shortcut =>
           To_Unbounded_String(Source => Get_Map_Accelerator(Index => 15)),
         Entry_Name => To_Unbounded_String(Source => ".map.center"),
         Config_Name => To_Unbounded_String(Source => "CenterMap")),
      27 =>
        (Shortcut =>
           To_Unbounded_String(Source => Get_Map_Accelerator(Index => 16)),
         Entry_Name => To_Unbounded_String(Source => ".map.centerhomebase"),
         Config_Name => To_Unbounded_String(Source => "CenterMapOnHomeBase")),
      28 =>
        (Shortcut =>
           To_Unbounded_String(Source => Get_Map_Accelerator(Index => 17)),
         Entry_Name => To_Unbounded_String(Source => ".map.mapupleft"),
         Config_Name => To_Unbounded_String(Source => "MoveMapUpLeft")),
      29 =>
        (Shortcut =>
           To_Unbounded_String(Source => Get_Map_Accelerator(Index => 18)),
         Entry_Name => To_Unbounded_String(Source => ".map.mapup"),
         Config_Name => To_Unbounded_String(Source => "MoveMapUp")),
      30 =>
        (Shortcut =>
           To_Unbounded_String(Source => Get_Map_Accelerator(Index => 19)),
         Entry_Name => To_Unbounded_String(Source => ".map.mapupright"),
         Config_Name => To_Unbounded_String(Source => "MoveMapUpRight")),
      31 =>
        (Shortcut =>
           To_Unbounded_String(Source => Get_Map_Accelerator(Index => 20)),
         Entry_Name => To_Unbounded_String(Source => ".map.mapleft"),
         Config_Name => To_Unbounded_String(Source => "MoveMapLeft")),
      32 =>
        (Shortcut =>
           To_Unbounded_String(Source => Get_Map_Accelerator(Index => 21)),
         Entry_Name => To_Unbounded_String(Source => ".map.mapright"),
         Config_Name => To_Unbounded_String(Source => "MoveMapRight")),
      33 =>
        (Shortcut =>
           To_Unbounded_String(Source => Get_Map_Accelerator(Index => 22)),
         Entry_Name => To_Unbounded_String(Source => ".map.mapdownleft"),
         Config_Name => To_Unbounded_String(Source => "MoveMapDownLeft")),
      34 =>
        (Shortcut =>
           To_Unbounded_String(Source => Get_Map_Accelerator(Index => 23)),
         Entry_Name => To_Unbounded_String(Source => ".map.mapdown"),
         Config_Name => To_Unbounded_String(Source => "MoveMapDown")),
      35 =>
        (Shortcut =>
           To_Unbounded_String(Source => Get_Map_Accelerator(Index => 24)),
         Entry_Name => To_Unbounded_String(Source => ".map.mapdownright"),
         Config_Name => To_Unbounded_String(Source => "MoveMapDownRight")),
      36 =>
        (Shortcut =>
           To_Unbounded_String(Source => Get_Map_Accelerator(Index => 25)),
         Entry_Name => To_Unbounded_String(Source => ".map.cursorupleft"),
         Config_Name => To_Unbounded_String(Source => "MoveCursorUpLeft")),
      37 =>
        (Shortcut =>
           To_Unbounded_String(Source => Get_Map_Accelerator(Index => 26)),
         Entry_Name => To_Unbounded_String(Source => ".map.cursorup"),
         Config_Name => To_Unbounded_String(Source => "MoveCursorUp")),
      38 =>
        (Shortcut =>
           To_Unbounded_String(Source => Get_Map_Accelerator(Index => 27)),
         Entry_Name => To_Unbounded_String(Source => ".map.cursorupright"),
         Config_Name => To_Unbounded_String(Source => "MoveCursorUpRight")),
      39 =>
        (Shortcut =>
           To_Unbounded_String(Source => Get_Map_Accelerator(Index => 28)),
         Entry_Name => To_Unbounded_String(Source => ".map.cursorleft"),
         Config_Name => To_Unbounded_String(Source => "MoveCursorLeft")),
      40 =>
        (Shortcut =>
           To_Unbounded_String(Source => Get_Map_Accelerator(Index => 29)),
         Entry_Name => To_Unbounded_String(Source => ".map.cursorright"),
         Config_Name => To_Unbounded_String(Source => "MoveCursorRight")),
      41 =>
        (Shortcut =>
           To_Unbounded_String(Source => Get_Map_Accelerator(Index => 30)),
         Entry_Name => To_Unbounded_String(Source => ".map.cursordownleft"),
         Config_Name => To_Unbounded_String(Source => "MoveCursorDownLeft")),
      42 =>
        (Shortcut =>
           To_Unbounded_String(Source => Get_Map_Accelerator(Index => 31)),
         Entry_Name => To_Unbounded_String(Source => ".map.cursordown"),
         Config_Name => To_Unbounded_String(Source => "MoveCursorDown")),
      43 =>
        (Shortcut =>
           To_Unbounded_String(Source => Get_Map_Accelerator(Index => 32)),
         Entry_Name => To_Unbounded_String(Source => ".map.cursordownright"),
         Config_Name => To_Unbounded_String(Source => "MoveCursorDownRight")),
      44 =>
        (Shortcut =>
           To_Unbounded_String(Source => Get_Map_Accelerator(Index => 33)),
         Entry_Name => To_Unbounded_String(Source => ".map.clickmouse"),
         Config_Name => To_Unbounded_String(Source => "LeftClickMouse")),
      45 =>
        (Shortcut =>
           To_Unbounded_String(Source => Get_Map_Accelerator(Index => 34)),
         Entry_Name => To_Unbounded_String(Source => ".movement.fullstop"),
         Config_Name => To_Unbounded_String(Source => "FullStop")),
      46 =>
        (Shortcut =>
           To_Unbounded_String(Source => Get_Map_Accelerator(Index => 35)),
         Entry_Name => To_Unbounded_String(Source => ".movement.quarterspeed"),
         Config_Name => To_Unbounded_String(Source => "QuarterSpeed")),
      47 =>
        (Shortcut =>
           To_Unbounded_String(Source => Get_Map_Accelerator(Index => 36)),
         Entry_Name => To_Unbounded_String(Source => ".movement.halfspeed"),
         Config_Name => To_Unbounded_String(Source => "HalfSpeed")),
      48 =>
        (Shortcut =>
           To_Unbounded_String(Source => Get_Map_Accelerator(Index => 37)),
         Entry_Name => To_Unbounded_String(Source => ".movement.fullspeed"),
         Config_Name => To_Unbounded_String(Source => "FullSpeed")),
      49 =>
        (Shortcut => To_Unbounded_String(Source => Get_Full_Screen_Accel),
         Entry_Name =>
           To_Unbounded_String(Source => ".interface.fullscreenkey"),
         Config_Name => To_Unbounded_String(Source => "FullScreen")),
      50 =>
        (Shortcut =>
           To_Unbounded_String(Source => Get_General_Accelerator(Index => 1)),
         Entry_Name => To_Unbounded_String(Source => ".ui.resizefirst"),
         Config_Name => To_Unbounded_String(Source => "ResizeFirst")),
      51 =>
        (Shortcut =>
           To_Unbounded_String(Source => Get_General_Accelerator(Index => 2)),
         Entry_Name => To_Unbounded_String(Source => ".ui.resizesecond"),
         Config_Name => To_Unbounded_String(Source => "ResizeSecond")),
      52 =>
        (Shortcut =>
           To_Unbounded_String(Source => Get_General_Accelerator(Index => 3)),
         Entry_Name => To_Unbounded_String(Source => ".ui.resizethird"),
         Config_Name => To_Unbounded_String(Source => "ResizeThird")),
      53 =>
        (Shortcut =>
           To_Unbounded_String(Source => Get_General_Accelerator(Index => 4)),
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
      Import => True,
      Convention => C,
      External_Name => "showOptionsTabCommand";
      -- ****

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
      Import => True,
      Convention => C,
      External_Name => "showOptionsCommand";
      -- ****

--   function Show_Options_Command
--     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
--      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
--      use Ada.Directories;
--      use Tcl.Tk.Ada.Widgets.Canvas;
--      use Tcl.Tk.Ada.Widgets.TtkFrame;
--      use Tcl.Tk.Ada.Widgets.TtkLabel;
--      use Tcl.Tk.Ada.Winfo;
--
--      Options_Frame: Ttk_Frame :=
--        Get_Widget(pathName => Main_Paned & ".optionsframe", Interp => Interp);
--      Options_Canvas: constant Tk_Canvas :=
--        Get_Widget(pathName => Options_Frame & ".canvas", Interp => Interp);
--      --## rule off IMPROPER_INITIALIZATION
--      Label: Ttk_Label;
--      Combo_Box_Widget: Ttk_ComboBox;
--      Spin_Box_Widget: Ttk_SpinBox;
--      Key_Entry: Ttk_Entry;
--      --## rule on IMPROPER_INITIALIZATION
--      Local_Themes_List: Unbounded_String := Null_Unbounded_String;
--      --## rule off TYPE_INITIAL_VALUES
--      type Widget_Data is record
--         Name: Unbounded_String;
--         Value: Unbounded_String;
--      end record;
--      --## rule on TYPE_INITIAL_VALUES
--      Labels_Array: constant array(1 .. 4) of Widget_Data :=
--        (1 =>
--           (Name => To_Unbounded_String(Source => "data"),
--            Value => Data_Directory),
--         2 =>
--           (Name => To_Unbounded_String(Source => "save"),
--            Value => Save_Directory),
--         3 =>
--           (Name => To_Unbounded_String(Source => "docs"),
--            Value => Doc_Directory),
--         4 =>
--           (Name => To_Unbounded_String(Source => "mods"),
--            Value => Mods_Directory));
--      Checkbox_Array: constant array(1 .. 11) of Widget_Data :=
--        (1 =>
--           (Name =>
--              To_Unbounded_String
--                (Source => Options_Canvas & ".options.general.autorest"),
--            Value =>
--              To_Unbounded_String
--                (Source =>
--                   (if Get_Boolean_Setting(Name => "autoRest") then "1"
--                    else "0"))),
--         2 =>
--           (Name =>
--              To_Unbounded_String
--                (Source => Options_Canvas & ".options.general.autocenter"),
--            Value =>
--              To_Unbounded_String
--                (Source =>
--                   (if Get_Boolean_Setting(Name => "autoCenter") then "1"
--                    else "0"))),
--         3 =>
--           (Name =>
--              To_Unbounded_String
--                (Source => Options_Canvas & ".options.general.autoreturn"),
--            Value =>
--              To_Unbounded_String
--                (Source =>
--                   (if Get_Boolean_Setting(Name => "autoReturn") then "1"
--                    else "0"))),
--         4 =>
--           (Name =>
--              To_Unbounded_String
--                (Source => Options_Canvas & ".options.general.autofinish"),
--            Value =>
--              To_Unbounded_String
--                (Source =>
--                   (if Get_Boolean_Setting(Name => "autoFinish") then "1"
--                    else "0"))),
--         5 =>
--           (Name =>
--              To_Unbounded_String
--                (Source =>
--                   Options_Canvas & ".options.general.autoaskforbases"),
--            Value =>
--              To_Unbounded_String
--                (Source =>
--                   (if Get_Boolean_Setting(Name => "autoAskForBases") then "1"
--                    else "0"))),
--         6 =>
--           (Name =>
--              To_Unbounded_String
--                (Source =>
--                   Options_Canvas & ".options.general.autoaskforevents"),
--            Value =>
--              To_Unbounded_String
--                (Source =>
--                   (if Get_Boolean_Setting(Name => "autoAskForEvents") then "1"
--                    else "0"))),
--         7 =>
--           (Name =>
--              To_Unbounded_String
--                (Source => Options_Canvas & ".options.interface.rightbutton"),
--            Value =>
--              To_Unbounded_String
--                (Source =>
--                   (if Get_Boolean_Setting(Name => "rightButton") then "1"
--                    else "0"))),
--         8 =>
--           (Name =>
--              To_Unbounded_String
--                (Source => Options_Canvas & ".options.interface.showtooltips"),
--            Value =>
--              To_Unbounded_String
--                (Source =>
--                   (if Get_Boolean_Setting(Name => "showTooltips") then "1"
--                    else "0"))),
--         9 =>
--           (Name =>
--              To_Unbounded_String
--                (Source => Options_Canvas & ".options.interface.showmessages"),
--            Value =>
--              To_Unbounded_String
--                (Source =>
--                   (if Get_Boolean_Setting(Name => "showLastMessages") then "1"
--                    else "0"))),
--         10 =>
--           (Name =>
--              To_Unbounded_String
--                (Source => Options_Canvas & ".options.interface.fullscreen"),
--            Value =>
--              To_Unbounded_String
--                (Source =>
--                   (if Get_Boolean_Setting(Name => "fullScreen") then "1"
--                    else "0"))),
--         11 =>
--           (Name =>
--              To_Unbounded_String
--                (Source => Options_Canvas & ".options.interface.shownumbers"),
--            Value =>
--              To_Unbounded_String
--                (Source =>
--                   (if Get_Boolean_Setting(Name => "showNumbers") then "1"
--                    else "0"))));
--      Spin_Box_Array: constant array(1 .. 11) of Widget_Data :=
--        (1 =>
--           (Name =>
--              To_Unbounded_String
--                (Source => Options_Canvas & ".options.general.fuel"),
--            Value =>
--              To_Unbounded_String
--                (Source =>
--                   Natural'Image(Get_Integer_Setting(Name => "lowFuel")))),
--         2 =>
--           (Name =>
--              To_Unbounded_String
--                (Source => Options_Canvas & ".options.general.drinks"),
--            Value =>
--              To_Unbounded_String
--                (Source =>
--                   Natural'Image(Get_Integer_Setting(Name => "lowDrinks")))),
--         3 =>
--           (Name =>
--              To_Unbounded_String
--                (Source => Options_Canvas & ".options.general.food"),
--            Value =>
--              To_Unbounded_String
--                (Source =>
--                   Natural'Image(Get_Integer_Setting(Name => "lowFood")))),
--         4 =>
--           (Name =>
--              To_Unbounded_String
--                (Source => Options_Canvas & ".options.general.messageslimit"),
--            Value =>
--              To_Unbounded_String
--                (Source =>
--                   Natural'Image
--                     (Get_Integer_Setting(Name => "messagesLimit")))),
--         5 =>
--           (Name =>
--              To_Unbounded_String
--                (Source => Options_Canvas & ".options.general.savedmessages"),
--            Value =>
--              To_Unbounded_String
--                (Source =>
--                   Natural'Image
--                     (Get_Integer_Setting(Name => "savedMessages")))),
--         6 =>
--           (Name =>
--              To_Unbounded_String
--                (Source =>
--                   Options_Canvas & ".options.interface.closemessages"),
--            Value =>
--              To_Unbounded_String
--                (Source =>
--                   Natural'Image
--                     (Get_Integer_Setting(Name => "autoCloseMessagesTime")))),
--         7 =>
--           (Name =>
--              To_Unbounded_String
--                (Source => Options_Canvas & ".options.interface.mapfont"),
--            Value =>
--              To_Unbounded_String
--                (Source =>
--                   Natural'Image(Get_Integer_Setting(Name => "mapFontSize")))),
--         8 =>
--           (Name =>
--              To_Unbounded_String
--                (Source =>
--                   Options_Canvas & ".options.interface.interfacefont"),
--            Value =>
--              To_Unbounded_String
--                (Source =>
--                   Natural'Image
--                     (Get_Integer_Setting(Name => "interfaceFontSize")))),
--         9 =>
--           (Name =>
--              To_Unbounded_String
--                (Source => Options_Canvas & ".options.interface.helpfont"),
--            Value =>
--              To_Unbounded_String
--                (Source =>
--                   Natural'Image
--                     (Get_Integer_Setting(Name => "helpFontSize")))),
--         10 =>
--           (Name =>
--              To_Unbounded_String
--                (Source => Options_Canvas & ".options.interface.listslimit"),
--            Value =>
--              To_Unbounded_String
--                (Source =>
--                   Natural'Image(Get_Integer_Setting(Name => "listsLimit")))),
--         11 =>
--           (Name =>
--              To_Unbounded_String
--                (Source => Options_Canvas & ".options.general.waitinterval"),
--            Value =>
--              To_Unbounded_String
--                (Source =>
--                   Natural'Image
--                     (Get_Integer_Setting(Name => "waitMinutes")))));
--      Combo_Box_Array: constant array(1 .. 4) of Widget_Data :=
--        (1 =>
--           (Name =>
--              To_Unbounded_String
--                (Source => Options_Canvas & ".options.general.speed"),
--            Value =>
--              To_Unbounded_String
--                (Source =>
--                   Natural'Image(Ship_Speed'Pos(Get_Undock_Speed) - 1))),
--         2 =>
--           (Name =>
--              To_Unbounded_String
--                (Source => Options_Canvas & ".options.general.automovestop"),
--            Value =>
--              To_Unbounded_String
--                (Source =>
--                   Natural'Image(Auto_Move_Break'Pos(Get_Auto_Move_Stop)))),
--         3 =>
--           (Name =>
--              To_Unbounded_String
--                (Source => Options_Canvas & ".options.general.messagesorder"),
--            Value =>
--              To_Unbounded_String
--                (Source =>
--                   Natural'Image
--                     (Messages_Order_Type'Pos(Get_Messages_Order)))),
--         4 =>
--           (Name =>
--              To_Unbounded_String
--                (Source => Options_Canvas & ".options.general.autosave"),
--            Value =>
--              To_Unbounded_String
--                (Source => Natural'Image(Auto_Save_Type'Pos(Get_Auto_Save)))));
--   begin
--      Label.Interp := Interp;
--      Combo_Box_Widget.Interp := Interp;
--      Tcl_SetVar(interp => Interp, varName => "newtab", newValue => "general");
--      if Winfo_Get(Widgt => Options_Canvas, Info => "exists") = "0" then
--         Tcl_EvalFile
--           (interp => Get_Context,
--            fileName =>
--              To_String(Source => Data_Directory) & "ui" & Dir_Separator &
--              "options.tcl");
--         Bind
--           (Widgt => Options_Frame, Sequence => "<Configure>",
--            Script => "{ResizeCanvas %W.canvas %w %h}");
--         Configure_Labels_Loop :
--         for Path_Label of Labels_Array loop
--            Label.Name :=
--              New_String
--                (Str =>
--                   Widget_Image(Win => Options_Canvas) & ".options.info." &
--                   To_String(Source => Path_Label.Name));
--            configure
--              (Widgt => Label,
--               options =>
--                 "-text {" &
--                 Full_Name(Name => To_String(Source => Path_Label.Value)) &
--                 " }");
--         end loop Configure_Labels_Loop;
--         Local_Themes_List := To_Unbounded_String(Source => Get_Themes_Names);
--         Combo_Box_Widget.Name :=
--           New_String
--             (Str => Options_Frame & ".canvas.options.interface.theme");
--         configure
--           (Widgt => Combo_Box_Widget,
--            options =>
--              "-values [list" & To_String(Source => Local_Themes_List) & "]");
--      elsif Winfo_Get(Widgt => Options_Canvas, Info => "ismapped") = "1" then
--         Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Close_Button);
--         Show_Sky_Map(Clear => True);
--         return TCL_OK;
--      end if;
--      Options_Frame.Name :=
--        New_String(Str => Options_Canvas & ".options.general");
--      Tcl.Tk.Ada.Grid.Grid
--        (Slave => Options_Frame, Options => "-sticky nwes -padx 10");
--      Set_Checkboxes_Loop :
--      for CheckBox of Checkbox_Array loop
--         Tcl_SetVar
--           (interp => Interp, varName => To_String(Source => CheckBox.Name),
--            newValue => To_String(Source => CheckBox.Value));
--      end loop Set_Checkboxes_Loop;
--      Set_Spinboxes_Loop :
--      for SpinBox of Spin_Box_Array loop
--         Spin_Box_Widget :=
--           Get_Widget
--             (pathName => To_String(Source => SpinBox.Name), Interp => Interp);
--         Set
--           (SpinBox => Spin_Box_Widget,
--            Value => To_String(Source => SpinBox.Value));
--      end loop Set_Spinboxes_Loop;
--      Set_Comboboxes_Loop :
--      for ComboBox of Combo_Box_Array loop
--         Combo_Box_Widget :=
--           Get_Widget
--             (pathName => To_String(Source => ComboBox.Name),
--              Interp => Interp);
--         Current
--           (ComboBox => Combo_Box_Widget,
--            NewIndex => To_String(Source => ComboBox.Value));
--      end loop Set_Comboboxes_Loop;
--      Options_Frame.Name :=
--        New_String
--          (Str => Widget_Image(Win => Options_Canvas) & ".options.interface");
--      Combo_Box_Widget.Name :=
--        New_String(Str => Widget_Image(Win => Options_Frame) & ".theme");
--      Set
--        (ComboBox => Combo_Box_Widget,
--         Value => "{" & Get_Icon(Name => "name") & "}");
--      Key_Entry.Interp := Interp;
--      Options_Frame.Name :=
--        New_String(Str => Widget_Image(Win => Options_Canvas) & ".options");
--      Load_Menu_Accelerators_Loop :
--      for I in 1 .. 11 loop
--         Accels(I).Shortcut :=
--           To_Unbounded_String(Source => Get_Menu_Accelerator(Index => I));
--      end loop Load_Menu_Accelerators_Loop;
--      Load_Map_Accelerators_Loop :
--      for I in 1 .. 37 loop
--         Accels(I + 11).Shortcut :=
--           To_Unbounded_String(Source => Get_Map_Accelerator(Index => I));
--      end loop Load_Map_Accelerators_Loop;
--      Accels(11 + 37 + 1).Shortcut :=
--        To_Unbounded_String(Source => Get_Full_Screen_Accel);
--      Load_General_Accelerators_Loop :
--      for I in 1 .. 4 loop
--         Accels(I + 11 + 37 + 1).Shortcut :=
--           To_Unbounded_String(Source => Get_General_Accelerator(Index => I));
--      end loop Load_General_Accelerators_Loop;
--      Load_Accelerators_Loop :
--      for Accel of Accels loop
--         Key_Entry.Name :=
--           New_String
--             (Str =>
--                Widget_Image(Win => Options_Frame) &
--                To_String(Source => Accel.Entry_Name));
--         Delete(TextEntry => Key_Entry, FirstIndex => "0", LastIndex => "end");
--         Insert
--           (TextEntry => Key_Entry, Index => "0",
--            Text => To_String(Source => Accel.Shortcut));
--      end loop Load_Accelerators_Loop;
--      if cget(Widgt => Close_Button, option => "-command") =
--        "ShowCombatUI" then
--         configure
--           (Widgt => Close_Button,
--            options => "-command {CloseOptions combat}");
--      else
--         configure
--           (Widgt => Close_Button, options => "-command {CloseOptions map}");
--      end if;
--      Tcl.Tk.Ada.Grid.Grid
--        (Slave => Close_Button, Options => "-row 0 -column 1");
--      configure
--        (Widgt => Options_Canvas,
--         options =>
--           "-height " & cget(Widgt => Main_Paned, option => "-height") &
--           " -width " & cget(Widgt => Main_Paned, option => "-width"));
--      Tcl_Eval(interp => Get_Context, strng => "update");
--      Canvas_Create
--        (Parent => Options_Canvas, Child_Type => "window",
--         Options =>
--           "0 0 -anchor nw -window " & Widget_Image(Win => Options_Frame));
--      Tcl_Eval(interp => Get_Context, strng => "update");
--      configure
--        (Widgt => Options_Canvas,
--         options =>
--           "-scrollregion [list " &
--           BBox(CanvasWidget => Options_Canvas, TagOrId => "all") & "]");
--      Show_Screen(New_Screen_Name => "optionsframe");
--      return
--        Show_Options_Tab_Command
--          (Client_Data => Client_Data, Interp => Interp, Argc => Argc,
--           Argv => Argv);
--   end Show_Options_Command;

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
            Font_Type => Help_Font_Type);
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
      Spin_Box: Ttk_SpinBox; --## rule line off IMPROPER_INITIALIZATION
      Spin_Box_Names: constant array(1 .. 3) of Unbounded_String :=
        (1 => To_Unbounded_String(Source => "map"),
         2 => To_Unbounded_String(Source => "interface"),
         3 => To_Unbounded_String(Source => "help"));
      Font_Types_Names: constant array(1 .. 3) of Config.Font_Types :=
        (1 => MAPFONT, 2 => INTERFACEFONT, 3 => Help_Font_Type);
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
      use Ada.Strings;
      use Tcl.Ada;
      use Tcl.Tk.Ada;
      use Tcl.Tk.Ada.TtkStyle;
      use Tcl.Tk.Ada.Widgets;
      use Tcl.Tk.Ada.Widgets.Text;
      use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
      use Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
      use Tcl.Tk.Ada.Wm;
      use Tcl.Tklib.Ada.Tooltip;
      use Combat.UI;
      use CoreUI;
      use Ships;

      Root_Name: constant String :=
        ".gameframe.paned.optionsframe.canvas.options";
      --## rule off IMPROPER_INITIALIZATION
      Key_Entry: Ttk_Entry;
      Map_View: Tk_Text;
      --## rule on IMPROPER_INITIALIZATION
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
      Set_Boolean_Setting
        (Name => "autoRest",
         Value => Get_Checkbox_Value(Check_Box_Name => ".general.autorest"));
      Set_Undock_Speed
        (Value =>
           Ship_Speed'Val
             (Get_Combobox_Value(Combo_Box_Name => ".general.speed") + 1));
      Set_Boolean_Setting
        (Name => "autoCenter",
         Value => Get_Checkbox_Value(Check_Box_Name => ".general.autocenter"));
      Set_Boolean_Setting
        (Name => "autoReturn",
         Value => Get_Checkbox_Value(Check_Box_Name => ".general.autoreturn"));
      Set_Boolean_Setting
        (Name => "autoFinish",
         Value => Get_Checkbox_Value(Check_Box_Name => ".general.autofinish"));
      Set_Boolean_Setting
        (Name => "autoAskForBases",
         Value =>
           Get_Checkbox_Value(Check_Box_Name => ".general.autoaskforbases"));
      Set_Boolean_Setting
        (Name => "autoAskForEvents",
         Value =>
           Get_Checkbox_Value(Check_Box_Name => ".general.autoaskforevents"));
      Set_Integer_Setting
        (Name => "lowFuel",
         Value => Get_Spinbox_Value(Spin_Box_Name => ".general.fuel"));
      Set_Integer_Setting
        (Name => "lowDrinks",
         Value => Get_Spinbox_Value(Spin_Box_Name => ".general.drinks"));
      Set_Integer_Setting
        (Name => "lowFood",
         Value => Get_Spinbox_Value(Spin_Box_Name => ".general.food"));
      Set_Auto_Move_Stop
        (Value =>
           Auto_Move_Break'Val
             (Get_Combobox_Value(Combo_Box_Name => ".general.automovestop")));
      Set_Integer_Setting
        (Name => "messagesLimit",
         Value =>
           Get_Spinbox_Value(Spin_Box_Name => ".general.messageslimit"));
      Set_Integer_Setting
        (Name => "savedMessages",
         Value =>
           Get_Spinbox_Value(Spin_Box_Name => ".general.savedmessages"));
      Set_Integer_Setting
        (Name => "waitMinutes",
         Value => Get_Spinbox_Value(Spin_Box_Name => ".general.waitinterval"));
      Set_Messages_Order
        (Value =>
           Messages_Order_Type'Val
             (Get_Combobox_Value(Combo_Box_Name => ".general.messagesorder")));
      Set_Auto_Save
        (Value =>
           Auto_Save_Type'Val
             (Get_Combobox_Value(Combo_Box_Name => ".general.autosave")));
      Set_New_Theme(Name => Get(Widgt => Theme_Combo_Box));
      Theme_Use(ThemeName => To_String(Source => Get_Interface_Theme));
      Set_Theme;
      Map_View := Get_Widget(pathName => ".gameframe.paned.mapframe.map");
      if Tcl_GetVar
          (interp => Interp, varName => Root_Name & ".interface.rightbutton") =
        "1" then
         Set_Boolean_Setting(Name => "rightButton", Value => True);
         Bind
           (Widgt => Map_View, Sequence => "<Button-3>",
            Script => "{ShowDestinationMenu %X %Y}");
         Unbind(Widgt => Map_View, Sequence => "<Button-1>");
      else
         Set_Boolean_Setting(Name => "rightButton", Value => False);
         Bind
           (Widgt => Map_View, Sequence => "<Button-1>",
            Script => "{ShowDestinationMenu %X %Y}");
         Unbind(Widgt => Map_View, Sequence => "<Button-3>");
      end if;
      if Tcl_GetVar
          (interp => Interp,
           varName => Root_Name & ".interface.showtooltips") =
        "1" then
         Set_Boolean_Setting(Name => "showToolips", Value => True);
         Enable;
      else
         Set_Boolean_Setting(Name => "showToolips", Value => False);
         Disable;
      end if;
      Set_Boolean_Setting
        (Name => "showLastMessages",
         Value =>
           (if
              Tcl_GetVar
                (interp => Interp,
                 varName => Root_Name & ".interface.showmessages") =
              "1"
            then True
            else False));
      if Tcl_GetVar
          (interp => Interp, varName => Root_Name & ".interface.fullscreen") =
        "1" then
         Set_Boolean_Setting(Name => "fullScreen", Value => True);
         Wm_Set
           (Widgt => Get_Main_Window(Interp => Interp), Action => "attributes",
            Options => "-fullscreen 1");
      else
         Set_Boolean_Setting(Name => "fullScreen", Value => False);
         Wm_Set
           (Widgt => Get_Main_Window(Interp => Interp), Action => "attributes",
            Options => "-fullscreen 0");
      end if;
      Set_Integer_Setting
        (Name => "autoCloseMessagesTime",
         Value =>
           Get_Spinbox_Value(Spin_Box_Name => ".interface.closemessages"));
      Set_Boolean_Setting
        (Name => "showNumbers",
         Value =>
           Get_Checkbox_Value(Check_Box_Name => ".interface.shownumbers"));
      Set_Integer_Setting
        (Name => "mapFontSize",
         Value => Get_Spinbox_Value(Spin_Box_Name => ".interface.mapfont"));
      Set_Integer_Setting
        (Name => "helpFontSize",
         Value => Get_Spinbox_Value(Spin_Box_Name => ".interface.helpfont"));
      Set_Integer_Setting
        (Name => "interfaceFontSize",
         Value =>
           Get_Spinbox_Value(Spin_Box_Name => ".interface.interfacefont"));
      Set_Integer_Setting
        (Name => "listsLimit",
         Value => Get_Spinbox_Value(Spin_Box_Name => ".interface.listslimit"));
      Save_Config;
      Key_Entry.Interp := Interp;
      --## rule off DIRECTLY_ACCESSED_GLOBALS
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
         --## rule off SIMPLIFIABLE_STATEMENTS
         if I < 12 then
            Set_Menu_Accelerator(Index => I, Value => Get(Widgt => Key_Entry));
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
                 "{InvokeMenu " & Get_Menu_Accelerator(Index => I) & "}");
         elsif I < 49 then
            Set_Map_Accelerator
              (Index => I - 11, Value => Get(Widgt => Key_Entry));
         elsif I = 49 then
            null;
         else
            Set_General_Accelerator
              (Index => I - 49, Value => Get(Widgt => Key_Entry));
         end if;
         --## rule on SIMPLIFIABLE_STATEMENTS
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
             Root_Name & To_String(Source => Accels(11 + 37 + 1).Entry_Name));
      Set_Full_Screen_Accel(Value => Get(Widgt => Key_Entry));
      Save_Keys_To_File_Block :
      declare
         use Ada.Text_IO;
         use Game;

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
      --## rule on DIRECTLY_ACCESSED_GLOBALS
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
      use GNAT.Directory_Operations;

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
            Entry_Name => To_Unbounded_String(Source => ".ui.resizefirst"),
            Config_Name => To_Unbounded_String(Source => "")),
         2 =>
           (Shortcut => To_Unbounded_String(Source => "Alt-b"),
            Entry_Name => To_Unbounded_String(Source => ".ui.resizesecond"),
            Config_Name => To_Unbounded_String(Source => "")),
         3 =>
           (Shortcut => To_Unbounded_String(Source => "Alt-c"),
            Entry_Name => To_Unbounded_String(Source => ".ui.resizethird"),
            Config_Name => To_Unbounded_String(Source => "")),
         4 =>
           (Shortcut => To_Unbounded_String(Source => "Alt-d"),
            Entry_Name => To_Unbounded_String(Source => ".ui.resizefourth"),
            Config_Name => To_Unbounded_String(Source => "")));
      Key_Entry: Ttk_Entry; --## rule line off IMPROPER_INITIALIZATION
   begin
      Key_Entry.Interp := Interp;
      if CArgv.Arg(Argv => Argv, N => 1) = "movement" then
         Reset_Movement_Keys_Loop :
         for Accel of Default_Movement_Accels loop
            Key_Entry.Name :=
              New_String
                (Str =>
                   ".gameframe.paned.optionsframe.canvas.options" &
                   To_String(Source => Accel.Entry_Name));
            Delete
              (TextEntry => Key_Entry, FirstIndex => "0", LastIndex => "end");
            Insert
              (TextEntry => Key_Entry, Index => "0",
               Text => To_String(Source => Accel.Shortcut));
         end loop Reset_Movement_Keys_Loop;
      elsif CArgv.Arg(Argv => Argv, N => 1) = "menu" then
         Reset_Menu_Keys_Loop :
         for Accel of Default_Menu_Accels loop
            Key_Entry.Name :=
              New_String
                (Str =>
                   ".gameframe.paned.optionsframe.canvas.options" &
                   To_String(Source => Accel.Entry_Name));
            Delete
              (TextEntry => Key_Entry, FirstIndex => "0", LastIndex => "end");
            Insert
              (TextEntry => Key_Entry, Index => "0",
               Text => To_String(Source => Accel.Shortcut));
         end loop Reset_Menu_Keys_Loop;
      elsif CArgv.Arg(Argv => Argv, N => 1) = "map" then
         Reset_Map_Keys_Loop :
         for Accel of Default_Map_Accels loop
            Key_Entry.Name :=
              New_String
                (Str =>
                   ".gameframe.paned.optionsframe.canvas.options" &
                   To_String(Source => Accel.Entry_Name));
            Delete
              (TextEntry => Key_Entry, FirstIndex => "0", LastIndex => "end");
            Insert
              (TextEntry => Key_Entry, Index => "0",
               Text => To_String(Source => Accel.Shortcut));
         end loop Reset_Map_Keys_Loop;
      elsif CArgv.Arg(Argv => Argv, N => 1) = "general" then
         Reset_General_Keys_Loop :
         for Accel of Default_General_Accels loop
            Key_Entry.Name :=
              New_String
                (Str =>
                   ".gameframe.paned.optionsframe.canvas.options" &
                   To_String(Source => Accel.Entry_Name));
            Delete
              (TextEntry => Key_Entry, FirstIndex => "0", LastIndex => "end");
            Insert
              (TextEntry => Key_Entry, Index => "0",
               Text => To_String(Source => Accel.Shortcut));
         end loop Reset_General_Keys_Loop;
      end if;
      return TCL_OK;
   end Reset_Keys_Command;

   procedure Add_Commands is
   begin
      Add_Command
        (Name => "ShowOptions", Ada_Command => Show_Options_Command'Access);
      Add_Command(Name => "SetFonts", Ada_Command => Set_Fonts_Command'Access);
      Add_Command
        (Name => "SetDefaultFonts",
         Ada_Command => Set_Default_Fonts_Command'Access);
      Add_Command
        (Name => "CloseOptions", Ada_Command => Close_Options_Command'Access);
      Add_Command
        (Name => "ShowOptionsTab",
         Ada_Command => Show_Options_Tab_Command'Access);
      Add_Command
        (Name => "ResetKeys", Ada_Command => Reset_Keys_Command'Access);
   end Add_Commands;

end GameOptions;
