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
        (1 => (To_Unbounded_String("data"), Data_Directory),
         2 => (To_Unbounded_String("save"), Save_Directory),
         3 => (To_Unbounded_String("docs"), Doc_Directory),
         4 => (To_Unbounded_String("mods"), Mods_Directory));
      Checkbox_Array: constant array(1 .. 11) of Widget_Data :=
        ((To_Unbounded_String(Options_Canvas & ".options.general.autorest"),
          To_Unbounded_String(if Game_Settings.Auto_Rest then "1" else "0")),
         (To_Unbounded_String(Options_Canvas & ".options.general.autocenter"),
          To_Unbounded_String(if Game_Settings.Auto_Center then "1" else "0")),
         (To_Unbounded_String(Options_Canvas & ".options.general.autoreturn"),
          To_Unbounded_String(if Game_Settings.Auto_Return then "1" else "0")),
         (To_Unbounded_String(Options_Canvas & ".options.general.autofinish"),
          To_Unbounded_String(if Game_Settings.Auto_Finish then "1" else "0")),
         (To_Unbounded_String
            (Options_Canvas & ".options.general.autoaskforbases"),
          To_Unbounded_String
            (if Game_Settings.Auto_Ask_For_Bases then "1" else "0")),
         (To_Unbounded_String
            (Options_Canvas & ".options.general.autoaskforevents"),
          To_Unbounded_String
            (if Game_Settings.Auto_Ask_For_Events then "1" else "0")),
         (To_Unbounded_String
            (Options_Canvas & ".options.interface.rightbutton"),
          To_Unbounded_String
            (if Game_Settings.Right_Button then "1" else "0")),
         (To_Unbounded_String
            (Options_Canvas & ".options.interface.showtooltips"),
          To_Unbounded_String
            (if Game_Settings.Show_Tooltips then "1" else "0")),
         (To_Unbounded_String
            (Options_Canvas & ".options.interface.showmessages"),
          To_Unbounded_String
            (if Game_Settings.Show_Last_Messages then "1" else "0")),
         (To_Unbounded_String
            (Options_Canvas & ".options.interface.fullscreen"),
          To_Unbounded_String(if Game_Settings.Full_Screen then "1" else "0")),
         (To_Unbounded_String
            (Options_Canvas & ".options.interface.shownumbers"),
          To_Unbounded_String
            (if Game_Settings.Show_Numbers then "1" else "0")));
      SpinBox_Array: constant array(1 .. 10) of Widget_Data :=
        ((To_Unbounded_String(Options_Canvas & ".options.general.fuel"),
          To_Unbounded_String(Natural'Image(Game_Settings.Low_Fuel))),
         (To_Unbounded_String(Options_Canvas & ".options.general.drinks"),
          To_Unbounded_String(Natural'Image(Game_Settings.Low_Drinks))),
         (To_Unbounded_String(Options_Canvas & ".options.general.food"),
          To_Unbounded_String(Natural'Image(Game_Settings.Low_Food))),
         (To_Unbounded_String
            (Options_Canvas & ".options.general.messageslimit"),
          To_Unbounded_String(Natural'Image(Game_Settings.Messages_Limit))),
         (To_Unbounded_String
            (Options_Canvas & ".options.general.savedmessages"),
          To_Unbounded_String(Natural'Image(Game_Settings.Saved_Messages))),
         (To_Unbounded_String
            (Options_Canvas & ".options.interface.closemessages"),
          To_Unbounded_String
            (Natural'Image(Game_Settings.Auto_Close_Messages_Time))),
         (To_Unbounded_String(Options_Canvas & ".options.interface.mapfont"),
          To_Unbounded_String(Natural'Image(Game_Settings.Map_Font_Size))),
         (To_Unbounded_String
            (Options_Canvas & ".options.interface.interfacefont"),
          To_Unbounded_String
            (Natural'Image(Game_Settings.Interface_Font_Size))),
         (To_Unbounded_String(Options_Canvas & ".options.interface.helpfont"),
          To_Unbounded_String(Natural'Image(Game_Settings.Help_Font_Size))),
         (To_Unbounded_String
            (Options_Canvas & ".options.interface.listslimit"),
          To_Unbounded_String(Natural'Image(Game_Settings.Lists_Limit))));
      ComboBox_Array: constant array(Positive range <>) of Widget_Data :=
        ((To_Unbounded_String(Options_Canvas & ".options.general.speed"),
          To_Unbounded_String
            (Natural'Image(Ship_Speed'Pos(Game_Settings.Undock_Speed) - 1))),
         (To_Unbounded_String
            (Options_Canvas & ".options.general.automovestop"),
          To_Unbounded_String
            (Natural'Image
               (Auto_Move_Break'Pos(Game_Settings.Auto_Move_Stop)))),
         (To_Unbounded_String
            (Options_Canvas & ".options.general.messagesorder"),
          To_Unbounded_String
            (Natural'Image
               (Messages_Order_Type'Pos(Game_Settings.Messages_Order)))),
         (To_Unbounded_String(Options_Canvas & ".options.general.autosave"),
          To_Unbounded_String
            (Natural'Image(Auto_Save_Type'Pos(Game_Settings.Auto_Save)))));
   begin
      Label.Interp := Interp;
      Combo_Box_Widget.Interp := Interp;
      Tcl_SetVar(Interp, "newtab", "general");
      if Winfo_Get(Options_Canvas, "exists") = "0" then
         Tcl_EvalFile
           (Get_Context,
            To_String(Data_Directory) & "ui" & Dir_Separator & "options.tcl");
         Bind(Options_Frame, "<Configure>", "{ResizeCanvas %W.canvas %w %h}");
         for Path_Label of Labels_Array loop
            Label.Name :=
              New_String
                (Widget_Image(Options_Canvas) & ".options.info." &
                 To_String(Path_Label.Name));
            configure
              (Label,
               "-text {" & Full_Name(To_String(Path_Label.Value)) & " }");
         end loop;
         Load_Themes_Loop :
         for Theme of Themes_List loop
            Append(Local_Themes_List, " {" & Theme.Name & "}");
         end loop Load_Themes_Loop;
         Combo_Box_Widget.Name :=
           New_String(Options_Frame & ".canvas.options.interface.theme");
         configure
           (Combo_Box_Widget,
            "-values [list" & To_String(Local_Themes_List) & "]");
      elsif Winfo_Get(Options_Canvas, "ismapped") = "1" then
         Tcl.Tk.Ada.Grid.Grid_Remove(Close_Button);
         Show_Sky_Map(True);
         return TCL_OK;
      end if;
      Options_Frame.Name := New_String(Options_Canvas & ".options.general");
      Tcl.Tk.Ada.Grid.Grid(Options_Frame, "-sticky nwes -padx 10");
      for CheckBox of Checkbox_Array loop
         Tcl_SetVar
           (Interp, To_String(CheckBox.Name), To_String(CheckBox.Value));
      end loop;
      for SpinBox of SpinBox_Array loop
         Spin_Box_Widget := Get_Widget(To_String(SpinBox.Name), Interp);
         Set(Spin_Box_Widget, To_String(SpinBox.Value));
      end loop;
      for ComboBox of ComboBox_Array loop
         Combo_Box_Widget := Get_Widget(To_String(ComboBox.Name), Interp);
         Current(Combo_Box_Widget, To_String(ComboBox.Value));
      end loop;
      Options_Frame.Name :=
        New_String(Widget_Image(Options_Canvas) & ".options.interface");
      Combo_Box_Widget.Name :=
        New_String(Widget_Image(Options_Frame) & ".theme");
      Set
        (Combo_Box_Widget,
         "{" &
         To_String
           (Themes_List(To_String(Game_Settings.Interface_Theme)).Name) &
         "}");
      Key_Entry.Interp := Interp;
      Options_Frame.Name :=
        New_String(Widget_Image(Options_Canvas) & ".options");
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
             (Widget_Image(Options_Frame) & To_String(Accel.Entry_Name));
         Delete(Key_Entry, "0", "end");
         Insert(Key_Entry, "0", To_String(Accel.Shortcut));
      end loop Load_Accelerators_Loop;
      if cget(Close_Button, "-command") = "ShowCombatUI" then
         configure(Close_Button, "-command {CloseOptions combat}");
      else
         configure(Close_Button, "-command {CloseOptions map}");
      end if;
      Tcl.Tk.Ada.Grid.Grid(Close_Button, "-row 0 -column 1");
      configure
        (Options_Canvas,
         "-height " & cget(Main_Paned, "-height") & " -width " &
         cget(Main_Paned, "-width"));
      Tcl_Eval(Get_Context, "update");
      Canvas_Create
        (Options_Canvas, "window",
         "0 0 -anchor nw -window " & Widget_Image(Options_Frame));
      Tcl_Eval(Get_Context, "update");
      configure
        (Options_Canvas,
         "-scrollregion [list " & BBox(Options_Canvas, "all") & "]");
      Show_Screen("optionsframe");
      return Show_Options_Tab_Command(Client_Data, Interp, Argc, Argv);
   end Show_Options_Command;

   -- ****o* GameOptions/GameOptions.Set_Fonts_Command
   -- FUNCTION
   -- Set the selected font
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetFonts fontfield
   -- Fontfield is the name of the spinbox which value changed.
   -- SOURCE
   function Set_Fonts_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Fonts_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      FrameName: constant String :=
        ".gameframe.paned.optionsframe.canvas.options.interface";
      SpinBox: constant Ttk_SpinBox := Get_Widget(CArgv.Arg(Argv, 1), Interp);
   begin
      if CArgv.Arg(Argv, 1) = FrameName & ".mapfont" then
         Set_Fonts
           (New_Size => Positive'Value(Get(SpinBox)), Font_Type => MAPFONT);
      elsif CArgv.Arg(Argv, 1) = FrameName & ".helpfont" then
         Set_Fonts
           (New_Size => Positive'Value(Get(SpinBox)), Font_Type => HELPFONT);
      else
         Set_Fonts
           (New_Size => Positive'Value(Get(SpinBox)),
            Font_Type => INTERFACEFONT);
      end if;
      Load_Theme_Images;
      return TCL_OK;
   end Set_Fonts_Command;

   -- ****o* GameOptions/GameOptions.Set_Default_Fonts_Command
   -- FUNCTION
   -- Set the default values for fonts
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetDefaultFonts
   -- SOURCE
   function Set_Default_Fonts_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Default_Fonts_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      SpinBox: Ttk_SpinBox;
      SpinBoxNames: constant array(1 .. 3) of Unbounded_String :=
        (To_Unbounded_String("map"), To_Unbounded_String("interface"),
         To_Unbounded_String("help"));
      FontTypesNames: constant array(1 .. 3) of Config.Font_Types :=
        (MAPFONT, INTERFACEFONT, HELPFONT);
   begin
      SpinBox.Interp := Interp;
      Set_Default_Fonts_Loop :
      for I in SpinBoxNames'Range loop
         SpinBox.Name :=
           New_String
             (".gameframe.paned.optionsframe.canvas.options.interface." &
              To_String(SpinBoxNames(I)) & "font");
         Set(SpinBox, Positive'Image(Default_Fonts_Sizes(I)));
         Set_Fonts
           (New_Size => Default_Fonts_Sizes(I),
            Font_Type => FontTypesNames(I));
      end loop Set_Default_Fonts_Loop;
      Load_Theme_Images;
      return TCL_OK;
   end Set_Default_Fonts_Command;

   -- ****o* GameOptions/GameOptions.Close_Options_Command
   -- FUNCTION
   -- Save all options and back to the map
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- CloseOptions oldscreen
   -- Oldscreen is name of the screen to which the game should return.
   -- Can be 'map' or 'combat'.
   -- SOURCE
   function Close_Options_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Close_Options_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      RootName: constant String :=
        ".gameframe.paned.optionsframe.canvas.options";
      KeyEntry: Ttk_Entry;
      MapView: Tk_Text;
      ThemeComboBox: constant Ttk_ComboBox :=
        Get_Widget(RootName & ".interface.theme", Interp);
      function Get_Spinbox_Value(SpinBox_Name: String) return Natural is
         SpinBox: constant Ttk_SpinBox :=
           Get_Widget(RootName & SpinBox_Name, Interp);
      begin
         return Natural'Value(Get(SpinBox));
      end Get_Spinbox_Value;
      function Get_Checkbox_Value(CheckBox_Name: String) return Boolean is
      begin
         if Tcl_GetVar(Interp, RootName & CheckBox_Name) = "1" then
            return True;
         end if;
         return False;
      end Get_Checkbox_Value;
      function Get_Combobox_Value(ComboBox_Name: String) return Natural is
         ComboBox: constant Ttk_ComboBox :=
           Get_Widget(RootName & ComboBox_Name, Interp);
      begin
         return Natural'Value(Current(ComboBox));
      end Get_Combobox_Value;
   begin
      configure(Close_Button, "-command ShowSkyMap");
      Tcl.Tk.Ada.Grid.Grid_Remove(Close_Button);
      Game_Settings.Auto_Rest := Get_Checkbox_Value(".general.autorest");
      Game_Settings.Undock_Speed :=
        Ship_Speed'Val(Get_Combobox_Value(".general.speed") + 1);
      Game_Settings.Auto_Center := Get_Checkbox_Value(".general.autocenter");
      Game_Settings.Auto_Return := Get_Checkbox_Value(".general.autoreturn");
      Game_Settings.Auto_Finish := Get_Checkbox_Value(".general.autofinish");
      Game_Settings.Auto_Ask_For_Bases :=
        Get_Checkbox_Value(".general.autoaskforbases");
      Game_Settings.Auto_Ask_For_Events :=
        Get_Checkbox_Value(".general.autoaskforevents");
      Game_Settings.Low_Fuel := Get_Spinbox_Value(".general.fuel");
      Game_Settings.Low_Drinks := Get_Spinbox_Value(".general.drinks");
      Game_Settings.Low_Food := Get_Spinbox_Value(".general.food");
      Game_Settings.Auto_Move_Stop :=
        Auto_Move_Break'Val(Get_Combobox_Value(".general.automovestop"));
      Game_Settings.Messages_Limit :=
        Get_Spinbox_Value(".general.messageslimit");
      Game_Settings.Saved_Messages :=
        Get_Spinbox_Value(".general.savedmessages");
      Game_Settings.Messages_Order :=
        Messages_Order_Type'Val(Get_Combobox_Value(".general.messagesorder"));
      Game_Settings.Auto_Save :=
        Auto_Save_Type'Val(Get_Combobox_Value(".general.autosave"));
      Set_Theme_Loop :
      for I in Themes_List.Iterate loop
         if Themes_List(I).Name = Get(ThemeComboBox) then
            Game_Settings.Interface_Theme :=
              To_Unbounded_String(Themes_Container.Key(I));
            exit Set_Theme_Loop;
         end if;
      end loop Set_Theme_Loop;
      Theme_Use(To_String(Game_Settings.Interface_Theme));
      Set_Theme;
      MapView := Get_Widget(".gameframe.paned.mapframe.map");
      if Tcl_GetVar(Interp, RootName & ".interface.rightbutton") = "1" then
         Game_Settings.Right_Button := True;
         Bind(MapView, "<Button-3>", "{ShowDestinationMenu %X %Y}");
         Unbind(MapView, "<Button-1>");
      else
         Game_Settings.Right_Button := False;
         Bind(MapView, "<Button-1>", "{ShowDestinationMenu %X %Y}");
         Unbind(MapView, "<Button-3>");
      end if;
      if Tcl_GetVar(Interp, RootName & ".interface.showtooltips") = "1" then
         Game_Settings.Show_Tooltips := True;
         Enable;
      else
         Game_Settings.Show_Tooltips := False;
         Disable;
      end if;
      Game_Settings.Show_Last_Messages :=
        (if Tcl_GetVar(Interp, RootName & ".interface.showmessages") = "1" then
           True
         else False);
      if Tcl_GetVar(Interp, RootName & ".interface.fullscreen") = "1" then
         Game_Settings.Full_Screen := True;
         Wm_Set(Get_Main_Window(Interp), "attributes", "-fullscreen 1");
      else
         Game_Settings.Full_Screen := False;
         Wm_Set(Get_Main_Window(Interp), "attributes", "-fullscreen 0");
      end if;
      Game_Settings.Auto_Close_Messages_Time :=
        Get_Spinbox_Value(".interface.closemessages");
      Game_Settings.Show_Numbers :=
        Get_Checkbox_Value(".interface.shownumbers");
      Game_Settings.Map_Font_Size := Get_Spinbox_Value(".interface.mapfont");
      Game_Settings.Help_Font_Size := Get_Spinbox_Value(".interface.helpfont");
      Game_Settings.Interface_Font_Size :=
        Get_Spinbox_Value(".interface.interfacefont");
      Game_Settings.Lists_Limit := Get_Spinbox_Value(".interface.listslimit");
      Save_Config;
      KeyEntry.Interp := Interp;
      Set_Accelerators_Loop :
      for I in Accels'Range loop
         Unbind_From_Main_Window
           (Interp,
            "<" &
            To_String
              (Insert
                 (Accels(I).Shortcut,
                  Index(Accels(I).Shortcut, "-", Backward) + 1, "KeyPress-")) &
            ">");
         KeyEntry.Name :=
           New_String(RootName & To_String(Accels(I).Entry_Name));
         if I < 12 then
            Menu_Accelerators(I) := To_Unbounded_String(Get(KeyEntry));
            Bind_To_Main_Window
              (Get_Context,
               "<" &
               To_String
                 (Insert
                    (To_Unbounded_String(Get(KeyEntry)),
                     Index(To_Unbounded_String(Get(KeyEntry)), "-", Backward) +
                     1,
                     "KeyPress-")) &
               ">",
               "{InvokeMenu " & To_String(Menu_Accelerators(I)) & "}");
         elsif I < 49 then
            Map_Accelerators(I - 11) := To_Unbounded_String(Get(KeyEntry));
         elsif I = 49 then
            null;
         else
            General_Accelerators(I - 49) := To_Unbounded_String(Get(KeyEntry));
         end if;
         Accels(I).Shortcut := To_Unbounded_String(Get(KeyEntry));
      end loop Set_Accelerators_Loop;
      Unbind_From_Main_Window
        (Interp,
         "<" &
         To_String
           (Insert
              (Accels(Accels'Last).Shortcut,
               Index(Accels(Accels'Last).Shortcut, "-", Backward) + 1,
               "KeyPress-")) &
         ">");
      KeyEntry.Name :=
        New_String
          (RootName &
           To_String
             (Accels(Menu_Accelerators'Last + Map_Accelerators'Last + 1)
                .Entry_Name));
      Full_Screen_Accel := To_Unbounded_String(Get(KeyEntry));
      Save_Keys_To_File_Block :
      declare
         KeysFile: File_Type;
      begin
         Create(KeysFile, Append_File, To_String(Save_Directory) & "keys.cfg");
         Save_Accelerators_Loop :
         for Accel of Accels loop
            Put_Line
              (KeysFile,
               To_String(Accel.Config_Name) & " = " &
               To_String(Accel.Shortcut));
         end loop Save_Accelerators_Loop;
         Close(KeysFile);
      end Save_Keys_To_File_Block;
      Set_Keys;
      if CArgv.Arg(Argv, 1) = "map" then
         Show_Sky_Map(True);
      else
         Show_Combat_Ui(False);
      end if;
      return TCL_OK;
   end Close_Options_Command;

   -- ****o* GameOptions/GameOptions.Reset_Keys_Command
   -- FUNCTION
   -- Reset the selected group of keys to their default values
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ResetKeys group
   -- Group is the group of keys which will be resetted. Possible values are
   -- movement, map, menu
   -- SOURCE
   function Reset_Keys_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Reset_Keys_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      Default_Movement_Accels: constant array(1 .. 14) of Accel_Data :=
        (1 =>
           (To_Unbounded_String
              ((if Dir_Separator = '\' then "Home" else "KP_Home")),
            To_Unbounded_String(".movement.upleft"), To_Unbounded_String("")),
         2 =>
           (To_Unbounded_String
              ((if Dir_Separator = '\' then "Up" else "KP_Up")),
            To_Unbounded_String(".movement.up"), To_Unbounded_String("")),
         3 =>
           (To_Unbounded_String
              ((if Dir_Separator = '\' then "Prior" else "KP_Prior")),
            To_Unbounded_String(".movement.upright"), To_Unbounded_String("")),
         4 =>
           (To_Unbounded_String
              ((if Dir_Separator = '\' then "Left" else "KP_Left")),
            To_Unbounded_String(".movement.left"), To_Unbounded_String("")),
         5 =>
           (To_Unbounded_String
              ((if Dir_Separator = '\' then "Clear" else "KP_Begin")),
            To_Unbounded_String(".movement.wait"), To_Unbounded_String("")),
         6 =>
           (To_Unbounded_String
              ((if Dir_Separator = '\' then "Right" else "KP_Right")),
            To_Unbounded_String(".movement.right"), To_Unbounded_String("")),
         7 =>
           (To_Unbounded_String
              ((if Dir_Separator = '\' then "End" else "KP_End")),
            To_Unbounded_String(".movement.downleft"),
            To_Unbounded_String("")),
         8 =>
           (To_Unbounded_String
              ((if Dir_Separator = '\' then "Down" else "KP_Down")),
            To_Unbounded_String(".movement.down"), To_Unbounded_String("")),
         9 =>
           (To_Unbounded_String
              ((if Dir_Separator = '\' then "Next" else "KP_Next")),
            To_Unbounded_String(".movement.downright"),
            To_Unbounded_String("")),
         10 =>
           (To_Unbounded_String
              ((if Dir_Separator = '\' then "slash" else "KP_Divide")),
            To_Unbounded_String(".movement.moveto"), To_Unbounded_String("")),
         11 =>
           (To_Unbounded_String("Control-a"),
            To_Unbounded_String(".movement.fullstop"),
            To_Unbounded_String("")),
         12 =>
           (To_Unbounded_String("Control-b"),
            To_Unbounded_String(".movement.quarterspeed"),
            To_Unbounded_String("")),
         13 =>
           (To_Unbounded_String("Control-c"),
            To_Unbounded_String(".movement.halfspeed"),
            To_Unbounded_String("")),
         14 =>
           (To_Unbounded_String("Control-d"),
            To_Unbounded_String(".movement.fullspeed"),
            To_Unbounded_String("")));
      Default_Menu_Accels: constant array(1 .. 12) of Accel_Data :=
        (1 =>
           (To_Unbounded_String("s"), To_Unbounded_String(".menu.shipinfo"),
            To_Unbounded_String("")),
         2 =>
           (To_Unbounded_String("o"), To_Unbounded_String(".menu.orders"),
            To_Unbounded_String("")),
         3 =>
           (To_Unbounded_String("r"), To_Unbounded_String(".menu.crafts"),
            To_Unbounded_String("")),
         4 =>
           (To_Unbounded_String("m"), To_Unbounded_String(".menu.messages"),
            To_Unbounded_String("")),
         5 =>
           (To_Unbounded_String("k"), To_Unbounded_String(".menu.knowledge"),
            To_Unbounded_String("")),
         6 =>
           (To_Unbounded_String("w"), To_Unbounded_String(".menu.waitorders"),
            To_Unbounded_String("")),
         7 =>
           (To_Unbounded_String("g"), To_Unbounded_String(".menu.gamestats"),
            To_Unbounded_String("")),
         8 =>
           (To_Unbounded_String("F1"), To_Unbounded_String(".menu.help"),
            To_Unbounded_String("")),
         9 =>
           (To_Unbounded_String("p"), To_Unbounded_String(".menu.gameoptions"),
            To_Unbounded_String("")),
         10 =>
           (To_Unbounded_String("q"), To_Unbounded_String(".menu.quit"),
            To_Unbounded_String("")),
         11 =>
           (To_Unbounded_String("x"), To_Unbounded_String(".menu.resign"),
            To_Unbounded_String("")),
         12 =>
           (To_Unbounded_String("e"), To_Unbounded_String(".menu.menu"),
            To_Unbounded_String("")));
      Default_Map_Accels: constant array(1 .. 23) of Accel_Data :=
        (1 =>
           (To_Unbounded_String("Shift-Return"),
            To_Unbounded_String(".map.center"), To_Unbounded_String("")),
         2 =>
           (To_Unbounded_String("Shift-h"),
            To_Unbounded_String(".map.centerhomebase"),
            To_Unbounded_String("")),
         3 =>
           (To_Unbounded_String
              ("Shift-" & (if Dir_Separator = '\' then "Home" else "KP_7")),
            To_Unbounded_String(".map.mapupleft"), To_Unbounded_String("")),
         4 =>
           (To_Unbounded_String
              ("Shift-" & (if Dir_Separator = '\' then "Up" else "KP_8")),
            To_Unbounded_String(".map.mapup"), To_Unbounded_String("")),
         5 =>
           (To_Unbounded_String
              ("Shift-" & (if Dir_Separator = '\' then "Prior" else "KP_9")),
            To_Unbounded_String(".map.mapupright"), To_Unbounded_String("")),
         6 =>
           (To_Unbounded_String
              ("Shift-" & (if Dir_Separator = '\' then "Left" else "KP_4")),
            To_Unbounded_String(".map.mapleft"), To_Unbounded_String("")),
         7 =>
           (To_Unbounded_String
              ("Shift-" & (if Dir_Separator = '\' then "Right" else "KP_6")),
            To_Unbounded_String(".map.mapright"), To_Unbounded_String("")),
         8 =>
           (To_Unbounded_String
              ("Shift-" & (if Dir_Separator = '\' then "End" else "KP_1")),
            To_Unbounded_String(".map.mapdownleft"), To_Unbounded_String("")),
         9 =>
           (To_Unbounded_String
              ("Shift-" & (if Dir_Separator = '\' then "Down" else "KP_2")),
            To_Unbounded_String(".map.mapdown"), To_Unbounded_String("")),
         10 =>
           (To_Unbounded_String
              ("Shift-" & (if Dir_Separator = '\' then "Next" else "KP_3")),
            To_Unbounded_String(".map.mapdownright"), To_Unbounded_String("")),
         11 =>
           (To_Unbounded_String
              ("Control-" &
               (if Dir_Separator = '\' then "Home" else "KP_Home")),
            To_Unbounded_String(".map.cursorupleft"), To_Unbounded_String("")),
         12 =>
           (To_Unbounded_String
              ("Control-" & (if Dir_Separator = '\' then "Up" else "KP_Up")),
            To_Unbounded_String(".map.cursorup"), To_Unbounded_String("")),
         13 =>
           (To_Unbounded_String
              ("Control-" &
               (if Dir_Separator = '\' then "Prior" else "KP_Prior")),
            To_Unbounded_String(".map.cursorupright"),
            To_Unbounded_String("")),
         14 =>
           (To_Unbounded_String
              ("Control-" &
               (if Dir_Separator = '\' then "Left" else "KP_Left")),
            To_Unbounded_String(".map.cursorleft"), To_Unbounded_String("")),
         15 =>
           (To_Unbounded_String
              ("Control-" &
               (if Dir_Separator = '\' then "Right" else "KP_Right")),
            To_Unbounded_String(".map.cursorright"), To_Unbounded_String("")),
         16 =>
           (To_Unbounded_String
              ("Control-" & (if Dir_Separator = '\' then "End" else "KP_End")),
            To_Unbounded_String(".map.cursordownleft"),
            To_Unbounded_String("")),
         17 =>
           (To_Unbounded_String
              ("Control-" &
               (if Dir_Separator = '\' then "Down" else "KP_Down")),
            To_Unbounded_String(".map.cursordown"), To_Unbounded_String("")),
         18 =>
           (To_Unbounded_String
              ("Control-" &
               (if Dir_Separator = '\' then "Next" else "KP_Next")),
            To_Unbounded_String(".map.cursordownright"),
            To_Unbounded_String("")),
         19 =>
           (To_Unbounded_String
              ("Control-" &
               (if Dir_Separator = '\' then "Begin" else "Return")),
            To_Unbounded_String(".map.clickmouse"), To_Unbounded_String("")),
         20 =>
           (To_Unbounded_String("Control-a"),
            To_Unbounded_String(".movement.fullstop"),
            To_Unbounded_String("")),
         21 =>
           (To_Unbounded_String("Control-b"),
            To_Unbounded_String(".movement.quarterspeed"),
            To_Unbounded_String("")),
         22 =>
           (To_Unbounded_String("Control-c"),
            To_Unbounded_String(".movement.halfspeed"),
            To_Unbounded_String("")),
         23 =>
           (To_Unbounded_String("Control-d"),
            To_Unbounded_String(".movement.fullspeed"),
            To_Unbounded_String("")));
      Default_General_Accels: constant array(1 .. 4) of Accel_Data :=
        (1 =>
           (To_Unbounded_String("Alt-a"),
            To_Unbounded_String(".ui.resizefirst"), To_Unbounded_String("")),
         2 =>
           (To_Unbounded_String("Alt-b"),
            To_Unbounded_String(".ui.resizesecond"), To_Unbounded_String("")),
         3 =>
           (To_Unbounded_String("Alt-c"),
            To_Unbounded_String(".ui.resizethird"), To_Unbounded_String("")),
         4 =>
           (To_Unbounded_String("Alt-d"),
            To_Unbounded_String(".ui.resizefourth"), To_Unbounded_String("")));
      KeyEntry: Ttk_Entry;
   begin
      KeyEntry.Interp := Interp;
      if CArgv.Arg(Argv, 1) = "movement" then
         Reset_Movement_Keys_Loop :
         for Accel of Default_Movement_Accels loop
            KeyEntry.Name :=
              New_String
                (".gameframe.paned.optionsframe.canvas.options" &
                 To_String(Accel.Entry_Name));
            Delete(KeyEntry, "0", "end");
            Insert(KeyEntry, "0", To_String(Accel.Shortcut));
         end loop Reset_Movement_Keys_Loop;
      elsif CArgv.Arg(Argv, 1) = "menu" then
         Reset_Menu_Keys_Loop :
         for Accel of Default_Menu_Accels loop
            KeyEntry.Name :=
              New_String
                (".gameframe.paned.optionsframe.canvas.options" &
                 To_String(Accel.Entry_Name));
            Delete(KeyEntry, "0", "end");
            Insert(KeyEntry, "0", To_String(Accel.Shortcut));
         end loop Reset_Menu_Keys_Loop;
      elsif CArgv.Arg(Argv, 1) = "map" then
         Reset_Map_Keys_Loop :
         for Accel of Default_Map_Accels loop
            KeyEntry.Name :=
              New_String
                (".gameframe.paned.optionsframe.canvas.options" &
                 To_String(Accel.Entry_Name));
            Delete(KeyEntry, "0", "end");
            Insert(KeyEntry, "0", To_String(Accel.Shortcut));
         end loop Reset_Map_Keys_Loop;
      elsif CArgv.Arg(Argv, 1) = "general" then
         Reset_General_Keys_Loop :
         for Accel of Default_General_Accels loop
            KeyEntry.Name :=
              New_String
                (".gameframe.paned.optionsframe.canvas.options" &
                 To_String(Accel.Entry_Name));
            Delete(KeyEntry, "0", "end");
            Insert(KeyEntry, "0", To_String(Accel.Shortcut));
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
