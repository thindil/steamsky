-- Copyright (c) 2020-2021 Bartek thindil Jasicki <thindil@laeran.pl>
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
with Tcl.Tk.Ada.Font;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.TtkStyle; use Tcl.Tk.Ada.TtkStyle;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Canvas; use Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.Menu; use Tcl.Tk.Ada.Widgets.Menu;
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
   -- ShortCut  - Keyboard shortcut
   -- EntryName - Name of the text entry which will be showing this shortcut
   -- SOURCE
   type Accel_Data is record
      ShortCut: Unbounded_String;
      EntryName: Unbounded_String;
   end record;
   -- ****

   -- ****iv* GameOptions/GameOptions.Accels
   -- FUNCTION
   -- Array with data to show keyboard shortcuts
   -- SOURCE
   Accels: array(1 .. 49) of Accel_Data :=
     (1 => (MenuAccelerators(1), To_Unbounded_String(".menu.shipinfo")),
      2 => (MenuAccelerators(2), To_Unbounded_String(".menu.orders")),
      3 => (MenuAccelerators(3), To_Unbounded_String(".menu.crafts")),
      4 => (MenuAccelerators(4), To_Unbounded_String(".menu.messages")),
      5 => (MenuAccelerators(5), To_Unbounded_String(".menu.knowledge")),
      6 => (MenuAccelerators(6), To_Unbounded_String(".menu.waitorders")),
      7 => (MenuAccelerators(7), To_Unbounded_String(".menu.gamestats")),
      8 => (MenuAccelerators(8), To_Unbounded_String(".menu.help")),
      9 => (MenuAccelerators(9), To_Unbounded_String(".menu.gameoptions")),
      10 => (MenuAccelerators(10), To_Unbounded_String(".menu.quit")),
      11 => (MenuAccelerators(11), To_Unbounded_String(".menu.resign")),
      12 => (MapAccelerators(1), To_Unbounded_String(".menu.menu")),
      13 => (MapAccelerators(2), To_Unbounded_String(".map.mapoptions")),
      14 => (MapAccelerators(3), To_Unbounded_String(".map.zoomin")),
      15 => (MapAccelerators(4), To_Unbounded_String(".map.zoomout")),
      16 => (MapAccelerators(5), To_Unbounded_String(".movement.upleft")),
      17 => (MapAccelerators(6), To_Unbounded_String(".movement.up")),
      18 => (MapAccelerators(7), To_Unbounded_String(".movement.upright")),
      19 => (MapAccelerators(8), To_Unbounded_String(".movement.left")),
      20 => (MapAccelerators(9), To_Unbounded_String(".movement.right")),
      21 => (MapAccelerators(10), To_Unbounded_String(".movement.wait")),
      22 => (MapAccelerators(11), To_Unbounded_String(".movement.downleft")),
      23 => (MapAccelerators(12), To_Unbounded_String(".movement.down")),
      24 => (MapAccelerators(13), To_Unbounded_String(".movement.downright")),
      25 => (MapAccelerators(14), To_Unbounded_String(".movement.moveto")),
      26 => (MapAccelerators(15), To_Unbounded_String(".map.center")),
      27 => (MapAccelerators(16), To_Unbounded_String(".map.centerhomebase")),
      28 => (MapAccelerators(17), To_Unbounded_String(".map.mapupleft")),
      29 => (MapAccelerators(18), To_Unbounded_String(".map.mapup")),
      30 => (MapAccelerators(19), To_Unbounded_String(".map.mapupright")),
      31 => (MapAccelerators(20), To_Unbounded_String(".map.mapleft")),
      32 => (MapAccelerators(21), To_Unbounded_String(".map.mapright")),
      33 => (MapAccelerators(22), To_Unbounded_String(".map.mapdownleft")),
      34 => (MapAccelerators(23), To_Unbounded_String(".map.mapdown")),
      35 => (MapAccelerators(24), To_Unbounded_String(".map.mapdownright")),
      36 => (MapAccelerators(25), To_Unbounded_String(".map.cursorupleft")),
      37 => (MapAccelerators(26), To_Unbounded_String(".map.cursorup")),
      38 => (MapAccelerators(27), To_Unbounded_String(".map.cursorupright")),
      39 => (MapAccelerators(28), To_Unbounded_String(".map.cursorleft")),
      40 => (MapAccelerators(29), To_Unbounded_String(".map.cursorright")),
      41 => (MapAccelerators(30), To_Unbounded_String(".map.cursordownleft")),
      42 => (MapAccelerators(31), To_Unbounded_String(".map.cursordown")),
      43 => (MapAccelerators(32), To_Unbounded_String(".map.cursordownright")),
      44 => (MapAccelerators(33), To_Unbounded_String(".map.clickmouse")),
      45 => (MapAccelerators(34), To_Unbounded_String(".movement.fullstop")),
      46 =>
        (MapAccelerators(35), To_Unbounded_String(".movement.quarterspeed")),
      47 => (MapAccelerators(36), To_Unbounded_String(".movement.halfspeed")),
      48 => (MapAccelerators(37), To_Unbounded_String(".movement.fullspeed")),
      49 =>
        (FullScreenAccel, To_Unbounded_String(".interface.fullscreenkey")));
   -- ****

   -- ****o* GameOptions/GameOptions.Show_Options_Command
   -- FUNCTION
   -- Show the game options to the player
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowOptions
   -- SOURCE
   function Show_Options_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Options_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      OptionsFrame: Ttk_Frame :=
        Get_Widget(Main_Paned & ".optionsframe", Interp);
      OptionsCanvas: constant Tk_Canvas :=
        Get_Widget(OptionsFrame & ".canvas", Interp);
      Label: Ttk_Label;
      ComboBox_Widget: Ttk_ComboBox;
      SpinBox_Widget: Ttk_SpinBox;
      KeyEntry: Ttk_Entry;
      ThemesList: Unbounded_String;
      type Widget_Data is record
         Name: Unbounded_String;
         Value: Unbounded_String;
      end record;
      Labels_Array: constant array(1 .. 4) of Widget_Data :=
        ((To_Unbounded_String("data"), Data_Directory),
         (To_Unbounded_String("save"), Save_Directory),
         (To_Unbounded_String("docs"), Doc_Directory),
         (To_Unbounded_String("mods"), Mods_Directory));
      Checkbox_Array: constant array(1 .. 11) of Widget_Data :=
        ((To_Unbounded_String(OptionsCanvas & ".options.general.autorest"),
          To_Unbounded_String(if Game_Settings.Auto_Rest then "1" else "0")),
         (To_Unbounded_String(OptionsCanvas & ".options.general.autocenter"),
          To_Unbounded_String(if Game_Settings.Auto_Center then "1" else "0")),
         (To_Unbounded_String(OptionsCanvas & ".options.general.autoreturn"),
          To_Unbounded_String(if Game_Settings.Auto_Return then "1" else "0")),
         (To_Unbounded_String(OptionsCanvas & ".options.general.autofinish"),
          To_Unbounded_String(if Game_Settings.Auto_Finish then "1" else "0")),
         (To_Unbounded_String
            (OptionsCanvas & ".options.general.autoaskforbases"),
          To_Unbounded_String
            (if Game_Settings.Auto_Ask_For_Bases then "1" else "0")),
         (To_Unbounded_String
            (OptionsCanvas & ".options.general.autoaskforevents"),
          To_Unbounded_String
            (if Game_Settings.Auto_Ask_For_Events then "1" else "0")),
         (To_Unbounded_String
            (OptionsCanvas & ".options.interface.rightbutton"),
          To_Unbounded_String
            (if Game_Settings.Right_Button then "1" else "0")),
         (To_Unbounded_String
            (OptionsCanvas & ".options.interface.showtooltips"),
          To_Unbounded_String
            (if Game_Settings.Show_Tooltips then "1" else "0")),
         (To_Unbounded_String
            (OptionsCanvas & ".options.interface.showmessages"),
          To_Unbounded_String
            (if Game_Settings.Show_Last_Messages then "1" else "0")),
         (To_Unbounded_String(OptionsCanvas & ".options.interface.fullscreen"),
          To_Unbounded_String(if Game_Settings.Full_Screen then "1" else "0")),
         (To_Unbounded_String
            (OptionsCanvas & ".options.interface.shownumbers"),
          To_Unbounded_String
            (if Game_Settings.Show_Numbers then "1" else "0")));
      SpinBox_Array: constant array(1 .. 9) of Widget_Data :=
        ((To_Unbounded_String(OptionsCanvas & ".options.general.fuel"),
          To_Unbounded_String(Natural'Image(Game_Settings.Low_Fuel))),
         (To_Unbounded_String(OptionsCanvas & ".options.general.drinks"),
          To_Unbounded_String(Natural'Image(Game_Settings.Low_Drinks))),
         (To_Unbounded_String(OptionsCanvas & ".options.general.food"),
          To_Unbounded_String(Natural'Image(Game_Settings.Low_Food))),
         (To_Unbounded_String
            (OptionsCanvas & ".options.general.messageslimit"),
          To_Unbounded_String(Natural'Image(Game_Settings.Messages_Limit))),
         (To_Unbounded_String
            (OptionsCanvas & ".options.general.savedmessages"),
          To_Unbounded_String(Natural'Image(Game_Settings.Saved_Messages))),
         (To_Unbounded_String
            (OptionsCanvas & ".options.interface.closemessages"),
          To_Unbounded_String
            (Natural'Image(Game_Settings.Auto_Close_Messages_Time))),
         (To_Unbounded_String(OptionsCanvas & ".options.interface.mapfont"),
          To_Unbounded_String(Natural'Image(Game_Settings.Map_Font_Size))),
         (To_Unbounded_String
            (OptionsCanvas & ".options.interface.interfacefont"),
          To_Unbounded_String
            (Natural'Image(Game_Settings.Interface_Font_Size))),
         (To_Unbounded_String(OptionsCanvas & ".options.interface.helpfont"),
          To_Unbounded_String(Natural'Image(Game_Settings.Help_Font_Size))));
      ComboBox_Array: constant array(Positive range <>) of Widget_Data :=
        ((To_Unbounded_String(OptionsCanvas & ".options.general.speed"),
          To_Unbounded_String
            (Natural'Image(ShipSpeed'Pos(Game_Settings.Undock_Speed) - 1))),
         (To_Unbounded_String(OptionsCanvas & ".options.general.automovestop"),
          To_Unbounded_String
            (Natural'Image
               (Auto_Move_Break'Pos(Game_Settings.Auto_Move_Stop)))),
         (To_Unbounded_String
            (OptionsCanvas & ".options.general.messagesorder"),
          To_Unbounded_String
            (Natural'Image
               (Messages_Order_Type'Pos(Game_Settings.Messages_Order)))),
         (To_Unbounded_String(OptionsCanvas & ".options.general.autosave"),
          To_Unbounded_String
            (Natural'Image(Auto_Save_Type'Pos(Game_Settings.Auto_Save)))));
   begin
      Label.Interp := Interp;
      ComboBox_Widget.Interp := Interp;
      Tcl_SetVar(Interp, "newtab", "general");
      if Winfo_Get(OptionsCanvas, "exists") = "0" then
         Tcl_EvalFile
           (Get_Context,
            To_String(Data_Directory) & "ui" & Dir_Separator & "options.tcl");
         Bind(OptionsFrame, "<Configure>", "{ResizeCanvas %W.canvas %w %h}");
         for Path_Label of Labels_Array loop
            Label.Name :=
              New_String
                (Widget_Image(OptionsCanvas) & ".options.info." &
                 To_String(Path_Label.Name));
            configure
              (Label,
               "-text {" & Full_Name(To_String(Path_Label.Value)) & " }");
         end loop;
         Load_Themes_Loop :
         for Theme of Themes_List loop
            Append(ThemesList, " {" & Theme.Name & "}");
         end loop Load_Themes_Loop;
         ComboBox_Widget.Name :=
           New_String(OptionsFrame & ".canvas.options.interface.theme");
         configure
           (ComboBox_Widget, "-values [list" & To_String(ThemesList) & "]");
      elsif Winfo_Get(OptionsCanvas, "ismapped") = "1" then
         Tcl.Tk.Ada.Grid.Grid_Remove(Close_Button);
         Entry_Configure(GameMenu, "Help", "-command {ShowHelp general}");
         ShowSkyMap(True);
         return TCL_OK;
      end if;
      Entry_Configure(GameMenu, "Help", "-command {ShowHelp general}");
      OptionsFrame.Name := New_String(OptionsCanvas & ".options.general");
      Tcl.Tk.Ada.Grid.Grid(OptionsFrame, "-sticky nwes -padx 10");
      for CheckBox of Checkbox_Array loop
         Tcl_SetVar
           (Interp, To_String(CheckBox.Name), To_String(CheckBox.Value));
      end loop;
      for SpinBox of SpinBox_Array loop
         SpinBox_Widget := Get_Widget(To_String(SpinBox.Name), Interp);
         Set(SpinBox_Widget, To_String(SpinBox.Value));
      end loop;
      for ComboBox of ComboBox_Array loop
         ComboBox_Widget := Get_Widget(To_String(ComboBox.Name), Interp);
         Current(ComboBox_Widget, To_String(ComboBox.Value));
      end loop;
      OptionsFrame.Name :=
        New_String(Widget_Image(OptionsCanvas) & ".options.interface");
      ComboBox_Widget.Name :=
        New_String(Widget_Image(OptionsFrame) & ".theme");
      Set
        (ComboBox_Widget,
         "{" &
         To_String
           (Themes_List(To_String(Game_Settings.Interface_Theme)).Name) &
         "}");
      KeyEntry.Interp := Interp;
      OptionsFrame.Name :=
        New_String(Widget_Image(OptionsCanvas) & ".options");
      Load_Menu_Accelerators_Loop :
      for I in MenuAccelerators'Range loop
         Accels(I).ShortCut := MenuAccelerators(I);
      end loop Load_Menu_Accelerators_Loop;
      Load_Map_Accelerators_Loop :
      for I in MapAccelerators'Range loop
         Accels(I + MenuAccelerators'Last).ShortCut := MapAccelerators(I);
      end loop Load_Map_Accelerators_Loop;
      Accels(Accels'Last).ShortCut := FullScreenAccel;
      Load_Accelerators_Loop :
      for Accel of Accels loop
         KeyEntry.Name :=
           New_String(Widget_Image(OptionsFrame) & To_String(Accel.EntryName));
         Delete(KeyEntry, "0", "end");
         Insert(KeyEntry, "0", To_String(Accel.ShortCut));
      end loop Load_Accelerators_Loop;
      if cget(Close_Button, "-command") = "ShowCombatUI" then
         configure(Close_Button, "-command {CloseOptions combat}");
      else
         configure(Close_Button, "-command {CloseOptions map}");
      end if;
      Tcl.Tk.Ada.Grid.Grid(Close_Button, "-row 0 -column 1");
      configure
        (OptionsCanvas,
         "-height " & cget(Main_Paned, "-height") & " -width " &
         cget(Main_Paned, "-width"));
      Tcl_Eval(Get_Context, "update");
      Canvas_Create
        (OptionsCanvas, "window",
         "0 0 -anchor nw -window " & Widget_Image(OptionsFrame));
      Tcl_Eval(Get_Context, "update");
      configure
        (OptionsCanvas,
         "-scrollregion [list " & BBox(OptionsCanvas, "all") & "]");
      ShowScreen("optionsframe");
      return TCL_OK;
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
   -- SetFonts
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
      HelpFonts: constant array(1 .. 4) of Unbounded_String :=
        (To_Unbounded_String("HelpFont"), To_Unbounded_String("BoldHelpFont"),
         To_Unbounded_String("UnderlineHelpFont"),
         To_Unbounded_String("ItalicHelpFont"));
      InterfaceFonts: constant array(1 .. 4) of Unbounded_String :=
        (To_Unbounded_String("InterfaceFont"),
         To_Unbounded_String("InterfaceIcons"),
         To_Unbounded_String("OverstrikedFont"),
         To_Unbounded_String("UnderlineFont"));
   begin
      if CArgv.Arg(Argv, 1) = FrameName & ".mapfont" then
         Game_Settings.Map_Font_Size := Positive'Value(Get(SpinBox));
         Font.Configure
           ("MapFont", "-size" & Positive'Image(Game_Settings.Map_Font_Size));
      elsif CArgv.Arg(Argv, 1) = FrameName & ".helpfont" then
         Game_Settings.Help_Font_Size := Positive'Value(Get(SpinBox));
         Set_Fonts_Loop :
         for FontName of HelpFonts loop
            Font.Configure
              (To_String(FontName),
               "-size" & Positive'Image(Game_Settings.Help_Font_Size));
         end loop Set_Fonts_Loop;
      else
         Game_Settings.Interface_Font_Size := Positive'Value(Get(SpinBox));
         for FontName of InterfaceFonts loop
            Font.Configure
              (To_String(FontName),
               "-size" & Positive'Image(Game_Settings.Interface_Font_Size));
         end loop;
      end if;
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
      FontNames: constant array(1 .. 3) of Unbounded_String :=
        (To_Unbounded_String("MapFont"), To_Unbounded_String("InterfaceFont"),
         To_Unbounded_String("HelpFont"));
   begin
      SpinBox.Interp := Interp;
      Set_Default_Fonts_Loop :
      for I in SpinBoxNames'Range loop
         SpinBox.Name :=
           New_String
             (".gameframe.paned.optionsframe.canvas.options.interface." &
              To_String(SpinBoxNames(I)) & "font");
         Set(SpinBox, Positive'Image(DefaultFontsSizes(I)));
         Font.Configure
           (To_String(FontNames(I)),
            "-size" & Positive'Image(DefaultFontsSizes(I)));
      end loop Set_Default_Fonts_Loop;
      Font.Configure
        ("InterfaceIcons", "-size" & Positive'Image(DefaultFontsSizes(2)));
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
      KeysFile: File_Type;
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
        ShipSpeed'Val(Get_Combobox_Value(".general.speed") + 1);
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
      SetTheme;
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
      Save_Config;
      KeyEntry.Interp := Interp;
      Set_Accelerators_Loop :
      for I in 1 .. (Accels'Last - 1) loop
         Unbind_From_Main_Window
           (Interp, "<" & To_String(Accels(I).ShortCut) & ">");
         KeyEntry.Name :=
           New_String(RootName & To_String(Accels(I).EntryName));
         if I < 12 then
            MenuAccelerators(I) := To_Unbounded_String(Get(KeyEntry));
            Bind_To_Main_Window
              (Get_Context, "<" & To_String(MenuAccelerators(I)) & ">",
               "{InvokeMenu " & To_String(MenuAccelerators(I)) & "}");
         else
            MapAccelerators(I - 11) := To_Unbounded_String(Get(KeyEntry));
         end if;
      end loop Set_Accelerators_Loop;
      Unbind_From_Main_Window
        (Interp, "<" & To_String(Accels(Accels'Last).ShortCut) & ">");
      KeyEntry.Name :=
        New_String(RootName & To_String(Accels(Accels'Last).EntryName));
      FullScreenAccel := To_Unbounded_String(Get(KeyEntry));
      Create(KeysFile, Append_File, To_String(Save_Directory) & "keys.cfg");
      Save_Menu_Accelerators_Loop :
      for Key of MenuAccelerators loop
         Put_Line(KeysFile, To_String(Key));
      end loop Save_Menu_Accelerators_Loop;
      Save_Map_Accelerators_Loop :
      for Key of MapAccelerators loop
         Put_Line(KeysFile, To_String(Key));
      end loop Save_Map_Accelerators_Loop;
      Put_Line(KeysFile, To_String(FullScreenAccel));
      Close(KeysFile);
      SetKeys;
      if CArgv.Arg(Argv, 1) = "map" then
         CreateGameMenu;
         ShowSkyMap(True);
      else
         ShowCombatUI(False);
      end if;
      return TCL_OK;
   end Close_Options_Command;

   -- ****o* GameOptions/GameOptions.Show_Options_Tab_Command
   -- FUNCTION
   -- Show the selected options tab
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowOptionsTab
   -- SOURCE
   function Show_Options_Tab_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Options_Tab_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      OptionsCanvas: constant Tk_Canvas :=
        Get_Widget(Main_Paned & ".optionsframe.canvas", Interp);
      OptionsFrame: constant Ttk_Frame :=
        Get_Widget(OptionsCanvas & ".options", Interp);
      Frame: constant Ttk_Frame :=
        Get_Widget(OptionsFrame & "." & Tcl_GetVar(Interp, "newtab"));
      OldFrame: constant Ttk_Frame :=
        Get_Widget(Tcl.Tk.Ada.Grid.Grid_Slaves(OptionsFrame, "-row 1"));
   begin
      Tcl.Tk.Ada.Grid.Grid_Remove(OldFrame);
      Tcl.Tk.Ada.Grid.Grid(Frame, "-sticky nwes -padx 10");
      Tcl_Eval(Interp, "update");
      configure
        (OptionsCanvas,
         "-scrollregion [list " & BBox(OptionsCanvas, "all") & "]");
      return TCL_OK;
   end Show_Options_Tab_Command;

   procedure AddCommands is
   begin
      AddCommand("ShowOptions", Show_Options_Command'Access);
      AddCommand("SetFonts", Set_Fonts_Command'Access);
      AddCommand("SetDefaultFonts", Set_Default_Fonts_Command'Access);
      AddCommand("CloseOptions", Close_Options_Command'Access);
      AddCommand("ShowOptionsTab", Show_Options_Tab_Command'Access);
   end AddCommands;

end GameOptions;
