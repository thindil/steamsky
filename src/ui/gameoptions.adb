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

with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
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
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkEntry; use Tcl.Tk.Ada.Widgets.TtkEntry;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkPanedWindow; use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Tcl.Tk.Ada.Wm; use Tcl.Tk.Ada.Wm;
with Tcl.Tklib.Ada.Tooltip; use Tcl.Tklib.Ada.Tooltip;
with Config; use Config;
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
      Paned: constant Ttk_PanedWindow :=
        Get_Widget(".gameframe.paned", Interp);
      OptionsFrame: Ttk_Frame := Get_Widget(Paned & ".optionsframe", Interp);
      OptionsCanvas: constant Tk_Canvas :=
        Get_Widget(OptionsFrame & ".canvas", Interp);
      CloseButton: constant Ttk_Button :=
        Get_Widget(".gameframe.header.closebutton", Interp);
      Label: Ttk_Label;
      ComboBox: Ttk_ComboBox;
      SpinBox: Ttk_SpinBox;
      KeyEntry: Ttk_Entry;
      ThemesList: Unbounded_String;
   begin
      Label.Interp := Interp;
      ComboBox.Interp := Interp;
      if Winfo_Get(OptionsCanvas, "exists") = "0" then
         Tcl_EvalFile
           (Get_Context,
            To_String(DataDirectory) & "ui" & Dir_Separator & "options.tcl");
         Bind(OptionsFrame, "<Configure>", "{ResizeCanvas %W.canvas %w %h}");
         Label.Name :=
           New_String
             (Widget_Image(OptionsCanvas) & ".options.notebook.info.data");
         configure(Label, "-text {" & To_String(DataDirectory) & "}");
         Label.Name :=
           New_String
             (Widget_Image(OptionsCanvas) & ".options.notebook.info.save");
         configure(Label, "-text {" & To_String(SaveDirectory) & "}");
         Label.Name :=
           New_String
             (Widget_Image(OptionsCanvas) & ".options.notebook.info.docs");
         configure(Label, "-text {" & To_String(DocDirectory) & "}");
         Label.Name :=
           New_String
             (Widget_Image(OptionsCanvas) & ".options.notebook.info.mods");
         configure(Label, "-text {" & To_String(ModsDirectory) & "}");
         for Theme of Themes_List loop
            Append(ThemesList, " {" & Theme.Name & "}");
         end loop;
         ComboBox.Name :=
           New_String
             (".gameframe.paned.optionsframe.canvas.options.notebook.interface.theme");
         configure(ComboBox, "-values [list" & To_String(ThemesList) & "]");
      elsif Winfo_Get(OptionsCanvas, "ismapped") = "1" then
         Tcl.Tk.Ada.Grid.Grid_Remove(CloseButton);
         Entry_Configure(GameMenu, "Help", "-command {ShowHelp general}");
         ShowSkyMap(True);
         return TCL_OK;
      end if;
      Entry_Configure(GameMenu, "Help", "-command {ShowHelp general}");
      OptionsFrame.Name :=
        New_String(Widget_Image(OptionsCanvas) & ".options.notebook.general");
      Tcl_SetVar
        (Interp, Widget_Image(OptionsFrame) & ".autorest",
         Trim(Natural'Image(Boolean'Pos(GameSettings.AutoRest)), Left));
      ComboBox.Name := New_String(Widget_Image(OptionsFrame) & ".speed");
      Current
        (ComboBox, Natural'Image(ShipSpeed'Pos(GameSettings.UndockSpeed) - 1));
      Tcl_SetVar
        (Interp, Widget_Image(OptionsFrame) & ".autocenter",
         Trim(Natural'Image(Boolean'Pos(GameSettings.AutoCenter)), Left));
      Tcl_SetVar
        (Interp, Widget_Image(OptionsFrame) & ".autoreturn",
         Trim(Natural'Image(Boolean'Pos(GameSettings.AutoReturn)), Left));
      Tcl_SetVar
        (Interp, Widget_Image(OptionsFrame) & ".autofinish",
         Trim(Natural'Image(Boolean'Pos(GameSettings.AutoFinish)), Left));
      Tcl_SetVar
        (Interp, Widget_Image(OptionsFrame) & ".autoaskforbases",
         Trim(Natural'Image(Boolean'Pos(GameSettings.AutoAskForBases)), Left));
      Tcl_SetVar
        (Interp, Widget_Image(OptionsFrame) & ".autoaskforevents",
         Trim
           (Natural'Image(Boolean'Pos(GameSettings.AutoAskForEvents)), Left));
      SpinBox := Get_Widget(Widget_Image(OptionsFrame) & ".fuel", Interp);
      Set(SpinBox, Natural'Image(GameSettings.LowFuel));
      SpinBox.Name := New_String(Widget_Image(OptionsFrame) & ".drinks");
      Set(SpinBox, Natural'Image(GameSettings.LowDrinks));
      SpinBox.Name := New_String(Widget_Image(OptionsFrame) & ".food");
      Set(SpinBox, Natural'Image(GameSettings.LowFood));
      ComboBox.Name :=
        New_String(Widget_Image(OptionsFrame) & ".automovestop");
      Current
        (ComboBox,
         Natural'Image(AutoMoveBreak'Pos(GameSettings.AutoMoveStop)));
      SpinBox.Name :=
        New_String(Widget_Image(OptionsFrame) & ".messageslimit");
      Set(SpinBox, Natural'Image(GameSettings.MessagesLimit));
      SpinBox.Name :=
        New_String(Widget_Image(OptionsFrame) & ".savedmessages");
      Set(SpinBox, Natural'Image(GameSettings.SavedMessages));
      ComboBox.Name :=
        New_String(Widget_Image(OptionsFrame) & ".messagesorder");
      Current
        (ComboBox,
         Natural'Image(MessagesOrderType'Pos(GameSettings.MessagesOrder)));
      ComboBox.Name := New_String(Widget_Image(OptionsFrame) & ".autosave");
      Current
        (ComboBox, Natural'Image(AutoSaveType'Pos(GameSettings.AutoSave)));
      OptionsFrame.Name :=
        New_String
          (Widget_Image(OptionsCanvas) & ".options.notebook.interface");
      ComboBox.Name := New_String(Widget_Image(OptionsFrame) & ".theme");
      Set
        (ComboBox,
         "{" &
         To_String(Themes_List(To_String(GameSettings.InterfaceTheme)).Name) &
         "}");
      Tcl_SetVar
        (Interp, Widget_Image(OptionsFrame) & ".showtooltips",
         Trim(Natural'Image(Boolean'Pos(GameSettings.ShowTooltips)), Left));
      Tcl_SetVar
        (Interp, Widget_Image(OptionsFrame) & ".showmessages",
         Trim
           (Natural'Image(Boolean'Pos(GameSettings.ShowLastMessages)), Left));
      Tcl_SetVar
        (Interp, Widget_Image(OptionsFrame) & ".fullscreen",
         Trim(Natural'Image(Boolean'Pos(GameSettings.FullScreen)), Left));
      SpinBox.Name :=
        New_String(Widget_Image(OptionsFrame) & ".closemessages");
      Set(SpinBox, Natural'Image(GameSettings.AutoCloseMessagesTime));
      Tcl_SetVar
        (Interp, Widget_Image(OptionsFrame) & ".shownumbers",
         Trim(Natural'Image(Boolean'Pos(GameSettings.ShowNumbers)), Left));
      SpinBox.Name := New_String(Widget_Image(OptionsFrame) & ".mapfont");
      Set(SpinBox, Natural'Image(GameSettings.MapFontSize));
      SpinBox.Name := New_String(Widget_Image(OptionsFrame) & ".helpfont");
      Set(SpinBox, Natural'Image(GameSettings.HelpFontSize));
      SpinBox.Name :=
        New_String(Widget_Image(OptionsFrame) & ".interfacefont");
      Set(SpinBox, Natural'Image(GameSettings.InterfaceFontSize));
      KeyEntry.Interp := Interp;
      OptionsFrame.Name :=
        New_String(Widget_Image(OptionsCanvas) & ".options.notebook");
      for I in MenuAccelerators'Range loop
         Accels(I).ShortCut := MenuAccelerators(I);
      end loop;
      for I in MapAccelerators'Range loop
         Accels(I + 11).ShortCut := MapAccelerators(I);
      end loop;
      Accels(Accels'Last).ShortCut := FullScreenAccel;
      for Accel of Accels loop
         KeyEntry.Name :=
           New_String(Widget_Image(OptionsFrame) & To_String(Accel.EntryName));
         Delete(KeyEntry, "0", "end");
         Insert(KeyEntry, "0", To_String(Accel.ShortCut));
      end loop;
      if cget(CloseButton, "-command") = "ShowCombatUI" then
         configure(CloseButton, "-command {CloseOptions combat}");
      else
         configure(CloseButton, "-command {CloseOptions map}");
      end if;
      Tcl.Tk.Ada.Grid.Grid(CloseButton, "-row 0 -column 1");
      OptionsFrame.Name :=
        New_String(Widget_Image(OptionsCanvas) & ".options");
      configure
        (OptionsCanvas,
         "-height " & cget(Paned, "-height") & " -width " &
         cget(Paned, "-width"));
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
      if CArgv.Arg(Argv, 1) =
        ".gameframe.paned.optionsframe.canvas.options.notebook.interface.mapfont" then
         GameSettings.MapFontSize := Positive'Value(Get(SpinBox));
         Font.Configure
           ("MapFont", "-size" & Positive'Image(GameSettings.MapFontSize));
      elsif CArgv.Arg(Argv, 1) =
        ".gameframe.paned.optionsframe.canvas.options.notebook.interface.helpfont" then
         GameSettings.HelpFontSize := Positive'Value(Get(SpinBox));
         for FontName of HelpFonts loop
            Font.Configure
              (To_String(FontName),
               "-size" & Positive'Image(GameSettings.HelpFontSize));
         end loop;
      else
         GameSettings.InterfaceFontSize := Positive'Value(Get(SpinBox));
         for FontName of InterfaceFonts loop
            Font.Configure
              (To_String(FontName),
               "-size" & Positive'Image(GameSettings.InterfaceFontSize));
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
      for I in SpinBoxNames'Range loop
         SpinBox.Name :=
           New_String
             (".gameframe.paned.optionsframe.canvas.options.notebook.interface." &
              To_String(SpinBoxNames(I)) & "font");
         Set(SpinBox, Positive'Image(DefaultFontsSizes(I)));
         Font.Configure
           (To_String(FontNames(I)),
            "-size" & Positive'Image(DefaultFontsSizes(I)));
      end loop;
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
      CloseButton: constant Ttk_Button :=
        Get_Widget(".gameframe.header.closebutton", Interp);
      RootName: constant String :=
        ".gameframe.paned.optionsframe.canvas.options.notebook";
      ComboBox: Ttk_ComboBox :=
        Get_Widget(RootName & ".general.speed", Interp);
      SpinBox: Ttk_SpinBox := Get_Widget(RootName & ".general.fuel", Interp);
      KeyEntry: Ttk_Entry;
      KeysFile: File_Type;
   begin
      configure(CloseButton, "-command ShowSkyMap");
      Tcl.Tk.Ada.Grid.Grid_Remove(CloseButton);
      if Tcl_GetVar(Interp, RootName & ".general.autorest") = "1" then
         GameSettings.AutoRest := True;
      else
         GameSettings.AutoRest := False;
      end if;
      GameSettings.UndockSpeed :=
        ShipSpeed'Val(Natural'Value(Current(ComboBox)) + 1);
      GameSettings.AutoCenter :=
        (if Tcl_GetVar(Interp, RootName & ".general.autocenter") = "1" then
           True
         else False);
      GameSettings.AutoReturn :=
        (if Tcl_GetVar(Interp, RootName & ".general.autoreturn") = "1" then
           True
         else False);
      GameSettings.AutoFinish :=
        (if Tcl_GetVar(Interp, RootName & ".general.autofinish") = "1" then
           True
         else False);
      GameSettings.AutoAskForBases :=
        (if Tcl_GetVar(Interp, RootName & ".general.autoaskforbases") = "1"
         then True
         else False);
      GameSettings.AutoAskForEvents :=
        (if Tcl_GetVar(Interp, RootName & ".general.autoaskforevents") = "1"
         then True
         else False);
      GameSettings.LowFuel := Positive'Value(Get(SpinBox));
      SpinBox.Name := New_String(RootName & ".general.drinks");
      GameSettings.LowDrinks := Positive'Value(Get(SpinBox));
      SpinBox.Name := New_String(RootName & ".general.food");
      GameSettings.LowFood := Positive'Value(Get(SpinBox));
      ComboBox.Name := New_String(RootName & ".general.automovestop");
      GameSettings.AutoMoveStop :=
        AutoMoveBreak'Val(Natural'Value(Current(ComboBox)));
      SpinBox.Name := New_String(RootName & ".general.messageslimit");
      GameSettings.MessagesLimit := Positive'Value(Get(SpinBox));
      SpinBox.Name := New_String(RootName & ".general.savedmessages");
      GameSettings.SavedMessages := Positive'Value(Get(SpinBox));
      ComboBox.Name := New_String(RootName & ".general.messagesorder");
      GameSettings.MessagesOrder :=
        MessagesOrderType'Val(Natural'Value(Current(ComboBox)));
      ComboBox.Name := New_String(RootName & ".general.autosave");
      GameSettings.AutoSave :=
        AutoSaveType'Val(Natural'Value(Current(ComboBox)));
      ComboBox.Name := New_String(RootName & ".interface.theme");
      for I in Themes_List.Iterate loop
         if Themes_List(I).Name = Get(ComboBox) then
            GameSettings.InterfaceTheme :=
              To_Unbounded_String(Themes_Container.Key(I));
            exit;
         end if;
      end loop;
      Theme_Use(To_String(GameSettings.InterfaceTheme));
      SetTheme;
      if Tcl_GetVar(Interp, RootName & ".interface.showtooltips") = "1" then
         GameSettings.ShowTooltips := True;
         Enable;
      else
         GameSettings.ShowTooltips := False;
         Disable;
      end if;
      GameSettings.ShowLastMessages :=
        (if Tcl_GetVar(Interp, RootName & ".interface.showmessages") = "1" then
           True
         else False);
      if Tcl_GetVar(Interp, RootName & ".interface.fullscreen") = "1" then
         GameSettings.FullScreen := True;
         Wm_Set(Get_Main_Window(Interp), "attributes", "-fullscreen 1");
      else
         GameSettings.FullScreen := False;
         Wm_Set(Get_Main_Window(Interp), "attributes", "-fullscreen 0");
      end if;
      SpinBox.Name := New_String(RootName & ".interface.closemessages");
      GameSettings.AutoCloseMessagesTime := Positive'Value(Get(SpinBox));
      GameSettings.ShowNumbers :=
        (if Tcl_GetVar(Interp, RootName & ".interface.shownumbers") = "1" then
           True
         else False);
      SpinBox.Name := New_String(RootName & ".interface.mapfont");
      GameSettings.MapFontSize := Positive'Value(Get(SpinBox));
      SpinBox.Name := New_String(RootName & ".interface.helpfont");
      GameSettings.HelpFontSize := Positive'Value(Get(SpinBox));
      SpinBox.Name := New_String(RootName & ".interface.interfacefont");
      GameSettings.InterfaceFontSize := Positive'Value(Get(SpinBox));
      SaveConfig;
      KeyEntry.Interp := Interp;
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
      end loop;
      Unbind_From_Main_Window
        (Interp, "<" & To_String(Accels(Accels'Last).ShortCut) & ">");
      KeyEntry.Name :=
        New_String(RootName & To_String(Accels(Accels'Last).EntryName));
      FullScreenAccel := To_Unbounded_String(Get(KeyEntry));
      Create(KeysFile, Append_File, To_String(SaveDirectory) & "keys.cfg");
      for Key of MenuAccelerators loop
         Put_Line(KeysFile, To_String(Key));
      end loop;
      for Key of MapAccelerators loop
         Put_Line(KeysFile, To_String(Key));
      end loop;
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

   procedure AddCommands is
   begin
      AddCommand("ShowOptions", Show_Options_Command'Access);
      AddCommand("SetFonts", Set_Fonts_Command'Access);
      AddCommand("SetDefaultFonts", Set_Default_Fonts_Command'Access);
      AddCommand("CloseOptions", Close_Options_Command'Access);
   end AddCommands;

end GameOptions;
