-- Copyright (c) 2020 Bartek thindil Jasicki <thindil@laeran.pl>
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
with Game; use Game;
with Maps.UI; use Maps.UI;
with Ships; use Ships;
with Utils.UI; use Utils.UI;

package body GameOptions is

   -- ****it* GameOptions/Accel_Data
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
   -- ****iv* GameOptions/Accels
   -- FUNCTION
   -- Array with data to show keyboard shortcuts
   -- SOURCE
   Accels: constant array(1 .. 54) of Accel_Data :=
     (1 => (MenuAccelerators(1), To_Unbounded_String(".menu.shipinfo")),
      2 => (MenuAccelerators(2), To_Unbounded_String(".menu.cargo")),
      3 => (MenuAccelerators(3), To_Unbounded_String(".menu.crew")),
      4 => (MenuAccelerators(4), To_Unbounded_String(".menu.orders")),
      5 => (MenuAccelerators(5), To_Unbounded_String(".menu.crafts")),
      6 => (MenuAccelerators(6), To_Unbounded_String(".menu.messages")),
      7 => (MenuAccelerators(7), To_Unbounded_String(".menu.bases")),
      8 => (MenuAccelerators(8), To_Unbounded_String(".menu.events")),
      9 => (MenuAccelerators(9), To_Unbounded_String(".menu.missions")),
      10 => (MenuAccelerators(10), To_Unbounded_String(".menu.stories")),
      11 => (MenuAccelerators(11), To_Unbounded_String(".menu.waitorders")),
      12 => (MenuAccelerators(12), To_Unbounded_String(".menu.gamestats")),
      13 => (MenuAccelerators(13), To_Unbounded_String(".menu.help")),
      14 => (MenuAccelerators(14), To_Unbounded_String(".menu.gameoptions")),
      15 => (MenuAccelerators(15), To_Unbounded_String(".menu.quit")),
      16 => (MenuAccelerators(16), To_Unbounded_String(".menu.resign")),
      17 => (MapAccelerators(1), To_Unbounded_String(".menu.menu")),
      18 => (MapAccelerators(2), To_Unbounded_String(".map.mapoptions")),
      19 => (MapAccelerators(3), To_Unbounded_String(".map.zoomin")),
      20 => (MapAccelerators(4), To_Unbounded_String(".map.zoomout")),
      21 => (MapAccelerators(15), To_Unbounded_String(".map.center")),
      22 => (MapAccelerators(16), To_Unbounded_String(".map.centerhomebase")),
      23 => (MapAccelerators(17), To_Unbounded_String(".map.mapupleft")),
      24 => (MapAccelerators(18), To_Unbounded_String(".map.mapup")),
      25 => (MapAccelerators(19), To_Unbounded_String(".map.mapupright")),
      26 => (MapAccelerators(20), To_Unbounded_String(".map.mapleft")),
      27 => (MapAccelerators(21), To_Unbounded_String(".map.mapright")),
      28 => (MapAccelerators(22), To_Unbounded_String(".map.mapdownleft")),
      29 => (MapAccelerators(23), To_Unbounded_String(".map.mapdown")),
      30 => (MapAccelerators(24), To_Unbounded_String(".map.mapdownright")),
      31 => (MapAccelerators(25), To_Unbounded_String(".map.cursorupleft")),
      32 => (MapAccelerators(26), To_Unbounded_String(".map.cursorup")),
      33 => (MapAccelerators(27), To_Unbounded_String(".map.cursorupright")),
      34 => (MapAccelerators(28), To_Unbounded_String(".map.cursorleft")),
      35 => (MapAccelerators(29), To_Unbounded_String(".map.cursorright")),
      36 => (MapAccelerators(30), To_Unbounded_String(".map.cursordownleft")),
      37 => (MapAccelerators(31), To_Unbounded_String(".map.cursordown")),
      38 => (MapAccelerators(32), To_Unbounded_String(".map.cursordownright")),
      39 => (MapAccelerators(33), To_Unbounded_String(".map.clickmouse")),
      40 => (MapAccelerators(5), To_Unbounded_String(".movement.upleft")),
      41 => (MapAccelerators(6), To_Unbounded_String(".movement.up")),
      42 => (MapAccelerators(7), To_Unbounded_String(".movement.upright")),
      43 => (MapAccelerators(8), To_Unbounded_String(".movement.left")),
      44 => (MapAccelerators(9), To_Unbounded_String(".movement.right")),
      45 => (MapAccelerators(10), To_Unbounded_String(".movement.wait")),
      46 => (MapAccelerators(11), To_Unbounded_String(".movement.downleft")),
      47 => (MapAccelerators(12), To_Unbounded_String(".movement.down")),
      48 => (MapAccelerators(13), To_Unbounded_String(".movement.downright")),
      49 => (MapAccelerators(14), To_Unbounded_String(".movement.moveto")),
      50 => (MapAccelerators(34), To_Unbounded_String(".movement.fullstop")),
      51 =>
        (MapAccelerators(35), To_Unbounded_String(".movement.quarterspeed")),
      52 => (MapAccelerators(36), To_Unbounded_String(".movement.halfspeed")),
      53 => (MapAccelerators(37), To_Unbounded_String(".movement.fullspeed")),
      54 =>
        (FullScreenAccel, To_Unbounded_String(".interface.fullscreenkey")));

   -- ****f* GameOptions/Show_Options_Command
   -- FUNCTION
   -- Show the game options to the player
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- SOURCE
   function Show_Options_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Options_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      Paned: Ttk_PanedWindow;
      OptionsCanvas: Tk_Canvas;
      OptionsFrame: Ttk_Frame;
      CloseButton: Ttk_Button;
      Label: Ttk_Label;
      ComboBox: Ttk_ComboBox;
      SpinBox: Ttk_SpinBox;
      KeyEntry: Ttk_Entry;
   begin
      Paned.Interp := Interp;
      Paned.Name := New_String(".paned");
      CloseButton.Interp := Interp;
      CloseButton.Name := New_String(".header.closebutton");
      OptionsFrame.Interp := Interp;
      OptionsFrame.Name := New_String(Widget_Image(Paned) & ".optionsframe");
      OptionsCanvas.Interp := Interp;
      OptionsCanvas.Name := New_String(Widget_Image(OptionsFrame) & ".canvas");
      Label.Interp := Interp;
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
      ComboBox.Interp := Interp;
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
      SpinBox.Interp := Interp;
      SpinBox.Name := New_String(Widget_Image(OptionsFrame) & ".fuel");
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
      Tcl_SetVar
        (Interp, Widget_Image(OptionsFrame) & ".animations",
         Trim(Natural'Image(GameSettings.AnimationsEnabled), Left));
      ComboBox.Name :=
        New_String(Widget_Image(OptionsFrame) & ".animationtype");
      Current(ComboBox, Natural'Image(GameSettings.AnimationType - 1));
      ComboBox.Name := New_String(Widget_Image(OptionsFrame) & ".theme");
      Set(ComboBox, To_String(GameSettings.InterfaceTheme));
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
      for Accel of Accels loop
         KeyEntry.Name :=
           New_String(Widget_Image(OptionsFrame) & To_String(Accel.EntryName));
         Delete(KeyEntry, "0", "end");
         Insert(KeyEntry, "0", To_String(Accel.ShortCut));
      end loop;
      configure(CloseButton, "-command CloseOptions");
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
         "[expr " & Winfo_Get(OptionsFrame, "reqwidth") & " / 2] [expr " &
         Winfo_Get(OptionsFrame, "reqheight") & " / 2] -window " &
         Widget_Image(OptionsFrame));
      Tcl_Eval(Get_Context, "update");
      configure
        (OptionsCanvas,
         "-scrollregion [list " & BBox(OptionsCanvas, "all") & "]");
      ShowScreen("optionsframe");
      return TCL_OK;
   end Show_Options_Command;

   -- ****f* GameOptions/Set_Fonts_Command
   -- FUNCTION
   -- Set the selected font
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- SOURCE
   function Set_Fonts_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Fonts_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      SpinBox: Ttk_SpinBox;
   begin
      SpinBox.Interp := Interp;
      SpinBox.Name := New_String(CArgv.Arg(Argv, 1));
      if CArgv.Arg(Argv, 1) =
        ".paned.optionsframe.canvas.options.notebook.interface.mapfont" then
         GameSettings.MapFontSize := Positive'Value(Get(SpinBox));
         Font.Configure
           ("MapFont", "-size" & Positive'Image(GameSettings.MapFontSize));
      elsif CArgv.Arg(Argv, 1) =
        ".paned.optionsframe.canvas.options.notebook.interface.helpfont" then
         GameSettings.HelpFontSize := Positive'Value(Get(SpinBox));
         Font.Configure
           ("HelpFont", "-size" & Positive'Image(GameSettings.HelpFontSize));
      else
         GameSettings.InterfaceFontSize := Positive'Value(Get(SpinBox));
         Font.Configure
           ("InterfaceFont",
            "-size" & Positive'Image(GameSettings.InterfaceFontSize));
      end if;
      return TCL_OK;
   end Set_Fonts_Command;

   -- ****f* GameOptions/Set_Default_Fonts_Command
   -- FUNCTION
   -- Set the default values for fonts
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- SOURCE
   function Set_Default_Fonts_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Default_Fonts_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
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
             (".paned.optionsframe.canvas.options.notebook.interface." &
              To_String(SpinBoxNames(I)) & "font");
         Set(SpinBox, Positive'Image(DefaultFontsSizes(I)));
         Font.Configure
           (To_String(FontNames(I)),
            "-size" & Positive'Image(DefaultFontsSizes(I)));
      end loop;
      return TCL_OK;
   end Set_Default_Fonts_Command;

   -- ****f* GameOptions/Close_Options_Command
   -- FUNCTION
   -- Save all options and back to the map
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- SOURCE
   function Close_Options_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Close_Options_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      CloseButton: Ttk_Button;
      RootName: constant String :=
        ".paned.optionsframe.canvas.options.notebook";
      ComboBox: Ttk_ComboBox;
      SpinBox: Ttk_SpinBox;
   begin
      CloseButton.Interp := Interp;
      CloseButton.Name := New_String(".header.closebutton");
      configure(CloseButton, "-command ShowSkyMap");
      Tcl.Tk.Ada.Grid.Grid_Remove(CloseButton);
      if Tcl_GetVar(Interp, RootName & ".general.autorest") = "1" then
         GameSettings.AutoRest := True;
      else
         GameSettings.AutoRest := False;
      end if;
      ComboBox.Interp := Interp;
      ComboBox.Name := New_String(RootName & ".general.speed");
      GameSettings.UndockSpeed :=
        ShipSpeed'Val(Natural'Value(Current(ComboBox)) + 1);
      if Tcl_GetVar(Interp, RootName & ".general.autocenter") = "1" then
         GameSettings.AutoCenter := True;
      else
         GameSettings.AutoCenter := False;
      end if;
      if Tcl_GetVar(Interp, RootName & ".general.autoreturn") = "1" then
         GameSettings.AutoReturn := True;
      else
         GameSettings.AutoReturn := False;
      end if;
      if Tcl_GetVar(Interp, RootName & ".general.autofinish") = "1" then
         GameSettings.AutoFinish := True;
      else
         GameSettings.AutoFinish := False;
      end if;
      if Tcl_GetVar(Interp, RootName & ".general.autoaskforbases") = "1" then
         GameSettings.AutoAskForBases := True;
      else
         GameSettings.AutoAskForBases := False;
      end if;
      if Tcl_GetVar(Interp, RootName & ".general.autoaskforevents") = "1" then
         GameSettings.AutoAskForEvents := True;
      else
         GameSettings.AutoAskForEvents := False;
      end if;
      SpinBox.Interp := Interp;
      SpinBox.Name := New_String(RootName & ".general.fuel");
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
      if Tcl_GetVar(Interp, RootName & ".interface.animations") = "1" then
         GameSettings.AnimationsEnabled := 1;
      else
         GameSettings.AnimationsEnabled := 0;
      end if;
      ComboBox.Name := New_String(RootName & ".interface.animationtype");
      GameSettings.AnimationType :=
        Natural'Value(Current(ComboBox)) + 1;
      ComboBox.Name := New_String(RootName & ".interface.theme");
      GameSettings.InterfaceTheme := To_Unbounded_String(Get(ComboBox));
      Theme_Use(To_String(GameSettings.InterfaceTheme));
      if Tcl_GetVar(Interp, RootName & ".interface.showtooltips") = "1" then
         GameSettings.ShowTooltips := True;
         Enable;
      else
         GameSettings.ShowTooltips := False;
         Disable;
      end if;
      if Tcl_GetVar(Interp, RootName & ".interface.showmessages") = "1" then
         GameSettings.ShowLastMessages := True;
      else
         GameSettings.ShowLastMessages := False;
      end if;
      if Tcl_GetVar(Interp, RootName & ".interface.fullscreen") = "1" then
         GameSettings.FullScreen := True;
         Wm_Set(Get_Main_Window(Interp), "attributes", "-fullscreen 1");
      else
         GameSettings.FullScreen := False;
         Wm_Set(Get_Main_Window(Interp), "attributes", "-fullscreen 0");
      end if;
      SaveConfig;
      ShowSkyMap(True);
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
