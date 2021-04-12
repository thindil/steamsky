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

with Ada.Containers; use Ada.Containers;
with Ada.Directories; use Ada.Directories;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Event; use Tcl.Tk.Ada.Event;
with Tcl.Tk.Ada.Font;
with Tcl.Tk.Ada.Image.Photo; use Tcl.Tk.Ada.Image.Photo;
with Tcl.Tk.Ada.Pack;
with Tcl.Tk.Ada.TtkStyle; use Tcl.Tk.Ada.TtkStyle;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Toplevel; use Tcl.Tk.Ada.Widgets.Toplevel;
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
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Tcl.Tk.Ada.Wm; use Tcl.Tk.Ada.Wm;
with Tcl.Tklib.Ada.Tooltip; use Tcl.Tklib.Ada.Tooltip;
with BasesTypes; use BasesTypes;
with Careers; use Careers;
with Config; use Config;
with Factions; use Factions;
with Game; use Game;
with Goals.UI;
with MainMenu.Commands;
with Maps.UI; use Maps.UI;
with Themes; use Themes;
with Utils.UI; use Utils.UI;

package body MainMenu is

   -- ****iv* MainMenu/MainMenuFrame
   -- FUNCTION
   -- Ttk Frame with content of main menu
   -- SOURCE
   MainMenuFrame: Ttk_Frame;
   -- ****

   -- ****iv* MainMenu/DataError
   -- FUNCTION
   -- Stores error message from loading the game data
   -- SOURCE
   DataError: Unbounded_String;
   -- ****

   procedure CreateMainMenu is
      UI_Directory: constant String :=
        To_String(Data_Directory) & "ui" & Dir_Separator;
      MainWindow: constant Tk_Toplevel := Get_Main_Window(Get_Context);
      Icon: constant Tk_Photo :=
        Create
          ("logo",
           "-file {" & UI_Directory & Dir_Separator & "images" &
           Dir_Separator & "icon.png}");
      TextEntry: Ttk_Entry :=
        Get_Widget(".newgamemenu.canvas.player.playername");
      ComboBox: Ttk_ComboBox :=
        Get_Widget(".newgamemenu.canvas.player.faction");
      Values: Unbounded_String;
      SpinBox: Ttk_SpinBox :=
        Get_Widget(".newgamemenu.canvas.difficulty.enemydamage");
      VersionLabel: constant Ttk_Label := Get_Widget(".mainmenu.version");
   begin
      MainMenu.Commands.AddCommands;
      Utils.UI.AddCommands;
      Goals.UI.AddCommands;
      Wm_Set(MainWindow, "iconphoto", "-default " & Icon);
      Load_Theme_Loop :
      for I in Themes_List.Iterate loop
         if Themes_Container.Key(I) = Game_Settings.Interface_Theme then
            Tcl_EvalFile(Get_Context, To_String(Themes_List(I).FileName));
            exit Load_Theme_Loop;
         end if;
      end loop Load_Theme_Loop;
      Theme_Use(To_String(Game_Settings.Interface_Theme));
      Tcl_EvalFile(Get_Context, UI_Directory & "mainmenu.tcl");
      MainMenuFrame.Interp := Get_Context;
      MainMenuFrame.Name := New_String(".mainmenu");
      if not Game_Settings.Show_Tooltips then
         Disable;
      end if;
      DefaultFontsSizes :=
        (Positive'Value(Font.Configure("MapFont", "-size")),
         Positive'Value(Font.Configure("InterfaceFont", "-size")),
         Positive'Value(Font.Configure("HelpFont", "-size")));
      Font.Configure
        ("MapFont", "-size" & Positive'Image(Game_Settings.Map_Font_Size));
      Font.Configure
        ("HelpFont", "-size" & Positive'Image(Game_Settings.Help_Font_Size));
      Font.Configure
        ("InterfaceFont",
         "-size" & Positive'Image(Game_Settings.Interface_Font_Size));
      DataError := To_Unbounded_String(Load_Game_Data);
      if DataError /= Null_Unbounded_String then
         ShowMainMenu;
         return;
      end if;
      configure(VersionLabel, "-text {" & Game_Version & "}");
      Delete(TextEntry, "0", "end");
      Insert(TextEntry, "0", To_String(New_Game_Settings.Player_Name));
      Tcl_SetVar
        (Get_Context, "playergender", "" & New_Game_Settings.Player_Gender);
      TextEntry.Name := New_String(".newgamemenu.canvas.player.shipname");
      Delete(TextEntry, "0", "end");
      Insert(TextEntry, "0", To_String(New_Game_Settings.Ship_Name));
      Load_Factions_Names_Loop :
      for I in Factions_List.Iterate loop
         if Factions_List(I).Careers.Length > 0 then
            Append(Values, " {" & Factions_List(I).Name & "}");
         end if;
      end loop Load_Factions_Names_Loop;
      Append(Values, " Random");
      configure(ComboBox, "-values [list" & To_String(Values) & "]");
      Set
        (ComboBox,
         To_String(Factions_List(New_Game_Settings.Player_Faction).Name));
      Tcl_Eval(Get_Context, "SetFaction");
      ComboBox.Name := New_String(".newgamemenu.canvas.player.career");
      Set
        (ComboBox,
         To_String(Careers_List(New_Game_Settings.Player_Career).Name));
      ComboBox.Name := New_String(".newgamemenu.canvas.player.base");
      if New_Game_Settings.Starting_Base /= To_Unbounded_String("Any") then
         Set
           (ComboBox,
            "{" &
            To_String(BasesTypes_List(New_Game_Settings.Starting_Base).Name) &
            "}");
      else
         Set(ComboBox, "Any");
      end if;
      ComboBox.Name :=
        New_String(".newgamemenu.canvas.difficulty.difficultylevel");
      Set
        (SpinBox,
         Natural'Image(Natural(New_Game_Settings.Enemy_Damage_Bonus * 100.0)));
      SpinBox.Name :=
        New_String(".newgamemenu.canvas.difficulty.playerdamage");
      Set
        (SpinBox,
         Natural'Image
           (Natural(New_Game_Settings.Player_Damage_Bonus * 100.0)));
      SpinBox.Name :=
        New_String(".newgamemenu.canvas.difficulty.enemymeleedamage");
      Set
        (SpinBox,
         Natural'Image
           (Natural(New_Game_Settings.Enemy_Melee_Damage_Bonus * 100.0)));
      SpinBox.Name :=
        New_String(".newgamemenu.canvas.difficulty.playermeleedamage");
      Set
        (SpinBox,
         Natural'Image
           (Natural(New_Game_Settings.Player_Melee_Damage_Bonus * 100.0)));
      SpinBox.Name := New_String(".newgamemenu.canvas.difficulty.experience");
      Set
        (SpinBox,
         Natural'Image(Natural(New_Game_Settings.Experience_Bonus * 100.0)));
      SpinBox.Name := New_String(".newgamemenu.canvas.difficulty.reputation");
      Set
        (SpinBox,
         Natural'Image(Natural(New_Game_Settings.Reputation_Bonus * 100.0)));
      SpinBox.Name := New_String(".newgamemenu.canvas.difficulty.upgrade");
      Set
        (SpinBox,
         Natural'Image(Natural(New_Game_Settings.Upgrade_Cost_Bonus * 100.0)));
      SpinBox.Name := New_String(".newgamemenu.canvas.difficulty.prices");
      Set
        (SpinBox,
         Natural'Image(Natural(New_Game_Settings.Prices_Bonus * 100.0)));
      Tcl_Eval(Get_Context, "SetPoints");
      ShowMainMenu;
      Current
        (ComboBox,
         Natural'Image
           (Difficulty_Type'Pos(New_Game_Settings.Difficulty_Level)));
      Generate(ComboBox, "<<ComboboxSelected>>");
   end CreateMainMenu;

   procedure ShowMainMenu is
      MainWindow: constant Tk_Toplevel := Get_Main_Window(Get_Context);
      X, Y: Integer;
      Files: Search_Type;
      Button: Ttk_Button := Get_Widget(".mainmenu.loadgame");
      OsName: constant String := Tcl_GetVar(Get_Context, "tcl_platform(os)");
      GameFrame: constant Ttk_Frame := Get_Widget(".gameframe");
   begin
      X := (Positive'Value(Winfo_Get(MainWindow, "vrootwidth")) - 600) / 2;
      if X < 0 then
         X := 0;
      end if;
      Y := (Positive'Value(Winfo_Get(MainWindow, "vrootheight")) - 400) / 2;
      if Y < 0 then
         Y := 0;
      end if;
      if Game_Settings.Full_Screen then
         Wm_Set(MainWindow, "attributes", "-fullscreen 0");
      end if;
      if OsName = "Linux" then
         Wm_Set(MainWindow, "attributes", "-zoomed 0");
      else
         Wm_Set(MainWindow, "state", "normal");
      end if;
      Wm_Set(MainWindow, "title", "{Steam Sky - Main Menu}");
      Wm_Set
        (MainWindow, "geometry",
         "600x400+" & Trim(Positive'Image(X), Left) & "+" &
         Trim(Positive'Image(Y), Left));
      if Winfo_Get(GameFrame, "exists") = "1" then
         Tcl.Tk.Ada.Pack.Pack_Forget(GameFrame);
      end if;
      Tcl.Tk.Ada.Pack.Pack(MainMenuFrame, "-fill both -expand true");
      Start_Search(Files, To_String(Save_Directory), "*.sav");
      if not More_Entries(Files) then
         Tcl.Tk.Ada.Pack.Pack_Forget(Button);
         Button.Name := New_String(".mainmenu.newgame");
         Focus(Button);
      else
         Tcl.Tk.Ada.Pack.Pack(Button, "-after .mainmenu.newgame");
         Focus(Button);
      end if;
      End_Search(Files);
      Button.Name := New_String(".mainmenu.halloffame");
      if not Exists(To_String(Save_Directory) & "halloffame.dat") then
         Tcl.Tk.Ada.Pack.Pack_Forget(Button);
      else
         Tcl.Tk.Ada.Pack.Pack(Button, "-before .mainmenu.news");
      end if;
      if DataError /= Null_Unbounded_String then
         Button.Name := New_String(".mainmenu.newgame");
         Tcl.Tk.Ada.Pack.Pack_Forget(Button);
         Button.Name := New_String(".mainmenu.loadgame");
         Tcl.Tk.Ada.Pack.Pack_Forget(Button);
         ShowMessage
           ("Can't load game data files. Error: " & To_String(DataError),
            ".mainmenu");
         return;
      end if;
      if not Is_Writable_File(Name => To_String(Source => Save_Directory)) then
         Button.Name := New_String(".mainmenu.newgame");
         Tcl.Tk.Ada.Pack.Pack_Forget(Button);
         Button.Name := New_String(".mainmenu.loadgame");
         Tcl.Tk.Ada.Pack.Pack_Forget(Button);
         if Dir_Separator = '/' then
            ShowMessage
              ("You don't have permissions to write to directory """ &
               To_String(Source => Save_Directory) &
               """ which is set as directory for saved games. Please select different directory.",
               ".mainmenu");
         else
            ShowMessage
              ("You don't have permissions to write to directory """ &
               To_String(Source => Save_Directory) &
               """ which is set as directory for saved games. Please run the game as Administrator or select different directory.",
               ".mainmenu");
         end if;
      end if;
   end ShowMainMenu;

end MainMenu;
