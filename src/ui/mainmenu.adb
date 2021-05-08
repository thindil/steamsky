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
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Dialogs; use Tcl.Tk.Ada.Dialogs;
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

   -- ****iv* MainMenu/Main_Menu_Frame
   -- FUNCTION
   -- Ttk Frame with content of main menu
   -- SOURCE
   Main_Menu_Frame: Ttk_Frame;
   -- ****

   -- ****iv* MainMenu/Data_Error
   -- FUNCTION
   -- Stores error message from loading the game data
   -- SOURCE
   Data_Error: Unbounded_String;
   -- ****

   procedure Create_Main_Menu is
      Ui_Directory: constant String :=
        To_String(Source => Data_Directory) & "ui" & Dir_Separator;
      Main_Window: constant Tk_Toplevel :=
        Get_Main_Window(Interp => Get_Context);
      Icon_Path: constant String :=
        Ui_Directory & "images" & Dir_Separator & "icon.png";
      Icon: Tk_Photo;
      Text_Entry: Ttk_Entry :=
        Get_Widget(pathName => ".newgamemenu.canvas.player.playername");
      Combo_Box: Ttk_ComboBox :=
        Get_Widget(pathName => ".newgamemenu.canvas.player.faction");
      Values: Unbounded_String := Null_Unbounded_String;
      Spin_Box: Ttk_SpinBox :=
        Get_Widget(pathName => ".newgamemenu.canvas.difficulty.enemydamage");
      Version_Label: constant Ttk_Label :=
        Get_Widget(pathName => ".mainmenu.version");
   begin
      if not Exists(Name => Icon_Path) then
         Wm_Set(Widgt => Main_Window, Action => "withdraw");
         if MessageBox
             (Options =>
                "-message {Couldn't not find the game data files and the game have to stop. Are you sure that directory """ &
                To_String(Source => Data_Directory) &
                """ is the proper place where the game data files exists?} -icon error -type ok") /=
           "" then
            Tcl_Exit(status => 1);
         end if;
         return;
      end if;
      Icon :=
        Create(pathName => "logo", options => "-file {" & Icon_Path & "}");
      MainMenu.Commands.AddCommands;
      Utils.UI.AddCommands;
      Goals.UI.AddCommands;
      Wm_Set
        (Widgt => Main_Window, Action => "iconphoto",
         Options => "-default " & Icon);
      Load_Theme_Loop :
      for I in Themes_List.Iterate loop
         if Themes_Container.Key(Position => I) =
           Game_Settings.Interface_Theme then
            Tcl_EvalFile
              (interp => Get_Context,
               fileName => To_String(Source => Themes_List(I).FileName));
            exit Load_Theme_Loop;
         end if;
      end loop Load_Theme_Loop;
      Theme_Use
        (ThemeName => To_String(Source => Game_Settings.Interface_Theme));
      Tcl_EvalFile
        (interp => Get_Context, fileName => Ui_Directory & "mainmenu.tcl");
      Main_Menu_Frame.Interp := Get_Context;
      Main_Menu_Frame.Name := New_String(Str => ".mainmenu");
      if not Game_Settings.Show_Tooltips then
         Disable;
      end if;
      DefaultFontsSizes :=
        (1 =>
           Positive'Value
             (Font.Configure(FontName => "MapFont", Option => "-size")),
         2 =>
           Positive'Value
             (Font.Configure(FontName => "InterfaceFont", Option => "-size")),
         3 =>
           Positive'Value
             (Font.Configure(FontName => "HelpFont", Option => "-size")));
      Font.Configure
        (FontName => "MapFont",
         Options => "-size" & Positive'Image(Game_Settings.Map_Font_Size));
      Font.Configure
        (FontName => "HelpFont",
         Options => "-size" & Positive'Image(Game_Settings.Help_Font_Size));
      Font.Configure
        (FontName => "InterfaceFont",
         Options =>
           "-size" & Positive'Image(Game_Settings.Interface_Font_Size));
      configure
        (Widgt => Version_Label,
         options => "-text {" & Game_Version & " development}");
      Data_Error := To_Unbounded_String(Source => Load_Game_Data);
      if Data_Error /= Null_Unbounded_String then
         Show_Main_Menu;
         return;
      end if;
      Delete(TextEntry => Text_Entry, FirstIndex => "0", LastIndex => "end");
      Insert
        (TextEntry => Text_Entry, Index => "0",
         Text => To_String(Source => New_Game_Settings.Player_Name));
      Tcl_SetVar
        (interp => Get_Context, varName => "playergender",
         newValue => "" & New_Game_Settings.Player_Gender);
      Text_Entry.Name :=
        New_String(Str => ".newgamemenu.canvas.player.shipname");
      Delete(TextEntry => Text_Entry, FirstIndex => "0", LastIndex => "end");
      Insert
        (TextEntry => Text_Entry, Index => "0",
         Text => To_String(Source => New_Game_Settings.Ship_Name));
      Load_Factions_Names_Loop :
      for I in Factions_List.Iterate loop
         if Factions_List(I).Careers.Length > 0 then
            Append
              (Source => Values,
               New_Item => " {" & Factions_List(I).Name & "}");
         end if;
      end loop Load_Factions_Names_Loop;
      Append(Source => Values, New_Item => " Random");
      configure
        (Widgt => Combo_Box,
         options => "-values [list" & To_String(Source => Values) & "]");
      Set
        (ComboBox => Combo_Box,
         Value =>
           To_String
             (Source => Factions_List(New_Game_Settings.Player_Faction).Name));
      Tcl_Eval(interp => Get_Context, strng => "SetFaction");
      Combo_Box.Name := New_String(Str => ".newgamemenu.canvas.player.career");
      Set
        (ComboBox => Combo_Box,
         Value =>
           To_String
             (Source => Careers_List(New_Game_Settings.Player_Career).Name));
      Combo_Box.Name := New_String(Str => ".newgamemenu.canvas.player.base");
      if New_Game_Settings.Starting_Base =
        To_Unbounded_String(Source => "Any") then
         Set(ComboBox => Combo_Box, Value => "Any");
      else
         Set
           (ComboBox => Combo_Box,
            Value =>
              "{" &
              To_String
                (Source =>
                   BasesTypes_List(New_Game_Settings.Starting_Base).Name) &
              "}");
      end if;
      Combo_Box.Name :=
        New_String(Str => ".newgamemenu.canvas.difficulty.difficultylevel");
      Set
        (SpinBox => Spin_Box,
         Value =>
           Natural'Image
             (Natural(New_Game_Settings.Enemy_Damage_Bonus * 100.0)));
      Spin_Box.Name :=
        New_String(Str => ".newgamemenu.canvas.difficulty.playerdamage");
      Set
        (SpinBox => Spin_Box,
         Value =>
           Natural'Image
             (Natural(New_Game_Settings.Player_Damage_Bonus * 100.0)));
      Spin_Box.Name :=
        New_String(Str => ".newgamemenu.canvas.difficulty.enemymeleedamage");
      Set
        (SpinBox => Spin_Box,
         Value =>
           Natural'Image
             (Natural(New_Game_Settings.Enemy_Melee_Damage_Bonus * 100.0)));
      Spin_Box.Name :=
        New_String(Str => ".newgamemenu.canvas.difficulty.playermeleedamage");
      Set
        (SpinBox => Spin_Box,
         Value =>
           Natural'Image
             (Natural(New_Game_Settings.Player_Melee_Damage_Bonus * 100.0)));
      Spin_Box.Name :=
        New_String(Str => ".newgamemenu.canvas.difficulty.experience");
      Set
        (SpinBox => Spin_Box,
         Value =>
           Natural'Image(Natural(New_Game_Settings.Experience_Bonus * 100.0)));
      Spin_Box.Name :=
        New_String(Str => ".newgamemenu.canvas.difficulty.reputation");
      Set
        (SpinBox => Spin_Box,
         Value =>
           Natural'Image(Natural(New_Game_Settings.Reputation_Bonus * 100.0)));
      Spin_Box.Name :=
        New_String(Str => ".newgamemenu.canvas.difficulty.upgrade");
      Set
        (SpinBox => Spin_Box,
         Value =>
           Natural'Image
             (Natural(New_Game_Settings.Upgrade_Cost_Bonus * 100.0)));
      Spin_Box.Name :=
        New_String(Str => ".newgamemenu.canvas.difficulty.prices");
      Set
        (SpinBox => Spin_Box,
         Value =>
           Natural'Image(Natural(New_Game_Settings.Prices_Bonus * 100.0)));
      Tcl_Eval(interp => Get_Context, strng => "SetPoints");
      Show_Main_Menu;
      Current
        (ComboBox => Combo_Box,
         NewIndex =>
           Natural'Image
             (Difficulty_Type'Pos(New_Game_Settings.Difficulty_Level)));
      Generate(Window => Combo_Box, EventName => "<<Combo_BoxSelected>>");
   end Create_Main_Menu;

   procedure Show_Main_Menu is
      Main_Window: constant Tk_Toplevel :=
        Get_Main_Window(Interp => Get_Context);
      X, Y: Integer;
      Files: Search_Type;
      Button: Ttk_Button := Get_Widget(pathName => ".mainmenu.loadgame");
      Game_Frame: constant Ttk_Frame := Get_Widget(pathName => ".gameframe");
   begin
      X :=
        (Positive'Value
           (Winfo_Get(Widgt => Main_Window, Info => "vrootwidth")) -
         600) /
        2;
      if X < 0 then
         X := 0;
      end if;
      Y :=
        (Positive'Value
           (Winfo_Get(Widgt => Main_Window, Info => "vrootheight")) -
         400) /
        2;
      if Y < 0 then
         Y := 0;
      end if;
      if Game_Settings.Full_Screen then
         Wm_Set
           (Widgt => Main_Window, Action => "attributes",
            Options => "-fullscreen 0");
      end if;
      if Tcl_GetVar(interp => Get_Context, varName => "tcl_platform(os)") =
        "Linux" then
         Wm_Set
           (Widgt => Main_Window, Action => "attributes",
            Options => "-zoomed 0");
      else
         Wm_Set(Widgt => Main_Window, Action => "state", Options => "normal");
      end if;
      Wm_Set
        (Widgt => Main_Window, Action => "title",
         Options => "{Steam Sky - Main Menu}");
      Wm_Set
        (Widgt => Main_Window, Action => "geometry",
         Options =>
           "600x400+" & Trim(Source => Positive'Image(X), Side => Left) & "+" &
           Trim(Source => Positive'Image(Y), Side => Left));
      if Winfo_Get(Game_Frame, "exists") = "1" then
         Tcl.Tk.Ada.Pack.Pack_Forget(Game_Frame);
      end if;
      Tcl.Tk.Ada.Pack.Pack(Main_Menu_Frame, "-fill both -expand true");
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
      if Data_Error /= Null_Unbounded_String then
         Button.Name := New_String(".mainmenu.newgame");
         Tcl.Tk.Ada.Pack.Pack_Forget(Button);
         Button.Name := New_String(".mainmenu.loadgame");
         Tcl.Tk.Ada.Pack.Pack_Forget(Button);
         ShowMessage
           ("Can't load game data files. Error: " & To_String(Data_Error),
            ".mainmenu");
         return;
      end if;
      declare
         Test_File: File_Type;
      begin
         Create
           (File => Test_File,
            Name =>
              To_String(Source => Save_Directory) & Dir_Separator &
              "test.txt");
         Close(Test_File);
         Delete_File
           (To_String(Source => Save_Directory) & Dir_Separator & "test.txt");
      exception
         when Ada.Text_IO.Use_Error =>
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
      end;
   end Show_Main_Menu;

end MainMenu;
