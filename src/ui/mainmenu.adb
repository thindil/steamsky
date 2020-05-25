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

with Ada.Directories; use Ada.Directories;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Image.Photo; use Tcl.Tk.Ada.Image.Photo;
with Tcl.Tk.Ada.Pack;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Toplevel; use Tcl.Tk.Ada.Widgets.Toplevel;
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Tcl.Tk.Ada.Wm; use Tcl.Tk.Ada.Wm;
with Game; use Game;
with MainMenu.Commands;
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
        To_String(DataDirectory) & "ui" & Dir_Separator;
      MainWindow: constant Tk_Toplevel := Get_Main_Window(Get_Context);
      Icon: constant Tk_Photo :=
        Create
          ("logo", "-file " & UI_Directory & "[file join images icon.png]");
      pragma Unreferenced(Icon);
   begin
      MainMenu.Commands.AddCommands;
      Utils.UI.AddCommands;
      Wm_Set(MainWindow, "iconphoto", "-default logo");
      DataError := To_Unbounded_String(LoadGameData);
      Tcl_EvalFile(Get_Context, UI_Directory & "mainmenu.tcl");
      MainMenuFrame.Interp := Get_Context;
      MainMenuFrame.Name := New_String(".mainmenu");
      ShowMainMenu;
   end CreateMainMenu;

   procedure ShowMainMenu is
      MainWindow: constant Tk_Toplevel := Get_Main_Window(Get_Context);
      X, Y: Integer;
      Files: Search_Type;
      Button: Ttk_Button;
   begin
      X := (Positive'Value(Winfo_Get(MainWindow, "vrootwidth")) - 600) / 2;
      if X < 0 then
         X := 0;
      end if;
      Y := (Positive'Value(Winfo_Get(MainWindow, "vrootheight")) - 400) / 2;
      if Y < 0 then
         Y := 0;
      end if;
      Wm_Set(MainWindow, "title", "{Steam Sky - main menu}");
      Wm_Set
        (MainWindow, "geometry",
         "600x400+" & Trim(Positive'Image(X), Left) & "+" &
         Trim(Positive'Image(Y), Left));
      Tcl_Eval(MainMenuFrame.Interp, "pack forget [pack slaves .]");
      Tcl.Tk.Ada.Pack.Pack(MainMenuFrame, "-fill both -expand true");
      Button.Interp := MainMenuFrame.Interp;
      Button.Name := New_String(".mainmenu.loadgame");
      Start_Search(Files, To_String(SaveDirectory), "*.sav");
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
      if not Exists(To_String(SaveDirectory) & "halloffame.dat") then
         Tcl.Tk.Ada.Pack.Pack_Forget(Button);
      else
         Tcl.Tk.Ada.Pack.Pack(Button, "-before .mainmenu.news");
      end if;
      if DataError /= Null_Unbounded_String then
         Button.Name := New_String(".mainmenu.newgame");
         Tcl.Tk.Ada.Pack.Pack_Forget(Button);
         Button.Name := New_String(".mainmenu.loadgame");
         Tcl.Tk.Ada.Pack.Pack_Forget(Button);
         ShowMessage("Can't load game data files. Error: " &
                 To_String(DataError));
      end if;
   end ShowMainMenu;

end MainMenu;
