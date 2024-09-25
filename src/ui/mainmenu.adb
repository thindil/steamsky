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

with Ada.Directories;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;
with Interfaces.C.Strings;
with GNAT.Directory_Operations;
with Tcl;
with Tcl.Ada;
with Tcl.Tk.Ada;
with Tcl.Tk.Ada.Pack;
with Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Toplevel;
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Winfo;
with Tcl.Tk.Ada.Wm;
with Config;
with Dialogs; use Dialogs;
with Game;
with Goals.UI;
with MainMenu.Commands;
with Table;
with Utils.UI;

package body MainMenu is

   --## rule off REDUCEABLE_SCOPE
   -- ****iv* MainMenu/MainMenu.Main_Menu_Frame
   -- FUNCTION
   -- Ttk Frame with content of main menu
   -- SOURCE
   Main_Menu_Frame: Ttk_Frame;
   -- ****

   --## rule off DIRECTLY_ACCESSED_GLOBALS
   -- ****iv* MainMenu/MainMenu.Data_Error
   -- FUNCTION
   -- Stores error message from loading the game data
   -- SOURCE
   Data_Error: Unbounded_String;
   -- ****
   --## rule on DIRECTLY_ACCESSED_GLOBALS

   -- ****if* MainMenu/MainMenu.Get_Data_Error
   -- FUNCTION
   -- Get the error message from loading the game data
   -- SOURCE
   function Get_Data_Error return String is
      -- ****
   begin
      return To_String(Source => Data_Error);
   end Get_Data_Error;
   --## rule on REDUCEABLE_SCOPE

   procedure Create_Main_Menu is
      procedure Create_Ada_Main_Menu with
         Convention => C,
         Import => True,
         External_Name => "createAdaMainMenu";
   begin
      MainMenu.Commands.Add_Commands;
      Dialogs.Add_Commands;
      Utils.UI.Add_Commands;
      Goals.UI.Add_Commands;
      Table.Add_Commands;
      Create_Ada_Main_Menu;
      Show_Main_Menu;
   end Create_Main_Menu;

   procedure Show_Main_Menu is
      use Ada.Directories;
      use Ada.Strings;
      use Ada.Strings.Fixed;
      use Interfaces.C.Strings;
      use GNAT.Directory_Operations;
      use Tcl.Ada;
      use Tcl.Tk.Ada;
      use Tcl.Tk.Ada.Widgets;
      use Tcl.Tk.Ada.Widgets.Toplevel;
      use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
      use Tcl.Tk.Ada.Widgets.TtkButton;
      use Tcl.Tk.Ada.Winfo;
      use Tcl.Tk.Ada.Wm;
      use Config;
      use Game;

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
      if Get_Boolean_Setting(Name => "fullScreen") then
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
      if Winfo_Get(Widgt => Game_Frame, Info => "exists") = "1" then
         Tcl.Tk.Ada.Pack.Pack_Forget(Slave => Game_Frame);
      end if;
      Main_Menu_Frame := Get_Widget(pathName => ".mainmenu");
      Tcl.Tk.Ada.Pack.Pack
        (Slave => Main_Menu_Frame, Options => "-fill both -expand true");
      Start_Search
        (Search => Files, Directory => To_String(Source => Save_Directory),
         Pattern => "*.sav");
      if More_Entries(Search => Files) then
         Tcl.Tk.Ada.Pack.Pack
           (Slave => Button, Options => "-after .mainmenu.newgame");
         Focus(Widgt => Button);
      else
         Tcl.Tk.Ada.Pack.Pack_Forget(Slave => Button);
         Button.Name := New_String(Str => ".mainmenu.newgame");
         Focus(Widgt => Button);
      end if;
      End_Search(Search => Files);
      Button.Name := New_String(Str => ".mainmenu.halloffame");
      if Exists
          (Name => To_String(Source => Save_Directory) & "halloffame.dat") then
         Tcl.Tk.Ada.Pack.Pack
           (Slave => Button, Options => "-before .mainmenu.news");
      else
         Tcl.Tk.Ada.Pack.Pack_Forget(Slave => Button);
      end if;
      if Get_Data_Error'Length > 0 then
         Button.Name := New_String(Str => ".mainmenu.newgame");
         Tcl.Tk.Ada.Pack.Pack_Forget(Slave => Button);
         Button.Name := New_String(Str => ".mainmenu.loadgame");
         Tcl.Tk.Ada.Pack.Pack_Forget(Slave => Button);
         Show_Message
           (Text => "Can't load game data files. Error: " & Get_Data_Error,
            Parent_Frame => ".", Title => "The game data error");
         return;
      end if;
      Check_Permissions_Block :
      declare
         use Ada.Text_IO;

         Test_File: File_Type;
      begin
         Create
           (File => Test_File,
            Name =>
              To_String(Source => Save_Directory) & Dir_Separator &
              "test.txt");
         Close(File => Test_File);
         Delete_File
           (Name =>
              To_String(Source => Save_Directory) & Dir_Separator &
              "test.txt");
      exception
         when Ada.Text_IO.Use_Error =>
            Button.Name := New_String(Str => ".mainmenu.newgame");
            Tcl.Tk.Ada.Pack.Pack_Forget(Slave => Button);
            Button.Name := New_String(Str => ".mainmenu.loadgame");
            Tcl.Tk.Ada.Pack.Pack_Forget(Slave => Button);
            if Dir_Separator = '/' then
               Show_Message
                 (Text =>
                    "You don't have permissions to write to directory """ &
                    To_String(Source => Save_Directory) &
                    """ which is set as directory for saved games. Please select different directory.",
                  Parent_Frame => ".", Title => "Can't save the game");
            else
               Show_Message
                 (Text =>
                    "You don't have permissions to write to directory """ &
                    To_String(Source => Save_Directory) &
                    """ which is set as directory for saved games. Please run the game as Administrator or select different directory.",
                  Parent_Frame => ".", Title => "Can't save the game");
            end if;
      end Check_Permissions_Block;
   end Show_Main_Menu;

end MainMenu;
