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

-- with Ada.Containers;
with Ada.Directories; use Ada.Directories;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
-- with Tcl.Tk.Ada.Dialogs;
-- with Tcl.Tk.Ada.Event;
-- with Tcl.Tk.Ada.Font;
-- with Tcl.Tk.Ada.Image.Photo;
with Tcl.Tk.Ada.Pack;
-- with Tcl.Tk.Ada.TtkStyle;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Toplevel; use Tcl.Tk.Ada.Widgets.Toplevel;
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
-- with Tcl.Tk.Ada.Widgets.TtkEntry;
-- with Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
-- with Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
-- with Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Winfo;
with Tcl.Tk.Ada.Wm; use Tcl.Tk.Ada.Wm;
-- with Tcl.Tklib.Ada.Tooltip;
-- with BasesTypes;
-- with Careers;
with Config; use Config;
with Dialogs; use Dialogs;
-- with Factions;
with Game; use Game;
with Goals.UI;
with MainMenu.Commands;
-- with Maps.UI;
with Table;
-- with Themes;
with Utils.UI;

package body MainMenu is

   -- ****iv* MainMenu/MainMenu.Main_Menu_Frame
   -- FUNCTION
   -- Ttk Frame with content of main menu
   -- SOURCE
   Main_Menu_Frame: Ttk_Frame;
   -- ****

   -- ****iv* MainMenu/MainMenu.Data_Error
   -- FUNCTION
   -- Stores error message from loading the game data
   -- SOURCE
   Data_Error: Unbounded_String;
   -- ****

   -- ****if* MainMenu/MainMenu.Get_Data_Error
   -- FUNCTION
   -- Get the error message from loading the game data
   -- SOURCE
   function Get_Data_Error return String is
      -- ****
   begin
      return To_String(Source => Data_Error);
   end Get_Data_Error;

   procedure Create_Main_Menu is
--      use Ada.Containers;
--      use Tcl.Tk.Ada.Dialogs;
--      use Tcl.Tk.Ada.Event;
--      use Tcl.Tk.Ada.Image.Photo;
--      use Tcl.Tk.Ada.TtkStyle;
--      use Tcl.Tk.Ada.Widgets.TtkEntry;
--      use Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
--      use Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
--      use Tcl.Tk.Ada.Widgets.TtkLabel;
--      use Tcl.Tklib.Ada.Tooltip;
--      use BasesTypes;
--      use Careers;
--      use Factions;
--      use Maps.UI;
--      use Themes;
--      use Tiny_String;
--      use Utils.UI;
--
--      Ui_Directory: constant String :=
--        To_String(Source => Data_Directory) & "ui" & Dir_Separator;
--      Main_Window: constant Tk_Toplevel :=
--        Get_Main_Window(Interp => Get_Context);
--      Icon_Path: constant String :=
--        Ui_Directory & "images" & Dir_Separator & "icon.png";
--      Icon: Tk_Photo;
--      Player_Frame_Name: constant String := ".newgamemenu.canvas.player";
--      Difficulty_Frame_Name: constant String :=
--        ".newgamemenu.canvas.difficulty";
--      Text_Entry: Ttk_Entry :=
--        Get_Widget(pathName => Player_Frame_Name & ".playername");
--      Combo_Box: Ttk_ComboBox :=
--        Get_Widget(pathName => Player_Frame_Name & ".faction");
--      Values: Unbounded_String := Null_Unbounded_String;
--      Spin_Box: Ttk_SpinBox :=
--        Get_Widget(pathName => Difficulty_Frame_Name & ".enemydamage");
--      Version_Label: constant Ttk_Label :=
--        Get_Widget(pathName => ".mainmenu.version");
--      Button: Ttk_Button :=
--        Get_Widget(pathName => ".newgamemenu.canvas.player.randomplayer");
--      Faction: Faction_Record; --## rule line off IMPROPER_INITIALIZATION
--      procedure Get_Ada_Font_Sizes(Map, Inter, Help: Positive) with
--         Import => True,
--         Convention => C,
--         External_Name => "getAdaFontSizes";
      procedure Create_Ada_Main_Menu with
         Convention => C,
         Import => True,
         External_Name => "createAdaMainMenu";
   begin
--      if not Exists(Name => Icon_Path) then
--         Wm_Set(Widgt => Main_Window, Action => "withdraw");
--         if MessageBox
--             (Options =>
--                "-message {Couldn't not find the game data files and the game have to stop. Are you sure that directory """ &
--                To_String(Source => Data_Directory) &
--                """ is the proper place where the game data files exists?} -icon error -type ok") /=
--           "" then
--            Tcl_Exit(status => 1);
--         end if;
--         return;
--      end if;
--      Icon :=
--        Create(pathName => "logo", options => "-file {" & Icon_Path & "}");
      MainMenu.Commands.Add_Commands;
      Dialogs.Add_Commands;
      Utils.UI.Add_Commands;
      Goals.UI.Add_Commands;
      Table.Add_Commands;
      Create_Ada_Main_Menu;
--      Wm_Set
--        (Widgt => Main_Window, Action => "iconphoto",
--         Options => "-default " & Icon);
--      Tcl_EvalFile
--        (interp => Get_Context, fileName => Get_Icon(Name => "fileName"));
--      Theme_Use(ThemeName => To_String(Source => Get_Interface_Theme));
--      Load_Theme_Images;
--      Tcl_EvalFile
--        (interp => Get_Context, fileName => Ui_Directory & "mainmenu.tcl");
--      Main_Menu_Frame := Get_Widget(pathName => ".mainmenu");
--      if not Get_Boolean_Setting(Name => "showTooltips") then
--         Disable;
--      end if;
--      Default_Fonts_Sizes :=
--        (1 =>
--           Positive'Value
--             (Font.Configure(FontName => "MapFont", Option => "-size")),
--         2 =>
--           Positive'Value
--             (Font.Configure(FontName => "InterfaceFont", Option => "-size")),
--         3 =>
--           Positive'Value
--             (Font.Configure(FontName => "HelpFont", Option => "-size")));
--      Get_Ada_Font_Sizes
--        (Map => Default_Fonts_Sizes(1), Inter => Default_Fonts_Sizes(2),
--         Help => Default_Fonts_Sizes(3));
--      Set_Fonts
--        (New_Size => Get_Integer_Setting(Name => "mapFontSize"),
--         Font_Type => MAPFONT);
--      Set_Fonts
--        (New_Size => Get_Integer_Setting(Name => "helpFontSize"),
--         Font_Type => Help_Font_Type);
--      Set_Fonts
--        (New_Size => Get_Integer_Setting(Name => "interfaceFontSize"),
--         Font_Type => INTERFACEFONT);
--      configure
--        (Widgt => Version_Label, options => "-text {" & Game_Version & " development}");
--      Data_Error := To_Unbounded_String(Source => Load_Game_Data);
--      if Get_Data_Error'Length > 0 then
--         Show_Main_Menu;
--         return;
--      end if;
--      Delete(TextEntry => Text_Entry, FirstIndex => "0", LastIndex => "end");
--      Insert
--        (TextEntry => Text_Entry, Index => "0",
--         Text => Get_String_Setting(Name => "playerName"));
--      Tcl_SetVar
--        (interp => Get_Context, varName => "playergender",
--         newValue => "" & Get_Gender);
--      Text_Entry.Name := New_String(Str => Player_Frame_Name & ".shipname");
--      Delete(TextEntry => Text_Entry, FirstIndex => "0", LastIndex => "end");
--      Insert
--        (TextEntry => Text_Entry, Index => "0",
--         Text => Get_String_Setting(Name => "shipName"));
--      Load_Factions_Names_Loop :
--      for I in 1 .. Get_Factions_Amount loop
--         Faction := Get_Faction(Number => I);
--         if Faction.Careers.Length > 0 then
--            Values := Values & " {" & To_String(Source => Faction.Name) & "}";
--         end if;
--      end loop Load_Factions_Names_Loop;
--      Append(Source => Values, New_Item => " Random");
--      configure
--        (Widgt => Combo_Box,
--         options => "-values [list" & To_String(Source => Values) & "]");
--      if Get_String_Setting(Name => "playerFaction") = "random" then
--         Set(ComboBox => Combo_Box, Value => "Random");
--      else
--         Set
--           (ComboBox => Combo_Box,
--            Value =>
--              To_String
--                (Source =>
--                   Get_Faction
--                     (Index =>
--                        To_Bounded_String
--                          (Source =>
--                             Get_String_Setting(Name => "playerFaction")))
--                     .Name));
--      end if;
--      Tcl_Eval(interp => Get_Context, strng => "SetFaction");
--      Combo_Box.Name := New_String(Str => Player_Frame_Name & ".career");
--      if Get_String_Setting(Name => "playerCareer") = "random" then
--         Set(ComboBox => Combo_Box, Value => "Random");
--      else
--         Set
--           (ComboBox => Combo_Box,
--            Value =>
--              To_String
--                (Source =>
--                   Get_Career
--                     (Career_Index =>
--                        Get_String_Setting(Name => "playerCareer"))
--                     .Name));
--      end if;
--      Combo_Box.Name := New_String(Str => Player_Frame_Name & ".base");
--      Set
--        (ComboBox => Combo_Box,
--         Value =>
--           (if Get_String_Setting(Name => "startingBase") = "Any" then "Any"
--            else "{" &
--              Get_Base_Type_Name
--                (Base_Type =>
--                   To_Bounded_String
--                     (Source => Get_String_Setting(Name => "startingBase"))) &
--              "}"));
--      Combo_Box.Name :=
--        New_String(Str => Difficulty_Frame_Name & ".difficultylevel");
--      Set
--        (SpinBox => Spin_Box,
--         Value =>
--           Natural'Image
--             (Natural(Get_Float_Setting(Name => "enemyDamageBonus") * 100.0)));
--      Spin_Box.Name :=
--        New_String(Str => Difficulty_Frame_Name & ".playerdamage");
--      Set
--        (SpinBox => Spin_Box,
--         Value =>
--           Natural'Image
--             (Natural
--                (Get_Float_Setting(Name => "playerDamageBonus") * 100.0)));
--      Spin_Box.Name :=
--        New_String(Str => Difficulty_Frame_Name & ".enemymeleedamage");
--      Set
--        (SpinBox => Spin_Box,
--         Value =>
--           Natural'Image
--             (Natural
--                (Get_Float_Setting(Name => "enemyMeleeDamageBonus") * 100.0)));
--      Spin_Box.Name :=
--        New_String(Str => Difficulty_Frame_Name & ".playermeleedamage");
--      Set
--        (SpinBox => Spin_Box,
--         Value =>
--           Natural'Image
--             (Natural
--                (Get_Float_Setting(Name => "playerMeleeDamageBonus") *
--                 100.0)));
--      Spin_Box.Name :=
--        New_String(Str => Difficulty_Frame_Name & ".experience");
--      Set
--        (SpinBox => Spin_Box,
--         Value =>
--           Natural'Image
--             (Natural(Get_Float_Setting(Name => "experienceBonus") * 100.0)));
--      Spin_Box.Name :=
--        New_String(Str => Difficulty_Frame_Name & ".reputation");
--      Set
--        (SpinBox => Spin_Box,
--         Value =>
--           Natural'Image
--             (Natural(Get_Float_Setting(Name => "reputationBonus") * 100.0)));
--      Spin_Box.Name := New_String(Str => Difficulty_Frame_Name & ".upgrade");
--      Set
--        (SpinBox => Spin_Box,
--         Value =>
--           Natural'Image
--             (Natural(Get_Float_Setting(Name => "upgradeCostBonus") * 100.0)));
--      Spin_Box.Name := New_String(Str => Difficulty_Frame_Name & ".prices");
--      Set
--        (SpinBox => Spin_Box,
--         Value =>
--           Natural'Image
--             (Natural(Get_Float_Setting(Name => "pricesBonus") * 100.0)));
--      Tcl_Eval(interp => Get_Context, strng => "SetPoints");
      Show_Main_Menu;
--      Current
--        (ComboBox => Combo_Box,
--         NewIndex => Natural'Image(Difficulty_Type'Pos(Get_Difficulty)));
--      Generate(Window => Combo_Box, EventName => "<<Combo_BoxSelected>>");
--      configure(Widgt => Button, options => "-image randomicon");
--      Button.Name :=
--        New_String(Str => ".newgamemenu.canvas.player.randomship");
--      configure(Widgt => Button, options => "-image randomicon");
--      Button.Name :=
--        New_String(Str => ".newgamemenu.canvas.player.gender.male");
--      configure(Widgt => Button, options => "-image maleicon");
--      Button.Name :=
--        New_String(Str => ".newgamemenu.canvas.player.gender.female");
--      configure(Widgt => Button, options => "-image femaleicon");
   end Create_Main_Menu;

   procedure Show_Main_Menu is
      use Ada.Strings;
      use Ada.Strings.Fixed;
      use Tcl.Tk.Ada.Winfo;

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
