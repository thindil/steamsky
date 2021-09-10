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

with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones; use Ada.Calendar.Time_Zones;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Containers.Vectors; use Ada.Containers;
with Ada.Directories; use Ada.Directories;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNAT.String_Split; use GNAT.String_Split;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid; use Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Pack;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Menu; use Tcl.Tk.Ada.Widgets.Menu;
with Tcl.Tk.Ada.Widgets.Text; use Tcl.Tk.Ada.Widgets.Text;
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
with Tcl.Tk.Ada.Widgets.TtkTreeView; use Tcl.Tk.Ada.Widgets.TtkTreeView;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Tcl.Tk.Ada.Wm; use Tcl.Tk.Ada.Wm;
with Tcl.Tklib.Ada.Tooltip; use Tcl.Tklib.Ada.Tooltip;
with BasesTypes; use BasesTypes;
with Config; use Config;
with Crew; use Crew;
with Dialogs; use Dialogs;
with Events; use Events;
with Factions; use Factions;
with Game; use Game;
with Game.SaveLoad; use Game.SaveLoad;
with Goals; use Goals;
with HallOfFame; use HallOfFame;
with Maps.UI; use Maps.UI;
with Ships; use Ships;
with Table; use Table;
with Utils; use Utils;
with Utils.UI; use Utils.UI;

package body MainMenu.Commands is

   function Open_Link_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc);
      Os_Name: constant String :=
        Tcl_GetVar(interp => Get_Context, varName => "tcl_platform(os)");
      Command: constant String :=
        Locate_Exec_On_Path
          (Exec_Name =>
             (if Os_Name = "Windows" then "start"
              elsif Os_Name = "Darwin" then "open" else "xdg-open")).all;
   begin
      if Non_Blocking_Spawn
          (Program_Name => Command,
           Args =>
             Argument_String_To_List
               (Arg_String => CArgv.Arg(Argv => Argv, N => 1)).all) =
        Invalid_Pid then
         return TCL_ERROR;
      end if;
      return TCL_OK;
   end Open_Link_Command;

   -- ****o* MCommands/MCommands.Show_File_Command
   -- FUNCTION
   -- Show the selected file content
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowFile filename
   -- Filename is the name of the file in the documentation directory which
   -- will be show
   -- SOURCE
   function Show_File_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_File_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc);
      Text_View: constant Tk_Text :=
        Get_Widget(pathName => ".showfilemenu.text", Interp => Interp);
      Show_File: File_Type;
      File_Name: constant String := CArgv.Arg(Argv => Argv, N => 1);
   begin
      configure(Widgt => Text_View, options => "-state normal");
      Delete(TextWidget => Text_View, StartIndex => "1.0", Indexes => "end");
      if Exists(Name => To_String(Source => Doc_Directory) & File_Name) then
         Open
           (File => Show_File, Mode => In_File,
            Name => To_String(Source => Doc_Directory) & File_Name);
         Load_File_Line_Loop :
         while not End_Of_File(File => Show_File) loop
            Insert
              (TextWidget => Text_View, Index => "end",
               Text => "{" & Get_Line(File => Show_File) & LF & "}");
         end loop Load_File_Line_Loop;
         Close(File => Show_File);
      else
         Insert
           (TextWidget => Text_View, Index => "end",
            Text =>
              "{Can't find file to load. Did '" & File_Name &
              "' file is in '" & To_String(Source => Doc_Directory) &
              "' directory?}");
      end if;
      configure(Widgt => Text_View, options => "-state disabled");
      Bind_To_Main_Window
        (Interp => Interp, Sequence => "<Alt-b>",
         Script => "{InvokeButton .showfilemenu.back}");
      Bind_To_Main_Window
        (Interp => Interp, Sequence => "<Escape>",
         Script => "{InvokeButton .showfilemenu.back}");
      return TCL_OK;
   end Show_File_Command;

   -- ****iv* MCommands/MCommands.AllNews
   -- FUNCTION
   -- If true, show all news, not only from last version. Default is false
   -- SOURCE
   All_News: Boolean := False;
   -- ****

   -- ****o* MCommands/MCommands.Show_News_Command
   -- FUNCTION
   -- Show changes in the game, all or just recent
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowNews boolean
   -- If boolean is true, show all news, otherwise only recent
   -- SOURCE
   function Show_News_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_News_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc);
      Text_View: constant Tk_Text :=
        Get_Widget(pathName => ".newsmenu.text", Interp => Interp);
      Changes_File: File_Type;
      File_Text: Unbounded_String := Null_Unbounded_String;
      All_News_Button: constant Ttk_Button :=
        Get_Widget(pathName => ".newsmenu.showall", Interp => Interp);
   begin
      if CArgv.Arg(Argv => Argv, N => 1) = "false" then
         All_News := False;
         configure
           (Widgt => All_News_Button,
            options => "-text {Show all changes} -command {ShowNews true}");
         Add
           (Widget => All_News_Button,
            Message =>
              "Show all changes to the game since previous big stable version");
      else
         All_News := True;
         configure
           (Widgt => All_News_Button,
            options =>
              "-text {Show only newest changes} -command {ShowNews false}");
         Add
           (Widget => All_News_Button,
            Message => "Show only changes to the game since previous relese");
      end if;
      configure(Widgt => Text_View, options => "-state normal");
      Delete(TextWidget => Text_View, StartIndex => "1.0", Indexes => "end");
      if Exists
          (Name => To_String(Source => Doc_Directory) & "CHANGELOG.md") then
         Open
           (File => Changes_File, Mode => In_File,
            Name => To_String(Source => Doc_Directory) & "CHANGELOG.md");
         Set_Line(File => Changes_File, To => 6);
         Load_Changes_File_Loop :
         while not End_Of_File(File => Changes_File) loop
            File_Text :=
              To_Unbounded_String(Source => Get_Line(File => Changes_File));
            if Length(Source => File_Text) > 1 and not All_News then
               exit Load_Changes_File_Loop when Slice
                   (Source => File_Text, Low => 1, High => 3) =
                 "## ";
            end if;
            Insert
              (TextWidget => Text_View, Index => "end",
               Text => "{" & To_String(Source => File_Text) & LF & "}");
         end loop Load_Changes_File_Loop;
         Close(File => Changes_File);
      else
         Insert
           (TextWidget => Text_View, Index => "end",
            Text =>
              "{Can't find changelog file. Did 'CHANGELOG.md' file is in '" &
              To_String(Source => Doc_Directory) & "' directory?}");
      end if;
      configure(Widgt => Text_View, options => "-state disabled");
      return TCL_OK;
   end Show_News_Command;

   -- ****o* MCommands/MCommands.Show_Hall_Of_Fame_Command
   -- FUNCTION
   -- Show the Hall of Fame
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowHallOfFame
   -- SOURCE
   function Show_Hall_Of_Fame_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Hall_Of_Fame_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc, Argv);
      Hof_View: constant Ttk_Tree_View :=
        Get_Widget(pathName => ".hofmenu.view", Interp => Interp);
   begin
      Delete
        (TreeViewWidget => Hof_View,
         ItemsList =>
           "[list " & Children(TreeViewWidget => Hof_View, Item => "{}") &
           "]");
      Load_Hall_Of_Fame_Loop :
      for I in Hall_Of_Fame_Array'Range loop
         exit Load_Hall_Of_Fame_Loop when Hall_Of_Fame_Array(I).Name =
           Null_Unbounded_String;
         Insert
           (TreeViewWidget => Hof_View,
            Options =>
              "{} end -values [list " & Positive'Image(I) & " " &
              To_String(Source => Hall_Of_Fame_Array(I).Name) & " " &
              Natural'Image(Hall_Of_Fame_Array(I).Points) & " " &
              To_String(Source => Hall_Of_Fame_Array(I).Death_Reason) & "]");
      end loop Load_Hall_Of_Fame_Loop;
      return TCL_OK;
   end Show_Hall_Of_Fame_Command;

   -- ****iv* MCommands/MCommands.LoadTable
   -- FUNCTION
   -- Table with info about the available saved games
   -- SOURCE
   Load_Table: Table_Widget (Amount => 3);
   -- ****

   -- ****if* MCommands/MCommands.Get_Load_Table
   -- FUNCTION
   -- Get the table with the list of the saved games
   -- RESULT
   -- The Table_Widtget with the list of available saved games
   -- HISTORY
   -- 6.6 - Added
   -- SOURCE
   function Get_Load_Table return Table_Widget is
      -- ****
   begin
      return Load_Table;
   end Get_Load_Table;

   -- ****it* MCommands/MCommands.Save_Sort_Orders
   -- FUNCTION
   -- Sorting orders for the saved games list
   -- OPTIONS
   -- PLAYERASC  - Sort by player name ascending
   -- PLAYERDESC - Sort by player name descending
   -- SHIPASC    - Sort by ship name ascending
   -- SHIPDESC   - Sort by ship name descending
   -- TIMEASC    - Sort by save time ascending
   -- TIMEDESC   - Sort by save time descending
   -- SOURCE
   type Save_Sort_Orders is
     (PLAYERASC, PLAYERDESC, SHIPASC, SHIPDESC, TIMEASC, TIMEDESC);
   -- ****

   -- ****iv* MCommands/MCommands.Save_Sort_Order
   -- FUNCTION
   -- The current sorting order for the saved game list
   -- SOURCE
   Save_Sort_Order: Save_Sort_Orders := TIMEDESC;
   -- ****

   -- ****if* MCommands/MCommands.Get_Save_Sort_Order
   -- FUNCTION
   -- Get the current sorting order for the saved games list
   -- RESULT
   -- The current sorting order of the saved games list
   -- HISTORY
   -- 6.5 - Added
   -- SOURCE
   function Get_Save_Sort_Order return Save_Sort_Orders is
   -- ****
   begin
      return Save_Sort_Order;
   end Get_Save_Sort_Order;

   -- ****o* MCommands/MCommads.Show_Load_Game_Command
   -- FUNCTION
   -- Show available saved games
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowLoadGame
   -- SOURCE
   function Show_Load_Game_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Load_Game_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc, Argv);
      Files: Search_Type;
      Found_File: Directory_Entry_Type;
      Tokens: Slice_Set; --## rule line off IMPROPER_INITIALIZATION
      type Save_Record is record  --## rule line off TYPE_INITIAL_VALUES
         Player_Name: Unbounded_String;
         Ship_Name: Unbounded_String;
         Save_Time: Unbounded_String;
         File_Name: Unbounded_String;
      end record;
      package Saves_Container is new Vectors
        (Index_Type => Positive, Element_Type => Save_Record);
      Saves: Saves_Container.Vector := Saves_Container.Empty_Vector;
      function "<"(Left, Right: Save_Record) return Boolean is
      begin
         if Get_Save_Sort_Order = PLAYERASC
           and then Left.Player_Name < Right.Player_Name then
            return True;
         end if;
         if Get_Save_Sort_Order = PLAYERDESC
           and then Left.Player_Name > Right.Player_Name then
            return True;
         end if;
         if Get_Save_Sort_Order = SHIPASC
           and then Left.Ship_Name < Right.Ship_Name then
            return True;
         end if;
         if Get_Save_Sort_Order = SHIPDESC
           and then Left.Ship_Name > Right.Ship_Name then
            return True;
         end if;
         if Get_Save_Sort_Order = TIMEASC
           and then Left.Save_Time < Right.Save_Time then
            return True;
         end if;
         if Get_Save_Sort_Order = TIMEDESC
           and then Left.Save_Time > Right.Save_Time then
            return True;
         end if;
         return False;
      end "<";
      package Saves_Sorting is new Saves_Container.Generic_Sorting;
      Local_Load_Table: Table_Widget := Get_Load_Table;
   begin
      if Local_Load_Table.Row_Height = 1 then
         Local_Load_Table :=
           CreateTable
             (Parent => ".loadmenu.list",
              Headers =>
                (1 => To_Unbounded_String(Source => "Player name"),
                 2 => To_Unbounded_String(Source => "Ship name"),
                 3 => To_Unbounded_String(Source => "Last saved")),
              Command => "SortSaves",
              Tooltip => "Press mouse button to sort the saved games.");
      else
         ClearTable(Table => Local_Load_Table);
      end if;
      Start_Search
        (Search => Files, Directory => To_String(Source => Save_Directory),
         Pattern => "*.sav");
      Load_Saves_List_Loop :
      while More_Entries(Search => Files) loop
         Get_Next_Entry(Search => Files, Directory_Entry => Found_File);
         Create
           (S => Tokens, From => Simple_Name(Directory_Entry => Found_File),
            Separators => "_");
         Saves.Append
           (New_Item =>
              (Player_Name =>
                 To_Unbounded_String(Source => Slice(S => Tokens, Index => 1)),
               Ship_Name =>
                 To_Unbounded_String(Source => Slice(S => Tokens, Index => 2)),
               Save_Time =>
                 To_Unbounded_String
                   (Source =>
                      Ada.Calendar.Formatting.Image
                        (Date =>
                           Modification_Time(Directory_Entry => Found_File),
                         Include_Time_Fraction => False,
                         Time_Zone => UTC_Time_Offset)),
               File_Name =>
                 To_Unbounded_String
                   (Source => Simple_Name(Directory_Entry => Found_File))));
      end loop Load_Saves_List_Loop;
      End_Search(Search => Files);
      Saves_Sorting.Sort(Container => Saves);
      Show_Saved_Games_Loop :
      for Save of Saves loop
         AddButton
           (Table => Local_Load_Table,
            Text => To_String(Source => Save.Player_Name),
            Tooltip =>
              "Press mouse " &
              (if Game_Settings.Right_Button then "right" else "left") &
              " button to show available options",
            Command =>
              "ShowLoadGameMenu " & To_String(Source => Save.File_Name),
            Column => 1);
         AddButton
           (Table => Local_Load_Table,
            Text => To_String(Source => Save.Ship_Name),
            Tooltip =>
              "Press mouse " &
              (if Game_Settings.Right_Button then "right" else "left") &
              " button to show available options",
            Command =>
              "ShowLoadGameMenu " & To_String(Source => Save.File_Name),
            Column => 2);
         AddButton
           (Table => Local_Load_Table,
            Text => To_String(Source => Save.Save_Time),
            Tooltip =>
              "Press mouse " &
              (if Game_Settings.Right_Button then "right" else "left") &
              " button to show available options",
            Command =>
              "ShowLoadGameMenu " & To_String(Source => Save.File_Name),
            Column => 3, NewRow => True);
      end loop Show_Saved_Games_Loop;
      UpdateTable(Table => Local_Load_Table);
      if Local_Load_Table.Row = 1 then
         Unbind_From_Main_Window(Interp => Interp, Sequence => "<Alt-b>");
         Unbind_From_Main_Window(Interp => Interp, Sequence => "<Escape>");
         Tcl.Tk.Ada.Pack.Pack_Forget
           (Slave => Ttk_Frame'(Get_Widget(pathName => ".loadmenu")));
         Show_Main_Menu;
      end if;
      Load_Table := Local_Load_Table;
      return TCL_OK;
   end Show_Load_Game_Command;

   -- ****o* MCommands/MCommands.Delete_Game_Command
   -- FUNCTION
   -- Delete the selected save file
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- DeleteGame file
   -- File is the name of the saved game to delete
   -- SOURCE
   function Delete_Game_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Delete_Game_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc);
   begin
      Tcl_SetVar
        (interp => Interp, varName => "deletesave",
         newValue => CArgv.Arg(Argv => Argv, N => 1));
      ShowQuestion
        (Question => "Are you sure you want delete this savegame?",
         Result => "deletesave", In_Game => False);
      return TCL_OK;
   end Delete_Game_Command;

   -- ****if* MCommands/MCommands.StartGame
   -- FUNCTION
   -- Start the game
   -- SOURCE
   procedure Start_Game is
      -- ****
      Main_Window: constant Tk_Toplevel :=
        Get_Main_Window(Interp => Get_Context);
      X, Y: Integer;
   begin
      X :=
        (Positive'Value
           (Winfo_Get(Widgt => Main_Window, Info => "vrootwidth")) -
         Game_Settings.Window_Width) /
        2;
      if X < 0 then
         X := 0;
      end if;
      Y :=
        (Positive'Value
           (Winfo_Get(Widgt => Main_Window, Info => "vrootheight")) -
         Game_Settings.Window_Height) /
        2;
      if Y < 0 then
         Y := 0;
      end if;
      Wm_Set
        (Widgt => Main_Window, Action => "geometry",
         Options =>
           Trim
             (Source => Positive'Image(Game_Settings.Window_Width),
              Side => Left) &
           "x" &
           Trim
             (Source => Positive'Image(Game_Settings.Window_Height),
              Side => Left) &
           "+" & Trim(Source => Positive'Image(X), Side => Left) & "+" &
           Trim(Source => Positive'Image(Y), Side => Left));
      GenerateTraders;
      CreateGameUI;
   end Start_Game;

   -- ****o* MCommands/MCommands.Load_Game_Command
   -- FUNCTION
   -- Load the selected save file and start the game
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- LoadGame file
   -- File is the name of the saved game which will be loaded
   -- SOURCE
   function Load_Game_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Load_Game_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc);
   begin
      Tcl.Tk.Ada.Pack.Pack_Forget
        (Slave => Ttk_Frame'(Get_Widget(pathName => ".loadmenu")));
      Save_Name := Save_Directory & CArgv.Arg(Argv => Argv, N => 1);
      Load_Game;
      Start_Game;
      return TCL_OK;
   exception
      when An_Exception : Save_Game_Invalid_Data =>
         Show_Main_Menu;
         ShowMessage
           (Text =>
              "Can't load this game. Reason: " &
              Exception_Message(X => An_Exception),
            ParentFrame => ".", Title => "Can't load the game");
         return TCL_OK;
   end Load_Game_Command;

   -- ****o* MCommands/MCommands.Set_Faction_Command
   -- FUNCTION
   -- Set faction destription and available bases and careers
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetFaction
   -- SOURCE
   function Set_Faction_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Faction_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc, Argv);
      Faction_Name: Unbounded_String;
      Values: Unbounded_String := Null_Unbounded_String;
      Frame_Name: constant String := ".newgamemenu.canvas.player";
      Combo_Box: Ttk_ComboBox :=
        Get_Widget(pathName => Frame_Name & ".faction", Interp => Interp);
      Label: Ttk_Label := Get_Widget(pathName => ".", Interp => Interp);
      Gender_Frame: constant Ttk_Frame :=
        Get_Widget(pathName => Frame_Name & ".gender", Interp => Interp);
      procedure Update_Info(New_Text: String) is
         Info_Text: constant Tk_Text :=
           Get_Widget(pathName => ".newgamemenu.info.text", Interp => Interp);
      begin
         configure(Widgt => Info_Text, options => "-state normal");
         Delete
           (TextWidget => Info_Text, StartIndex => "1.0", Indexes => "end");
         Insert
           (TextWidget => Info_Text, Index => "end",
            Text =>
              "{Select your faction from a list. Factions have the biggest impact on game. They determine the amount of bases and some playing styles. More information about each faction can be found after selecting it. You can't change this later." &
              LF & LF & "}");
         Insert(TextWidget => Info_Text, Index => "end", Text => New_Text);
         configure(Widgt => Info_Text, options => "-state disabled");
      end Update_Info;
   begin
      Faction_Name := To_Unbounded_String(Source => Get(Widgt => Combo_Box));
      if Faction_Name = To_Unbounded_String(Source => "Random") then
         Label.Name := New_String(Str => Frame_Name & ".labelcareer");
         Grid_Remove(Slave => Label);
         Combo_Box.Name := New_String(Str => Frame_Name & ".career");
         Set(ComboBox => Combo_Box, Value => "Random");
         Grid_Remove(Slave => Combo_Box);
         Label.Name := New_String(Str => Frame_Name & ".labelbase");
         Grid_Remove(Slave => Label);
         Combo_Box.Name := New_String(Str => Frame_Name & ".base");
         Set(ComboBox => Combo_Box, Value => "Any");
         Grid_Remove(Slave => Combo_Box);
         Update_Info
           (New_Text =>
              "{Faction, career and base type will be randomly selected for you during creating new game. Not recommended for new player.}");
         return TCL_OK;
      end if;
      Label.Name := New_String(Str => Frame_Name & ".labelcareer");
      Tcl.Tk.Ada.Grid.Grid(Slave => Label);
      Combo_Box.Name := New_String(Str => Frame_Name & ".career");
      Tcl.Tk.Ada.Grid.Grid(Slave => Combo_Box);
      Label.Name := New_String(Str => Frame_Name & ".labelbase");
      Tcl.Tk.Ada.Grid.Grid(Slave => Label);
      Combo_Box.Name := New_String(Str => Frame_Name & ".base");
      Tcl.Tk.Ada.Grid.Grid(Slave => Combo_Box);
      Load_Faction_Based_Info_Loop :
      for Faction of Factions_List loop
         if Faction.Name /= Faction_Name then
            goto End_Of_Faction_Info_Loop;
         end if;
         if Faction.Flags.Contains
             (Item => To_Unbounded_String(Source => "nogender")) then
            Label.Name := New_String(Str => Frame_Name & ".labelgender");
            Grid_Remove(Slave => Label);
            Grid_Remove(Slave => Gender_Frame);
            Tcl_SetVar
              (interp => Interp, varName => "playergender", newValue => "M");
         else
            Label.Name := New_String(Str => Frame_Name & ".labelgender");
            Tcl.Tk.Ada.Grid.Grid(Slave => Label);
            Tcl.Tk.Ada.Grid.Grid(Slave => Gender_Frame);
         end if;
         Values := Null_Unbounded_String;
         Load_Careers_Loop :
         for I in Faction.Careers.Iterate loop
            Append
              (Source => Values, New_Item => " " & Faction.Careers(I).Name);
         end loop Load_Careers_Loop;
         Append(Source => Values, New_Item => " Random");
         Combo_Box.Name := New_String(Str => Frame_Name & ".career");
         configure
           (Widgt => Combo_Box,
            options => "-values [list " & To_String(Source => Values) & "]");
         Set(ComboBox => Combo_Box, Value => "General");
         Values := To_Unbounded_String(Source => " Any");
         Load_Bases_Types_Loop :
         for I in Faction.BasesTypes.Iterate loop
            Append
              (Source => Values,
               New_Item =>
                 " {" &
                 BasesTypes_List(BaseType_Container.Key(Position => I)).Name &
                 "}");
         end loop Load_Bases_Types_Loop;
         Combo_Box.Name := New_String(Str => Frame_Name & ".base");
         configure
           (Widgt => Combo_Box,
            options => "-values [list " & To_String(Source => Values) & "]");
         Set(ComboBox => Combo_Box, Value => "Any");
         Update_Info
           (New_Text => "{" & To_String(Source => Faction.Description) & "}");
         exit Load_Faction_Based_Info_Loop;
         <<End_Of_Faction_Info_Loop>>
      end loop Load_Faction_Based_Info_Loop;
      return TCL_OK;
   end Set_Faction_Command;

   -- ****o* MCommands/MCommands.Set_Career_Command
   -- FUNCTION
   -- Set career description
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetCareer
   -- SOURCE
   function Set_Career_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Career_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc, Argv);
      Faction_Name, Career_Name: Unbounded_String;
      Frame_Name: constant String := ".newgamemenu.canvas.player";
      Combo_Box: Ttk_ComboBox :=
        Get_Widget(pathName => Frame_Name & ".faction", Interp => Interp);
      Info_Text: constant Tk_Text :=
        Get_Widget(pathName => ".newgamemenu.info.text", Interp => Interp);
   begin
      Faction_Name := To_Unbounded_String(Source => Get(Widgt => Combo_Box));
      Combo_Box.Name := New_String(Str => Frame_Name & ".career");
      Career_Name := To_Unbounded_String(Source => Get(Widgt => Combo_Box));
      configure(Widgt => Info_Text, options => "-state normal");
      Delete(TextWidget => Info_Text, StartIndex => "1.0", Indexes => "end");
      Insert
        (TextWidget => Info_Text, Index => "end",
         Text =>
           "{Select your career from a list. Careers have some impact on gameplay (each have bonuses to gaining experience in some fields plus they determine your starting ship and crew). More info about each career can be found after selecting it. You can't change career later." &
           LF & LF & "}");
      Set_Faction_Careers_Loop :
      for Faction of Factions_List loop
         if Faction.Name = Faction_Name then
            Load_Careers_Loop :
            for Career of Faction.Careers loop
               if Career.Name = Career_Name then
                  Insert
                    (TextWidget => Info_Text, Index => "end",
                     Text =>
                       "{" & To_String(Source => Career.Description) & "}");
                  exit Set_Faction_Careers_Loop;
               end if;
            end loop Load_Careers_Loop;
         end if;
      end loop Set_Faction_Careers_Loop;
      if Career_Name = "Random" then
         Insert
           (TextWidget => Info_Text, Index => "end",
            Text =>
              "{Career will be randomly selected for you during creating new game. Not recommended for new player.}");
      end if;
      configure(Widgt => Info_Text, options => "-state disabled");
      return TCL_OK;
   end Set_Career_Command;

   -- ****o* MCommands/MCommands.Set_Base_Command
   -- FUNCTION
   -- Set starting base description
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetBase
   -- SOURCE
   function Set_Base_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Base_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc, Argv);
      Base_Name: Unbounded_String;
      Combo_Box: constant Ttk_ComboBox :=
        Get_Widget
          (pathName => ".newgamemenu.canvas.player.base", Interp => Interp);
      Info_Text: constant Tk_Text :=
        Get_Widget(pathName => ".newgamemenu.info.text", Interp => Interp);
   begin
      Base_Name := To_Unbounded_String(Source => Get(Widgt => Combo_Box));
      configure(Widgt => Info_Text, options => "-state normal");
      Delete(TextWidget => Info_Text, StartIndex => "1.0", Indexes => "end");
      Insert
        (TextWidget => Info_Text, Index => "end",
         Text =>
           "{Select your career from a list. Careers have some impact on gameplay (each have bonuses to gaining experience in some fields plus they determine your starting ship and crew). More info about each career can be found after selecting it. You can't change career later." &
           LF & LF & "}");
      Find_Base_Type_Loop :
      for Base of BasesTypes_List loop
         if Base.Name = Base_Name then
            Insert
              (TextWidget => Info_Text, Index => "end",
               Text => "{" & To_String(Source => Base.Description) & "}");
            exit Find_Base_Type_Loop;
         end if;
      end loop Find_Base_Type_Loop;
      if Base_Name = "Any" then
         Insert
           (TextWidget => Info_Text, Index => "end",
            Text => "{Start the game in randomly selected base type.}");
      end if;
      configure(Widgt => Info_Text, options => "-state disabled");
      return TCL_OK;
   end Set_Base_Command;

   -- ****o* MCommands/MCommands.Random_Name_Command
   -- FUNCTION
   -- Generate random player or ship name
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- RandomName type
   -- Type is type of name which should be generated. Possible options are
   -- player or ship
   -- SOURCE
   function Random_Name_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Random_Name_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc);
      Combo_Box: constant Ttk_ComboBox :=
        Get_Widget
          (pathName => ".newgamemenu.canvas.player.faction", Interp => Interp);
      Faction_Name: constant Unbounded_String :=
        To_Unbounded_String(Source => Get(Widgt => Combo_Box));
      Faction_Index: Unbounded_String := Null_Unbounded_String;
      Gender: Character := 'M';
      Name_Entry: constant Ttk_Entry :=
        Get_Widget
          (pathName =>
             ".newgamemenu.canvas.player." & CArgv.Arg(Argv => Argv, N => 1) &
             "name",
           Interp => Interp);
   begin
      Find_Faction_Index_Loop :
      for I in Factions_List.Iterate loop
         if Factions_List(I).Name = Faction_Name then
            Faction_Index := Factions_Container.Key(Position => I);
            exit Find_Faction_Index_Loop;
         end if;
      end loop Find_Faction_Index_Loop;
      if CArgv.Arg(Argv => Argv, N => 1) = "player" then
         Gender := Tcl_GetVar(interp => Interp, varName => "playergender")(1);
         Delete
           (TextEntry => Name_Entry, FirstIndex => "0", LastIndex => "end");
         Insert
           (TextEntry => Name_Entry, Index => "end",
            Text =>
              To_String
                (Source =>
                   GenerateMemberName
                     (Gender => Gender, FactionIndex => Faction_Index)));
         return TCL_OK;
      end if;
      Delete(TextEntry => Name_Entry, FirstIndex => "0", LastIndex => "end");
      Insert
        (TextEntry => Name_Entry, Index => "end",
         Text =>
           To_String(Source => Generate_Ship_Name(Owner => Faction_Index)));
      return TCL_OK;
   end Random_Name_Command;

   -- ****o* MCommands/MCommands.New_Game_Command
   -- FUNCTION
   -- Set all parameters and start a new game
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- NewGame
   -- SOURCE
   function New_Game_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function New_Game_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc, Argv);
      Player_Frame_Name: constant String := ".newgamemenu.canvas.player";
      Difficulty_Frame_Name: constant String :=
        ".newgamemenu.canvas.difficulty";
      Combo_Box: Ttk_ComboBox :=
        Get_Widget
          (pathName => Player_Frame_Name & ".faction", Interp => Interp);
      Goal_Button: constant Ttk_Button :=
        Get_Widget(pathName => Player_Frame_Name & ".goal", Interp => Interp);
      Text_Entry: Ttk_Entry :=
        Get_Widget
          (pathName => Player_Frame_Name & ".playername", Interp => Interp);
      Spin_Box: Ttk_SpinBox :=
        Get_Widget
          (pathName => Difficulty_Frame_Name & ".enemydamage",
           Interp => Interp);
   begin
      New_Game_Settings.Player_Gender :=
        Tcl_GetVar(interp => Interp, varName => "playergender")(1);
      if cget(Widgt => Goal_Button, option => "-text") = "Random" then
         ClearCurrentGoal;
         CurrentGoal :=
           Goals_List
             (GetRandom
                (Min => Goals_List.First_Index, Max => Goals_List.Last_Index));
      end if;
      New_Game_Settings.Player_Name :=
        To_Unbounded_String(Source => Get(Widgt => Text_Entry));
      Text_Entry.Name := New_String(Str => Player_Frame_Name & ".shipname");
      New_Game_Settings.Ship_Name :=
        To_Unbounded_String(Source => Get(Widgt => Text_Entry));
      Find_Faction_Loop :
      for I in Factions_List.Iterate loop
         if Factions_List(I).Name =
           To_Unbounded_String(Source => Get(Widgt => Combo_Box)) then
            New_Game_Settings.Player_Faction :=
              Factions_Container.Key(Position => I);
            Combo_Box.Name := New_String(Str => Player_Frame_Name & ".career");
            Find_Career_Loop :
            for J in Factions_List(I).Careers.Iterate loop
               if Factions_List(I).Careers(J).Name =
                 To_Unbounded_String(Source => Get(Widgt => Combo_Box)) then
                  New_Game_Settings.Player_Career :=
                    Careers_Container.Key(Position => J);
                  exit Find_Faction_Loop;
               end if;
            end loop Find_Career_Loop;
         end if;
      end loop Find_Faction_Loop;
      Combo_Box.Name := New_String(Str => Player_Frame_Name & ".base");
      Set_Starting_Base_Loop :
      for I in BasesTypes_List.Iterate loop
         if BasesTypes_List(I).Name =
           To_Unbounded_String(Source => Get(Widgt => Combo_Box)) then
            New_Game_Settings.Starting_Base :=
              BasesTypes_Container.Key(Position => I);
            exit Set_Starting_Base_Loop;
         end if;
      end loop Set_Starting_Base_Loop;
      Combo_Box.Name :=
        New_String(Str => Difficulty_Frame_Name & ".difficultylevel");
      New_Game_Settings.Difficulty_Level :=
        Difficulty_Type'Val(Natural'Value(Current(ComboBox => Combo_Box)));
      New_Game_Settings.Enemy_Damage_Bonus :=
        Bonus_Type'Value(Get(Widgt => Spin_Box)) / 100.0;
      Spin_Box.Name :=
        New_String(Str => Difficulty_Frame_Name & ".playerdamage");
      New_Game_Settings.Player_Damage_Bonus :=
        Bonus_Type'Value(Get(Widgt => Spin_Box)) / 100.0;
      --## rule off ASSIGNMENTS
      Spin_Box.Name :=
        New_String(Str => Difficulty_Frame_Name & ".enemymeleedamage");
      New_Game_Settings.Enemy_Melee_Damage_Bonus :=
        Bonus_Type'Value(Get(Widgt => Spin_Box)) / 100.0;
      Spin_Box.Name :=
        New_String(Str => Difficulty_Frame_Name & ".playermeleedamage");
      New_Game_Settings.Player_Melee_Damage_Bonus :=
        Bonus_Type'Value(Get(Widgt => Spin_Box)) / 100.0;
      Spin_Box.Name :=
        New_String(Str => Difficulty_Frame_Name & ".experience");
      New_Game_Settings.Experience_Bonus :=
        Bonus_Type'Value(Get(Widgt => Spin_Box)) / 100.0;
      Spin_Box.Name :=
        New_String(Str => Difficulty_Frame_Name & ".reputation");
      New_Game_Settings.Reputation_Bonus :=
        Bonus_Type'Value(Get(Widgt => Spin_Box)) / 100.0;
      Spin_Box.Name := New_String(Str => Difficulty_Frame_Name & ".upgrade");
      New_Game_Settings.Upgrade_Cost_Bonus :=
        Bonus_Type'Value(Get(Widgt => Spin_Box)) / 100.0;
      Spin_Box.Name := New_String(Str => Difficulty_Frame_Name & ".prices");
      --## rule on ASSIGNMENTS
      New_Game_Settings.Prices_Bonus :=
        Bonus_Type'Value(Get(Widgt => Spin_Box)) / 100.0;
      New_Game;
      Start_Game;
      return TCL_OK;
   end New_Game_Command;

   -- ****o* MCommands/MCommands.Show_Main_Menu_Command
   -- FUNCTION
   -- Clear the main game window and show main menu
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowMainMenu
   -- SOURCE
   function Show_Main_Menu_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Main_Menu_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc, Argv);
   begin
      Show_Main_Menu;
      return TCL_OK;
   end Show_Main_Menu_Command;

   -- ****o* MCommands/MCommands.Show_Load_Game_Menu_Command
   -- FUNCTION
   -- Show available options for the selected saved game
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowLoadGameMenu file
   -- File is the filename of the saved game to manipulate
   -- SOURCE
   function Show_Load_Game_Menu_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Load_Game_Menu_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc);
      Load_Menu: Tk_Menu :=
        Get_Widget(pathName => ".loadfilemenu", Interp => Interp);
   begin
      if Winfo_Get(Widgt => Load_Menu, Info => "exists") = "0" then
         Load_Menu :=
           Create(pathName => ".loadfilemenu", options => "-tearoff false");
      end if;
      Delete(MenuWidget => Load_Menu, StartIndex => "0", EndIndex => "end");
      Menu.Add
        (MenuWidget => Load_Menu, EntryType => "command",
         Options =>
           "-label {Load the game} -command {LoadGame " &
           CArgv.Arg(Argv => Argv, N => 1) & "}");
      Menu.Add
        (MenuWidget => Load_Menu, EntryType => "command",
         Options =>
           "-label {Delete the game} -command {DeleteGame " &
           CArgv.Arg(Argv => Argv, N => 1) & "}");
      Tk_Popup
        (MenuWidget => Load_Menu,
         X =>
           Winfo_Get
             (Widgt => Get_Main_Window(Interp => Interp), Info => "pointerx"),
         Y =>
           Winfo_Get
             (Widgt => Get_Main_Window(Interp => Interp), Info => "pointery"));
      return TCL_OK;
   end Show_Load_Game_Menu_Command;

   -- ****o* MCommands/MCommands.Sort_Saves_Command
   -- FUNCTION
   -- Sort the saved games list
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SortSaves x
   -- X is X axis coordinate where the player clicked the mouse button
   -- SOURCE
   function Sort_Saves_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Sort_Saves_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      Column: constant Positive :=
        Get_Column_Number
          (Table => Get_Load_Table,
           X_Position => Natural'Value(CArgv.Arg(Argv => Argv, N => 1)));
   begin
      case Column is
         when 1 =>
            if Get_Save_Sort_Order = PLAYERASC then
               Save_Sort_Order := PLAYERDESC;
            else
               Save_Sort_Order := PLAYERASC;
            end if;
         when 2 =>
            if Get_Save_Sort_Order = SHIPASC then
               Save_Sort_Order := SHIPDESC;
            else
               Save_Sort_Order := SHIPASC;
            end if;
         when 3 =>
            if Get_Save_Sort_Order = TIMEASC then
               Save_Sort_Order := TIMEDESC;
            else
               Save_Sort_Order := TIMEASC;
            end if;
         when others =>
            null;
      end case;
      return
        Show_Load_Game_Command
          (Client_Data => Client_Data, Interp => Interp, Argc => Argc,
           Argv => Argv);
   end Sort_Saves_Command;

   procedure Add_Commands is
   begin
      AddCommand(Name => "OpenLink", AdaCommand => Open_Link_Command'Access);
      AddCommand(Name => "ShowFile", AdaCommand => Show_File_Command'Access);
      AddCommand(Name => "ShowNews", AdaCommand => Show_News_Command'Access);
      AddCommand
        (Name => "ShowHallOfFame",
         AdaCommand => Show_Hall_Of_Fame_Command'Access);
      AddCommand
        (Name => "ShowLoadGame", AdaCommand => Show_Load_Game_Command'Access);
      AddCommand
        (Name => "DeleteGame", AdaCommand => Delete_Game_Command'Access);
      AddCommand(Name => "LoadGame", AdaCommand => Load_Game_Command'Access);
      AddCommand
        (Name => "SetFaction", AdaCommand => Set_Faction_Command'Access);
      AddCommand(Name => "SetCareer", AdaCommand => Set_Career_Command'Access);
      AddCommand(Name => "SetBase", AdaCommand => Set_Base_Command'Access);
      AddCommand
        (Name => "RandomName", AdaCommand => Random_Name_Command'Access);
      AddCommand(Name => "NewGame", AdaCommand => New_Game_Command'Access);
      AddCommand
        (Name => "ShowMainMenu", AdaCommand => Show_Main_Menu_Command'Access);
      AddCommand
        (Name => "ShowLoadGameMenu",
         AdaCommand => Show_Load_Game_Menu_Command'Access);
      AddCommand(Name => "SortSaves", AdaCommand => Sort_Saves_Command'Access);
   end Add_Commands;

end MainMenu.Commands;
