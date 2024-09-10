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

-- with Ada.Calendar.Formatting;
-- with Ada.Calendar.Time_Zones;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
-- with Ada.Containers.Vectors;
-- with Ada.Directories;
with Ada.Exceptions;
with Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C.Strings; use Interfaces.C.Strings;
-- with GNAT.String_Split;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Pack;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Text; use Tcl.Tk.Ada.Widgets.Text;
-- with Tcl.Tk.Ada.Widgets.Toplevel;
-- with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
-- use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkEntry; use Tcl.Tk.Ada.Widgets.TtkEntry;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel;
with BasesTypes; use BasesTypes;
with Combat.UI;
with Config; use Config;
with CoreUI;
with Crew;
with Dialogs; use Dialogs;
with Factions; use Factions;
with Game; use Game;
with Game.SaveLoad;
with Goals;
with Maps.UI;
with Ships;
with Table; use Table;
with Utils;
with Utils.UI; use Utils.UI;

package body MainMenu.Commands is

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
     (PLAYERASC, PLAYERDESC, SHIPASC, SHIPDESC, TIMEASC, TIMEDESC) with
      Default_Value => TIMEDESC;
   -- ****

     -- ****id* MCommands/MCommands.Default_Save_Sort_Order
     -- FUNCTION
     -- Default sorting order for the saved game list
     -- HISTORY
     -- 6.6 - Added
     -- SOURCE
   Default_Save_Sort_Order: constant Save_Sort_Orders := TIMEDESC;
   -- ****

   -- ****iv* MCommands/MCommands.Save_Sort_Order
   -- FUNCTION
   -- The current sorting order for the saved game list
   -- SOURCE
   Save_Sort_Order: Save_Sort_Orders := Default_Save_Sort_Order;
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
   begin
      return Save_Sort_Order;
   end Get_Save_Sort_Order;
   -- ****

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
--      pragma Unreferenced(Client_Data, Argc, Argv);
--      use Ada.Calendar.Time_Zones;
--      use Ada.Containers;
--      use Ada.Directories;
--      use GNAT.String_Split;

--      Files: Search_Type;
--      Found_File: Directory_Entry_Type;
--      Tokens: Slice_Set; --## rule line off IMPROPER_INITIALIZATION
--      type Save_Record is record  --## rule line off TYPE_INITIAL_VALUES
--         Player_Name: Unbounded_String;
--         Ship_Name: Unbounded_String;
--         Save_Time: Unbounded_String;
--         File_Name: Unbounded_String;
--      end record;
--      package Saves_Container is new Vectors
--        (Index_Type => Positive, Element_Type => Save_Record);
--      Saves: Saves_Container.Vector := Saves_Container.Empty_Vector;
--      function "<"(Left, Right: Save_Record) return Boolean is
--      begin
--         if Get_Save_Sort_Order = PLAYERASC
--           and then Left.Player_Name < Right.Player_Name then
--            return True;
--         end if;
--         if Get_Save_Sort_Order = PLAYERDESC
--           and then Left.Player_Name > Right.Player_Name then
--            return True;
--         end if;
--         if Get_Save_Sort_Order = SHIPASC
--           and then Left.Ship_Name < Right.Ship_Name then
--            return True;
--         end if;
--         if Get_Save_Sort_Order = SHIPDESC
--           and then Left.Ship_Name > Right.Ship_Name then
--            return True;
--         end if;
--         if Get_Save_Sort_Order = TIMEASC
--           and then Left.Save_Time < Right.Save_Time then
--            return True;
--         end if;
--         if Get_Save_Sort_Order = TIMEDESC
--           and then Left.Save_Time > Right.Save_Time then
--            return True;
--         end if;
--         return False;
--      end "<";
--      package Saves_Sorting is new Saves_Container.Generic_Sorting;
      Local_Load_Table: Table_Widget := Get_Load_Table;
      function Show_Ada_Load_Game_Command
        (C_Data: Integer; I: Tcl.Tcl_Interp; Ac: Interfaces.C.int;
         Av: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
         Import => True,
         Convention => C,
         External_Name => "showLoadGameCommand";
   begin
      if Local_Load_Table.Row_Height = 1 then
         Local_Load_Table :=
           Create_Table
             (Parent => ".loadmenu.list",
              Headers =>
                (1 => To_Unbounded_String(Source => "Player name"),
                 2 => To_Unbounded_String(Source => "Ship name"),
                 3 => To_Unbounded_String(Source => "Last saved")),
              Command => "SortSaves",
              Tooltip_Text => "Press mouse button to sort the saved games.");
      else
         Clear_Table(Table => Local_Load_Table);
      end if;
      Load_Table := Local_Load_Table;
      return Show_Ada_Load_Game_Command(C_Data => Client_Data, I => Interp, Ac => Argc, Av => Argv);
--      Start_Search
--        (Search => Files, Directory => To_String(Source => Save_Directory),
--         Pattern => "*.sav");
--      Load_Saves_List_Loop :
--      while More_Entries(Search => Files) loop
--         Get_Next_Entry(Search => Files, Directory_Entry => Found_File);
--         Create
--           (S => Tokens, From => Simple_Name(Directory_Entry => Found_File),
--            Separators => "_");
--         if Slice_Count(S => Tokens) = 3 then
--            Saves.Append
--              (New_Item =>
--                 (Player_Name =>
--                    To_Unbounded_String
--                      (Source => Slice(S => Tokens, Index => 1)),
--                  Ship_Name =>
--                    To_Unbounded_String
--                      (Source => Slice(S => Tokens, Index => 2)),
--                  Save_Time =>
--                    To_Unbounded_String
--                      (Source =>
--                         Ada.Calendar.Formatting.Image
--                           (Date =>
--                              Modification_Time(Directory_Entry => Found_File),
--                            Include_Time_Fraction => False,
--                            Time_Zone => UTC_Time_Offset)),
--                  File_Name =>
--                    To_Unbounded_String
--                      (Source => Simple_Name(Directory_Entry => Found_File))));
--         else
--            Saves.Append
--              (New_Item =>
--                 (Player_Name => To_Unbounded_String(Source => "Unknown"),
--                  Ship_Name => To_Unbounded_String(Source => "Unknown"),
--                  Save_Time =>
--                    To_Unbounded_String
--                      (Source =>
--                         Ada.Calendar.Formatting.Image
--                           (Date =>
--                              Modification_Time(Directory_Entry => Found_File),
--                            Include_Time_Fraction => False,
--                            Time_Zone => UTC_Time_Offset)),
--                  File_Name =>
--                    To_Unbounded_String
--                      (Source => Simple_Name(Directory_Entry => Found_File))));
--         end if;
--      end loop Load_Saves_List_Loop;
--      End_Search(Search => Files);
--      Saves_Sorting.Sort(Container => Saves);
--      Show_Saved_Games_Loop :
--      for Save of Saves loop
--         Add_Button
--           (Table => Local_Load_Table,
--            Text => To_String(Source => Save.Player_Name),
--            Tooltip =>
--              "Press mouse " &
--              (if Get_Boolean_Setting(Name => "rightButton") then "right"
--               else "left") &
--              " button to show available options",
--            Command =>
--              "ShowLoadGameMenu " & To_String(Source => Save.File_Name),
--            Column => 1);
--         Add_Button
--           (Table => Local_Load_Table,
--            Text => To_String(Source => Save.Ship_Name),
--            Tooltip =>
--              "Press mouse " &
--              (if Get_Boolean_Setting(Name => "rightButton") then "right"
--               else "left") &
--              " button to show available options",
--            Command =>
--              "ShowLoadGameMenu " & To_String(Source => Save.File_Name),
--            Column => 2);
--         Add_Button
--           (Table => Local_Load_Table,
--            Text => To_String(Source => Save.Save_Time),
--            Tooltip =>
--              "Press mouse " &
--              (if Get_Boolean_Setting(Name => "rightButton") then "right"
--               else "left") &
--              " button to show available options",
--            Command =>
--              "ShowLoadGameMenu " & To_String(Source => Save.File_Name),
--            Column => 3, New_Row => True);
--      end loop Show_Saved_Games_Loop;
--      Update_Table(Table => Local_Load_Table);
--      if Local_Load_Table.Row = 1 then
--         Unbind_From_Main_Window(Interp => Interp, Sequence => "<Alt-b>");
--         Unbind_From_Main_Window(Interp => Interp, Sequence => "<Escape>");
--         Tcl.Tk.Ada.Pack.Pack_Forget
--           (Slave => Ttk_Frame'(Get_Widget(pathName => ".loadmenu")));
--         Show_Main_Menu;
--      end if;
--      return TCL_OK;
   end Show_Load_Game_Command;

   -- ****if* MCommands/MCommands.StartGame
   -- FUNCTION
   -- Start the game
   -- SOURCE
   procedure Start_Game is
      -- ****
      use Maps.UI;

      procedure Start_Ada_Game with
         Convention => C,
         Import => True,
         External_Name => "startGame";
   begin
      Start_Ada_Game;
      Create_Game_Ui;
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
      use Ada.Exceptions;
      use Game.SaveLoad;

   begin
      Tcl.Tk.Ada.Pack.Pack_Forget
        (Slave => Ttk_Frame'(Get_Widget(pathName => ".loadmenu")));
      Load_Game
        (File_Name =>
           To_String(Source => Save_Directory) &
           CArgv.Arg(Argv => Argv, N => 1));
      Start_Game;
      return TCL_OK;
   exception
      when An_Exception : Save_Game_Invalid_Data =>
         Show_Main_Menu;
         Show_Message
           (Text =>
              "Can't load this game. Reason: " &
              Exception_Message(X => An_Exception),
            Parent_Frame => ".", Title => "Can't load the game");
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
      use Tcl.Tk.Ada.Grid;
      use Tcl.Tk.Ada.Widgets.TtkLabel;
      use Tiny_String;

      Faction_Name: Bounded_String;
      Values: Unbounded_String := Null_Unbounded_String;
      Frame_Name: constant String := ".newgamemenu.canvas.player";
      Combo_Box: Ttk_ComboBox :=
        Get_Widget(pathName => Frame_Name & ".faction", Interp => Interp);
      Label: Ttk_Label := Get_Widget(pathName => ".", Interp => Interp);
      Gender_Frame: constant Ttk_Frame :=
        Get_Widget(pathName => Frame_Name & ".gender", Interp => Interp);
      Faction: Faction_Record; --## rule line off IMPROPER_INITIALIZATION
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
      Faction_Name := To_Bounded_String(Source => Get(Widgt => Combo_Box));
      if Faction_Name = To_Bounded_String(Source => "Random") then
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
      for I in 1 .. Get_Factions_Amount loop
         Faction := Get_Faction(Number => I);
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
         for J in Faction.Careers.Iterate loop
            Append
              (Source => Values, New_Item => " " & Faction.Careers(J).Name);
         end loop Load_Careers_Loop;
         Append(Source => Values, New_Item => " Random");
         Combo_Box.Name := New_String(Str => Frame_Name & ".career");
         configure
           (Widgt => Combo_Box,
            options => "-values [list " & To_String(Source => Values) & "]");
         Set(ComboBox => Combo_Box, Value => "General");
         Values := To_Unbounded_String(Source => " Any");
         Load_Bases_Types_Loop :
         for J in Faction.Bases_Types.Iterate loop
            Append
              (Source => Values,
               New_Item =>
                 " {" &
                 Get_Base_Type_Name
                   (Base_Type => BaseType_Container.Key(Position => J)) &
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
      use Tiny_String;

      Faction_Name: Bounded_String;
      Career_Name: Unbounded_String;
      Frame_Name: constant String := ".newgamemenu.canvas.player";
      Combo_Box: Ttk_ComboBox :=
        Get_Widget(pathName => Frame_Name & ".faction", Interp => Interp);
      Info_Text: constant Tk_Text :=
        Get_Widget(pathName => ".newgamemenu.info.text", Interp => Interp);
      Faction: Faction_Record; --## rule line off IMPROPER_INITIALIZATION
   begin
      Faction_Name := To_Bounded_String(Source => Get(Widgt => Combo_Box));
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
      for I in 1 .. Get_Factions_Amount loop
         Faction := Get_Faction(Number => I);
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
           "{Select your starting base type from a list. Your starting base is your home base, where you can gain faster experience. Home base can be changed later. Some types of bases are better starting points than others. More info about each base type can be found after selecting it." &
           LF & LF & "}");
      Find_Base_Type_Loop :
      for Base_Type of Bases_Types loop
         exit Find_Base_Type_Loop when Tiny_String.Length
             (Source => Base_Type) =
           0;
         if Get_Base_Type_Name(Base_Type => Base_Type) = Base_Name then
            Insert
              (TextWidget => Info_Text, Index => "end",
               Text =>
                 "{" & Get_Base_Type_Description(Base_Type => Base_Type) &
                 "}");
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
      use Crew;
      use Ships;
      use Tiny_String;

      Combo_Box: constant Ttk_ComboBox :=
        Get_Widget
          (pathName => ".newgamemenu.canvas.player.faction", Interp => Interp);
      Faction_Name: constant Bounded_String :=
        To_Bounded_String(Source => Get(Widgt => Combo_Box));
      Faction_Index: Bounded_String := Null_Bounded_String;
      Gender: Character := 'M';
      Name_Entry: constant Ttk_Entry :=
        Get_Widget
          (pathName =>
             ".newgamemenu.canvas.player." & CArgv.Arg(Argv => Argv, N => 1) &
             "name",
           Interp => Interp);
   begin
      Find_Faction_Index_Loop :
      for I in 1 .. Get_Factions_Amount loop
         if Get_Faction(Number => I).Name = Faction_Name then
            Faction_Index := Get_Faction_Index(Number => I);
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
                   Generate_Member_Name
                     (Gender => Gender, Faction_Index => Faction_Index)));
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
      use Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
      use Goals;
      use Tiny_String;
      use Utils;

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
      Faction: Faction_Record; --## rule line off IMPROPER_INITIALIZATION
   begin
      Set_Gender
        (Value => Tcl_GetVar(interp => Interp, varName => "playergender")(1));
      if cget(Widgt => Goal_Button, option => "-text") = "Random" then
         Clear_Current_Goal;
         Set_Current_Goal
           (Index => Get_Random(Min => 1, Max => Get_Goals_Amount));
      end if;
      Set_String_Setting
        (Name => "playerName", Value => Get(Widgt => Text_Entry));
      Text_Entry.Name := New_String(Str => Player_Frame_Name & ".shipname");
      Set_String_Setting
        (Name => "shipName", Value => Get(Widgt => Text_Entry));
      if Get(Widgt => Combo_Box) = "Random" then
         Set_String_Setting(Name => "playerFaction", Value => "random");
      else
         Find_Faction_Loop :
         for I in 1 .. Get_Factions_Amount loop
            Faction := Get_Faction(Number => I);
            if Faction.Name =
              To_Bounded_String(Source => Get(Widgt => Combo_Box)) then
               Set_String_Setting
                 (Name => "playerFaction",
                  Value =>
                    To_String(Source => Get_Faction_Index(Number => I)));
               Combo_Box.Name :=
                 New_String(Str => Player_Frame_Name & ".career");
               Find_Career_Loop :
               for J in Faction.Careers.Iterate loop
                  if Faction.Careers(J).Name =
                    To_Unbounded_String(Source => Get(Widgt => Combo_Box)) then
                     Set_String_Setting
                       (Name => "playerCareer",
                        Value =>
                          To_String
                            (Source => Careers_Container.Key(Position => J)));
                     exit Find_Faction_Loop;
                  end if;
               end loop Find_Career_Loop;
            end if;
         end loop Find_Faction_Loop;
      end if;
      Combo_Box.Name := New_String(Str => Player_Frame_Name & ".career");
      if Get(Widgt => Combo_Box) = "Random" then
         Set_String_Setting(Name => "playerCareer", Value => "random");
      end if;
      Combo_Box.Name := New_String(Str => Player_Frame_Name & ".base");
      Set_String_Setting(Name => "startingBase", Value => "Any");
      Set_Starting_Base_Loop :
      for Base_Type of Bases_Types loop
         exit Set_Starting_Base_Loop when Length(Source => Base_Type) = 0;
         if Get_Base_Type_Name(Base_Type => Base_Type) =
           Get(Widgt => Combo_Box) then
            Set_String_Setting
              (Name => "startingBase",
               Value => To_String(Source => Base_Type));
            exit Set_Starting_Base_Loop;
         end if;
      end loop Set_Starting_Base_Loop;
      Combo_Box.Name :=
        New_String(Str => Difficulty_Frame_Name & ".difficultylevel");
      Set_Difficulty
        (Value =>
           Difficulty_Type'Val(Natural'Value(Current(ComboBox => Combo_Box))));
      Set_Float_Setting
        (Name => "enemyDamageBonus",
         Value => Bonus_Type'Value(Get(Widgt => Spin_Box)) / 100.0);
      Spin_Box.Name :=
        New_String(Str => Difficulty_Frame_Name & ".playerdamage");
      Set_Float_Setting
        (Name => "playerDamageBonus",
         Value => Bonus_Type'Value(Get(Widgt => Spin_Box)) / 100.0);
      --## rule off ASSIGNMENTS
      Spin_Box.Name :=
        New_String(Str => Difficulty_Frame_Name & ".enemymeleedamage");
      Set_Float_Setting
        (Name => "enemyMeleeDamageBonus",
         Value => Bonus_Type'Value(Get(Widgt => Spin_Box)) / 100.0);
      Spin_Box.Name :=
        New_String(Str => Difficulty_Frame_Name & ".playermeleedamage");
      Set_Float_Setting
        (Name => "playerMeleeDamageBonus",
         Value => Bonus_Type'Value(Get(Widgt => Spin_Box)) / 100.0);
      Spin_Box.Name :=
        New_String(Str => Difficulty_Frame_Name & ".experience");
      Set_Float_Setting
        (Name => "experienceBonus",
         Value => Bonus_Type'Value(Get(Widgt => Spin_Box)) / 100.0);
      Spin_Box.Name :=
        New_String(Str => Difficulty_Frame_Name & ".reputation");
      Set_Float_Setting
        (Name => "reputationBonus",
         Value => Bonus_Type'Value(Get(Widgt => Spin_Box)) / 100.0);
      Spin_Box.Name := New_String(Str => Difficulty_Frame_Name & ".upgrade");
      Set_Float_Setting
        (Name => "upgradeCostBonus",
         Value => Bonus_Type'Value(Get(Widgt => Spin_Box)) / 100.0);
      Spin_Box.Name := New_String(Str => Difficulty_Frame_Name & ".prices");
      --## rule on ASSIGNMENTS
      Set_Float_Setting
        (Name => "pricesBonus",
         Value => Bonus_Type'Value(Get(Widgt => Spin_Box)) / 100.0);
      New_Game;
      Start_Game;
      return TCL_OK;
   end New_Game_Command;

   -- ****o* MCommands/MCommands.Show_Main_Menu_Command
   -- FUNCTION
   -- Clear the main game window and show main menu
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
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
      pragma Unreferenced(Client_Data, Argc, Argv);
      use CoreUI;

   begin
      Widgets.configure
        (Widgt => Close_Button, options => "-command ShowSkyMap");
      Tcl_SetVar
        (interp => Interp, varName => "gamestate", newValue => "general");
      Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Close_Button);
      Show_Screen(New_Screen_Name => "mapframe");
      Tcl_Eval(interp => Get_Context, strng => "DrawMap");
      Tcl_Eval(interp => Get_Context, strng => "update");
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
      pragma Unreferenced(Client_Data, Interp, Argc);

      Load_Menu: constant Ttk_Frame :=
        Create_Dialog
          (Name => ".loadfilemenu", Title => "Actions", Parent_Name => ".");
      procedure Add_Button(Name, Label, Command: String) is
         Button: constant Ttk_Button :=
           Create
             (pathName => Load_Menu & Name,
              options =>
                "-text {" & Label & "} -command {CloseDialog " & Load_Menu &
                " .;" & Command & "}");
      begin
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Button,
            Options =>
              "-sticky we -padx 5" &
              (if Command'Length = 0 then " -pady {0 3}" else ""));
         Bind
           (Widgt => Button, Sequence => "<Escape>",
            Script => "{CloseDialog " & Load_Menu & " .;break}");
         if Command'Length = 0 then
            Bind
              (Widgt => Button, Sequence => "<Tab>",
               Script => "{focus " & Load_Menu & ".load;break}");
            Focus(Widgt => Button);
         end if;
      end Add_Button;
   begin
      Add_Button
        (Name => ".load", Label => "Load the game",
         Command => "LoadGame " & CArgv.Arg(Argv => Argv, N => 1));
      Add_Button
        (Name => ".delete", Label => "Delete the game",
         Command => "DeleteGame " & CArgv.Arg(Argv => Argv, N => 1));
      Add_Button(Name => ".close", Label => "Close", Command => "");
      Show_Dialog(Dialog => Load_Menu, Parent_Frame => ".");
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

   function Show_File_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "showFileCommand";

   function Show_News_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "showNewsCommand";

   function Show_Hall_Of_Fame_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "showHallOfFameCommand";

   function Delete_Game_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "deleteGameCommand";

   procedure Add_Commands is
   begin
      Add_Command(Name => "OpenLink", Ada_Command => Open_Link_Command'Access);
      Add_Command(Name => "ShowFile", Ada_Command => Show_File_Command'Access);
      Add_Command(Name => "ShowNews", Ada_Command => Show_News_Command'Access);
      Add_Command
        (Name => "ShowHallOfFame",
         Ada_Command => Show_Hall_Of_Fame_Command'Access);
      Add_Command
        (Name => "DeleteGame", Ada_Command => Delete_Game_Command'Access);
      Add_Command
        (Name => "ShowLoadGame", Ada_Command => Show_Load_Game_Command'Access);
      Add_Command(Name => "LoadGame", Ada_Command => Load_Game_Command'Access);
      Add_Command
        (Name => "SetFaction", Ada_Command => Set_Faction_Command'Access);
      Add_Command
        (Name => "SetCareer", Ada_Command => Set_Career_Command'Access);
      Add_Command(Name => "SetBase", Ada_Command => Set_Base_Command'Access);
      Add_Command
        (Name => "RandomName", Ada_Command => Random_Name_Command'Access);
      Add_Command(Name => "NewGame", Ada_Command => New_Game_Command'Access);
      Add_Command
        (Name => "ShowMainMenu", Ada_Command => Show_Main_Menu_Command'Access);
      Add_Command
        (Name => "ShowLoadGameMenu",
         Ada_Command => Show_Load_Game_Menu_Command'Access);
      Add_Command
        (Name => "SortSaves", Ada_Command => Sort_Saves_Command'Access);
      Combat.UI.Add_Combat_Commands;
   end Add_Commands;

end MainMenu.Commands;
