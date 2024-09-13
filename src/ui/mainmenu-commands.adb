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

with Ada.Exceptions;
with Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C.Strings;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkEntry; use Tcl.Tk.Ada.Widgets.TtkEntry;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
with Tcl.Tk.Ada.Widgets.TtkFrame;
with BasesTypes; use BasesTypes;
with Combat.UI;
with Config;
with CoreUI;
with Dialogs; use Dialogs;
with Factions; use Factions;
with Game; use Game;
with Game.SaveLoad;
with Goals;
with Maps.UI;
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
   --## rule off REDUCEABLE_SCOPE
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
   --## rule on REDUCEABLE_SCOPE

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
      return
        Show_Ada_Load_Game_Command
          (C_Data => Client_Data, I => Interp, Ac => Argc, Av => Argv);
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
      use Ada.Exceptions;
      use Game.SaveLoad;

      function Load_Ada_Game_Command
        (C_Data: Integer; I: Tcl.Tcl_Interp; Ac: Interfaces.C.int;
         Av: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
         Import => True,
         Convention => C,
         External_Name => "loadGameCommand";
   begin
      if Load_Ada_Game_Command
          (C_Data => Client_Data, I => Interp, Ac => Argc, Av => Argv) =
        TCL_ERROR then
         return TCL_ERROR;
      end if;
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
      Import => True,
      Convention => C,
      External_Name => "setFactionCommand";
      -- ****

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
      Import => True,
      Convention => C,
      External_Name => "setCareerCommand";
      -- ****

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
      Import => True,
      Convention => C,
      External_Name => "setBaseCommand";
      -- ****

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
      Import => True,
      Convention => C,
      External_Name => "randomNameCommand";
      -- ****

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
      use Interfaces.C.Strings;
      use Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
      use Config;
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
      use Tcl.Tk.Ada.Widgets.TtkFrame;

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
