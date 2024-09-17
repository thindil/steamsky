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
with Ada.Strings.Unbounded;
with Combat.UI;
with Dialogs; use Dialogs;
with Game;
with Game.SaveLoad;
with Maps.UI;
with Table; use Table;
with Utils;
with Utils.UI;

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
      use Ada.Strings.Unbounded;

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
      function New_Ada_Game_Command
        (C_Data: Integer; I: Tcl.Tcl_Interp; Ac: Interfaces.C.int;
         Av: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
         Import => True,
         Convention => C,
         External_Name => "newGameCommand";
   begin
      if New_Ada_Game_Command
          (C_Data => Client_Data, I => Interp, Ac => Argc, Av => Argv) =
        TCL_ERROR then
         return TCL_OK;
      end if;
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
      Import => True,
      Convention => C,
      External_Name => "showMainMenuCommand";
      -- ****

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
      Import => True,
      Convention => C,
      External_Name => "showLoadGameMenuCommand";
      -- ****

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
      use Utils.UI;
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
