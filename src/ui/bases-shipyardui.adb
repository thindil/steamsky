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

with Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C; use Interfaces.C;
with CArgv; use CArgv;
with Tcl; use Tcl;
with Utils.UI;

package body Bases.ShipyardUI is

   -- ****f* ShipyardUI/ShipyardUI.Show_Shipyard_Command
   -- FUNCTION
   -- Show the selected base shipyard
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command.
   -- Argv        - Values of arguments passed to the command.
   -- COMMAND
   -- ShowShipyard ?moduletype? ?modulename?
   -- Show the base shipyard and load all available and installed modules
   -- lists. Moduletype is the type of modules to show in available modules,
   -- modulename is the name of the module to search in available modules.
   -- SOURCE
   function Show_Shipyard_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C,
      Import => True,
      External_Name => "showShipyardCommand";
      -- ****

   -- ****iv* ShipyardUI/ShipyardUI.Module_Index
   -- SOURCE
--   Module_Index: Positive;
   -- ****

   --## rule off REDUCEABLE_SCOPE
   -- ****if* ShipyardUI/ShipyardUI.Get_Module_Index
   -- FUNCTION
   -- Get the index of the currently selected module
   -- RESULT
   -- The index of the currently selected module
   -- SOURCE
--   function Get_Module_Index return Positive is
--      -- ****
--   begin
--      return Module_Index;
--   end Get_Module_Index;

   -- ****if* ShipyardUI/ShipyardUI.Set_Module_Info
   -- FUNCTION
   -- Show information about selected module
   -- PARAMETERS
   -- Installing - If true, player looking at installing modules list
   -- Row        - The current row in the dialog
   -- New_Info   - If true, create the new UI for the info, otherwise reuse old
   --              one. Default value is True.
   -- SOURCE
--   procedure Set_Module_Info
--     (Installing: Boolean; Row: in out Positive; New_Info: Boolean := True) is
--      -- ****
--      procedure Set_Ada_Module_Info
--        (I: Integer; R: in out Positive; N_Info, M_Index: Integer) with
--         Convention => C,
--         Import => True,
--         External_Name => "setAdaModuleInfo";
--   begin
--      Set_Ada_Module_Info
--        (I => (if Installing then 1 else 0), R => Row,
--         N_Info => (if New_Info then 1 else 0), M_Index => Get_Module_Index);
--   end Set_Module_Info;
   --## rule on REDUCEABLE_SCOPE

   -- ****f* ShipyardUI/ShipyardUI.Show_Install_Info_Command
   -- FUNCTION
   -- Show information about the selected module to install
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command.
   -- Argv        - Values of arguments passed to the command. Unused
   -- SOURCE
   function Show_Install_Info_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C,
      Import => True,
      External_Name => "showInstallInfoCommand";
      -- ****

--   function Show_Install_Info_Command
--     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
--      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
--      function Show_Ada_Install_Info_Command
--        (C_Data: Integer; I: Tcl.Tcl_Interp; Ac: Interfaces.C.int;
--         Av: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
--         Convention => C,
--         Import => True,
--         External_Name => "showInstallInfoCommand";
--   begin
--      Module_Index := Natural'Value(CArgv.Arg(Argv => Argv, N => 1));
--      return
--        Show_Ada_Install_Info_Command
--          (C_Data => Client_Data, I => Interp, Ac => Argc, Av => Argv);
--   end Show_Install_Info_Command;

   -- ****f* ShipyardUI/ShipyardUI.Manipulate_Module_Command
   -- FUNCTION
   -- Install or remove the selected module
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- SOURCE
   function Manipulate_Module_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C,
      Import => True,
      External_Name => "manipulateModuleCommand";
      -- ****

   -- ****f* ShipyardUI/ShipyardUI.Show_Remove_Info_Command
   -- FUNCTION
   -- Show information about the selected module to remove
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- SOURCE
   function Show_Remove_Info_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C,
      Import => True,
      External_Name => "showRemoveInfoCommand";
      -- ****

--   function Show_Remove_Info_Command
--     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
--      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
--      function Show_Ada_Remove_Info_Command
--        (C_Data: Integer; I: Tcl.Tcl_Interp; Ac: Interfaces.C.int;
--         Av: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
--         Convention => C,
--         Import => True,
--         External_Name => "showRemoveInfoCommand";
--   begin
--      --## rule off DIRECTLY_ACCESSED_GLOBALS
--      Module_Index := Natural'Value(CArgv.Arg(Argv => Argv, N => 1));
--      --## rule on DIRECTLY_ACCESSED_GLOBALS
--      return
--        Show_Ada_Remove_Info_Command
--          (C_Data => Client_Data, I => Interp, Ac => Argc, Av => Argv);
--   end Show_Remove_Info_Command;

   -- ****o* ShipyardUI/ShipyardUI.Show_Shipyard_Tab_Command
   -- FUNCTION
   -- Show the install or remove modules options in shipyard
   -- PARAMETERS
   -- Client_Data - Custom data send to the command.
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command.
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowShipyardTab
   -- SOURCE
   function Show_Shipyard_Tab_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C,
      Import => True,
      External_Name => "showShipyardTabCommand";
      -- ****

   -- ****o* ShipyardUI/ShipyardUI.Sort_Modules_Command
   -- FUNCTION
   -- Sort the ship modules lists
   -- PARAMETERS
   -- Client_Data - Custom data send to the command.
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SortShipModules action moduletype page x
   -- Action is a type of action, can be install or remove, moduletype is a
   -- type of modules to show, page is the number of currently showed page
   -- of list and x is X axis coordinate where the player clicked the mouse
   -- button
   -- SOURCE
   function Sort_Modules_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C,
      Import => True,
      External_Name => "sortShipyardModulesCommand";
      -- ****

   -- ****o* ShipyardUI/ShipyardUI.Compare_Modules_Command
   -- FUNCTION
   -- Show the comparison between the selected modules in install info
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- CompareModules
   -- SOURCE
   function Compare_Modules_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C,
      Import => True,
      External_Name => "compareModulesCommand";
      -- ****

--   function Compare_Modules_Command
--     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
--      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
--      pragma Unreferenced(Client_Data, Interp, Argc, Argv);
--      Row: Positive := 3;
--   begin
--      Set_Module_Info(Installing => True, Row => Row, New_Info => False);
--      return TCL_OK;
--   end Compare_Modules_Command;

   procedure Add_Commands is
      use Utils.UI;
   begin
      Add_Command
        (Name => "ShowShipyard", Ada_Command => Show_Shipyard_Command'Access);
      Add_Command
        (Name => "ShowInstallInfo",
         Ada_Command => Show_Install_Info_Command'Access);
      Add_Command
        (Name => "ManipulateModule",
         Ada_Command => Manipulate_Module_Command'Access);
      Add_Command
        (Name => "ShowRemoveInfo",
         Ada_Command => Show_Remove_Info_Command'Access);
      Add_Command
        (Name => "ShowShipyardTab",
         Ada_Command => Show_Shipyard_Tab_Command'Access);
      Add_Command
        (Name => "SortShipyardModules",
         Ada_Command => Sort_Modules_Command'Access);
      Add_Command
        (Name => "CompareModules",
         Ada_Command => Compare_Modules_Command'Access);
   end Add_Commands;

end Bases.ShipyardUI;
