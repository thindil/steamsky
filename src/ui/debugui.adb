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
with Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Directory_Operations;
with CArgv;
with Tcl; use Tcl;
with Tcl.Ada;
with Tcl.Tk.Ada;
with Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.TtkEntry;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
with Bases; use Bases;
with BasesTypes;
with Events; use Events;
with Factions;
with Game; use Game;
with Items; use Items;
with ShipModules;
with Ships;
with Utils.UI;

package body DebugUI is

   -- ****o* DebugUI/DebugUI.Refresh_Module_Command
   -- FUNCTION
   -- Refresh the information about selected module
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- RefreshModule
   -- SOURCE
   function Refresh_Module_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C,
      Import => True,
      External_Name => "refreshModuleCommand";
      -- ****

   -- ****o* DebugUI/DebugUI.Refresh_Member_Command
   -- FUNCTION
   -- Refresh the information about selected crew member
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- RefreshMember
   -- SOURCE
   function Refresh_Member_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C,
      Import => True,
      External_Name => "refreshMemberCommand";
      -- ****

   -- ****o* DebugUI/DebugUI.Refresh_Cargo_Command
   -- FUNCTION
   -- Refresh the information about the player ship cargo
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- RefreshCargo
   -- SOURCE
   function Refresh_Cargo_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C,
      Import => True,
      External_Name => "refreshCargoCommand";
      -- ****

   -- ****o* DebugUI/DebugUI.Refresh_Events_Command
   -- FUNCTION
   -- Refresh the list of events
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- RefreshEvents
   -- SOURCE
   function Refresh_Events_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C,
      Import => True,
      External_Name => "refreshEventsCommand";
      -- ****

   -- ****o* DebugUI/DebugUI.Refresh_Command
   -- FUNCTION
   -- Refresh the whole game information
   -- PARAMETERS
   -- Client_Data - Custom data send to the command.
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command.
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- Refresh
   -- SOURCE
   function Refresh_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C,
      Import => True,
      External_Name => "refreshCommand";
      -- ****

   -- ****o* DebugUI/DebugUI.Refresh_Base_Command
   -- FUNCTION
   -- Refresh the information about the selected base
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- RefreshBase
   -- SOURCE
   function Refresh_Base_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C,
      Import => True,
      External_Name => "refreshBaseCommand";
      -- ****

   -- ****o* DebugUI/DebugUI.Save_Game_Command
   -- FUNCTION
   -- Save the game
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- DebugSaveGame
   -- SOURCE
   function Save_Game_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C,
      Import => True,
      External_Name => "debugSaveGameCommand";
      -- ****

   -- ****o* DebugUI/DebugUI.Move_Ship_Command
   -- FUNCTION
   -- Move the player ship
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- DebugMoveShip
   -- SOURCE
   function Move_Ship_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C,
      Import => True,
      External_Name => "debugMoveShipCommand";
      -- ****

   -- ****o* DebugUI/DebugUI.Update_Module_Command
   -- FUNCTION
   -- Update the selected module
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- DebugUpdateModule
   -- SOURCE
   function Update_Module_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C,
      Import => True,
      External_Name => "debugUpdateModuleCommand";
      -- ****

   -- ****o* DebugUI/DebugUI.Add_Skill_Command
   -- FUNCTION
   -- Add a new skill to the selected crew member
   -- PARAMETERS
   -- Client_Data - Custom data send to the command.
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command.
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- DebugAddSkill
   -- SOURCE
   function Add_Skill_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C,
      Import => True,
      External_Name => "debugAddSkillCommand";
      -- ****

   -- ****o* DebugUI/DebugUI.Update_Member_Command
   -- FUNCTION
   -- Update the selected crew member
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- DebugUpdateMember
   -- SOURCE
   function Update_Member_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C,
      Import => True,
      External_Name => "debugUpdateMemberCommand";
      -- ****

   -- ****o* DebugUI/DebugUI.Add_Item_Command
   -- FUNCTION
   -- Add a new item to the player ship cargo
   -- PARAMETERS
   -- Client_Data - Custom data send to the command.
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command.
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- DebugAddItem
   -- SOURCE
   function Add_Item_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C,
      Import => True,
      External_Name => "debugAddItemCommand";
      -- ****

   -- ****o* DebugUI/DebugUI.Update_Item_Command
   -- FUNCTION
   -- Update the amount of an item in the player ship cargo
   -- PARAMETERS
   -- Client_Data - Custom data send to the command.
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command.
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- DebugUpdateItem
   -- SOURCE
   function Update_Item_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C,
      Import => True,
      External_Name => "debugUpdateItemCommand";
      -- ****

   -- ****o* DebugUI/DebugUI.Update_Base_Command
   -- FUNCTION
   -- Update the selected base
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- DebugUpdateBase
   -- SOURCE
   function Update_Base_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C,
      Import => True,
      External_Name => "debugUpdateBaseCommand";
      -- ****

   -- ****o* DebugUI/DebugUI.Add_Ship_Command
   -- FUNCTION
   -- Add a new ship based event to the game
   -- PARAMETERS
   -- Client_Data - Custom data send to the command.
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command.
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- DebugAddShip
   -- SOURCE
   function Add_Ship_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C,
      Import => True,
      External_Name => "debugAddShipCommand";
      -- ****

   -- ****o* DebugUI/DebugUI.Toggle_Item_Entry_Command
   -- FUNCTION
   -- Show or hide item entry for bases events
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ToggleItemEntry
   -- SOURCE
   function Toggle_Item_Entry_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C,
      Import => True,
      External_Name => "toggleItemEntryCommand";
      -- ****

   -- ****o* DebugUI/DebugUI.Add_Event_Command
   -- FUNCTION
   -- Add a new base event to the game
   -- PARAMETERS
   -- Client_Data - Custom data send to the command.
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command.
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- DebugAddEvent
   -- SOURCE
   function Add_Event_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C,
      Import => True,
      External_Name => "debugAddEventCommand";
      -- ****

   -- ****o* DebugUI/DebugUI.Delete_Event_Command
   -- FUNCTION
   -- Remove the selected event from the game
   -- PARAMETERS
   -- Client_Data - Custom data send to the command.
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command.
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- DebugDeleteEvent
   -- SOURCE
   function Delete_Event_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Delete_Event_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      Event_Box: constant Ttk_ComboBox :=
        Get_Widget
          (pathName => ".debugdialog.main.world.deleteevent.delete",
           Interp => Interp);
   begin
      Delete_Event
        (Event_Index => Natural'Value(Current(ComboBox => Event_Box)) + 1);
      return
        Refresh_Events_Command
          (Client_Data => Client_Data, Interp => Interp, Argc => Argc,
           Argv => Argv);
   end Delete_Event_Command;

   procedure Show_Debug_Ui is
      use GNAT.Directory_Operations;
      use Tcl.Ada;
      use Tcl.Tk.Ada;
      use Tcl.Tk.Ada.Widgets;
      use BasesTypes;
      use Factions;
      use Utils.UI;
      use Ships;
      use Tiny_String;

      Frame_Name: constant String := ".debugdialog.main.bases";
      Combo_Box: Ttk_ComboBox := Get_Widget(pathName => Frame_Name & ".type");
      Values_List: Unbounded_String := Null_Unbounded_String;
   begin
      Tcl_EvalFile
        (interp => Get_Context,
         fileName =>
           To_String(Source => Data_Directory) & "ui" & Dir_Separator &
           "debug.tcl");
      Add_Command(Name => "Refresh", Ada_Command => Refresh_Command'Access);
      Add_Command
        (Name => "RefreshModule",
         Ada_Command => Refresh_Module_Command'Access);
      Add_Command
        (Name => "RefreshMember",
         Ada_Command => Refresh_Member_Command'Access);
      Add_Command
        (Name => "RefreshCargo", Ada_Command => Refresh_Cargo_Command'Access);
      Add_Command
        (Name => "RefreshBase", Ada_Command => Refresh_Base_Command'Access);
      Add_Command
        (Name => "RefreshEvents",
         Ada_Command => Refresh_Events_Command'Access);
      Add_Command
        (Name => "DebugSaveGame", Ada_Command => Save_Game_Command'Access);
      Add_Command
        (Name => "DebugMoveShip", Ada_Command => Move_Ship_Command'Access);
      Add_Command
        (Name => "DebugUpdateModule",
         Ada_Command => Update_Module_Command'Access);
      Add_Command
        (Name => "DebugAddSkill", Ada_Command => Add_Skill_Command'Access);
      Add_Command
        (Name => "DebugUpdateMember",
         Ada_Command => Update_Member_Command'Access);
      Add_Command
        (Name => "DebugAddItem", Ada_Command => Add_Item_Command'Access);
      Add_Command
        (Name => "DebugUpdateItem", Ada_Command => Update_Item_Command'Access);
      Add_Command
        (Name => "DebugUpdateBase", Ada_Command => Update_Base_Command'Access);
      Add_Command
        (Name => "DebugAddShip", Ada_Command => Add_Ship_Command'Access);
      Add_Command
        (Name => "ToggleItemEntry",
         Ada_Command => Toggle_Item_Entry_Command'Access);
      Add_Command
        (Name => "DebugAddEvent", Ada_Command => Add_Event_Command'Access);
      Add_Command
        (Name => "DebugDeleteEvent",
         Ada_Command => Delete_Event_Command'Access);
      Load_Bases_Types_Loop :
      for BaseType of Bases_Types loop
         exit Load_Bases_Types_Loop when Length(Source => BaseType) = 0;
         Append
           (Source => Values_List,
            New_Item =>
              " {" & Get_Base_Type_Name(Base_Type => BaseType) & "}");
      end loop Load_Bases_Types_Loop;
      configure
        (Widgt => Combo_Box,
         options => "-values [list" & To_String(Source => Values_List) & "]");
      Values_List := Null_Unbounded_String;
      Combo_Box.Name := New_String(Str => Frame_Name & ".owner");
      Load_Factions_Loop :
      for I in 1 .. Get_Factions_Amount loop
         Append
           (Source => Values_List,
            New_Item =>
              " {" & To_String(Source => Get_Faction(Number => I).Name) & "}");
      end loop Load_Factions_Loop;
      configure
        (Widgt => Combo_Box,
         options => "-values [list" & To_String(Source => Values_List) & "]");
      Values_List := Null_Unbounded_String;
      Combo_Box.Name := New_String(Str => Frame_Name & ".name");
      Load_Bases_Loop :
      for Base of Sky_Bases loop
         Append
           (Source => Values_List,
            New_Item => " {" & To_String(Source => Base.Name) & "}");
      end loop Load_Bases_Loop;
      configure
        (Widgt => Combo_Box,
         options => "-values [list" & To_String(Source => Values_List) & "]");
      Combo_Box.Name := New_String(Str => ".debugdialog.main.world.base");
      configure
        (Widgt => Combo_Box,
         options => "-values [list" & To_String(Source => Values_List) & "]");
      Values_List := Null_Unbounded_String;
      Combo_Box.Name := New_String(Str => ".debugdialog.main.ship.proto");
      Load_Proto_Modules_Block :
      declare
         use ShipModules;

         Module: Base_Module_Data := (others => <>);
      begin
         Load_Modules_Prototypes_Loop :
         for I in 1 .. Get_Modules_Amount loop
            Module := Get_Module(Index => I);
            if Length(Source => Module.Name) > 0 then
               Append
                 (Source => Values_List,
                  New_Item =>
                    " {" & To_String(Source => Get_Module(Index => I).Name) &
                    "}");
            end if;
         end loop Load_Modules_Prototypes_Loop;
      end Load_Proto_Modules_Block;
      configure
        (Widgt => Combo_Box,
         options => "-values [list" & To_String(Source => Values_List) & "]");
      Values_List := Null_Unbounded_String;
      Combo_Box.Name := New_String(Str => ".debugdialog.main.cargo.add");
      Load_Items_Loop :
      for I in 1 .. Get_Proto_Amount loop
         Append
           (Source => Values_List,
            New_Item =>
              " {" & To_String(Source => Get_Proto_Item(Index => I).Name) &
              "}");
      end loop Load_Items_Loop;
      configure
        (Widgt => Combo_Box,
         options => "-values [list" & To_String(Source => Values_List) & "]");
      Combo_Box.Name := New_String(Str => ".debugdialog.main.world.item");
      configure
        (Widgt => Combo_Box,
         options => "-values [list" & To_String(Source => Values_List) & "]");
      Values_List := Null_Unbounded_String;
      Combo_Box.Name := New_String(Str => ".debugdialog.main.world.ship");
      Load_Ships_Loop :
      for I in 1 .. Get_Proto_Ships_Amount loop
         Append
           (Source => Values_List,
            New_Item =>
              " {" &
              To_String(Source => Get_Proto_Ship(Proto_Index => I).Name) &
              "}");
      end loop Load_Ships_Loop;
      configure
        (Widgt => Combo_Box,
         options => "-values [list" & To_String(Source => Values_List) & "]");
      Tcl_Eval(interp => Get_Context, strng => "Refresh");
   end Show_Debug_Ui;

end DebugUI;
