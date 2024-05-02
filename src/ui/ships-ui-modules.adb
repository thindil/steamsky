-- Copyright (c) 2024 Bartek thindil Jasicki
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

with Interfaces.C;
with CArgv;
with Tcl;
with Utils.UI;

package body Ships.UI.Modules is

   function Show_Module_Info_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "showModuleInfoCommand";

   function Set_Upgrade_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "setUpgradeCommand";

   function Assign_Module_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "assignModuleCommand";

   function Disable_Engine_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "disableEngineCommand";

   function Stop_Upgrading_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "stopUpgradingCommand";

   function Set_Repair_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "setRepairCommand";

   function Reset_Destination_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "resetDestinationCommand";

   function Update_Assign_Crew_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "updateAssignCrewCommand";

   function Show_Assign_Crew_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "showAssignCrewCommand";

   function Show_Assign_Skill_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "showAssignSkillCommand";

   function Cancel_Order_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "cancelOrderCommand";

   function Get_Active_Button_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "getActiveButtonCommand";

   function Show_Modules_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "showModulesCommand";

   function Sort_Ship_Modules_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "sortShipModulesCommand";

   function Show_Assign_Ammo_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "showAssignAmmoCommand";

   procedure Add_Modules_Commands is
      use Utils.UI;
   begin
      Add_Command
        (Name => "ShowModuleInfo",
         Ada_Command => Show_Module_Info_Command'Access);
      Add_Command
        (Name => "SetUpgrade", Ada_Command => Set_Upgrade_Command'Access);
      Add_Command
        (Name => "AssignModule", Ada_Command => Assign_Module_Command'Access);
      Add_Command
        (Name => "DisableEngine",
         Ada_Command => Disable_Engine_Command'Access);
      Add_Command
        (Name => "StopUpgrading",
         Ada_Command => Stop_Upgrading_Command'Access);
      Add_Command
        (Name => "SetRepair", Ada_Command => Set_Repair_Command'Access);
      Add_Command
        (Name => "ResetDestination",
         Ada_Command => Reset_Destination_Command'Access);
      Add_Command
        (Name => "UpdateAssignCrew",
         Ada_Command => Update_Assign_Crew_Command'Access);
      Add_Command
        (Name => "ShowAssignCrew",
         Ada_Command => Show_Assign_Crew_Command'Access);
      Add_Command
        (Name => "ShowAssignSkill",
         Ada_Command => Show_Assign_Skill_Command'Access);
      Add_Command
        (Name => "CancelOrder", Ada_Command => Cancel_Order_Command'Access);
      Add_Command
        (Name => "GetActiveButton",
         Ada_Command => Get_Active_Button_Command'Access);
      Add_Command
        (Name => "ShowModules", Ada_Command => Show_Modules_Command'Access);
      Add_Command
        (Name => "SortShipModules",
         Ada_Command => Sort_Ship_Modules_Command'Access);
      Add_Command
        (Name => "ShowAssignAmmo",
         Ada_Command => Show_Assign_Ammo_Command'Access);
   end Add_Modules_Commands;

end Ships.UI.Modules;
