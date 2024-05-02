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
   end Add_Modules_Commands;

end Ships.UI.Modules;
