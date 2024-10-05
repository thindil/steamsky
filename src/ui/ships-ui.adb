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

with Interfaces.C;
with CArgv;
with Tcl;
with Ships.UI.Modules;
with Utils.UI;

package body Ships.UI is

   function Show_Ship_Info_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "showShipInfoCommand";

   function Set_Ship_Name_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "setShipNameCommand";

   function Ship_Max_Min_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "shipMaxMinCommand";

   procedure Add_Commands is
      use Utils.UI;
      procedure Add_Ada_Commands with
         Import => True,
         Convention => C,
         External_Name => "addAdaShipsCommands";
   begin
      Add_Command
        (Name => "ShowShipInfo", Ada_Command => Show_Ship_Info_Command'Access);
      Add_Command
        (Name => "SetShipName", Ada_Command => Set_Ship_Name_Command'Access);
      Add_Command
        (Name => "ShipMaxMin", Ada_Command => Ship_Max_Min_Command'Access);
      Add_Ada_Commands;
      Ships.UI.Modules.Add_Modules_Commands;
   end Add_Commands;

end Ships.UI;
