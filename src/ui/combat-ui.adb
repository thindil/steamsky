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

with Interfaces.C; use Interfaces.C;
with CArgv;
with Tcl;
with Utils.UI;

package body Combat.UI is

   function Next_Turn_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "nextTurnCommand";

   function Show_Combat_Ui_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "showCombatUiCommand";

   function Set_Combat_Order_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "setCombatOrderCommand";

   function Set_Boarding_Order_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "nextTurnCommand";

   function Set_Combat_Party_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "setCombatPartyCommand";

   function Set_Combat_Position_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "setCombatPositionCommand";

   function Show_Combat_Info_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "showCombatInfoCommand";

   function Combat_Max_Min_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "combatMaxMinCommand";

   function Toggle_All_Combat_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "toggleAllCombatCommand";

   function Set_Party_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "setPartyCommand";

   procedure Add_Combat_Commands is
      use Utils.UI;
   begin
      Add_Command(Name => "NextTurn", Ada_Command => Next_Turn_Command'Access);
      Add_Command
        (Name => "ShowCombatUI", Ada_Command => Show_Combat_Ui_Command'Access);
      Add_Command
        (Name => "SetCombatOrder",
         Ada_Command => Set_Combat_Order_Command'Access);
      Add_Command
        (Name => "SetBoardingOrder",
         Ada_Command => Set_Boarding_Order_Command'Access);
      Add_Command
        (Name => "SetCombatParty",
         Ada_Command => Set_Combat_Party_Command'Access);
      Add_Command
        (Name => "SetCombatPosition",
         Ada_Command => Set_Combat_Position_Command'Access);
      Add_Command
        (Name => "ShowCombatInfo",
         Ada_Command => Show_Combat_Info_Command'Access);
      Add_Command
        (Name => "CombatMaxMin", Ada_Command => Combat_Max_Min_Command'Access);
      Add_Command
        (Name => "ToggleAllCombat",
         Ada_Command => Toggle_All_Combat_Command'Access);
      Add_Command(Name => "SetParty", Ada_Command => Set_Party_Command'Access);
   end Add_Combat_Commands;

end Combat.UI;
