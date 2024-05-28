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
with Bases;
with Utils.UI;

package body OrdersMenu is

   function Show_Orders_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "showOrdersCommand";

   function Docking_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "dockingCommand";

   function Ask_For_Bases_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;

   function Ask_For_Bases_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      use Interfaces.C;
      use Tcl;
      use Bases;

      function Ask_Ada_For_Bases_Command
        (C_Data: Integer; I: Tcl.Tcl_Interp; Ac: Interfaces.C.int;
         Av: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
         Import => True,
         Convention => C,
         External_Name => "askForBasesCommand";
   begin
      if Ask_Ada_For_Bases_Command
          (C_Data => Client_Data, I => Interp, Ac => Argc, Av => Argv) =
        TCL_OK then
         Get_Bases_Loop :
         for I in Sky_Bases'Range loop
            Get_Base_From_Nim(Base_Index => I);
         end loop Get_Bases_Loop;
      end if;
      return TCL_OK;
   end Ask_For_Bases_Command;

   function Ask_For_Events_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "askForEventsCommand";

   function Attack_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "attackCommand";

   function Pray_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "prayCommand";

   function Set_As_Home_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "setAsHomeCommand";

   function Show_Trader_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "showTraderCommand";

   function Start_Mission_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "startMissionCommand";

   function Complete_Mission_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "completeMissionCommand";

   function Execute_Story_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "executeStoryCommand";

   function Deliver_Medicines_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "deliverMedicinesCommand";

   procedure Add_Commands is
      use Utils.UI;
   begin
      Add_Command
        (Name => "ShowOrders", Ada_Command => Show_Orders_Command'Access);
      Add_Command(Name => "Docking", Ada_Command => Docking_Command'Access);
      Add_Command
        (Name => "AskForBases", Ada_Command => Ask_For_Bases_Command'Access);
      Add_Command
        (Name => "AskForEvents", Ada_Command => Ask_For_Events_Command'Access);
      Add_Command(Name => "Attack", Ada_Command => Attack_Command'Access);
      Add_Command(Name => "Pray", Ada_Command => Pray_Command'Access);
      Add_Command
        (Name => "SetAsHome", Ada_Command => Set_As_Home_Command'Access);
      Add_Command
        (Name => "ShowTrader", Ada_Command => Show_Trader_Command'Access);
      Add_Command
        (Name => "StartMission", Ada_Command => Start_Mission_Command'Access);
      Add_Command
        (Name => "CompleteMission",
         Ada_Command => Complete_Mission_Command'Access);
      Add_Command
        (Name => "ExecuteStory", Ada_Command => Execute_Story_Command'Access);
      Add_Command
        (Name => "DeliverMedicines",
         Ada_Command => Deliver_Medicines_Command'Access);
   end Add_Commands;

end OrdersMenu;
