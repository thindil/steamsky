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
with Utils.UI;
with Ships.UI.Crew.Inventory;

package body Ships.UI.Crew is

   function Order_For_All_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "orderForAllCommand";

   function Toggle_Crew_Member_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "toggleCrewMemberCommand";

   function Dismiss_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "dismissCommand";

   function Set_Crew_Order_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "setCrewOrderCommand";

   function Show_Member_Tab_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "showMemberTabCommand";

   function Show_Member_Info_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "showMemberInfoCommand";

   function Show_Crew_Stats_Info_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "showCrewStatsInfoCommand";

   function Show_Crew_Skill_Info_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "showCrewSkillInfoCommand";

   function Set_Priority_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "setPriorityCommand";

   function Show_Crew_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "showCrewCommand";

   function Sort_Ship_Crew_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "sortCrewCommand";

   function Select_Crew_Skill_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "selectCrewSkillCommand";

   function Show_Crew_Order_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "showCrewOrderCommand";

   function Select_Crew_Order_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "selectCrewOrderCommand";

   function Toggle_All_Crew_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "toggleAllCrewCommand";

   procedure Add_Crew_Commands is
      use Utils.UI;
      procedure Add_Ada_Commands with
         Import => True,
         Convention => C,
         External_Name => "addAdaCrewCommands";
   begin
      Add_Command
        (Name => "OrderForAll", Ada_Command => Order_For_All_Command'Access);
      Add_Command
        (Name => "ToggleCrewMember",
         Ada_Command => Toggle_Crew_Member_Command'Access);
      Add_Command(Name => "Dismiss", Ada_Command => Dismiss_Command'Access);
      Add_Command
        (Name => "SetCrewOrder", Ada_Command => Set_Crew_Order_Command'Access);
      Add_Command
        (Name => "ShowMemberTab",
         Ada_Command => Show_Member_Tab_Command'Access);
      Add_Command
        (Name => "ShowMemberInfo",
         Ada_Command => Show_Member_Info_Command'Access);
      Add_Command
        (Name => "ShowCrewStatsInfo",
         Ada_Command => Show_Crew_Stats_Info_Command'Access);
      Add_Command
        (Name => "ShowCrewSkillInfo",
         Ada_Command => Show_Crew_Skill_Info_Command'Access);
      Add_Command
        (Name => "SetPriority", Ada_Command => Set_Priority_Command'Access);
      Add_Command(Name => "ShowCrew", Ada_Command => Show_Crew_Command'Access);
      Add_Command
        (Name => "SortShipCrew", Ada_Command => Sort_Ship_Crew_Command'Access);
      Add_Command
        (Name => "SelectCrewSkill",
         Ada_Command => Select_Crew_Skill_Command'Access);
      Add_Command
        (Name => "ShowCrewOrder",
         Ada_Command => Show_Crew_Order_Command'Access);
      Add_Command
        (Name => "SelectCrewOrder",
         Ada_Command => Select_Crew_Order_Command'Access);
      Add_Command
        (Name => "ToggleAllCrew",
         Ada_Command => Toggle_All_Crew_Command'Access);
      Add_Ada_Commands;
      Ships.UI.Crew.Inventory.Add_Inventory_Commands;
   end Add_Crew_Commands;

end Ships.UI.Crew;
