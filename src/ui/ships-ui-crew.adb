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
with Table;

package body Ships.UI.Crew is

   procedure Update_Crew_Info(Page: Positive := 1; Skill: Natural := 0) is
      use Table;

      --## rule off TYPE_INITIAL_VALUES
      type Crew_Array is array(0 .. 50) of Natural;
      --## rule on TYPE_INITIAL_VALUES
      C_Array: Crew_Array := (others => 0);
      N_Width: Nim_Width := (others => 0);
      Index: Natural := 0;
      --## rule off IMPROPER_INITIALIZATION
      Crew_Table: Table_Widget (Amount => 9);
      Crew_Indexes: Positive_Container.Vector;
      procedure Update_Ada_Crew_Info
        (P: Positive; S: Natural; M: Crew_Array; W: out Nim_Width;
         Row, Height: out Positive) with
         Import => True,
         Convention => C,
         External_Name => "updateAdaCrewInfo";
   begin
      if Crew_Indexes.Length /= Player_Ship.Crew.Length then
         Crew_Indexes.Clear;
         Update_Crew_Indexes_Loop :
         for I in Player_Ship.Crew.Iterate loop
            Crew_Indexes.Append
              (New_Item => Crew_Container.To_Index(Position => I));
         end loop Update_Crew_Indexes_Loop;
      end if;
      --## rule on IMPROPER_INITIALIZATION
      Convert_Crew_Indexes_Loop :
      for C_Index of Crew_Indexes loop
         C_Array(Index) := C_Index;
         Index := Index + 1;
      end loop Convert_Crew_Indexes_Loop;
      Update_Ada_Crew_Info
        (P => Page, S => Skill, M => C_Array, W => N_Width,
         Row => Crew_Table.Row, Height => Crew_Table.Row_Height);
   end Update_Crew_Info;

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
