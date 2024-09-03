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
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Directory_Operations;
with CArgv;
with Tcl; use Tcl;
with Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.TtkEntry; use Tcl.Tk.Ada.Widgets.TtkEntry;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
with Tcl.Tk.Ada.Widgets.TtkLabel;
with Bases; use Bases;
with BasesTypes; use BasesTypes;
with Crew; use Crew;
with Events; use Events;
with Factions; use Factions;
with Game; use Game;
with Items; use Items;
with Maps; use Maps;
with ShipModules; use ShipModules;
with Ships; use Ships;
with Ships.Cargo; use Ships.Cargo;
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
      Convention => C;
      -- ****

   function Add_Skill_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      use Tiny_String;

      Frame_Name: constant String := ".debugdialog.main.crew";
      Combo_Box: Ttk_ComboBox :=
        Get_Widget(pathName => Frame_Name & ".member", Interp => Interp);
      Member_Index: constant Positive :=
        Natural'Value(Current(ComboBox => Combo_Box)) + 1;
      Skill_Name: Unbounded_String;
   begin
      Set_Ada_Crew(Ship => Player_Ship);
      Combo_Box.Name := New_String(Str => Frame_Name & ".addskill.skills");
      Skill_Name := To_Unbounded_String(Source => Get(Widgt => Combo_Box));
      Add_Skill_Loop :
      for I in 1 .. Skills_Amount loop
         if To_Unbounded_String
             (Source =>
                To_String
                  (Source =>
                     SkillsData_Container.Element
                       (Container => Skills_List, Index => I)
                       .Name)) =
           Skill_Name then
            Skills_Container.Append
              (Container => Player_Ship.Crew(Member_Index).Skills,
               New_Item => (Index => I, Level => 1, Experience => 0));
            return
              Refresh_Member_Command
                (Client_Data => Client_Data, Interp => Interp, Argc => Argc,
                 Argv => Argv);
         end if;
      end loop Add_Skill_Loop;
      Get_Ada_Crew;
      return TCL_OK;
   end Add_Skill_Command;

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
      Convention => C;
      -- ****

   function Update_Member_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc, Argv);
      use Ada.Strings;
      use Ada.Strings.Fixed;

      Frame_Name: constant String := ".debugdialog.main.crew";
      Combo_Box: constant Ttk_ComboBox :=
        Get_Widget(pathName => Frame_Name & ".member", Interp => Interp);
      Member_Index: Positive;
      Spin_Box: Ttk_SpinBox :=
        Get_Widget
          (pathName => Frame_Name & ".stats2.health", Interp => Interp);
      --## rule off IMPROPER_INITIALIZATION
      Local_Attribute: Mob_Attribute_Record;
      --## rule on IMPROPER_INITIALIZATION
   begin
      Set_Ada_Crew(Ship => Player_Ship);
      Member_Index := Natural'Value(Current(ComboBox => Combo_Box)) + 1;
      Player_Ship.Crew(Member_Index).Health :=
        Skill_Range'Value(Get(Widgt => Spin_Box));
      Spin_Box.Name := New_String(Str => Frame_Name & ".stats2.thirst");
      Player_Ship.Crew(Member_Index).Thirst :=
        Skill_Range'Value(Get(Widgt => Spin_Box));
      --## rule off ASSIGNMENTS
      Spin_Box.Name := New_String(Str => Frame_Name & ".stats2.hunger");
      Player_Ship.Crew(Member_Index).Hunger :=
        Skill_Range'Value(Get(Widgt => Spin_Box));
      Spin_Box.Name := New_String(Str => Frame_Name & ".stats2.tired");
      Player_Ship.Crew(Member_Index).Tired :=
        Skill_Range'Value(Get(Widgt => Spin_Box));
      Spin_Box.Name := New_String(Str => Frame_Name & ".stats2.morale");
      Player_Ship.Crew(Member_Index).Morale(1) :=
        Skill_Range'Value(Get(Widgt => Spin_Box));
      Spin_Box.Name := New_String(Str => Frame_Name & ".stats2.loyalty");
      --## rule on ASSIGNMENTS
      Player_Ship.Crew(Member_Index).Loyalty :=
        Skill_Range'Value(Get(Widgt => Spin_Box));
      Update_Stats_Loop :
      for I in Player_Ship.Crew(Member_Index).Attributes'Range loop
         Spin_Box.Name :=
           New_String
             (Str =>
                Frame_Name & ".stats.value" &
                Trim(Source => Positive'Image(I), Side => Left));
         Local_Attribute :=
           (Level => Positive'Value(Get(Widgt => Spin_Box)),
            Experience =>
              Player_Ship.Crew(Member_Index).Attributes(I).Experience);
         Player_Ship.Crew(Member_Index).Attributes(I) := Local_Attribute;
      end loop Update_Stats_Loop;
      Update_Skills_Loop :
      for I in
        Skills_Container.First_Index
          (Container => Player_Ship.Crew(Member_Index).Skills) ..
          Skills_Container.Last_Index
            (Container => Player_Ship.Crew(Member_Index).Skills) loop
         Spin_Box.Name :=
           New_String
             (Str =>
                Frame_Name & ".skills.value" &
                Trim(Source => Skills_Amount_Range'Image(I), Side => Left));
         Update_Skill_Block :
         declare
            New_Skill: Skill_Info :=
              Skills_Container.Element
                (Container => Player_Ship.Crew(Member_Index).Skills,
                 Index => I);
         begin
            New_Skill.Level := Positive'Value(Get(Widgt => Spin_Box));
            Skills_Container.Replace_Element
              (Container => Player_Ship.Crew(Member_Index).Skills, Index => I,
               New_Item => New_Skill);
         end Update_Skill_Block;
      end loop Update_Skills_Loop;
      Get_Ada_Crew;
      return TCL_OK;
   end Update_Member_Command;

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
      Convention => C;
      -- ****

   function Add_Item_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      use Tiny_String;

      Frame_Name: constant String := ".debugdialog.main.cargo";
      Item_Entry: constant Ttk_Entry :=
        Get_Widget(pathName => Frame_Name & ".add", Interp => Interp);
      Item_Box: constant Ttk_SpinBox :=
        Get_Widget(pathName => Frame_Name & ".amount", Interp => Interp);
      Item_Name: Bounded_String;
      Item_Index: Natural := 0;
   begin
      Item_Name := To_Bounded_String(Source => Get(Widgt => Item_Entry));
      Find_Index_Loop :
      for I in 1 .. Get_Proto_Amount loop
         if Get_Proto_Item(Index => I).Name = Item_Name then
            Item_Index := I;
            exit Find_Index_Loop;
         end if;
      end loop Find_Index_Loop;
      if Item_Index = 0 then
         return TCL_OK;
      end if;
      Update_Cargo
        (Ship => Player_Ship, Proto_Index => Item_Index,
         Amount => Positive'Value(Get(Widgt => Item_Box)));
      return
        Refresh_Command
          (Client_Data => Client_Data, Interp => Interp, Argc => Argc,
           Argv => Argv);
   end Add_Item_Command;

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
      Convention => C;
      -- ****

   function Update_Item_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      Frame_Name: constant String := ".debugdialog.main.cargo";
      Item_Combo: constant Ttk_ComboBox :=
        Get_Widget(pathName => Frame_Name & ".update", Interp => Interp);
      Item_Box: constant Ttk_SpinBox :=
        Get_Widget(pathName => Frame_Name & ".updateamount", Interp => Interp);
      Item_Index: Positive;
   begin
      Item_Index := Natural'Value(Current(ComboBox => Item_Combo)) + 1;
      Update_Cargo
        (Ship => Player_Ship, Amount => Positive'Value(Get(Widgt => Item_Box)),
         Cargo_Index => Item_Index);
      return
        Refresh_Command
          (Client_Data => Client_Data, Interp => Interp, Argc => Argc,
           Argv => Argv);
   end Update_Item_Command;

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
      Convention => C;
      -- ****

   function Update_Base_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc, Argv);
      use Tiny_String;

      Frame_Name: constant String := ".debugdialog.main.bases";
      Base_Index: Natural := 0;
      Base_Entry: constant Ttk_Entry :=
        Get_Widget(pathName => Frame_Name & ".name", Interp => Interp);
      Base_Name: Bounded_String;
      Base_Combo: Ttk_ComboBox :=
        Get_Widget(pathName => Frame_Name & ".type", Interp => Interp);
      Base_Box: Ttk_SpinBox :=
        Get_Widget(pathName => Frame_Name & ".population", Interp => Interp);
      Item: Base_Cargo; --## rule line off IMPROPER_INITIALIZATION
   begin
      Base_Name := To_Bounded_String(Source => Get(Widgt => Base_Entry));
      Find_Index_Loop :
      for I in Sky_Bases'Range loop
         Get_Base_From_Nim(Base_Index => I);
         if Sky_Bases(I).Name = Base_Name then
            Base_Index := I;
            exit Find_Index_Loop;
         end if;
      end loop Find_Index_Loop;
      if Base_Index = 0 then
         return TCL_OK;
      end if;
      Update_Base_Type_Loop :
      for Base_Type of Bases_Types loop
         exit Update_Base_Type_Loop when Length(Source => Base_Type) = 0;
         if Get_Base_Type_Name(Base_Type => Base_Type) =
           Get(Widgt => Base_Combo) then
            Sky_Bases(Base_Index).Base_Type := Base_Type;
            exit Update_Base_Type_Loop;
         end if;
      end loop Update_Base_Type_Loop;
      Base_Combo.Name := New_String(Str => Frame_Name & ".owner");
      Update_Base_Owner_Loop :
      for I in 1 .. Get_Factions_Amount loop
         if Get_Faction(Number => I).Name =
           To_Bounded_String(Source => Get(Widgt => Base_Combo)) then
            Sky_Bases(Base_Index).Owner := Get_Faction_Index(Number => I);
            exit Update_Base_Owner_Loop;
         end if;
      end loop Update_Base_Owner_Loop;
      Base_Combo.Name := New_String(Str => Frame_Name & ".size");
      Sky_Bases(Base_Index).Size := Bases_Size'Value(Get(Widgt => Base_Combo));
      Sky_Bases(Base_Index).Population :=
        Natural'Value(Get(Widgt => Base_Box));
      Base_Box.Name := New_String(Str => Frame_Name & ".reputation");
      Sky_Bases(Base_Index).Reputation.Level :=
        Integer'Value(Get(Widgt => Base_Box));
      --## rule off ASSIGNMENTS
      Base_Box.Name := New_String(Str => Frame_Name & ".money");
      Item :=
        BaseCargo_Container.Element
          (Container => Sky_Bases(Base_Index).Cargo, Index => 1);
      Item.Amount := Natural'Value(Get(Widgt => Base_Box));
      --## rule on ASSIGNMENTS
      BaseCargo_Container.Replace_Element
        (Container => Sky_Bases(Base_Index).Cargo, Index => 1,
         New_Item => Item);
      Set_Base_In_Nim(Base_Index => Base_Index);
      return TCL_OK;
   end Update_Base_Command;

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
      Convention => C;
      -- ****

   function Add_Ship_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      use Tiny_String;

      Frame_Name: constant String := ".debugdialog.main.world";
      Ship_Entry: constant Ttk_Entry :=
        Get_Widget(pathName => Frame_Name & ".ship", Interp => Interp);
      Ship_Name: Bounded_String;
      Npc_Ship_X, Npc_Ship_Y, Duration: Positive;
      Ship_Box: Ttk_SpinBox :=
        Get_Widget(pathName => Frame_Name & ".x", Interp => Interp);
   begin
      Ship_Name := To_Bounded_String(Source => Get(Widgt => Ship_Entry));
      Npc_Ship_X := Positive'Value(Get(Widgt => Ship_Box));
      Ship_Box.Name := New_String(Str => Frame_Name & ".y");
      Npc_Ship_Y := Positive'Value(Get(Widgt => Ship_Box));
      --## rule off ASSIGNMENTS
      Ship_Box.Name := New_String(Str => Frame_Name & ".duration");
      --## rule on ASSIGNMENTS
      Duration := Positive'Value(Get(Widgt => Ship_Box));
      Add_Ship_Event_Loop :
      for I in 1 .. Get_Proto_Ships_Amount loop
         if Get_Proto_Ship(Proto_Index => I).Name = Ship_Name then
            if Get_Trader_Or_Friendly(Index => I, Get_Trader => 1) > 0 then
               Get_Ada_Event
                 (Index => Get_Events_Amount + 1, X => Npc_Ship_X,
                  Y => Npc_Ship_Y, Time => Duration,
                  E_Type => Events_Types'Pos(TRADER), Data => I);
            elsif Get_Trader_Or_Friendly(Index => I, Get_Trader => 0) > 0 then
               Get_Ada_Event
                 (Index => Get_Events_Amount + 1, X => Npc_Ship_X,
                  Y => Npc_Ship_Y, Time => Duration,
                  E_Type => Events_Types'Pos(FRIENDLYSHIP), Data => I);
            else
               Get_Ada_Event
                 (Index => Get_Events_Amount + 1, X => Npc_Ship_X,
                  Y => Npc_Ship_Y, Time => Duration,
                  E_Type => Events_Types'Pos(ENEMYSHIP), Data => I);
            end if;
            Get_Ada_Map_Cell
              (X => Npc_Ship_X, Y => Npc_Ship_Y,
               Base_Index => Sky_Map(Npc_Ship_X, Npc_Ship_Y).Base_Index,
               Event_Index => Get_Events_Amount,
               Mission_Index => Sky_Map(Npc_Ship_X, Npc_Ship_Y).Mission_Index,
               Visited =>
                 (if Sky_Map(Npc_Ship_X, Npc_Ship_Y).Visited then 1 else 0));
            return
              Refresh_Events_Command
                (Client_Data => Client_Data, Interp => Interp, Argc => Argc,
                 Argv => Argv);
         end if;
      end loop Add_Ship_Event_Loop;
      return TCL_OK;
   end Add_Ship_Command;

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
      Convention => C;
      -- ****

   function Toggle_Item_Entry_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc, Argv);
      use Tcl.Tk.Ada.Widgets.TtkLabel;

      Frame_Name: constant String := ".debugdialog.main.world";
      Event_Combo: constant Ttk_ComboBox :=
        Get_Widget(pathName => Frame_Name & ".event", Interp => Interp);
      Item_Entry: constant Ttk_Entry :=
        Get_Widget(pathName => Frame_Name & ".item", Interp => Interp);
      Item_Label: constant Ttk_Label :=
        Get_Widget(pathName => Frame_Name & ".itemlbl", Interp => Interp);
   begin
      if Current(ComboBox => Event_Combo) = "1" then
         Tcl.Tk.Ada.Grid.Grid(Slave => Item_Label);
         Tcl.Tk.Ada.Grid.Grid(Slave => Item_Entry);
      else
         Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Item_Label);
         Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Item_Entry);
      end if;
      return TCL_OK;
   end Toggle_Item_Entry_Command;

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
      Convention => C;
      -- ****

   function Add_Event_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      use Tiny_String;
      Frame_Name: constant String := ".debugdialog.main.world";
      Event_Entry: constant Ttk_Entry :=
        Get_Widget(pathName => Frame_Name & ".base", Interp => Interp);
      Event_Name: Unbounded_String;
      Base_Index, Event_Type: Natural := 0;
      Event_Box: Ttk_ComboBox :=
        Get_Widget(pathName => Frame_Name & ".event", Interp => Interp);
      Duration_Box: constant Ttk_SpinBox :=
        Get_Widget(pathName => Frame_Name & ".baseduration", Interp => Interp);
      Added: Boolean := True;
   begin
      Event_Name := To_Unbounded_String(Source => Get(Widgt => Event_Entry));
      Find_Base_Index_Loop :
      for I in Sky_Bases'Range loop
         if To_String(Source => Sky_Bases(I).Name) =
           To_String(Source => Event_Name) then
            Base_Index := I;
            exit Find_Base_Index_Loop;
         end if;
      end loop Find_Base_Index_Loop;
      if Base_Index = 0 then
         return TCL_OK;
      end if;
      Event_Type := Natural'Value(Current(ComboBox => Event_Box));
      case Event_Type is
         when 0 =>
            Get_Ada_Event
              (Index => Get_Events_Amount + 1,
               X => Sky_Bases(Base_Index).Sky_X,
               Y => Sky_Bases(Base_Index).Sky_Y,
               Time => Positive'Value(Get(Widgt => Duration_Box)),
               E_Type => Events_Types'Pos(DISEASE), Data => 1);
         when 1 =>
            Event_Box.Name := New_String(Str => Frame_Name & ".item");
            Event_Name :=
              To_Unbounded_String(Source => Get(Widgt => Event_Box));
            Added := False;
            Find_Item_Loop :
            for I in 1 .. Get_Proto_Amount loop
               if To_String(Source => Get_Proto_Item(Index => I).Name) =
                 To_String(Source => Event_Name) then
                  Get_Ada_Event
                    (Index => Get_Events_Amount + 1,
                     X => Sky_Bases(Base_Index).Sky_X,
                     Y => Sky_Bases(Base_Index).Sky_Y,
                     Time => Positive'Value(Get(Widgt => Duration_Box)),
                     E_Type => Events_Types'Pos(DOUBLEPRICE), Data => I);
                  Added := True;
                  exit Find_Item_Loop;
               end if;
            end loop Find_Item_Loop;
         when 2 =>
            Get_Ada_Event
              (Index => Get_Events_Amount + 1,
               X => Sky_Bases(Base_Index).Sky_X,
               Y => Sky_Bases(Base_Index).Sky_Y,
               Time => Positive'Value(Get(Widgt => Duration_Box)),
               E_Type => Events_Types'Pos(FULLDOCKS), Data => 1);
         when others =>
            null;
      end case;
      if not Added then
         return TCL_OK;
      end if;
      Get_Ada_Map_Cell
        (X => Sky_Bases(Base_Index).Sky_X, Y => Sky_Bases(Base_Index).Sky_Y,
         Base_Index =>
           Sky_Map(Sky_Bases(Base_Index).Sky_X, Sky_Bases(Base_Index).Sky_Y)
             .Base_Index,
         Event_Index => Get_Events_Amount,
         Mission_Index =>
           Sky_Map(Sky_Bases(Base_Index).Sky_X, Sky_Bases(Base_Index).Sky_Y)
             .Mission_Index,
         Visited =>
           (if
              Sky_Map(Sky_Bases(Base_Index).Sky_X, Sky_Bases(Base_Index).Sky_Y)
                .Visited
            then 1
            else 0));
      return
        Refresh_Events_Command
          (Client_Data => Client_Data, Interp => Interp, Argc => Argc,
           Argv => Argv);
   end Add_Event_Command;

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
      use Utils.UI;
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
