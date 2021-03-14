-- Copyright (c) 2020-2021 Bartek thindil Jasicki <thindil@laeran.pl>
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

with Ada.Containers; use Ada.Containers;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.String_Split; use GNAT.String_Split;
with CArgv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkEntry; use Tcl.Tk.Ada.Widgets.TtkEntry;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Bases; use Bases;
with BasesTypes; use BasesTypes;
with Crew; use Crew;
with Events; use Events;
with Factions; use Factions;
with Game; use Game;
with Game.SaveLoad; use Game.SaveLoad;
with Items; use Items;
with Maps; use Maps;
with Maps.UI; use Maps.UI;
with ShipModules; use ShipModules;
with Ships; use Ships;
with Ships.Cargo; use Ships.Cargo;
with Utils.UI; use Utils.UI;

package body DebugUI is

   -- ****o* DebugUI/DebugUI.Refresh_Module_Command
   -- FUNCTION
   -- Refresh the information about selected module
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- RefreshModule
   -- SOURCE
   function Refresh_Module_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Refresh_Module_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      ProtoEntry: constant Ttk_Entry :=
        Get_Widget(".debugdialog.main.ship.proto", Interp);
      ModuleCombo: constant Ttk_ComboBox :=
        Get_Widget(".debugdialog.main.ship.module", Interp);
      ModuleIndex: Positive;
      SpinBox: Ttk_SpinBox :=
        Get_Widget(".debugdialog.main.ship.weight", Interp);
   begin
      ModuleIndex := Natural'Value(Current(ModuleCombo)) + 1;
      Delete(ProtoEntry, "0", "end");
      Insert
        (ProtoEntry, "0",
         To_String
           (Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex).Name));
      Set(SpinBox, Positive'Image(PlayerShip.Modules(ModuleIndex).Weight));
      SpinBox.Name := New_String(".debugdialog.main.ship.dur");
      Set(SpinBox, Integer'Image(PlayerShip.Modules(ModuleIndex).Durability));
      SpinBox.Name := New_String(".debugdialog.main.ship.maxdur");
      Set
        (SpinBox,
         Positive'Image(PlayerShip.Modules(ModuleIndex).MaxDurability));
      SpinBox.Name := New_String(".debugdialog.main.ship.upgrade");
      Set
        (SpinBox,
         Natural'Image(PlayerShip.Modules(ModuleIndex).UpgradeProgress));
      return TCL_OK;
   end Refresh_Module_Command;

   -- ****o* DebugUI/DebugUI.Refresh_Member_Command
   -- FUNCTION
   -- Refresh the information about selected crew member
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- RefreshMember
   -- SOURCE
   function Refresh_Member_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Refresh_Member_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      ComboBox: Ttk_ComboBox :=
        Get_Widget(".debugdialog.main.crew.member", Interp);
      SpinBox: Ttk_SpinBox :=
        Get_Widget(".debugdialog.main.crew.health", Interp);
      MemberFrame: Ttk_Frame :=
        Get_Widget(".debugdialog.main.crew.stats", Interp);
      Rows: Natural := 0;
      Tokens: Slice_Set;
      Label: Ttk_Label;
      Member: Member_Data;
      SkillsIndexes: Positive_Container.Vector;
      SkillsList: Unbounded_String;
   begin
      Member := PlayerShip.Crew(Natural'Value(Current(ComboBox)) + 1);
      Set(SpinBox, Positive'Image(Member.Health));
      SpinBox.Name := New_String(".debugdialog.main.crew.thirst");
      Set(SpinBox, Positive'Image(Member.Thirst));
      SpinBox.Name := New_String(".debugdialog.main.crew.hunger");
      Set(SpinBox, Positive'Image(Member.Hunger));
      SpinBox.Name := New_String(".debugdialog.main.crew.tired");
      Set(SpinBox, Positive'Image(Member.Tired));
      SpinBox.Name := New_String(".debugdialog.main.crew.morale");
      Set(SpinBox, Positive'Image(Member.Morale(1)));
      SpinBox.Name := New_String(".debugdialog.main.crew.loyalty");
      Set(SpinBox, Positive'Image(Member.Loyalty));
      Create(Tokens, Tcl.Tk.Ada.Grid.Grid_Size(MemberFrame), " ");
      Rows := Natural'Value(Slice(Tokens, 2));
      Delete_Widgets(1, Rows - 1, MemberFrame);
      Show_Stats_Loop :
      for I in Member.Attributes.Iterate loop
         Label :=
           Create
             (".debugdialog.main.crew.stats.label" &
              Trim(Positive'Image(Attributes_Container.To_Index(I)), Left),
              "-text {" &
              To_String
                (Attributes_List(Attributes_Container.To_Index(I)).Name) &
              "}");
         Tcl.Tk.Ada.Grid.Grid(Label);
         SpinBox :=
           Create
             (".debugdialog.main.crew.stats.value" &
              Trim(Positive'Image(Attributes_Container.To_Index(I)), Left),
              "-from 1 -to 50 -validate key -validatecommand {ValidateSpinbox %W %P}");
         Set(SpinBox, Positive'Image(Member.Attributes(I)(1)));
         Tcl.Tk.Ada.Grid.Grid
           (SpinBox,
            "-column 1 -row" &
            Positive'Image(Attributes_Container.To_Index(I)));
      end loop Show_Stats_Loop;
      MemberFrame.Name := New_String(".debugdialog.main.crew.skills");
      Create(Tokens, Tcl.Tk.Ada.Grid.Grid_Size(MemberFrame), " ");
      Rows := Natural'Value(Slice(Tokens, 2));
      Delete_Widgets(1, Rows - 1, MemberFrame);
      Show_Skills_Loop :
      for I in Member.Skills.Iterate loop
         Label :=
           Create
             (".debugdialog.main.crew.skills.label" &
              Trim(Positive'Image(Skills_Container.To_Index(I)), Left),
              "-text {" & To_String(Skills_List(Member.Skills(I)(1)).Name) &
              "}");
         Tcl.Tk.Ada.Grid.Grid(Label);
         SpinBox :=
           Create
             (".debugdialog.main.crew.skills.value" &
              Trim(Positive'Image(Skills_Container.To_Index(I)), Left),
              "-from 1 -to 100 -validate key -validatecommand {ValidateSpinbox %W %P}");
         Set(SpinBox, Positive'Image(Member.Skills(I)(2)));
         Tcl.Tk.Ada.Grid.Grid
           (SpinBox,
            "-column 1 -row" & Positive'Image(Skills_Container.To_Index(I)));
         SkillsIndexes.Append(Member.Skills(I)(1));
      end loop Show_Skills_Loop;
      Show_Add_Skills_Loop :
      for I in Skills_List.Iterate loop
         if not SkillsIndexes.Contains(SkillsData_Container.To_Index(I)) then
            Append(SkillsList, " " & Skills_List(I).Name);
         end if;
      end loop Show_Add_Skills_Loop;
      ComboBox.Name := New_String(".debugdialog.main.crew.addskill.skills");
      configure(ComboBox, "-values [list" & To_String(SkillsList) & "]");
      Current(ComboBox, "0");
      return TCL_OK;
   end Refresh_Member_Command;

   -- ****o* DebugUI/DebugUI.Refresh_Cargo_Command
   -- FUNCTION
   -- Refresh the information about the player ship cargo
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- RefreshCargo
   -- SOURCE
   function Refresh_Cargo_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Refresh_Cargo_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      CargoCombo: constant Ttk_ComboBox :=
        Get_Widget(".debugdialog.main.cargo.update", Interp);
      ItemIndex: Positive;
      AmountBox: constant Ttk_SpinBox :=
        Get_Widget(".debugdialog.main.cargo.updateamount", Interp);
   begin
      ItemIndex := Natural'Value(Current(CargoCombo)) + 1;
      Set(AmountBox, Positive'Image(PlayerShip.Cargo(ItemIndex).Amount));
      return TCL_OK;
   end Refresh_Cargo_Command;

   -- ****o* DebugUI/DebugUI.Refresh_Command
   -- FUNCTION
   -- Refresh the whole game information
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- Refresh
   -- SOURCE
   function Refresh_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Refresh_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      SpinBox: Ttk_SpinBox := Get_Widget(".debugdialog.main.ship.x", Interp);
      ComboBox: Ttk_ComboBox :=
        Get_Widget(".debugdialog.main.ship.module", Interp);
      ValuesList: Unbounded_String;
   begin
      Set(SpinBox, Positive'Image(PlayerShip.SkyX));
      SpinBox.Name := New_String(".debugdialog.main.ship.y");
      Set(SpinBox, Positive'Image(PlayerShip.SkyY));
      Update_Modules_Loop :
      for Module of PlayerShip.Modules loop
         Append(ValuesList, " {" & Module.Name & "}");
      end loop Update_Modules_Loop;
      configure(ComboBox, "-values [list" & To_String(ValuesList) & "]");
      Current(ComboBox, "0");
      Tcl_Eval(Get_Context, "RefreshModule");
      ComboBox.Name := New_String(".debugdialog.main.crew.member");
      ValuesList := Null_Unbounded_String;
      Update_Members_Loop :
      for Member of PlayerShip.Crew loop
         Append(ValuesList, " {" & Member.Name & "}");
      end loop Update_Members_Loop;
      configure(ComboBox, "-values [list" & To_String(ValuesList) & "]");
      Current(ComboBox, "0");
      Tcl_Eval(Get_Context, "RefreshMember");
      ComboBox.Name := New_String(".debugdialog.main.cargo.update");
      ValuesList := Null_Unbounded_String;
      Update_Cargo_Loop :
      for Item of PlayerShip.Cargo loop
         Append(ValuesList, " {" & GetItemName(Item, False, False) & "}");
      end loop Update_Cargo_Loop;
      configure(ComboBox, "-values [list" & To_String(ValuesList) & "]");
      Current(ComboBox, "0");
      Tcl_Eval(Get_Context, "RefreshCargo");
      Tcl_Eval(Get_Context, "RefreshEvents");
      return TCL_OK;
   end Refresh_Command;

   -- ****o* DebugUI/DebugUI.Refresh_Base_Command
   -- FUNCTION
   -- Refresh the information about the selected base
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- RefreshBase
   -- SOURCE
   function Refresh_Base_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Refresh_Base_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      NameEntry: constant Ttk_Entry :=
        Get_Widget(".debugdialog.main.bases.name", Interp);
      BaseIndex: Natural := 0;
      BaseName: Unbounded_String;
      ComboBox: Ttk_ComboBox :=
        Get_Widget(".debugdialog.main.bases.type", Interp);
      SpinBox: Ttk_SpinBox :=
        Get_Widget(".debugdialog.main.bases.population", Interp);
   begin
      BaseName := To_Unbounded_String(Get(NameEntry));
      Find_Base_Index_Loop :
      for I in SkyBases'Range loop
         if SkyBases(I).Name = BaseName then
            BaseIndex := I;
            exit Find_Base_Index_Loop;
         end if;
      end loop Find_Base_Index_Loop;
      if BaseIndex = 0 then
         return TCL_OK;
      end if;
      Set
        (ComboBox,
         To_String(BasesTypes_List(SkyBases(BaseIndex).BaseType).Name));
      ComboBox.Name := New_String(".debugdialog.main.bases.owner");
      Set(ComboBox, To_String(Factions_List(SkyBases(BaseIndex).Owner).Name));
      ComboBox.Name := New_String(".debugdialog.main.bases.size");
      Current
        (ComboBox, Natural'Image(Bases_Size'Pos(SkyBases(BaseIndex).Size)));
      Set(SpinBox, Natural'Image(SkyBases(BaseIndex).Population));
      SpinBox.Name := New_String(".debugdialog.main.bases.reputation");
      Set(SpinBox, Integer'Image(SkyBases(BaseIndex).Reputation(1)));
      SpinBox.Name := New_String(".debugdialog.main.bases.money");
      if SkyBases(BaseIndex).Cargo.Length > 0 then
         Set(SpinBox, Natural'Image(SkyBases(BaseIndex).Cargo(1).Amount));
      else
         Set(SpinBox, "0");
      end if;
      return TCL_OK;
   end Refresh_Base_Command;

   -- ****o* DebugUI/DebugUI.Refresh_Events_Command
   -- FUNCTION
   -- Refresh the list of events
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- RefreshEvents
   -- SOURCE
   function Refresh_Events_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Refresh_Events_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      EventsBox: constant Ttk_ComboBox :=
        Get_Widget(".debugdialog.main.world.delete", Interp);
      ValuesList: Unbounded_String;
      EventsButton: constant Ttk_Button :=
        Get_Widget(".debugdialog.main.world.deleteevent", Interp);
   begin
      if Events_List.Length = 0 then
         Tcl.Tk.Ada.Grid.Grid_Remove(EventsButton);
         Tcl.Tk.Ada.Grid.Grid_Remove(EventsBox);
         return TCL_OK;
      else
         Tcl.Tk.Ada.Grid.Grid(EventsButton);
         Tcl.Tk.Ada.Grid.Grid(EventsBox);
      end if;
      Update_Events_Loop :
      for Event of Events_List loop
         case Event.EType is
            when EnemyShip =>
               Append
                 (ValuesList,
                  " {Enemy ship: " &
                  To_String(ProtoShips_List(Event.ShipIndex).Name) & "}");
            when AttackOnBase =>
               Append
                 (ValuesList,
                  " {Attack on base: " &
                  To_String(ProtoShips_List(Event.ShipIndex).Name) & "}");
            when Disease =>
               Append
                 (ValuesList,
                  " {Disease in base: " &
                  To_String
                    (SkyBases(SkyMap(Event.SkyX, Event.SkyY).BaseIndex).Name) &
                  "}");
            when DoublePrice =>
               Append
                 (ValuesList,
                  " {Double price in base: " &
                  To_String
                    (SkyBases(SkyMap(Event.SkyX, Event.SkyY).BaseIndex).Name) &
                  "}");
            when FullDocks =>
               Append
                 (ValuesList,
                  " {Full docks in base: " &
                  To_String
                    (SkyBases(SkyMap(Event.SkyX, Event.SkyY).BaseIndex).Name) &
                  "}");
            when EnemyPatrol =>
               Append
                 (ValuesList,
                  " {Enemy patrol: " &
                  To_String(ProtoShips_List(Event.ShipIndex).Name) & "}");
            when Trader =>
               Append
                 (ValuesList,
                  " {Trader: " &
                  To_String(ProtoShips_List(Event.ShipIndex).Name) & "}");
            when FriendlyShip =>
               Append
                 (ValuesList,
                  " {Friendly ship: " &
                  To_String(ProtoShips_List(Event.ShipIndex).Name) & "}");
            when others =>
               null;
         end case;
      end loop Update_Events_Loop;
      configure(EventsBox, "-values [list" & To_String(ValuesList) & "]");
      Current(EventsBox, "0");
      return TCL_OK;
   end Refresh_Events_Command;

   -- ****o* DebugUI/DebugUI.Save_Game_Command
   -- FUNCTION
   -- Save the game
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- DebugSaveGame
   -- SOURCE
   function Save_Game_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Save_Game_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc, Argv);
   begin
      SaveGame(True);
      return TCL_OK;
   end Save_Game_Command;

   -- ****o* DebugUI/DebugUI.Move_Ship_Command
   -- FUNCTION
   -- Move the player ship
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- DebugMoveShip
   -- SOURCE
   function Move_Ship_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Move_Ship_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      SpinBox: Ttk_SpinBox := Get_Widget(".debugdialog.main.ship.x", Interp);
   begin
      PlayerShip.SkyX := Positive'Value(Get(SpinBox));
      SpinBox.Name := New_String(".debugdialog.main.ship.y");
      PlayerShip.SkyY := Positive'Value(Get(SpinBox));
      ShowSkyMap(True);
      return TCL_OK;
   end Move_Ship_Command;

   -- ****o* DebugUI/DebugUI.Update_Module_Command
   -- FUNCTION
   -- Update the selected module
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- DebugUpdateModule
   -- SOURCE
   function Update_Module_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Update_Module_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      ModuleBox: constant Ttk_ComboBox :=
        Get_Widget(".debugdialog.main.ship.module", Interp);
      ModuleIndex: Positive;
      ModuleEntry: constant Ttk_Entry :=
        Get_Widget(".debugdialog.main.ship.proto", Interp);
      Value: Unbounded_String;
      SpinBox: Ttk_SpinBox :=
        Get_Widget(".debugdialog.main.ship.weight", Interp);
   begin
      ModuleIndex := Natural'Value(Current(ModuleBox)) + 1;
      Value := To_Unbounded_String(Get(ModuleEntry));
      Update_Proto_Index_Loop :
      for I in Modules_List.Iterate loop
         if Modules_List(I).Name = Value then
            Value := Null_Unbounded_String;
            PlayerShip.Modules(ModuleIndex).ProtoIndex :=
              BaseModules_Container.Key(I);
            exit Update_Proto_Index_Loop;
         end if;
      end loop Update_Proto_Index_Loop;
      PlayerShip.Modules(ModuleIndex).Weight := Natural'Value(Get(SpinBox));
      SpinBox.Name := New_String(".debugdialog.main.ship.dur");
      PlayerShip.Modules(ModuleIndex).Durability :=
        Natural'Value(Get(SpinBox));
      SpinBox.Name := New_String(".debugdialog.main.ship.maxdur");
      PlayerShip.Modules(ModuleIndex).MaxDurability :=
        Natural'Value(Get(SpinBox));
      SpinBox.Name := New_String(".debugdialog.main.ship.upgrade");
      PlayerShip.Modules(ModuleIndex).UpgradeProgress :=
        Natural'Value(Get(SpinBox));
      return TCL_OK;
   end Update_Module_Command;

   -- ****o* DebugUI/DebugUI.Add_Skill_Command
   -- FUNCTION
   -- Add a new skill to the selected crew member
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- DebugAddSkill
   -- SOURCE
   function Add_Skill_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Add_Skill_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      ComboBox: Ttk_ComboBox :=
        Get_Widget(".debugdialog.main.crew.member", Interp);
      MemberIndex: Positive;
      SkillName: Unbounded_String;
   begin
      MemberIndex := Natural'Value(Current(ComboBox)) + 1;
      ComboBox.Name := New_String(".debugdialog.main.crew.addskill.skills");
      SkillName := To_Unbounded_String(Get(ComboBox));
      for I in Skills_List.Iterate loop
         if Skills_List(I).Name = SkillName then
            PlayerShip.Crew(MemberIndex).Skills.Append
              ((SkillsData_Container.To_Index(I), 1, 0));
            Tcl_Eval(Interp, "RefreshMember");
            exit;
         end if;
      end loop;
      return TCL_OK;
   end Add_Skill_Command;

   -- ****o* DebugUI/DebugUI.Update_Member_Command
   -- FUNCTION
   -- Update the selected crew member
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- DebugUpdateMember
   -- SOURCE
   function Update_Member_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Update_Member_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      ComboBox: constant Ttk_ComboBox :=
        Get_Widget(".debugdialog.main.crew.member", Interp);
      MemberIndex: Positive;
      SpinBox: Ttk_SpinBox :=
        Get_Widget(".debugdialog.main.crew.health", Interp);
   begin
      MemberIndex := Natural'Value(Current(ComboBox)) + 1;
      PlayerShip.Crew(MemberIndex).Health := Skill_Range'Value(Get(SpinBox));
      SpinBox.Name := New_String(".debugdialog.main.crew.thirst");
      PlayerShip.Crew(MemberIndex).Thirst := Skill_Range'Value(Get(SpinBox));
      SpinBox.Name := New_String(".debugdialog.main.crew.hunger");
      PlayerShip.Crew(MemberIndex).Hunger := Skill_Range'Value(Get(SpinBox));
      SpinBox.Name := New_String(".debugdialog.main.crew.tired");
      PlayerShip.Crew(MemberIndex).Tired := Skill_Range'Value(Get(SpinBox));
      SpinBox.Name := New_String(".debugdialog.main.crew.morale");
      PlayerShip.Crew(MemberIndex).Morale(1) :=
        Skill_Range'Value(Get(SpinBox));
      SpinBox.Name := New_String(".debugdialog.main.crew.loyalty");
      PlayerShip.Crew(MemberIndex).Loyalty := Skill_Range'Value(Get(SpinBox));
      for I in PlayerShip.Crew(MemberIndex).Attributes.Iterate loop
         SpinBox.Name :=
           New_String
             (".debugdialog.main.crew.stats.value" &
              Trim(Positive'Image(Attributes_Container.To_Index(I)), Left));
         PlayerShip.Crew(MemberIndex).Attributes(I)(1) :=
           Positive'Value(Get(SpinBox));
      end loop;
      for I in PlayerShip.Crew(MemberIndex).Skills.Iterate loop
         SpinBox.Name :=
           New_String
             (".debugdialog.main.crew.skills.value" &
              Trim(Positive'Image(Skills_Container.To_Index(I)), Left));
         PlayerShip.Crew(MemberIndex).Skills(I)(2) :=
           Positive'Value(Get(SpinBox));
      end loop;
      return TCL_OK;
   end Update_Member_Command;

   -- ****o* DebugUI/DebugUI.Add_Item_Command
   -- FUNCTION
   -- Add a new item to the player ship cargo
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- DebugAddItem
   -- SOURCE
   function Add_Item_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Add_Item_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      ItemEntry: constant Ttk_Entry :=
        Get_Widget(".debugdialog.main.cargo.add", Interp);
      ItemBox: constant Ttk_SpinBox :=
        Get_Widget(".debugdialog.main.cargo.amount", Interp);
      ItemIndex, ItemName: Unbounded_String;
   begin
      ItemName := To_Unbounded_String(Get(ItemEntry));
      for I in Items_List.Iterate loop
         if Items_List(I).Name = ItemName then
            ItemIndex := Objects_Container.Key(I);
            exit;
         end if;
      end loop;
      if ItemIndex = Null_Unbounded_String then
         return TCL_OK;
      end if;
      UpdateCargo(PlayerShip, ItemIndex, Positive'Value(Get(ItemBox)));
      return Refresh_Command(ClientData, Interp, Argc, Argv);
   end Add_Item_Command;

   -- ****o* DebugUI/DebugUI.Update_Item_Command
   -- FUNCTION
   -- Update the amount of an item in the player ship cargo
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- DebugUpdateItem
   -- SOURCE
   function Update_Item_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Update_Item_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      ItemCombo: constant Ttk_ComboBox :=
        Get_Widget(".debugdialog.main.cargo.update", Interp);
      ItemBox: constant Ttk_SpinBox :=
        Get_Widget(".debugdialog.main.cargo.updateamount", Interp);
      ItemIndex: Positive;
   begin
      ItemIndex := Natural'Value(Current(ItemCombo)) + 1;
      UpdateCargo
        (Ship => PlayerShip, Amount => Positive'Value(Get(ItemBox)),
         CargoIndex => ItemIndex);
      return Refresh_Command(ClientData, Interp, Argc, Argv);
   end Update_Item_Command;

   -- ****o* DebugUI/DebugUI.Update_Base_Command
   -- FUNCTION
   -- Update the selected base
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- DebugUpdateBase
   -- SOURCE
   function Update_Base_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Update_Base_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      BaseIndex: Natural := 0;
      BaseEntry: constant Ttk_Entry :=
        Get_Widget(".debugdialog.main.bases.name", Interp);
      BaseName: Unbounded_String;
      BaseCombo: Ttk_ComboBox :=
        Get_Widget(".debugdialog.main.bases.type", Interp);
      BaseBox: Ttk_SpinBox :=
        Get_Widget(".debugdialog.main.bases.population", Interp);
   begin
      BaseName := To_Unbounded_String(Get(BaseEntry));
      for I in SkyBases'Range loop
         if SkyBases(I).Name = BaseName then
            BaseIndex := I;
            exit;
         end if;
      end loop;
      if BaseIndex = 0 then
         return TCL_OK;
      end if;
      for I in BasesTypes_List.Iterate loop
         if BasesTypes_List(I).Name = To_Unbounded_String(Get(BaseCombo)) then
            SkyBases(BaseIndex).BaseType := BasesTypes_Container.Key(I);
            exit;
         end if;
      end loop;
      BaseCombo.Name := New_String(".debugdialog.main.bases.owner");
      for I in Factions_List.Iterate loop
         if Factions_List(I).Name = To_Unbounded_String(Get(BaseCombo)) then
            SkyBases(BaseIndex).Owner := Factions_Container.Key(I);
            exit;
         end if;
      end loop;
      BaseCombo.Name := New_String(".debugdialog.main.bases.size");
      SkyBases(BaseIndex).Size := Bases_Size'Value(Get(BaseCombo));
      SkyBases(BaseIndex).Population := Natural'Value(Get(BaseBox));
      BaseBox.Name := New_String(".debugdialog.main.bases.reputation");
      SkyBases(BaseIndex).Reputation(1) := Integer'Value(Get(BaseBox));
      BaseBox.Name := New_String(".debugdialog.main.bases.money");
      SkyBases(BaseIndex).Cargo(1).Amount := Natural'Value(Get(BaseBox));
      return TCL_OK;
   end Update_Base_Command;

   -- ****o* DebugUI/DebugUI.Add_Ship_Command
   -- FUNCTION
   -- Add a new ship based event to the game
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- DebugAddShip
   -- SOURCE
   function Add_Ship_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Add_Ship_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      ShipEntry: constant Ttk_Entry :=
        Get_Widget(".debugdialog.main.world.ship", Interp);
      ShipName: Unbounded_String;
      NpcShipX, NpcShipY, Duration: Positive;
      ShipBox: Ttk_SpinBox := Get_Widget(".debugdialog.main.world.x", Interp);
   begin
      ShipName := To_Unbounded_String(Get(ShipEntry));
      NpcShipX := Positive'Value(Get(ShipBox));
      ShipBox.Name := New_String(".debugdialog.main.world.y");
      NpcShipY := Positive'Value(Get(ShipBox));
      ShipBox.Name := New_String(".debugdialog.main.world.duration");
      Duration := Positive'Value(Get(ShipBox));
      for I in ProtoShips_List.Iterate loop
         if ProtoShips_List(I).Name = ShipName then
            if Traders.Contains(ProtoShips_Container.Key(I)) then
               Events_List.Append
                 (New_Item =>
                    (Trader, NpcShipX, NpcShipY, Duration,
                     ProtoShips_Container.Key(I)));
            elsif FriendlyShips.Contains(ProtoShips_Container.Key(I)) then
               Events_List.Append
                 (New_Item =>
                    (FriendlyShip, NpcShipX, NpcShipY, Duration,
                     ProtoShips_Container.Key(I)));
            else
               Events_List.Append
                 (New_Item =>
                    (EnemyShip, NpcShipX, NpcShipY, Duration,
                     ProtoShips_Container.Key(I)));
            end if;
            SkyMap(NpcShipX, NpcShipY).EventIndex := Events_List.Last_Index;
            return Refresh_Events_Command(ClientData, Interp, Argc, Argv);
         end if;
      end loop;
      return TCL_OK;
   end Add_Ship_Command;

   -- ****o* DebugUI/DebugUI.Toggle_Item_Entry_Command
   -- FUNCTION
   -- Show or hide item entry for bases events
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ToggleItemEntry
   -- SOURCE
   function Toggle_Item_Entry_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Toggle_Item_Entry_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      EventCombo: constant Ttk_ComboBox :=
        Get_Widget(".debugdialog.main.world.event", Interp);
      ItemEntry: constant Ttk_Entry :=
        Get_Widget(".debugdialog.main.world.item", Interp);
      ItemLabel: constant Ttk_Label :=
        Get_Widget(".debugdialog.main.world.itemlbl", Interp);
   begin
      if Current(EventCombo) = "1" then
         Tcl.Tk.Ada.Grid.Grid(ItemLabel);
         Tcl.Tk.Ada.Grid.Grid(ItemEntry);
      else
         Tcl.Tk.Ada.Grid.Grid_Remove(ItemLabel);
         Tcl.Tk.Ada.Grid.Grid_Remove(ItemEntry);
      end if;
      return TCL_OK;
   end Toggle_Item_Entry_Command;

   -- ****o* DebugUI/DebugUI.Add_Event_Command
   -- FUNCTION
   -- Add a new base event to the game
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- DebugAddEvent
   -- SOURCE
   function Add_Event_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Add_Event_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      EventEntry: constant Ttk_Entry :=
        Get_Widget(".debugdialog.main.world.base", Interp);
      EventName: Unbounded_String;
      BaseIndex, EventType: Natural := 0;
      EventBox: Ttk_ComboBox :=
        Get_Widget(".debugdialog.main.world.event", Interp);
      DurationBox: constant Ttk_SpinBox :=
        Get_Widget(".debugdialog.main.world.baseduration", Interp);
      Added: Boolean := True;
   begin
      EventName := To_Unbounded_String(Get(EventEntry));
      for I in SkyBases'Range loop
         if SkyBases(I).Name = EventName then
            BaseIndex := I;
            exit;
         end if;
      end loop;
      if BaseIndex = 0 then
         return TCL_OK;
      end if;
      EventType := Natural'Value(Current(EventBox));
      case EventType is
         when 0 =>
            Events_List.Append
              (New_Item =>
                 (Disease, SkyBases(BaseIndex).SkyX, SkyBases(BaseIndex).SkyY,
                  Positive'Value(Get(DurationBox)), 1));
         when 1 =>
            EventBox.Name := New_String(".debugdialog.main.world.item");
            EventName := To_Unbounded_String(Get(EventBox));
            Added := False;
            for I in Items_List.Iterate loop
               if Items_List(I).Name = EventName then
                  Events_List.Append
                    (New_Item =>
                       (DoublePrice, SkyBases(BaseIndex).SkyX,
                        SkyBases(BaseIndex).SkyY,
                        Positive'Value(Get(DurationBox)),
                        Objects_Container.Key(I)));
                  Added := True;
                  exit;
               end if;
            end loop;
         when 2 =>
            Events_List.Append
              (New_Item =>
                 (Disease, SkyBases(BaseIndex).SkyX, SkyBases(BaseIndex).SkyY,
                  Positive'Value(Get(DurationBox)), 1));
         when others =>
            null;
            null;
      end case;
      if not Added then
         return TCL_OK;
      end if;
      SkyMap(SkyBases(BaseIndex).SkyX, SkyBases(BaseIndex).SkyY).EventIndex :=
        Events_List.Last_Index;
      return Refresh_Events_Command(ClientData, Interp, Argc, Argv);
   end Add_Event_Command;

   -- ****o* DebugUI/DebugUI.Delete_Event_Command
   -- FUNCTION
   -- Remove the selected event from the game
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- DebugDeleteEvent
   -- SOURCE
   function Delete_Event_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Delete_Event_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      EventBox: constant Ttk_ComboBox :=
        Get_Widget(".debugdialog.main.world.delete", Interp);
   begin
      DeleteEvent(Natural'Value(Current(EventBox)) + 1);
      return Refresh_Events_Command(ClientData, Interp, Argc, Argv);
   end Delete_Event_Command;

   procedure ShowDebugUI is
      ComboBox: Ttk_ComboBox := Get_Widget(".debugdialog.main.bases.type");
      ValuesList: Unbounded_String;
   begin
      Tcl_EvalFile
        (Get_Context,
         To_String(Data_Directory) & "ui" & Dir_Separator & "debug.tcl");
      AddCommand("Refresh", Refresh_Command'Access);
      AddCommand("RefreshModule", Refresh_Module_Command'Access);
      AddCommand("RefreshMember", Refresh_Member_Command'Access);
      AddCommand("RefreshCargo", Refresh_Cargo_Command'Access);
      AddCommand("RefreshBase", Refresh_Base_Command'Access);
      AddCommand("RefreshEvents", Refresh_Events_Command'Access);
      AddCommand("DebugSaveGame", Save_Game_Command'Access);
      AddCommand("DebugMoveShip", Move_Ship_Command'Access);
      AddCommand("DebugUpdateModule", Update_Module_Command'Access);
      AddCommand("DebugAddSkill", Add_Skill_Command'Access);
      AddCommand("DebugUpdateMember", Update_Member_Command'Access);
      AddCommand("DebugAddItem", Add_Item_Command'Access);
      AddCommand("DebugUpdateItem", Update_Item_Command'Access);
      AddCommand("DebugUpdateBase", Update_Base_Command'Access);
      AddCommand("DebugAddShip", Add_Ship_Command'Access);
      AddCommand("ToggleItemEntry", Toggle_Item_Entry_Command'Access);
      AddCommand("DebugAddEvent", Add_Event_Command'Access);
      AddCommand("DebugDeleteEvent", Delete_Event_Command'Access);
      for BaseType of BasesTypes_List loop
         Append(ValuesList, " " & BaseType.Name);
      end loop;
      configure(ComboBox, "-values [list" & To_String(ValuesList) & "]");
      ValuesList := Null_Unbounded_String;
      ComboBox.Name := New_String(".debugdialog.main.bases.owner");
      for Faction of Factions_List loop
         Append(ValuesList, " " & Faction.Name);
      end loop;
      configure(ComboBox, "-values [list" & To_String(ValuesList) & "]");
      Tcl_Eval(Get_Context, "Refresh");
   end ShowDebugUI;

end DebugUI;
