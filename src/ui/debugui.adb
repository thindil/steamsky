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
      FrameName: constant String := ".debugdialog.main.ship";
      ProtoEntry: constant Ttk_Entry :=
        Get_Widget(FrameName & ".proto", Interp);
      ModuleCombo: constant Ttk_ComboBox :=
        Get_Widget(FrameName & ".module", Interp);
      ModuleIndex: Positive;
      SpinBox: Ttk_SpinBox := Get_Widget(FrameName & ".weight", Interp);
   begin
      ModuleIndex := Natural'Value(Current(ModuleCombo)) + 1;
      Delete(ProtoEntry, "0", "end");
      Insert
        (ProtoEntry, "0",
         To_String
           (Modules_List(Player_Ship.Modules(ModuleIndex).Proto_Index).Name));
      Set(SpinBox, Positive'Image(Player_Ship.Modules(ModuleIndex).Weight));
      SpinBox.Name := New_String(FrameName & ".dur");
      Set(SpinBox, Integer'Image(Player_Ship.Modules(ModuleIndex).Durability));
      SpinBox.Name := New_String(FrameName & ".maxdur");
      Set
        (SpinBox,
         Positive'Image(Player_Ship.Modules(ModuleIndex).Max_Durability));
      SpinBox.Name := New_String(FrameName & ".upgrade");
      Set
        (SpinBox,
         Natural'Image(Player_Ship.Modules(ModuleIndex).Upgrade_Progress));
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
      FrameName: constant String := ".debugdialog.main.crew";
      ComboBox: Ttk_ComboBox := Get_Widget(FrameName & ".member", Interp);
      SpinBox: Ttk_SpinBox := Get_Widget(FrameName & ".health", Interp);
      MemberFrame: Ttk_Frame := Get_Widget(FrameName & ".stats", Interp);
      Rows: Natural := 0;
      Tokens: Slice_Set;
      Label: Ttk_Label;
      Member: Member_Data;
      SkillsIndexes: Positive_Container.Vector;
      SkillsList: Unbounded_String;
   begin
      Member := Player_Ship.Crew(Natural'Value(Current(ComboBox)) + 1);
      Set(SpinBox, Positive'Image(Member.Health));
      SpinBox.Name := New_String(FrameName & ".thirst");
      Set(SpinBox, Positive'Image(Member.Thirst));
      SpinBox.Name := New_String(FrameName & ".hunger");
      Set(SpinBox, Positive'Image(Member.Hunger));
      SpinBox.Name := New_String(FrameName & ".tired");
      Set(SpinBox, Positive'Image(Member.Tired));
      SpinBox.Name := New_String(FrameName & ".morale");
      Set(SpinBox, Positive'Image(Member.Morale(1)));
      SpinBox.Name := New_String(FrameName & ".loyalty");
      Set(SpinBox, Positive'Image(Member.Loyalty));
      Create(Tokens, Tcl.Tk.Ada.Grid.Grid_Size(MemberFrame), " ");
      Rows := Natural'Value(Slice(Tokens, 2));
      Delete_Widgets(1, Rows - 1, MemberFrame);
      Show_Stats_Loop :
      for I in Member.Attributes.Iterate loop
         Label :=
           Create
             (MemberFrame & ".label" &
              Trim(Positive'Image(Attributes_Container.To_Index(I)), Left),
              "-text {" &
              To_String
                (Attributes_List(Attributes_Container.To_Index(I)).Name) &
              "}");
         Tcl.Tk.Ada.Grid.Grid(Label);
         SpinBox :=
           Create
             (MemberFrame & ".value" &
              Trim(Positive'Image(Attributes_Container.To_Index(I)), Left),
              "-from 1 -to 50 -validate key -validatecommand {ValidateSpinbox %W %P}");
         Set(SpinBox, Positive'Image(Member.Attributes(I)(1)));
         Tcl.Tk.Ada.Grid.Grid
           (SpinBox,
            "-column 1 -row" &
            Positive'Image(Attributes_Container.To_Index(I)));
      end loop Show_Stats_Loop;
      MemberFrame.Name := New_String(FrameName & ".skills");
      Create(Tokens, Tcl.Tk.Ada.Grid.Grid_Size(MemberFrame), " ");
      Rows := Natural'Value(Slice(Tokens, 2));
      Delete_Widgets(1, Rows - 1, MemberFrame);
      Show_Skills_Loop :
      for I in Member.Skills.Iterate loop
         Label :=
           Create
             (MemberFrame & ".label" &
              Trim(Positive'Image(Skills_Container.To_Index(I)), Left),
              "-text {" & To_String(Skills_List(Member.Skills(I)(1)).Name) &
              "}");
         Tcl.Tk.Ada.Grid.Grid(Label);
         SpinBox :=
           Create
             (MemberFrame & ".value" &
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
      ComboBox.Name := New_String(FrameName & ".addskill.skills");
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
      FrameName: constant String := ".debugdialog.main.cargo";
      CargoCombo: constant Ttk_ComboBox :=
        Get_Widget(FrameName & ".update", Interp);
      ItemIndex: Positive;
      AmountBox: constant Ttk_SpinBox :=
        Get_Widget(FrameName & ".updateamount", Interp);
   begin
      ItemIndex := Natural'Value(Current(CargoCombo)) + 1;
      Set(AmountBox, Positive'Image(Player_Ship.Cargo(ItemIndex).Amount));
      return TCL_OK;
   end Refresh_Cargo_Command;

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
      FrameName: constant String := ".debugdialog.main.world";
      EventsBox: constant Ttk_ComboBox :=
        Get_Widget(FrameName & ".delete", Interp);
      ValuesList: Unbounded_String;
      EventsButton: constant Ttk_Button :=
        Get_Widget(FrameName & ".deleteevent", Interp);
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
                  To_String(Proto_Ships_List(Event.ShipIndex).Name) & "}");
            when AttackOnBase =>
               Append
                 (ValuesList,
                  " {Attack on base: " &
                  To_String(Proto_Ships_List(Event.ShipIndex).Name) & "}");
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
                  To_String(Proto_Ships_List(Event.ShipIndex).Name) & "}");
            when Trader =>
               Append
                 (ValuesList,
                  " {Trader: " &
                  To_String(Proto_Ships_List(Event.ShipIndex).Name) & "}");
            when FriendlyShip =>
               Append
                 (ValuesList,
                  " {Friendly ship: " &
                  To_String(Proto_Ships_List(Event.ShipIndex).Name) & "}");
            when others =>
               null;
         end case;
      end loop Update_Events_Loop;
      configure(EventsBox, "-values [list" & To_String(ValuesList) & "]");
      Current(EventsBox, "0");
      return TCL_OK;
   end Refresh_Events_Command;

   -- ****o* DebugUI/DebugUI.Refresh_Command
   -- FUNCTION
   -- Refresh the whole game information
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
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
      use Interfaces.C;

      FrameName: constant String := ".debugdialog.main";
      SpinBox: Ttk_SpinBox := Get_Widget(FrameName & ".ship.x", Interp);
      ComboBox: Ttk_ComboBox := Get_Widget(FrameName & ".ship.module", Interp);
      ValuesList: Unbounded_String;
   begin
      Set(SpinBox, Positive'Image(Player_Ship.Sky_X));
      SpinBox.Name := New_String(FrameName & ".ship.y");
      Set(SpinBox, Positive'Image(Player_Ship.Sky_Y));
      Update_Modules_Loop :
      for Module of Player_Ship.Modules loop
         Append(ValuesList, " {" & Module.Name & "}");
      end loop Update_Modules_Loop;
      configure(ComboBox, "-values [list" & To_String(ValuesList) & "]");
      Current(ComboBox, "0");
      if Refresh_Module_Command(ClientData, Interp, Argc, Argv) /= TCL_OK then
         return TCL_ERROR;
      end if;
      ComboBox.Name := New_String(FrameName & ".crew.member");
      ValuesList := Null_Unbounded_String;
      Update_Members_Loop :
      for Member of Player_Ship.Crew loop
         Append(ValuesList, " {" & Member.Name & "}");
      end loop Update_Members_Loop;
      configure(ComboBox, "-values [list" & To_String(ValuesList) & "]");
      Current(ComboBox, "0");
      if Refresh_Member_Command(ClientData, Interp, Argc, Argv) /= TCL_OK then
         return TCL_ERROR;
      end if;
      ComboBox.Name := New_String(FrameName & ".cargo.update");
      ValuesList := Null_Unbounded_String;
      Update_Cargo_Loop :
      for Item of Player_Ship.Cargo loop
         Append(ValuesList, " {" & GetItemName(Item, False, False) & "}");
      end loop Update_Cargo_Loop;
      configure(ComboBox, "-values [list" & To_String(ValuesList) & "]");
      Current(ComboBox, "0");
      if Refresh_Cargo_Command(ClientData, Interp, Argc, Argv) /= TCL_OK then
         return TCL_ERROR;
      end if;
      if Refresh_Events_Command(ClientData, Interp, Argc, Argv) /= TCL_OK then
         return TCL_ERROR;
      end if;
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
      FrameName: constant String := ".debugdialog.main.bases";
      NameEntry: constant Ttk_Entry := Get_Widget(FrameName & ".name", Interp);
      BaseIndex: Natural := 0;
      BaseName: constant Unbounded_String :=
        To_Unbounded_String(Get(NameEntry));
      ComboBox: Ttk_ComboBox := Get_Widget(FrameName & ".type", Interp);
      SpinBox: Ttk_SpinBox := Get_Widget(FrameName & ".population", Interp);
   begin
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
      ComboBox.Name := New_String(FrameName & ".owner");
      Set(ComboBox, To_String(Factions_List(SkyBases(BaseIndex).Owner).Name));
      ComboBox.Name := New_String(FrameName & ".size");
      Current
        (ComboBox, Natural'Image(Bases_Size'Pos(SkyBases(BaseIndex).Size)));
      Set(SpinBox, Natural'Image(SkyBases(BaseIndex).Population));
      SpinBox.Name := New_String(FrameName & ".reputation");
      Set(SpinBox, Integer'Image(SkyBases(BaseIndex).Reputation(1)));
      SpinBox.Name := New_String(FrameName & ".money");
      if SkyBases(BaseIndex).Cargo.Length > 0 then
         Set(SpinBox, Natural'Image(SkyBases(BaseIndex).Cargo(1).Amount));
      else
         Set(SpinBox, "0");
      end if;
      return TCL_OK;
   end Refresh_Base_Command;

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
      Save_Game(True);
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
      FrameName: constant String := ".debugdialog.main.ship";
      SpinBox: Ttk_SpinBox := Get_Widget(FrameName & ".x", Interp);
   begin
      Player_Ship.Sky_X := Positive'Value(Get(SpinBox));
      SpinBox.Name := New_String(FrameName & ".y");
      Player_Ship.Sky_Y := Positive'Value(Get(SpinBox));
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
      FrameName: constant String := ".debugdialog.main.ship";
      ModuleBox: constant Ttk_ComboBox :=
        Get_Widget(FrameName & ".module", Interp);
      ModuleIndex: constant Positive := Natural'Value(Current(ModuleBox)) + 1;
      ModuleEntry: constant Ttk_Entry :=
        Get_Widget(FrameName & ".proto", Interp);
      Value: Unbounded_String := To_Unbounded_String(Get(ModuleEntry));
      SpinBox: Ttk_SpinBox := Get_Widget(FrameName & ".weight", Interp);
   begin
      Update_Proto_Index_Loop :
      for I in Modules_List.Iterate loop
         if Modules_List(I).Name = Value then
            Value := Null_Unbounded_String;
            Player_Ship.Modules(ModuleIndex).Proto_Index :=
              BaseModules_Container.Key(I);
            exit Update_Proto_Index_Loop;
         end if;
      end loop Update_Proto_Index_Loop;
      Player_Ship.Modules(ModuleIndex).Weight := Natural'Value(Get(SpinBox));
      SpinBox.Name := New_String(FrameName & ".dur");
      Player_Ship.Modules(ModuleIndex).Durability :=
        Natural'Value(Get(SpinBox));
      SpinBox.Name := New_String(FrameName & ".maxdur");
      Player_Ship.Modules(ModuleIndex).Max_Durability :=
        Natural'Value(Get(SpinBox));
      SpinBox.Name := New_String(FrameName & ".upgrade");
      Player_Ship.Modules(ModuleIndex).Upgrade_Progress :=
        Natural'Value(Get(SpinBox));
      return TCL_OK;
   end Update_Module_Command;

   -- ****o* DebugUI/DebugUI.Add_Skill_Command
   -- FUNCTION
   -- Add a new skill to the selected crew member
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
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
      FrameName: constant String := ".debugdialog.main.crew";
      ComboBox: Ttk_ComboBox := Get_Widget(FrameName & ".member", Interp);
      MemberIndex: constant Positive := Natural'Value(Current(ComboBox)) + 1;
      SkillName: Unbounded_String;
   begin
      ComboBox.Name := New_String(FrameName & ".addskill.skills");
      SkillName := To_Unbounded_String(Get(ComboBox));
      Add_Skill_Loop :
      for I in Skills_List.Iterate loop
         if Skills_List(I).Name = SkillName then
            Player_Ship.Crew(MemberIndex).Skills.Append
              ((SkillsData_Container.To_Index(I), 1, 0));
            return Refresh_Member_Command(ClientData, Interp, Argc, Argv);
         end if;
      end loop Add_Skill_Loop;
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
      FrameName: constant String := ".debugdialog.main.crew";
      ComboBox: constant Ttk_ComboBox :=
        Get_Widget(FrameName & ".member", Interp);
      MemberIndex: Positive;
      SpinBox: Ttk_SpinBox := Get_Widget(FrameName & ".health", Interp);
   begin
      MemberIndex := Natural'Value(Current(ComboBox)) + 1;
      Player_Ship.Crew(MemberIndex).Health := Skill_Range'Value(Get(SpinBox));
      SpinBox.Name := New_String(FrameName & ".thirst");
      Player_Ship.Crew(MemberIndex).Thirst := Skill_Range'Value(Get(SpinBox));
      SpinBox.Name := New_String(FrameName & ".hunger");
      Player_Ship.Crew(MemberIndex).Hunger := Skill_Range'Value(Get(SpinBox));
      SpinBox.Name := New_String(FrameName & ".tired");
      Player_Ship.Crew(MemberIndex).Tired := Skill_Range'Value(Get(SpinBox));
      SpinBox.Name := New_String(FrameName & ".morale");
      Player_Ship.Crew(MemberIndex).Morale(1) :=
        Skill_Range'Value(Get(SpinBox));
      SpinBox.Name := New_String(FrameName & ".loyalty");
      Player_Ship.Crew(MemberIndex).Loyalty := Skill_Range'Value(Get(SpinBox));
      Update_Stats_Loop :
      for I in Player_Ship.Crew(MemberIndex).Attributes.Iterate loop
         SpinBox.Name :=
           New_String
             (FrameName & ".stats.value" &
              Trim(Positive'Image(Attributes_Container.To_Index(I)), Left));
         Player_Ship.Crew(MemberIndex).Attributes(I)(1) :=
           Positive'Value(Get(SpinBox));
      end loop Update_Stats_Loop;
      Update_Skills_Loop :
      for I in Player_Ship.Crew(MemberIndex).Skills.Iterate loop
         SpinBox.Name :=
           New_String
             (FrameName & ".skills.value" &
              Trim(Positive'Image(Skills_Container.To_Index(I)), Left));
         Player_Ship.Crew(MemberIndex).Skills(I)(2) :=
           Positive'Value(Get(SpinBox));
      end loop Update_Skills_Loop;
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
      FrameName: constant String := ".debugdialog.main.cargo";
      ItemEntry: constant Ttk_Entry := Get_Widget(FrameName & ".add", Interp);
      ItemBox: constant Ttk_SpinBox :=
        Get_Widget(FrameName & ".amount", Interp);
      ItemIndex, ItemName: Unbounded_String;
   begin
      ItemName := To_Unbounded_String(Get(ItemEntry));
      Find_Index_Loop :
      for I in Items_List.Iterate loop
         if Items_List(I).Name = ItemName then
            ItemIndex := Objects_Container.Key(I);
            exit Find_Index_Loop;
         end if;
      end loop Find_Index_Loop;
      if ItemIndex = Null_Unbounded_String then
         return TCL_OK;
      end if;
      UpdateCargo(Player_Ship, ItemIndex, Positive'Value(Get(ItemBox)));
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
      FrameName: constant String := ".debugdialog.main.cargo";
      ItemCombo: constant Ttk_ComboBox :=
        Get_Widget(FrameName & ".update", Interp);
      ItemBox: constant Ttk_SpinBox :=
        Get_Widget(FrameName & ".updateamount", Interp);
      ItemIndex: Positive;
   begin
      ItemIndex := Natural'Value(Current(ItemCombo)) + 1;
      UpdateCargo
        (Ship => Player_Ship, Amount => Positive'Value(Get(ItemBox)),
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
      FrameName: constant String := ".debugdialog.main.bases";
      BaseIndex: Natural := 0;
      BaseEntry: constant Ttk_Entry := Get_Widget(FrameName & ".name", Interp);
      BaseName: Unbounded_String;
      BaseCombo: Ttk_ComboBox := Get_Widget(FrameName & ".type", Interp);
      BaseBox: Ttk_SpinBox := Get_Widget(FrameName & ".population", Interp);
   begin
      BaseName := To_Unbounded_String(Get(BaseEntry));
      Find_Index_Loop :
      for I in SkyBases'Range loop
         if SkyBases(I).Name = BaseName then
            BaseIndex := I;
            exit Find_Index_Loop;
         end if;
      end loop Find_Index_Loop;
      if BaseIndex = 0 then
         return TCL_OK;
      end if;
      Update_Base_Type_Loop :
      for I in BasesTypes_List.Iterate loop
         if BasesTypes_List(I).Name = To_Unbounded_String(Get(BaseCombo)) then
            SkyBases(BaseIndex).BaseType := BasesTypes_Container.Key(I);
            exit Update_Base_Type_Loop;
         end if;
      end loop Update_Base_Type_Loop;
      BaseCombo.Name := New_String(FrameName & ".owner");
      Update_Base_Owner_Loop :
      for I in Factions_List.Iterate loop
         if Factions_List(I).Name = To_Unbounded_String(Get(BaseCombo)) then
            SkyBases(BaseIndex).Owner := Factions_Container.Key(I);
            exit Update_Base_Owner_Loop;
         end if;
      end loop Update_Base_Owner_Loop;
      BaseCombo.Name := New_String(FrameName & ".size");
      SkyBases(BaseIndex).Size := Bases_Size'Value(Get(BaseCombo));
      SkyBases(BaseIndex).Population := Natural'Value(Get(BaseBox));
      BaseBox.Name := New_String(FrameName & ".reputation");
      SkyBases(BaseIndex).Reputation(1) := Integer'Value(Get(BaseBox));
      BaseBox.Name := New_String(FrameName & ".money");
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
      FrameName: constant String := ".debugdialog.main.world";
      ShipEntry: constant Ttk_Entry := Get_Widget(FrameName & ".ship", Interp);
      ShipName: Unbounded_String;
      NpcShipX, NpcShipY, Duration: Positive;
      ShipBox: Ttk_SpinBox := Get_Widget(FrameName & ".x", Interp);
   begin
      ShipName := To_Unbounded_String(Get(ShipEntry));
      NpcShipX := Positive'Value(Get(ShipBox));
      ShipBox.Name := New_String(FrameName & ".y");
      NpcShipY := Positive'Value(Get(ShipBox));
      ShipBox.Name := New_String(FrameName & ".duration");
      Duration := Positive'Value(Get(ShipBox));
      Add_Ship_Event_Loop :
      for I in Proto_Ships_List.Iterate loop
         if Proto_Ships_List(I).Name = ShipName then
            if Traders.Contains(Proto_Ships_Container.Key(I)) then
               Events_List.Append
                 (New_Item =>
                    (Trader, NpcShipX, NpcShipY, Duration,
                     Proto_Ships_Container.Key(I)));
            elsif FriendlyShips.Contains(Proto_Ships_Container.Key(I)) then
               Events_List.Append
                 (New_Item =>
                    (FriendlyShip, NpcShipX, NpcShipY, Duration,
                     Proto_Ships_Container.Key(I)));
            else
               Events_List.Append
                 (New_Item =>
                    (EnemyShip, NpcShipX, NpcShipY, Duration,
                     Proto_Ships_Container.Key(I)));
            end if;
            SkyMap(NpcShipX, NpcShipY).EventIndex := Events_List.Last_Index;
            return Refresh_Events_Command(ClientData, Interp, Argc, Argv);
         end if;
      end loop Add_Ship_Event_Loop;
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
      FrameName: constant String := ".debugdialog.main.world";
      EventCombo: constant Ttk_ComboBox :=
        Get_Widget(FrameName & ".event", Interp);
      ItemEntry: constant Ttk_Entry := Get_Widget(FrameName & ".item", Interp);
      ItemLabel: constant Ttk_Label :=
        Get_Widget(FrameName & ".itemlbl", Interp);
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
      FrameName: constant String := ".debugdialog.main.world";
      EventEntry: constant Ttk_Entry :=
        Get_Widget(FrameName & ".base", Interp);
      EventName: Unbounded_String;
      BaseIndex, EventType: Natural := 0;
      EventBox: Ttk_ComboBox := Get_Widget(FrameName & ".event", Interp);
      DurationBox: constant Ttk_SpinBox :=
        Get_Widget(FrameName & ".baseduration", Interp);
      Added: Boolean := True;
   begin
      EventName := To_Unbounded_String(Get(EventEntry));
      Find_Base_Index_Loop :
      for I in SkyBases'Range loop
         if SkyBases(I).Name = EventName then
            BaseIndex := I;
            exit Find_Base_Index_Loop;
         end if;
      end loop Find_Base_Index_Loop;
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
            EventBox.Name := New_String(FrameName & ".item");
            EventName := To_Unbounded_String(Get(EventBox));
            Added := False;
            Find_Item_Loop :
            for I in Items_List.Iterate loop
               if Items_List(I).Name = EventName then
                  Events_List.Append
                    (New_Item =>
                       (DoublePrice, SkyBases(BaseIndex).SkyX,
                        SkyBases(BaseIndex).SkyY,
                        Positive'Value(Get(DurationBox)),
                        Objects_Container.Key(I)));
                  Added := True;
                  exit Find_Item_Loop;
               end if;
            end loop Find_Item_Loop;
         when 2 =>
            Events_List.Append
              (New_Item =>
                 (Disease, SkyBases(BaseIndex).SkyX, SkyBases(BaseIndex).SkyY,
                  Positive'Value(Get(DurationBox)), 1));
         when others =>
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
      FrameName: constant String := ".debugdialog.main.bases";
      ComboBox: Ttk_ComboBox := Get_Widget(FrameName & ".type");
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
      Load_Bases_Types_Loop :
      for BaseType of BasesTypes_List loop
         Append(ValuesList, " " & BaseType.Name);
      end loop Load_Bases_Types_Loop;
      configure(ComboBox, "-values [list" & To_String(ValuesList) & "]");
      ValuesList := Null_Unbounded_String;
      ComboBox.Name := New_String(FrameName & ".owner");
      Load_Factions_Loop :
      for Faction of Factions_List loop
         Append(ValuesList, " " & Faction.Name);
      end loop Load_Factions_Loop;
      configure(ComboBox, "-values [list" & To_String(ValuesList) & "]");
      Tcl_Eval(Get_Context, "Refresh");
   end ShowDebugUI;

end DebugUI;
