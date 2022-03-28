-- Copyright (c) 2020-2022 Bartek thindil Jasicki <thindil@laeran.pl>
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
      ProtoCombo: constant Ttk_ComboBox :=
        Get_Widget(FrameName & ".proto", Interp);
      ModuleCombo: constant Ttk_ComboBox :=
        Get_Widget(FrameName & ".module", Interp);
      ModuleIndex: Positive;
      SpinBox: Ttk_SpinBox := Get_Widget(FrameName & ".weight", Interp);
   begin
      ModuleIndex := Natural'Value(Current(ModuleCombo)) + 1;
      Set
        (ProtoCombo,
         "{" &
         To_String
           (Modules_List(Player_Ship.Modules(ModuleIndex).Proto_Index).Name) &
         "}");
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
      use Tiny_String;

      FrameName: constant String := ".debugdialog.main.crew";
      ComboBox: Ttk_ComboBox := Get_Widget(FrameName & ".member", Interp);
      SpinBox: Ttk_SpinBox := Get_Widget(FrameName & ".stats2.health", Interp);
      MemberFrame: Ttk_Frame := Get_Widget(FrameName & ".stats", Interp);
      Rows: Natural := 0;
      Tokens: Slice_Set;
      Label: Ttk_Label;
      Member: Member_Data
        (Amount_Of_Attributes => Attributes_Amount,
         Amount_Of_Skills => Skills_Amount);
      SkillsIndexes: Positive_Container.Vector;
      SkillsList: Unbounded_String;
   begin
      Member := Player_Ship.Crew(Natural'Value(Current(ComboBox)) + 1);
      Set(SpinBox, Positive'Image(Member.Health));
      SpinBox.Name := New_String(FrameName & ".stats2.thirst");
      Set(SpinBox, Positive'Image(Member.Thirst));
      SpinBox.Name := New_String(FrameName & ".stats2.hunger");
      Set(SpinBox, Positive'Image(Member.Hunger));
      SpinBox.Name := New_String(FrameName & ".stats2.tired");
      Set(SpinBox, Positive'Image(Member.Tired));
      SpinBox.Name := New_String(FrameName & ".stats2.morale");
      Set(SpinBox, Positive'Image(Member.Morale(1)));
      SpinBox.Name := New_String(FrameName & ".stats2.loyalty");
      Set(SpinBox, Positive'Image(Member.Loyalty));
      Create(Tokens, Tcl.Tk.Ada.Grid.Grid_Size(MemberFrame), " ");
      Rows := Natural'Value(Slice(Tokens, 2));
      Delete_Widgets(1, Rows - 1, MemberFrame);
      Show_Stats_Loop :
      for I in Member.Attributes'Range loop
         Label :=
           Create
             (MemberFrame & ".label" & Trim(Positive'Image(I), Left),
              "-text {" &
              To_String
                (AttributesData_Container.Element(Attributes_List, I).Name) &
              "}");
         Tcl.Tk.Ada.Grid.Grid(Label);
         SpinBox :=
           Create
             (MemberFrame & ".value" & Trim(Positive'Image(I), Left),
              "-from 1 -to 50 -validate key -validatecommand {ValidateSpinbox %W %P} -width 5");
         Set(SpinBox, Positive'Image(Member.Attributes(I).Level));
         Tcl.Tk.Ada.Grid.Grid(SpinBox, "-column 1 -row" & Positive'Image(I));
      end loop Show_Stats_Loop;
      MemberFrame.Name := New_String(FrameName & ".skills");
      Create(Tokens, Tcl.Tk.Ada.Grid.Grid_Size(MemberFrame), " ");
      Rows := Natural'Value(Slice(Tokens, 2));
      Delete_Widgets(1, Rows - 1, MemberFrame);
      Show_Skills_Loop :
      for I in
        Skills_Container.First_Index(Container => Member.Skills) ..
          Skills_Container.Last_Index(Container => Member.Skills) loop
         Label :=
           Create
             (MemberFrame & ".label" &
              Trim(Skills_Amount_Range'Image(I), Left),
              "-text {" &
              To_String
                (SkillsData_Container.Element
                   (Skills_List,
                    Skills_Container.Element
                      (Container => Member.Skills, Index => I)
                      .Index)
                   .Name) &
              "}");
         Tcl.Tk.Ada.Grid.Grid(Label);
         SpinBox :=
           Create
             (MemberFrame & ".value" &
              Trim(Skills_Amount_Range'Image(I), Left),
              "-from 1 -to 100 -validate key -validatecommand {ValidateSpinbox %W %P} -width 5");
         Set
           (SpinBox,
            Positive'Image
              (Skills_Container.Element(Container => Member.Skills, Index => I)
                 .Level));
         Tcl.Tk.Ada.Grid.Grid
           (SpinBox, "-column 1 -row" & Skills_Amount_Range'Image(I));
         SkillsIndexes.Append
           (Natural
              (Skills_Container.Element(Container => Member.Skills, Index => I)
                 .Index));
      end loop Show_Skills_Loop;
      Show_Add_Skills_Loop :
      for I in 1 .. Skills_Amount loop
         if not SkillsIndexes.Contains(Natural(I)) then
            Append
              (SkillsList,
               " " &
               To_String(SkillsData_Container.Element(Skills_List, I).Name));
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
      Set
        (AmountBox,
         Positive'Image
           (Inventory_Container.Element
              (Container => Player_Ship.Cargo, Index => ItemIndex)
              .Amount));
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
      use Tiny_String;

      FrameName: constant String := ".debugdialog.main.world.deleteevent";
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
         case Event.E_Type is
            when ENEMYSHIP =>
               Append
                 (ValuesList,
                  " {Enemy ship: " &
                  To_String(Proto_Ships_List(Event.Ship_Index).Name) & "}");
            when ATTACKONBASE =>
               Append
                 (ValuesList,
                  " {Attack on base: " &
                  To_String(Proto_Ships_List(Event.Ship_Index).Name) & "}");
            when DISEASE =>
               Append
                 (ValuesList,
                  " {Disease in base: " &
                  To_String
                    (Sky_Bases(Sky_Map(Event.Sky_X, Event.Sky_Y).Base_Index)
                       .Name) &
                  "}");
            when DOUBLEPRICE =>
               Append
                 (ValuesList,
                  " {Double price in base: " &
                  To_String
                    (Sky_Bases(Sky_Map(Event.Sky_X, Event.Sky_Y).Base_Index)
                       .Name) &
                  "}");
            when FULLDOCKS =>
               Append
                 (ValuesList,
                  " {Full docks in base: " &
                  To_String
                    (Sky_Bases(Sky_Map(Event.Sky_X, Event.Sky_Y).Base_Index)
                       .Name) &
                  "}");
            when ENEMYPATROL =>
               Append
                 (ValuesList,
                  " {Enemy patrol: " &
                  To_String(Proto_Ships_List(Event.Ship_Index).Name) & "}");
            when TRADER =>
               Append
                 (ValuesList,
                  " {Trader: " &
                  To_String(Proto_Ships_List(Event.Ship_Index).Name) & "}");
            when FRIENDLYSHIP =>
               Append
                 (ValuesList,
                  " {Friendly ship: " &
                  To_String(Proto_Ships_List(Event.Ship_Index).Name) & "}");
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
      use Tiny_String;

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
         Append(ValuesList, " {" & To_String(Source => Member.Name) & "}");
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
         Append(ValuesList, " {" & Get_Item_Name(Item, False, False) & "}");
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
      use Tiny_String;

      FrameName: constant String := ".debugdialog.main.bases";
      NameEntry: constant Ttk_Entry := Get_Widget(FrameName & ".name", Interp);
      BaseIndex: Natural := 0;
      BaseName: constant Bounded_String := To_Bounded_String(Get(NameEntry));
      ComboBox: Ttk_ComboBox := Get_Widget(FrameName & ".type", Interp);
      SpinBox: Ttk_SpinBox := Get_Widget(FrameName & ".population", Interp);
   begin
      Find_Base_Index_Loop :
      for I in Sky_Bases'Range loop
         if Sky_Bases(I).Name = BaseName then
            BaseIndex := I;
            exit Find_Base_Index_Loop;
         end if;
      end loop Find_Base_Index_Loop;
      if BaseIndex = 0 then
         return TCL_OK;
      end if;
      Set
        (ComboBox,
         To_String(Bases_Types_List(Sky_Bases(BaseIndex).Base_Type).Name));
      ComboBox.Name := New_String(FrameName & ".owner");
      Set(ComboBox, To_String(Factions_List(Sky_Bases(BaseIndex).Owner).Name));
      ComboBox.Name := New_String(FrameName & ".size");
      Current
        (ComboBox, Natural'Image(Bases_Size'Pos(Sky_Bases(BaseIndex).Size)));
      Set(SpinBox, Natural'Image(Sky_Bases(BaseIndex).Population));
      SpinBox.Name := New_String(FrameName & ".reputation");
      Set(SpinBox, Integer'Image(Sky_Bases(BaseIndex).Reputation.Level));
      SpinBox.Name := New_String(FrameName & ".money");
      if BaseCargo_Container.Length(Container => Sky_Bases(BaseIndex).Cargo) >
        0 then
         Set
           (SpinBox,
            Natural'Image
              (BaseCargo_Container.Element
                 (Container => Sky_Bases(BaseIndex).Cargo, Index => 1)
                 .Amount));
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
      Show_Sky_Map(True);
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
      ProtoCombo: constant Ttk_ComboBox :=
        Get_Widget(FrameName & ".proto", Interp);
      Value: Unbounded_String := To_Unbounded_String(Get(ProtoCombo));
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
      use Tiny_String;

      FrameName: constant String := ".debugdialog.main.crew";
      ComboBox: Ttk_ComboBox := Get_Widget(FrameName & ".member", Interp);
      MemberIndex: constant Positive := Natural'Value(Current(ComboBox)) + 1;
      SkillName: Unbounded_String;
   begin
      ComboBox.Name := New_String(FrameName & ".addskill.skills");
      SkillName := To_Unbounded_String(Get(ComboBox));
      Add_Skill_Loop :
      for I in 1 .. Skills_Amount loop
         if To_Unbounded_String
             (To_String(SkillsData_Container.Element(Skills_List, I).Name)) =
           SkillName then
            Skills_Container.Append
              (Container => Player_Ship.Crew(MemberIndex).Skills,
               New_Item => (I, 1, 0));
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
      SpinBox: Ttk_SpinBox := Get_Widget(FrameName & ".stats2.health", Interp);
      Local_Attribute: Mob_Attribute_Record;
   begin
      MemberIndex := Natural'Value(Current(ComboBox)) + 1;
      Player_Ship.Crew(MemberIndex).Health := Skill_Range'Value(Get(SpinBox));
      SpinBox.Name := New_String(FrameName & ".stats2.thirst");
      Player_Ship.Crew(MemberIndex).Thirst := Skill_Range'Value(Get(SpinBox));
      SpinBox.Name := New_String(FrameName & ".stats2.hunger");
      Player_Ship.Crew(MemberIndex).Hunger := Skill_Range'Value(Get(SpinBox));
      SpinBox.Name := New_String(FrameName & ".stats2.tired");
      Player_Ship.Crew(MemberIndex).Tired := Skill_Range'Value(Get(SpinBox));
      SpinBox.Name := New_String(FrameName & ".stats2.morale");
      Player_Ship.Crew(MemberIndex).Morale(1) :=
        Skill_Range'Value(Get(SpinBox));
      SpinBox.Name := New_String(FrameName & ".stats2.loyalty");
      Player_Ship.Crew(MemberIndex).Loyalty := Skill_Range'Value(Get(SpinBox));
      Update_Stats_Loop :
      for I in Player_Ship.Crew(MemberIndex).Attributes'Range loop
         SpinBox.Name :=
           New_String
             (FrameName & ".stats.value" & Trim(Positive'Image(I), Left));
         Local_Attribute :=
           (Positive'Value(Get(SpinBox)),
            Player_Ship.Crew(MemberIndex).Attributes(I).Experience);
         Player_Ship.Crew(MemberIndex).Attributes(I) := Local_Attribute;
      end loop Update_Stats_Loop;
      Update_Skills_Loop :
      for I in
        Skills_Container.First_Index
          (Container => Player_Ship.Crew(MemberIndex).Skills) ..
          Skills_Container.Last_Index
            (Container => Player_Ship.Crew(MemberIndex).Skills) loop
         SpinBox.Name :=
           New_String
             (FrameName & ".skills.value" &
              Trim(Skills_Amount_Range'Image(I), Left));
         Update_Skill_Block :
         declare
            New_Skill: Skill_Info :=
              Skills_Container.Element
                (Container => Player_Ship.Crew(MemberIndex).Skills,
                 Index => I);
         begin
            New_Skill.Level := Positive'Value(Get(SpinBox));
            Skills_Container.Replace_Element
              (Container => Player_Ship.Crew(MemberIndex).Skills, Index => I,
               New_Item => New_Skill);
         end Update_Skill_Block;
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
      use Tiny_String;

      FrameName: constant String := ".debugdialog.main.cargo";
      ItemEntry: constant Ttk_Entry := Get_Widget(FrameName & ".add", Interp);
      ItemBox: constant Ttk_SpinBox :=
        Get_Widget(FrameName & ".amount", Interp);
      ItemName: Unbounded_String;
      ItemIndex: Bounded_String;
   begin
      ItemName := To_Unbounded_String(Get(ItemEntry));
      Find_Index_Loop :
      for I in Items_List.Iterate loop
         if Items_List(I).Name = ItemName then
            ItemIndex := Objects_Container.Key(I);
            exit Find_Index_Loop;
         end if;
      end loop Find_Index_Loop;
      if ItemIndex = Null_Bounded_String then
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
      use Tiny_String;

      FrameName: constant String := ".debugdialog.main.bases";
      BaseIndex: Natural := 0;
      BaseEntry: constant Ttk_Entry := Get_Widget(FrameName & ".name", Interp);
      BaseName: Bounded_String;
      BaseCombo: Ttk_ComboBox := Get_Widget(FrameName & ".type", Interp);
      BaseBox: Ttk_SpinBox := Get_Widget(FrameName & ".population", Interp);
      Item: Base_Cargo;
   begin
      BaseName := To_Bounded_String(Get(BaseEntry));
      Find_Index_Loop :
      for I in Sky_Bases'Range loop
         if Sky_Bases(I).Name = BaseName then
            BaseIndex := I;
            exit Find_Index_Loop;
         end if;
      end loop Find_Index_Loop;
      if BaseIndex = 0 then
         return TCL_OK;
      end if;
      Update_Base_Type_Loop :
      for I in Bases_Types_List.Iterate loop
         if Bases_Types_List(I).Name = To_Unbounded_String(Get(BaseCombo)) then
            Sky_Bases(BaseIndex).Base_Type := BasesTypes_Container.Key(I);
            exit Update_Base_Type_Loop;
         end if;
      end loop Update_Base_Type_Loop;
      BaseCombo.Name := New_String(FrameName & ".owner");
      Update_Base_Owner_Loop :
      for I in Factions_List.Iterate loop
         if Factions_List(I).Name = To_Bounded_String(Get(BaseCombo)) then
            Sky_Bases(BaseIndex).Owner := Factions_Container.Key(I);
            exit Update_Base_Owner_Loop;
         end if;
      end loop Update_Base_Owner_Loop;
      BaseCombo.Name := New_String(FrameName & ".size");
      Sky_Bases(BaseIndex).Size := Bases_Size'Value(Get(BaseCombo));
      Sky_Bases(BaseIndex).Population := Natural'Value(Get(BaseBox));
      BaseBox.Name := New_String(FrameName & ".reputation");
      Sky_Bases(BaseIndex).Reputation.Level := Integer'Value(Get(BaseBox));
      BaseBox.Name := New_String(FrameName & ".money");
      Item :=
        BaseCargo_Container.Element
          (Container => Sky_Bases(BaseIndex).Cargo, Index => 1);
      Item.Amount := Natural'Value(Get(BaseBox));
      BaseCargo_Container.Replace_Element
        (Container => Sky_Bases(BaseIndex).Cargo, Index => 1,
         New_Item => Item);
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
                    (TRADER, NpcShipX, NpcShipY, Duration,
                     Proto_Ships_Container.Key(I)));
            elsif Friendly_Ships.Contains(Proto_Ships_Container.Key(I)) then
               Events_List.Append
                 (New_Item =>
                    (FRIENDLYSHIP, NpcShipX, NpcShipY, Duration,
                     Proto_Ships_Container.Key(I)));
            else
               Events_List.Append
                 (New_Item =>
                    (ENEMYSHIP, NpcShipX, NpcShipY, Duration,
                     Proto_Ships_Container.Key(I)));
            end if;
            Sky_Map(NpcShipX, NpcShipY).Event_Index := Events_List.Last_Index;
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
      use Tiny_String;
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
      for I in Sky_Bases'Range loop
         if To_String(Sky_Bases(I).Name) = To_String(EventName) then
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
                 (DISEASE, Sky_Bases(BaseIndex).Sky_X,
                  Sky_Bases(BaseIndex).Sky_Y, Positive'Value(Get(DurationBox)),
                  1));
         when 1 =>
            EventBox.Name := New_String(FrameName & ".item");
            EventName := To_Unbounded_String(Get(EventBox));
            Added := False;
            Find_Item_Loop :
            for I in Items_List.Iterate loop
               if Items_List(I).Name = EventName then
                  Events_List.Append
                    (New_Item =>
                       (DOUBLEPRICE, Sky_Bases(BaseIndex).Sky_X,
                        Sky_Bases(BaseIndex).Sky_Y,
                        Positive'Value(Get(DurationBox)),
                        Objects_Container.Key(I)));
                  Added := True;
                  exit Find_Item_Loop;
               end if;
            end loop Find_Item_Loop;
         when 2 =>
            Events_List.Append
              (New_Item =>
                 (DISEASE, Sky_Bases(BaseIndex).Sky_X,
                  Sky_Bases(BaseIndex).Sky_Y, Positive'Value(Get(DurationBox)),
                  1));
         when others =>
            null;
      end case;
      if not Added then
         return TCL_OK;
      end if;
      Sky_Map(Sky_Bases(BaseIndex).Sky_X, Sky_Bases(BaseIndex).Sky_Y)
        .Event_Index :=
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
        Get_Widget(".debugdialog.main.world.deleteevent.delete", Interp);
   begin
      Delete_Event(Natural'Value(Current(EventBox)) + 1);
      return Refresh_Events_Command(ClientData, Interp, Argc, Argv);
   end Delete_Event_Command;

   procedure ShowDebugUI is
      use Tiny_String;

      FrameName: constant String := ".debugdialog.main.bases";
      ComboBox: Ttk_ComboBox := Get_Widget(FrameName & ".type");
      ValuesList: Unbounded_String;
   begin
      Tcl_EvalFile
        (Get_Context,
         To_String(Data_Directory) & "ui" & Dir_Separator & "debug.tcl");
      Add_Command("Refresh", Refresh_Command'Access);
      Add_Command("RefreshModule", Refresh_Module_Command'Access);
      Add_Command("RefreshMember", Refresh_Member_Command'Access);
      Add_Command("RefreshCargo", Refresh_Cargo_Command'Access);
      Add_Command("RefreshBase", Refresh_Base_Command'Access);
      Add_Command("RefreshEvents", Refresh_Events_Command'Access);
      Add_Command("DebugSaveGame", Save_Game_Command'Access);
      Add_Command("DebugMoveShip", Move_Ship_Command'Access);
      Add_Command("DebugUpdateModule", Update_Module_Command'Access);
      Add_Command("DebugAddSkill", Add_Skill_Command'Access);
      Add_Command("DebugUpdateMember", Update_Member_Command'Access);
      Add_Command("DebugAddItem", Add_Item_Command'Access);
      Add_Command("DebugUpdateItem", Update_Item_Command'Access);
      Add_Command("DebugUpdateBase", Update_Base_Command'Access);
      Add_Command("DebugAddShip", Add_Ship_Command'Access);
      Add_Command("ToggleItemEntry", Toggle_Item_Entry_Command'Access);
      Add_Command("DebugAddEvent", Add_Event_Command'Access);
      Add_Command("DebugDeleteEvent", Delete_Event_Command'Access);
      Load_Bases_Types_Loop :
      for BaseType of Bases_Types_List loop
         Append(ValuesList, " {" & BaseType.Name & "}");
      end loop Load_Bases_Types_Loop;
      configure(ComboBox, "-values [list" & To_String(ValuesList) & "]");
      ValuesList := Null_Unbounded_String;
      ComboBox.Name := New_String(FrameName & ".owner");
      Load_Factions_Loop :
      for Faction of Factions_List loop
         Append(ValuesList, " {" & To_String(Source => Faction.Name) & "}");
      end loop Load_Factions_Loop;
      configure(ComboBox, "-values [list" & To_String(ValuesList) & "]");
      ValuesList := Null_Unbounded_String;
      ComboBox.Name := New_String(FrameName & ".name");
      Load_Bases_Loop :
      for Base of Sky_Bases loop
         Append(ValuesList, " {" & To_String(Source => Base.Name) & "}");
      end loop Load_Bases_Loop;
      configure(ComboBox, "-values [list" & To_String(ValuesList) & "]");
      ComboBox.Name := New_String(".debugdialog.main.world.base");
      configure(ComboBox, "-values [list" & To_String(ValuesList) & "]");
      ValuesList := Null_Unbounded_String;
      ComboBox.Name := New_String(".debugdialog.main.ship.proto");
      Load_Modules_Prototypes_Loop :
      for Module of Modules_List loop
         Append(ValuesList, " {" & Module.Name & "}");
      end loop Load_Modules_Prototypes_Loop;
      configure(ComboBox, "-values [list" & To_String(ValuesList) & "]");
      ValuesList := Null_Unbounded_String;
      ComboBox.Name := New_String(".debugdialog.main.cargo.add");
      Load_Items_Loop :
      for Item of Items_List loop
         Append(ValuesList, " {" & Item.Name & "}");
      end loop Load_Items_Loop;
      configure(ComboBox, "-values [list" & To_String(ValuesList) & "]");
      ComboBox.Name := New_String(".debugdialog.main.world.item");
      configure(ComboBox, "-values [list" & To_String(ValuesList) & "]");
      ValuesList := Null_Unbounded_String;
      ComboBox.Name := New_String(".debugdialog.main.world.ship");
      Load_Ships_Loop :
      for Ship of Proto_Ships_List loop
         Append(ValuesList, " {" & Ship.Name & "}");
      end loop Load_Ships_Loop;
      configure(ComboBox, "-values [list" & To_String(ValuesList) & "]");
      Tcl_Eval(Get_Context, "Refresh");
   end ShowDebugUI;

end DebugUI;
