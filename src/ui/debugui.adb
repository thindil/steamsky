-- Copyright (c) 2020 Bartek thindil Jasicki <thindil@laeran.pl>
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

   -- ****f* DebugUI/Refresh_Module_Command
   -- FUNCTION
   -- Refresh the information about selected module
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- SOURCE
   function Refresh_Module_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Refresh_Module_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      ProtoEntry: Ttk_Entry;
      ModuleCombo: Ttk_ComboBox;
      ModuleIndex: Positive;
      SpinBox: Ttk_SpinBox;
   begin
      ModuleCombo.Interp := Interp;
      ModuleCombo.Name := New_String(".debugdialog.main.ship.module");
      ModuleIndex := Natural'Value(Current(ModuleCombo)) + 1;
      ProtoEntry.Interp := Interp;
      ProtoEntry.Name := New_String(".debugdialog.main.ship.proto");
      Delete(ProtoEntry, "0", "end");
      Insert
        (ProtoEntry, "0",
         To_String
           (Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex).Name));
      SpinBox.Interp := Interp;
      SpinBox.Name := New_String(".debugdialog.main.ship.weight");
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

   -- ****f* DebugUI/Refresh_Member_Command
   -- FUNCTION
   -- Refresh the information about selected crew member
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- SOURCE
   function Refresh_Member_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Refresh_Member_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      ComboBox: Ttk_ComboBox;
      SpinBox: Ttk_SpinBox;
      MemberFrame, Item: Ttk_Frame;
      Rows: Natural := 0;
      Tokens: Slice_Set;
      Label: Ttk_Label;
      Member: Member_Data;
      SkillsIndexes: Positive_Container.Vector;
      SkillsList: Unbounded_String;
   begin
      ComboBox.Interp := Interp;
      ComboBox.Name := New_String(".debugdialog.main.crew.member");
      Member := PlayerShip.Crew(Natural'Value(Current(ComboBox)) + 1);
      SpinBox.Interp := Interp;
      SpinBox.Name := New_String(".debugdialog.main.crew.health");
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
      MemberFrame.Interp := Interp;
      MemberFrame.Name := New_String(".debugdialog.main.crew.stats");
      Create(Tokens, Tcl.Tk.Ada.Grid.Grid_Size(MemberFrame), " ");
      Rows := Natural'Value(Slice(Tokens, 2));
      for I in 1 .. (Rows - 1) loop
         Create
           (Tokens,
            Tcl.Tk.Ada.Grid.Grid_Slaves
              (MemberFrame, "-row" & Positive'Image(I)),
            " ");
         for J in 1 .. Slice_Count(Tokens) loop
            Item.Interp := Interp;
            Item.Name := New_String(Slice(Tokens, J));
            Destroy(Item);
         end loop;
      end loop;
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
              "-from 1 -to 50 -validate key -validatecommand {ValidateSpinbox %S %s 50}");
         Set(SpinBox, Positive'Image(Member.Attributes(I)(1)));
         Tcl.Tk.Ada.Grid.Grid
           (SpinBox,
            "-column 1 -row" &
            Positive'Image(Attributes_Container.To_Index(I)));
      end loop;
      MemberFrame.Name := New_String(".debugdialog.main.crew.skills");
      Create(Tokens, Tcl.Tk.Ada.Grid.Grid_Size(MemberFrame), " ");
      Rows := Natural'Value(Slice(Tokens, 2));
      for I in 1 .. (Rows - 1) loop
         Create
           (Tokens,
            Tcl.Tk.Ada.Grid.Grid_Slaves
              (MemberFrame, "-row" & Positive'Image(I)),
            " ");
         for J in 1 .. Slice_Count(Tokens) loop
            Item.Interp := Interp;
            Item.Name := New_String(Slice(Tokens, J));
            Destroy(Item);
         end loop;
      end loop;
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
              "-from 1 -to 100 -validate key -validatecommand {ValidateSpinbox %S %s 100}");
         Set(SpinBox, Positive'Image(Member.Skills(I)(2)));
         Tcl.Tk.Ada.Grid.Grid
           (SpinBox,
            "-column 1 -row" & Positive'Image(Skills_Container.To_Index(I)));
         SkillsIndexes.Append(Member.Skills(I)(1));
      end loop;
      for I in Skills_List.Iterate loop
         if not SkillsIndexes.Contains(SkillsData_Container.To_Index(I)) then
            Append(SkillsList, " " & Skills_List(I).Name);
         end if;
      end loop;
      ComboBox.Name := New_String(".debugdialog.main.crew.addskill.skills");
      configure(ComboBox, "-values [list" & To_String(SkillsList) & "]");
      Current(ComboBox, "0");
      return TCL_OK;
   end Refresh_Member_Command;

   -- ****f* DebugUI/Refresh_Cargo_Command
   -- FUNCTION
   -- Refresh the information about the player ship cargo
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- SOURCE
   function Refresh_Cargo_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Refresh_Cargo_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      CargoCombo: Ttk_ComboBox;
      ItemIndex: Positive;
      AmountBox: Ttk_SpinBox;
   begin
      CargoCombo.Interp := Interp;
      CargoCombo.Name := New_String(".debugdialog.main.cargo.update");
      ItemIndex := Natural'Value(Current(CargoCombo)) + 1;
      AmountBox.Interp := Interp;
      AmountBox.Name := New_String(".debugdialog.main.cargo.updateamount");
      Set(AmountBox, Positive'Image(PlayerShip.Cargo(ItemIndex).Amount));
      return TCL_OK;
   end Refresh_Cargo_Command;

   -- ****f* DebugUI/Refresh_Command
   -- FUNCTION
   -- Refresh the whole game information
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- SOURCE
   function Refresh_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Refresh_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      SpinBox: Ttk_SpinBox;
      ComboBox: Ttk_ComboBox;
      ValuesList: Unbounded_String;
   begin
      SpinBox.Interp := Interp;
      SpinBox.Name := New_String(".debugdialog.main.ship.x");
      Set(SpinBox, Positive'Image(PlayerShip.SkyX));
      SpinBox.Name := New_String(".debugdialog.main.ship.y");
      Set(SpinBox, Positive'Image(PlayerShip.SkyY));
      ComboBox.Interp := Interp;
      ComboBox.Name := New_String(".debugdialog.main.ship.module");
      for Module of PlayerShip.Modules loop
         Append(ValuesList, " {" & Module.Name & "}");
      end loop;
      configure(ComboBox, "-values [list" & To_String(ValuesList) & "]");
      Current(ComboBox, "0");
      Tcl_Eval(Get_Context, "RefreshModule");
      ComboBox.Name := New_String(".debugdialog.main.crew.member");
      ValuesList := Null_Unbounded_String;
      for Member of PlayerShip.Crew loop
         Append(ValuesList, " {" & Member.Name & "}");
      end loop;
      configure(ComboBox, "-values [list" & To_String(ValuesList) & "]");
      Current(ComboBox, "0");
      Tcl_Eval(Get_Context, "RefreshMember");
      ComboBox.Name := New_String(".debugdialog.main.cargo.update");
      ValuesList := Null_Unbounded_String;
      for Item of PlayerShip.Cargo loop
         Append(ValuesList, " {" & GetItemName(Item, False, False) & "}");
      end loop;
      configure(ComboBox, "-values [list" & To_String(ValuesList) & "]");
      Current(ComboBox, "0");
      Tcl_Eval(Get_Context, "RefreshCargo");
      Tcl_Eval(Get_Context, "RefreshEvents");
      return TCL_OK;
   end Refresh_Command;

   -- ****f* DebugUI/Refresh_Base_Command
   -- FUNCTION
   -- Refresh the information about the selected base
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- SOURCE
   function Refresh_Base_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Refresh_Base_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      NameEntry: Ttk_Entry;
      BaseIndex: Natural := 0;
      BaseName: Unbounded_String;
      ComboBox: Ttk_ComboBox;
      SpinBox: Ttk_SpinBox;
   begin
      NameEntry.Interp := Interp;
      NameEntry.Name := New_String(".debugdialog.main.bases.name");
      BaseName := To_Unbounded_String(Get(NameEntry));
      for I in SkyBases'Range loop
         if SkyBases(I).Name = BaseName then
            BaseIndex := I;
            exit;
         end if;
      end loop;
      if BaseIndex = 0 then
         return TCL_OK;
      end if;
      ComboBox.Interp := Interp;
      ComboBox.Name := New_String(".debugdialog.main.bases.type");
      Set
        (ComboBox,
         To_String(BasesTypes_List(SkyBases(BaseIndex).BaseType).Name));
      ComboBox.Name := New_String(".debugdialog.main.bases.owner");
      Set(ComboBox, To_String(Factions_List(SkyBases(BaseIndex).Owner).Name));
      ComboBox.Name := New_String(".debugdialog.main.bases.size");
      Current
        (ComboBox, Natural'Image(Bases_Size'Pos(SkyBases(BaseIndex).Size)));
      SpinBox.Interp := Interp;
      SpinBox.Name := New_String(".debugdialog.main.bases.population");
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

   -- ****f* DebugUI/Refresh_Events_Command
   -- FUNCTION
   -- Refresh the list of events
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- SOURCE
   function Refresh_Events_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Refresh_Events_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      EventsBox: Ttk_ComboBox;
      ValuesList: Unbounded_String;
      EventsButton: Ttk_Button;
   begin
      EventsBox.Interp := Interp;
      EventsBox.Name := New_String(".debugdialog.main.world.delete");
      EventsButton.Interp := Interp;
      EventsButton.Name := New_String(".debugdialog.main.world.deleteevent");
      if Events_List.Length = 0 then
         Tcl.Tk.Ada.Grid.Grid_Remove(EventsButton);
         Tcl.Tk.Ada.Grid.Grid_Remove(EventsBox);
         return TCL_OK;
      else
         Tcl.Tk.Ada.Grid.Grid(EventsButton);
         Tcl.Tk.Ada.Grid.Grid(EventsBox);
      end if;
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
      end loop;
      configure(EventsBox, "-values [list" & To_String(ValuesList) & "]");
      Current(EventsBox, "0");
      return TCL_OK;
   end Refresh_Events_Command;

   -- ****f* DebugUI/Save_Game_Command
   -- FUNCTION
   -- Save the game
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- SOURCE
   function Save_Game_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Save_Game_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc, Argv);
   begin
      SaveGame(True);
      return TCL_OK;
   end Save_Game_Command;

   -- ****f* DebugUI/Move_Ship_Command
   -- FUNCTION
   -- Move the player ship
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- SOURCE
   function Move_Ship_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Move_Ship_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      SpinBox: Ttk_SpinBox;
   begin
      SpinBox.Interp := Interp;
      SpinBox.Name := New_String(".debugdialog.main.ship.x");
      PlayerShip.SkyX := Positive'Value(Get(SpinBox));
      SpinBox.Name := New_String(".debugdialog.main.ship.y");
      PlayerShip.SkyY := Positive'Value(Get(SpinBox));
      ShowSkyMap(True);
      return TCL_OK;
   end Move_Ship_Command;

   -- ****f* DebugUI/Update_Module_Command
   -- FUNCTION
   -- Update the selected module
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- SOURCE
   function Update_Module_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Update_Module_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      ModuleBox: Ttk_ComboBox;
      ModuleIndex: Positive;
      ModuleEntry: Ttk_Entry;
      Value: Unbounded_String;
      SpinBox: Ttk_SpinBox;
   begin
      ModuleBox.Interp := Interp;
      ModuleBox.Name := New_String(".debugdialog.main.ship.module");
      ModuleIndex := Natural'Value(Current(ModuleBox)) + 1;
      ModuleEntry.Interp := Interp;
      ModuleEntry.Name := New_String(".debugdialog.main.ship.proto");
      Value := To_Unbounded_String(Get(ModuleEntry));
      for I in Modules_List.Iterate loop
         if Modules_List(I).Name = Value then
            Value := Null_Unbounded_String;
            PlayerShip.Modules(ModuleIndex).ProtoIndex :=
              BaseModules_Container.Key(I);
            exit;
         end if;
      end loop;
      SpinBox.Interp := Interp;
      SpinBox.Name := New_String(".debugdialog.main.ship.weight");
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

   -- ****f* DebugUI/Add_Skill_Command
   -- FUNCTION
   -- Add a new skill to the selected crew member
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- SOURCE
   function Add_Skill_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Add_Skill_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      ComboBox: Ttk_ComboBox;
      MemberIndex: Positive;
      SkillName: Unbounded_String;
   begin
      ComboBox.Interp := Interp;
      ComboBox.Name := New_String(".debugdialog.main.crew.member");
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

   -- ****f* DebugUI/Update_Member_Command
   -- FUNCTION
   -- Update the selected crew member
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- SOURCE
   function Update_Member_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Update_Member_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      ComboBox: Ttk_ComboBox;
      MemberIndex: Positive;
      SpinBox: Ttk_SpinBox;
   begin
      ComboBox.Interp := Interp;
      ComboBox.Name := New_String(".debugdialog.main.crew.member");
      MemberIndex := Natural'Value(Current(ComboBox)) + 1;
      SpinBox.Interp := Interp;
      SpinBox.Name := New_String(".debugdialog.main.crew.health");
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

   -- ****f* DebugUI/Add_Item_Command
   -- FUNCTION
   -- Add a new item to the player ship cargo
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- SOURCE
   function Add_Item_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Add_Item_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      ItemEntry: Ttk_Entry;
      ItemBox: Ttk_SpinBox;
      ItemIndex, ItemName: Unbounded_String;
   begin
      ItemEntry.Interp := Interp;
      ItemEntry.Name := New_String(".debugdialog.main.cargo.add");
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
      ItemBox.Interp := Interp;
      ItemBox.Name := New_String(".debugdialog.main.cargo.amount");
      UpdateCargo(PlayerShip, ItemIndex, Positive'Value(Get(ItemBox)));
      return Refresh_Command(ClientData, Interp, Argc, Argv);
   end Add_Item_Command;

   -- ****f* DebugUI/Update_Item_Command
   -- FUNCTION
   -- Update the amount of an item in the player ship cargo
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- SOURCE
   function Update_Item_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Update_Item_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      ItemCombo: Ttk_ComboBox;
      ItemBox: Ttk_SpinBox;
      ItemIndex: Positive;
   begin
      ItemCombo.Interp := Interp;
      ItemCombo.Name := New_String(".debugdialog.main.cargo.update");
      ItemIndex := Natural'Value(Current(ItemCombo)) + 1;
      ItemBox.Interp := Interp;
      ItemBox.Name := New_String(".debugdialog.main.cargo.updateamount");
      UpdateCargo
        (Ship => PlayerShip, Amount => Positive'Value(Get(ItemBox)),
         CargoIndex => ItemIndex);
      return Refresh_Command(ClientData, Interp, Argc, Argv);
   end Update_Item_Command;

   -- ****f* DebugUI/Update_Base_Command
   -- FUNCTION
   -- Update the selected base
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- SOURCE
   function Update_Base_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Update_Base_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      BaseIndex: Natural := 0;
      BaseEntry: Ttk_Entry;
      BaseName: Unbounded_String;
      BaseCombo: Ttk_ComboBox;
      BaseBox: Ttk_SpinBox;
   begin
      BaseEntry.Interp := Interp;
      BaseEntry.Name := New_String(".debugdialog.main.bases.name");
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
      BaseCombo.Interp := Interp;
      BaseCombo.Name := New_String(".debugdialog.main.bases.type");
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
      BaseBox.Interp := Interp;
      BaseBox.Name := New_String(".debugdialog.main.bases.population");
      SkyBases(BaseIndex).Population := Natural'Value(Get(BaseBox));
      BaseBox.Name := New_String(".debugdialog.main.bases.reputation");
      SkyBases(BaseIndex).Reputation(1) := Integer'Value(Get(BaseBox));
      BaseBox.Name := New_String(".debugdialog.main.bases.money");
      SkyBases(BaseIndex).Cargo(1).Amount := Natural'Value(Get(BaseBox));
      return TCL_OK;
   end Update_Base_Command;

   -- ****f* DebugUI/Add_Ship_Command
   -- FUNCTION
   -- Add a new ship based event to the game
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- SOURCE
   function Add_Ship_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Add_Ship_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      ShipEntry: Ttk_Entry;
      ShipName: Unbounded_String;
      NpcShipX, NpcShipY, Duration: Positive;
      ShipBox: Ttk_SpinBox;
   begin
      ShipEntry.Interp := Interp;
      ShipEntry.Name := New_String(".debugdialog.main.world.ship");
      ShipName := To_Unbounded_String(Get(ShipEntry));
      ShipBox.Interp := Interp;
      ShipBox.Name := New_String(".debugdialog.main.world.x");
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

   -- ****f* DebugUI/Toggle_Item_Entry_Command
   -- FUNCTION
   -- Show or hide item entry for bases events
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- SOURCE
   function Toggle_Item_Entry_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Toggle_Item_Entry_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      EventCombo: Ttk_ComboBox;
      ItemEntry: Ttk_Entry;
      ItemLabel: Ttk_Label;
   begin
      EventCombo.Interp := Interp;
      EventCombo.Name := New_String(".debugdialog.main.world.event");
      ItemEntry.Interp := Interp;
      ItemEntry.Name := New_String(".debugdialog.main.world.item");
      ItemLabel.Interp := Interp;
      ItemLabel.Name := New_String(".debugdialog.main.world.itemlbl");
      if Current(EventCombo) = "1" then
         Tcl.Tk.Ada.Grid.Grid(ItemLabel);
         Tcl.Tk.Ada.Grid.Grid(ItemEntry);
      else
         Tcl.Tk.Ada.Grid.Grid_Remove(ItemLabel);
         Tcl.Tk.Ada.Grid.Grid_Remove(ItemEntry);
      end if;
      return TCL_OK;
   end Toggle_Item_Entry_Command;

   -- ****f* DebugUI/Add_Event_Command
   -- FUNCTION
   -- Add a new base event to the game
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- SOURCE
   function Add_Event_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Add_Event_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      EventEntry: Ttk_Entry;
      EventName: Unbounded_String;
      BaseIndex: Natural := 0;
      EventType: Events_Types;
      EventBox: Ttk_ComboBox;
      DurationBox: Ttk_SpinBox;
      Added: Boolean := True;
   begin
      EventEntry.Interp := Interp;
      EventEntry.Name := New_String(".debugdialog.main.world.base");
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
      EventBox.Interp := Interp;
      EventBox.Name := New_String(".debugdialog.main.world.event");
      EventType := Events_Types'Value(Get(EventBox));
      DurationBox.Interp := Interp;
      DurationBox.Name := New_String(".debugdialog.main.world.baseduration");
      case EventType is
         when Disease =>
            Events_List.Append
              (New_Item =>
                 (Disease, SkyBases(BaseIndex).SkyX, SkyBases(BaseIndex).SkyY,
                  Positive'Value(Get(DurationBox)), 1));
         when DoublePrice =>
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
         when FullDocks =>
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

   -- ****f* DebugUI/Delete_Event_Command
   -- FUNCTION
   -- Remove the selected event from the game
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- SOURCE
   function Delete_Event_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Delete_Event_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      EventBox: Ttk_ComboBox;
   begin
      EventBox.Interp := Interp;
      EventBox.Name := New_String(".debugdialog.main.world.delete");
      DeleteEvent(Natural'Value(Current(EventBox)) + 1);
      return Refresh_Events_Command(ClientData, Interp, Argc, Argv);
   end Delete_Event_Command;

   procedure ShowDebugUI is
      ComboBox: Ttk_ComboBox;
      ValuesList: Unbounded_String;
   begin
      Tcl_EvalFile
        (Get_Context,
         To_String(DataDirectory) & "ui" & Dir_Separator & "debug.tcl");
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
      ComboBox.Interp := Get_Context;
      ComboBox.Name := New_String(".debugdialog.main.bases.type");
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
