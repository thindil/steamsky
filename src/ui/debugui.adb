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
with Tcl.Tk.Ada.Widgets.TtkEntry; use Tcl.Tk.Ada.Widgets.TtkEntry;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Crew; use Crew;
with Game; use Game;
with Items; use Items;
with ShipModules; use ShipModules;
with Ships; use Ships;
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
      return TCL_OK;
   end Refresh_Command;

   procedure ShowDebugUI is
   begin
      Tcl_EvalFile
        (Get_Context,
         To_String(DataDirectory) & "ui" & Dir_Separator & "debug.tcl");
      AddCommand("Refresh", Refresh_Command'Access);
      AddCommand("RefreshModule", Refresh_Module_Command'Access);
      AddCommand("RefreshMember", Refresh_Member_Command'Access);
      AddCommand("RefreshCargo", Refresh_Cargo_Command'Access);
      Tcl_Eval(Get_Context, "Refresh");
   end ShowDebugUI;

end DebugUI;
