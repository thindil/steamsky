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
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with CArgv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.TtkEntry; use Tcl.Tk.Ada.Widgets.TtkEntry;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
with Game; use Game;
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
      return TCL_OK;
   end Refresh_Command;

   procedure ShowDebugUI is
   begin
      Tcl_EvalFile
        (Get_Context,
         To_String(DataDirectory) & "ui" & Dir_Separator & "debug.tcl");
      AddCommand("Refresh", Refresh_Command'Access);
      AddCommand("RefreshModule", Refresh_Module_Command'Access);
      Tcl_Eval(Get_Context, "Refresh");
   end ShowDebugUI;

end DebugUI;
