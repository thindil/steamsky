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
with Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with CArgv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkTreeView; use Tcl.Tk.Ada.Widgets.TtkTreeView;
with Game; use Game;
with Utils; use Utils;
with Utils.UI; use Utils.UI;

package body Goals.UI is

   -- ****if* GUI/Show_Goals_Command
   -- FUNCTION
   -- Show goals UI to the player
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- SOURCE
   function Show_Goals_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Goals_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      GoalsView: Ttk_Tree_View;
      SelectButton: Ttk_Button;
   begin
      Tcl_EvalFile
        (Interp,
         To_String(DataDirectory) & "ui" & Dir_Separator & "goals.tcl");
      GoalsView.Interp := Interp;
      GoalsView.Name := New_String(".goalsdialog.view");
      for I in Goals_List.Iterate loop
         Insert
           (GoalsView,
            GoalTypes'Image(Goals_List(I).GType) & " end -id {" &
            Trim(Positive'Image(Goals_Container.To_Index(I)), Left) &
            "} -text {" & GoalText(Goals_Container.To_Index(I)) & "}");
      end loop;
      SelectButton.Interp := Interp;
      SelectButton.Name := New_String(".goalsdialog.selectbutton");
      configure(SelectButton, "-command {SetGoal " & CArgv.Arg(Argv, 1) & "}");
      return TCL_OK;
   end Show_Goals_Command;

   -- ****if* GUI/Set_Goal_Command
   -- FUNCTION
   -- Set selected goal as a current goal
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- SOURCE
   function Set_Goal_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Goal_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      GoalsView: Ttk_Tree_View;
      GoalButton: Ttk_Button;
      SelectedGoal: Natural;
      ButtonName: constant String := CArgv.Arg(Argv, 1);
   begin
      GoalsView.Interp := Interp;
      GoalsView.Name := New_String(".goalsdialog.view");
      SelectedGoal := Natural'Value(Selection(GoalsView));
      ClearCurrentGoal;
      if SelectedGoal > 0 then
         CurrentGoal := Goals_List(SelectedGoal);
      elsif Index(ButtonName, "newgamemenu") = 0 then
         CurrentGoal :=
           Goals_List
             (GetRandom(Goals_List.First_Index, Goals_List.Last_Index));
      end if;
      GoalButton.Interp := Interp;
      GoalButton.Name := New_String(ButtonName);
      if SelectedGoal > 0 then
         configure(GoalButton, "-text {" & GoalText(SelectedGoal) & "}");
      else
         configure(GoalButton, "-text {Random}");
      end if;
      Tcl_Eval(Interp, ".goalsdialog.closebutton invoke");
      return TCL_OK;
   end Set_Goal_Command;

   procedure AddCommands is
   begin
      AddCommand("ShowGoals", Show_Goals_Command'Access);
      AddCommand("SetGoal", Set_Goal_Command'Access);
   end AddCommands;

end Goals.UI;
