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

with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Interfaces.C;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with CArgv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkTreeView; use Tcl.Tk.Ada.Widgets.TtkTreeView;
with Tcl.Tklib.Ada.Tooltip; use Tcl.Tklib.Ada.Tooltip;
with Config; use Config;
with Game; use Game;
with Utils; use Utils;
with Utils.UI; use Utils.UI;

package body Goals.UI is

   -- ****o* GUI/GUI.Show_Goals_Command
   -- FUNCTION
   -- Show goals UI to the player
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowGoals buttonpath
   -- Buttonpath is path to the button which is used to set the goal
   -- SOURCE
   function Show_Goals_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Goals_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      Goals_Dialog: constant Ttk_Frame := Get_Widget(".goalsdialog", Interp);
      GoalsView: constant Ttk_Tree_View :=
        Get_Widget(Goals_Dialog & ".view", Interp);
      SelectButton: constant Ttk_Button :=
        Get_Widget(Goals_Dialog & ".selectbutton", Interp);
      Dialog_Header: constant Ttk_Label :=
        Get_Widget(Goals_Dialog & ".header", Interp);
   begin
      Tcl_EvalFile
        (Interp,
         To_String(Data_Directory) & "ui" & Dir_Separator & "goals.tcl");
      Load_Goals_Loop :
      for I in Goals_List.Iterate loop
         Insert
           (GoalsView,
            Goal_Types'Image(Goals_List(I).G_Type) & " end -id {" &
            Trim(Positive'Image(Goals_Container.To_Index(I)), Left) &
            "} -text {" & Goal_Text(Goals_Container.To_Index(I)) & "}");
      end loop Load_Goals_Loop;
      configure(SelectButton, "-command {SetGoal " & CArgv.Arg(Argv, 1) & "}");
      Bind
        (Dialog_Header,
         "<ButtonPress-" & (if Game_Settings.Right_Button then "3" else "1") &
         ">",
         "{SetMousePosition " & Dialog_Header & " %X %Y}");
      Bind
        (Dialog_Header, "<Motion>", "{MoveDialog " & Goals_Dialog & " %X %Y}");
      Bind
        (Dialog_Header,
         "<ButtonRelease-" &
         (if Game_Settings.Right_Button then "3" else "1") & ">",
         "{SetMousePosition " & Dialog_Header & " 0 0}");
      return TCL_OK;
   end Show_Goals_Command;

   -- ****o* GUI/GUI.Set_Goal_Command
   -- FUNCTION
   -- Set selected goal as a current goal
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetGoal buttonpath
   -- Buttonpath is path to the button which is used to set the goal
   -- SOURCE
   function Set_Goal_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Goal_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      GoalsView: constant Ttk_Tree_View :=
        Get_Widget(".goalsdialog.view", Interp);
      SelectedGoal: Natural;
      ButtonName: constant String := CArgv.Arg(Argv, 1);
      GoalButton: constant Ttk_Button := Get_Widget(ButtonName, Interp);
      ButtonText: Unbounded_String;
   begin
      SelectedGoal := Natural'Value(Selection(GoalsView));
      Clear_Current_Goal;
      if SelectedGoal > 0 then
         Current_Goal := Goals_List(SelectedGoal);
      elsif Index(ButtonName, "newgamemenu") = 0 then
         Current_Goal :=
           Goals_List
             (Get_Random(Goals_List.First_Index, Goals_List.Last_Index));
      end if;
      if SelectedGoal > 0 then
         ButtonText := To_Unbounded_String(Goal_Text(SelectedGoal));
         Add(GoalButton, To_String(ButtonText));
         if Length(ButtonText) > 16 then
            ButtonText := Unbounded_Slice(ButtonText, 1, 17) & "...";
         end if;
         configure(GoalButton, "-text {" & To_String(ButtonText) & "}");
      else
         configure(GoalButton, "-text {Random}");
      end if;
      Tcl_Eval(Interp, ".goalsdialog.closebutton invoke");
      return TCL_OK;
   end Set_Goal_Command;

   procedure Add_Commands is
   begin
      Add_Command("ShowGoals", Show_Goals_Command'Access);
      Add_Command("SetGoal", Set_Goal_Command'Access);
   end Add_Commands;

end Goals.UI;
