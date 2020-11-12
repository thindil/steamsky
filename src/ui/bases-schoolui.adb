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

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with CArgv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Canvas; use Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.Menu; use Tcl.Tk.Ada.Widgets.Menu;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkPanedWindow; use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
with Tcl.Tk.Ada.Widgets.TtkTreeView; use Tcl.Tk.Ada.Widgets.TtkTreeView;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Bases.Trade; use Bases.Trade;
with Maps.UI; use Maps.UI;
with Trades; use Trades;
with Utils.UI; use Utils.UI;

package body Bases.SchoolUI is

   -- ****o* SchoolUI/Show_School_Command
   -- FUNCTION
   -- Show the selected base school
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowSchool
   -- SOURCE
   function Show_School_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_School_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argv);
      Paned: constant Ttk_PanedWindow :=
        Get_Widget(".gameframe.paned", Interp);
      SchoolFrame: Ttk_Frame := Get_Widget(Paned & ".schoolframe", Interp);
      SchoolCanvas: constant Tk_Canvas :=
        Get_Widget(SchoolFrame & ".canvas", Interp);
      CloseButton: constant Ttk_Button :=
        Get_Widget(".gameframe.header.closebutton", Interp);
      CrewView: Ttk_Tree_View;
   begin
      if Winfo_Get(SchoolCanvas, "exists") = "0" then
         Tcl_EvalFile
           (Get_Context,
            To_String(DataDirectory) & "ui" & Dir_Separator & "school.tcl");
         Bind(SchoolFrame, "<Configure>", "{ResizeCanvas %W.canvas %w %h}");
      elsif Winfo_Get(SchoolCanvas, "ismapped") = "1" and Argc = 1 then
         Tcl.Tk.Ada.Grid.Grid_Remove(CloseButton);
         Entry_Configure(GameMenu, "Help", "-command {ShowHelp general}");
         ShowSkyMap(True);
         return TCL_OK;
      end if;
      Entry_Configure(GameMenu, "Help", "-command {ShowHelp crew}");
      SchoolFrame.Name := New_String(Widget_Image(SchoolCanvas) & ".school");
      CrewView := Get_Widget(SchoolFrame & ".crew.view", Interp);
      Delete(CrewView, "[list " & Children(CrewView, "{}") & "]");
      for I in PlayerShip.Crew.Iterate loop
         Insert
           (CrewView,
            "{} end -id" & Positive'Image(Crew_Container.To_Index(I)) &
            " -text {" & To_String(PlayerShip.Crew(I).Name) & "}");
      end loop;
      Selection_Set(CrewView, "[list 1]");
      Tcl.Tk.Ada.Grid.Grid(CloseButton, "-row 0 -column 1");
      SchoolFrame.Name := New_String(Widget_Image(SchoolCanvas) & ".school");
      configure
        (SchoolCanvas,
         "-height [expr " & SashPos(Paned, "0") & " - 20] -width " &
         cget(Paned, "-width"));
      Tcl_Eval(Get_Context, "update");
      Canvas_Create
        (SchoolCanvas, "window",
         "0 0 -anchor nw -window " & Widget_Image(SchoolFrame));
      Tcl_Eval(Get_Context, "update");
      configure
        (SchoolCanvas,
         "-scrollregion [list " & BBox(SchoolCanvas, "all") & "]");
      ShowScreen("schoolframe");
      return TCL_OK;
   end Show_School_Command;

      -- ****iv* SchoolUI/MemberIndex
      -- FUNCTION
      -- The currently selected crew member index
      -- SOURCE
   MemberIndex: Positive;
   -- ****

   -- ****o* SchoolUI/Show_Training_Info_Command
   -- FUNCTION
   -- Show training costs for the selected crew member
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowTrainingInfo
   -- SOURCE
   function Show_Training_Info_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Training_Info_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      CrewView: constant Ttk_Tree_View :=
        Get_Widget
          (".gameframe.paned.schoolframe.canvas.school.crew.view", Interp);
      SkillsView: constant Ttk_Tree_View :=
        Get_Widget
          (".gameframe.paned.schoolframe.canvas.school.skills.view", Interp);
      Cost, MoneyIndex2, FirstIndex: Natural := 0;
      MoneyLabel: constant Ttk_Label :=
        Get_Widget
          (".gameframe.paned.schoolframe.canvas.school.skills.money", Interp);
      TrainButton: constant Ttk_Button :=
        Get_Widget
          (".gameframe.paned.schoolframe.canvas.school.skills.train", Interp);
   begin
      MemberIndex := Positive'Value(Selection(CrewView));
      Delete(SkillsView, "[list " & Children(SkillsView, "{}") & "]");
      for I in Skills_List.Iterate loop
         Cost := TrainCost(MemberIndex, SkillsData_Container.To_Index(I));
         if Cost > 0 then
            if FirstIndex = 0 then
               FirstIndex := SkillsData_Container.To_Index(I);
            end if;
            Insert
              (SkillsView,
               "{} end -id" &
               Positive'Image(SkillsData_Container.To_Index(I)) &
               " -values [list {" & To_String(Skills_List(I).Name) & "}" &
               Natural'Image(Cost) & "]");
         end if;
      end loop;
      MoneyIndex2 := FindItem(PlayerShip.Cargo, MoneyIndex);
      if MoneyIndex2 > 0 then
         configure
           (MoneyLabel,
            "-text {You have" &
            Natural'Image(PlayerShip.Cargo(MoneyIndex2).Amount) & " " &
            To_String(MoneyName) & ".}");
      else
         configure
           (MoneyLabel,
            "-text {You don't have any " & To_String(MoneyName) &
            " to pay for learning.}");
      end if;
      if Children(SkillsView, "{}") /= "{}" then
         Selection_Set(SkillsView, "[list" & Positive'Image(FirstIndex) & "]");
         configure(TrainButton, "-state normal");
      else
         configure(TrainButton, "-state disabled");
      end if;
      return TCL_OK;
   end Show_Training_Info_Command;

   -- ****o* SchoolUI/Train_Skill_Command
   -- FUNCTION
   -- Train the selected skill
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- TrainSkill
   -- SOURCE
   function Train_Skill_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Train_Skill_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      SkillsView: constant Ttk_Tree_View :=
        Get_Widget
          (".gameframe.paned.schoolframe.canvas.school.skills.view", Interp);
      SkillIndex: Positive;
   begin
      SkillIndex := Positive'Value(Selection(SkillsView));
      TrainSkill(MemberIndex, SkillIndex);
      UpdateMessages;
      return Show_Training_Info_Command(ClientData, Interp, Argc, Argv);
   exception
      when Trade_No_Money =>
         ShowMessage
           ("You don't have any " & To_String(MoneyName) &
            " to pay for learning.");
         return TCL_OK;
      when Trade_Not_Enough_Money =>
         ShowMessage
           ("You don't have enough " & To_String(MoneyName) &
            " to pay for learning this skill.");
         return TCL_OK;
      when Trade_Cant_Train =>
         ShowMessage("You can't train this skill any more.");
         return TCL_OK;
   end Train_Skill_Command;

   procedure AddCommands is
   begin
      AddCommand("ShowSchool", Show_School_Command'Access);
      AddCommand("ShowTrainingInfo", Show_Training_Info_Command'Access);
      AddCommand("TrainSkill", Train_Skill_Command'Access);
   end AddCommands;

end Bases.SchoolUI;
