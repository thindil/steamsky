-- Copyright (c) 2020-2023 Bartek thindil Jasicki
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
with Interfaces.C; use Interfaces.C;
with GNAT.Directory_Operations;
with CArgv; use CArgv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
with Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkPanedWindow;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Bases.Trade; use Bases.Trade;
with CoreUI; use CoreUI;
with Dialogs;
with Maps.UI; use Maps.UI;
with Trades;
with Utils.UI; use Utils.UI;

package body Bases.SchoolUI is

   -- ****o* SchoolUI/SchoolUI.Set_School_Skills_Command
   -- FUNCTION
   -- Set list of available to train skills for the selected crew member
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetSchoolSkills
   -- SOURCE
   function Set_School_Skills_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "setSchoolSkillsCommand";
      -- ****

   -- ****o* SchoolUI/SchoolUI.Show_School_Command
   -- FUNCTION
   -- Show the selected base school
   -- PARAMETERS
   -- Client_Data - Custom data send to the command.
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command.
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowSchool
   -- SOURCE
   function Show_School_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_School_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      use GNAT.Directory_Operations;
      use Tcl.Tk.Ada;
      use Tcl.Tk.Ada.Widgets.Canvas;
      use Tcl.Tk.Ada.Widgets.TtkButton;
      use Tcl.Tk.Ada.Widgets.TtkFrame;
      use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
      use Tiny_String;

      School_Frame: Ttk_Frame :=
        Get_Widget(pathName => Main_Paned & ".schoolframe", Interp => Interp);
      School_Canvas: constant Tk_Canvas :=
        Get_Widget(pathName => School_Frame & ".canvas", Interp => Interp);
      Combo_Box: Ttk_ComboBox :=
        Get_Widget
          (pathName => School_Canvas & ".school.setting.crew",
           Interp => Interp);
      Combo_List: Unbounded_String := Null_Unbounded_String;
      Money_Label: constant Ttk_Label :=
        Get_Widget
          (pathName => School_Canvas & ".school.money", Interp => Interp);
      Money_Index_2: Natural;
      Train_Button: constant Ttk_Button :=
        Get_Widget
          (pathName => School_Canvas & ".school.setting.train",
           Interp => Interp);
   begin
      if Winfo_Get(Widgt => School_Canvas, Info => "exists") = "0" then
         Tcl_EvalFile
           (interp => Get_Context,
            fileName =>
              To_String(Source => Data_Directory) & "ui" & Dir_Separator &
              "school.tcl");
         Bind
           (Widgt => School_Frame, Sequence => "<Configure>",
            Script => "{ResizeCanvas %W.canvas %w %h}");
      elsif Winfo_Get(Widgt => School_Canvas, Info => "ismapped") = "1" and
        Argc = 1 then
         Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Close_Button);
         Show_Sky_Map(Clear => True);
         return TCL_OK;
      end if;
      Tcl_SetVar(interp => Interp, varName => "gamestate", newValue => "crew");
      Money_Index_2 :=
        Find_Item(Inventory => Player_Ship.Cargo, Proto_Index => Money_Index);
      if Money_Index_2 > 0 then
         configure
           (Widgt => Money_Label,
            options =>
              "-text {You have" &
              Natural'Image
                (Inventory_Container.Element
                   (Container => Player_Ship.Cargo, Index => Money_Index_2)
                   .Amount) &
              " " & To_String(Source => Money_Name) & ".}");
      else
         configure
           (Widgt => Money_Label,
            options =>
              "-text {You don't have any " & To_String(Source => Money_Name) &
              " to pay for learning.}");
      end if;
      School_Frame.Name := New_String(Str => School_Canvas & ".school");
      if Argc = 1 then
         Add_Crew_Loop :
         for Member of Player_Ship.Crew loop
            Append
              (Source => Combo_List,
               New_Item => " " & To_String(Source => Member.Name));
         end loop Add_Crew_Loop;
         configure
           (Widgt => Combo_Box,
            options =>
              "-values [list" & To_String(Source => Combo_List) & "]");
         Current(ComboBox => Combo_Box, NewIndex => "0");
      end if;
      if Set_School_Skills_Command
          (Client_Data => Client_Data, Interp => Interp, Argc => Argc,
           Argv => Argv) /=
        TCL_OK then
         return TCL_ERROR;
      end if;
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Close_Button, Options => "-row 0 -column 1");
      configure
        (Widgt => School_Canvas,
         options =>
           "-height [expr " & SashPos(Paned => Main_Paned, Index => "0") &
           " - 20] -width " & cget(Widgt => Main_Paned, option => "-width"));
      Tcl_Eval(interp => Get_Context, strng => "update");
      Canvas_Create
        (Parent => School_Canvas, Child_Type => "window",
         Options => "0 0 -anchor nw -window " & School_Frame);
      Tcl_Eval(interp => Get_Context, strng => "update");
      configure
        (Widgt => School_Canvas,
         options =>
           "-scrollregion [list " &
           BBox(CanvasWidget => School_Canvas, TagOrId => "all") & "]");
      Combo_Box :=
        Get_Widget(pathName => School_Canvas & ".school.costbox.amount");
      Bind
        (Widgt => Combo_Box, Sequence => "<Tab>",
         Script => "{focus " & Train_Button & ";break}");
      Show_Screen(New_Screen_Name => "schoolframe");
      Focus(Widgt => Train_Button, Option => "-force");
      return TCL_OK;
   end Show_School_Command;

   -- ****if* SchoolUI/SchoolUI.Get_Member_Index
   -- FUNCTION
   -- Get the index in the player ship of the currently selected member
   -- RESULT
   -- The index of the currently selected crew member
   -- SOURCE
   function Get_Member_Index return Positive is
      -- ****
      use Tiny_String;

      Member_Box: constant Ttk_ComboBox :=
        Get_Widget
          (pathName => Main_Paned & ".schoolframe.canvas.school.setting.crew");
      Member_Index: Positive := 1;
   begin
      Find_Member_Index_Loop :
      for Member of Player_Ship.Crew loop
         exit Find_Member_Index_Loop when Member.Name =
           To_Bounded_String(Source => Get(Widgt => Member_Box));
         Member_Index := Member_Index + 1;
      end loop Find_Member_Index_Loop;
      return Member_Index;
   end Get_Member_Index;

   -- ****if* SchoolUI/SchoolUI.Get_Skill_Index
   -- FUNCTION
   -- Get the index of the currently selected skill
   -- RESULT
   -- The index of the currently selected skill
   -- SOURCE
   function Get_Skill_Index return Positive is
      -- ****
      use Tiny_String;

      Skill_Box: constant Ttk_ComboBox :=
        Get_Widget
          (pathName =>
             Main_Paned & ".schoolframe.canvas.school.setting.skill");
      Skill_Index: Positive := 1;
      Combo_Box_Value: constant String := Get(Widgt => Skill_Box);
      Skill_Name: constant Bounded_String :=
        Bounded_Slice
          (Source => To_Bounded_String(Source => Combo_Box_Value), Low => 1,
           High => Index(Source => Combo_Box_Value, Pattern => ":") - 1);
   begin
      Find_Skill_Index_Loop :
      for I in 1 .. Skills_Amount loop
         exit Find_Skill_Index_Loop when SkillsData_Container.Element
             (Container => Skills_List, Index => I)
             .Name =
           Skill_Name;
         Skill_Index := Skill_Index + 1;
      end loop Find_Skill_Index_Loop;
      return Skill_Index;
   end Get_Skill_Index;

   -- ****o* SchoolUI/SchoolUI.Train_Skill_Command
   -- FUNCTION
   -- Train the selected skill
   -- PARAMETERS
   -- Client_Data - Custom data send to the command.
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- TrainSkill
   -- SOURCE
   function Train_Skill_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Train_Skill_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      use Dialogs;
      use Trades;

      pragma Unreferenced(Argc, Argv);
      Amount_Box: constant Ttk_SpinBox :=
        Get_Widget
          (pathName =>
             Main_Paned & ".schoolframe.canvas.school." &
             Tcl_GetVar(interp => Interp, varName => "traintype") &
             "box.amount",
           Interp => Interp);
   begin
      if Get(Widgt => Amount_Box) = "0" then
         return TCL_OK;
      end if;
      Train_Skill
        (Member_Index => Get_Member_Index,
         Skill_Index => Skills_Amount_Range(Get_Skill_Index),
         Amount => Positive'Value(Get(Widgt => Amount_Box)),
         Is_Amount =>
           (if Tcl_GetVar(interp => Interp, varName => "traintype") = "amount"
            then True
            else False));
      Update_Messages;
      return
        Show_School_Command
          (Client_Data => Client_Data, Interp => Interp, Argc => 2,
           Argv =>
             CArgv.Empty & "TrainSkill" &
             Trim(Source => Positive'Image(Get_Member_Index), Side => Left));
   exception
      when Trade_No_Money =>
         Show_Message
           (Text =>
              "You don't have any " & To_String(Source => Money_Name) &
              " to pay for learning.",
            Title => "Can't train");
         return TCL_OK;
      when Trade_Not_Enough_Money =>
         Show_Message
           (Text =>
              "You don't have enough " & To_String(Source => Money_Name) &
              " to pay for learning this skill.",
            Title => "Can't train");
         return TCL_OK;
   end Train_Skill_Command;

   -- ****o* SchoolUI/SchoolUI.Update_School_Cost_Command
   -- FUNCTION
   -- Update the cost of training
   -- PARAMETERS
   -- Client_Data - Custom data send to the command.
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command.
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- UpdateSchoolCost combobox amount
   -- Combobox is the Tk path to the ttk::combobox with the amount of
   -- training sessions, amount is the amount of the requested training
   -- sessions
   -- SOURCE
   function Update_School_Cost_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Update_School_Cost_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc);
      Combo_Box: constant Ttk_ComboBox :=
        Get_Widget
          (pathName => CArgv.Arg(Argv => Argv, N => 1), Interp => Interp);
      Label: constant Ttk_Label :=
        Get_Widget
          (pathName =>
             Winfo_Get(Widgt => Combo_Box, Info => "parent") & ".cost",
           Interp => Interp);
      Amount, Cost: Natural;
   begin
      if CArgv.Arg(Argv => Argv, N => 2) = "" then
         Tcl_SetResult(interp => Interp, str => "1");
         return TCL_OK;
      end if;
      Amount := Natural'Value(CArgv.Arg(Argv => Argv, N => 2));
      --## rule off SIMPLIFIABLE_STATEMENTS
      if Amount < 1 then
         Amount := 1;
      elsif Amount > 100 then
         Amount := 100;
      end if;
      --## rule on SIMPLIFIABLE_STATEMENTS
      Cost :=
        Train_Cost
          (Member_Index => Get_Member_Index,
           Skill_Index => Skills_Amount_Range(Get_Skill_Index)) *
        Amount;
      configure
        (Widgt => Label,
         options =>
           "-text {" & Positive'Image(Cost) & " " &
           To_String(Source => Money_Name) & "}");
      Tcl_SetResult(interp => Interp, str => "1");
      return TCL_OK;
   exception
      when Constraint_Error =>
         Tcl_SetResult(interp => Interp, str => "0");
         return TCL_OK;
   end Update_School_Cost_Command;

   -- ****o* SchoolUI/SchoolUI.Update_School_Selected_Cost_Command
   -- FUNCTION
   -- Update the minimal and maximum values of spinbox with training cost
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- UpdateSchoolSelectedCost
   -- SOURCE
   function Update_School_Selected_Cost_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Update_School_Selected_Cost_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc, Argv);
      Amount_Box: constant Ttk_SpinBox :=
        Get_Widget
          (pathName =>
             Main_Paned & ".schoolframe.canvas.school.costbox.amount",
           Interp => Interp);
      Money_Index_2: constant Natural :=
        Find_Item(Inventory => Player_Ship.Cargo, Proto_Index => Money_Index);
      Cost: constant Natural :=
        Train_Cost
          (Member_Index => Get_Member_Index,
           Skill_Index => Skills_Amount_Range(Get_Skill_Index));
   begin
      if Money_Index_2 > 0
        and then Cost <=
          Inventory_Container.Element
            (Container => Player_Ship.Cargo, Index => Money_Index_2)
            .Amount then
         configure
           (Widgt => Amount_Box,
            options =>
              "-from" & Positive'Image(Cost) & " -to" &
              Positive'Image
                (Inventory_Container.Element
                   (Container => Player_Ship.Cargo, Index => Money_Index_2)
                   .Amount));
         Bind
           (Widgt => Amount_Box, Sequence => "<<Increment>>",
            Script =>
              "{" & Amount_Box & " set [expr [" & Amount_Box & " get] +" &
              Positive'Image(Cost) & " - 1]}");
         Bind
           (Widgt => Amount_Box, Sequence => "<<Decrement>>",
            Script =>
              "{" & Amount_Box & " set [expr [" & Amount_Box & " get] -" &
              Positive'Image(Cost) & " + 1]}");
      else
         configure(Widgt => Amount_Box, options => "-from 0 -to 0");
         Set(SpinBox => Amount_Box, Value => "0");
         Unbind(Widgt => Amount_Box, Sequence => "<<Increment>>");
         Unbind(Widgt => Amount_Box, Sequence => "<<Decrement>>");
      end if;
      Set(SpinBox => Amount_Box, Value => Natural'Image(Cost));
      return TCL_OK;
   end Update_School_Selected_Cost_Command;

   procedure Add_Commands is
   begin
      Add_Command
        (Name => "ShowSchool", Ada_Command => Show_School_Command'Access);
      Add_Command
        (Name => "TrainSkill", Ada_Command => Train_Skill_Command'Access);
      Add_Command
        (Name => "SetSchoolSkills",
         Ada_Command => Set_School_Skills_Command'Access);
      Add_Command
        (Name => "UpdateSchoolCost",
         Ada_Command => Update_School_Cost_Command'Access);
      Add_Command
        (Name => "UpdateSchoolSelectedCost",
         Ada_Command => Update_School_Selected_Cost_Command'Access);
   end Add_Commands;

end Bases.SchoolUI;
