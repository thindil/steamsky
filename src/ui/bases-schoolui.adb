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
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with CArgv; use CArgv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Canvas; use Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkPanedWindow; use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Bases.Trade; use Bases.Trade;
with CoreUI; use CoreUI;
with Dialogs; use Dialogs;
with Maps.UI; use Maps.UI;
with Trades; use Trades;
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
      Convention => C;
      -- ****

   function Set_School_Skills_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc, Argv);
      use Tiny_String;

      Frame_Name: constant String := Main_Paned & ".schoolframe.canvas.school";
      Combo_Box: Ttk_ComboBox :=
        Get_Widget(pathName => Frame_Name & ".setting.crew", Interp => Interp);
      Member_Index: constant Positive := Natural'Value(Current(ComboBox => Combo_Box)) + 1;
      Combo_List, Old_Combo_List: Unbounded_String;
      Spin_Box: constant Ttk_SpinBox :=
        Get_Widget(pathName => Frame_Name & ".amountbox.amount", Interp => Interp);
      Skill_Level: Skill_Range;
   begin
      Add_Skills_Loop :
      for I in 1 .. Skills_Amount loop
         Skill_Level := 0;
         Find_Skill_Loop:
         for Skill of Player_Ship.Crew(Member_Index).Skills loop
            if Skill.Index = I then
               Skill_Level := Skill.Level;
               if Skill.Level = 100 then
                  goto End_Of_Add_Skills_Loop;
               end if;
            end if;
         end loop Find_Skill_Loop;
         Append
           (Source => Combo_List,
            New_Item => " {" &
            To_String(Source => SkillsData_Container.Element(Container => Skills_List, Index => I).Name) &
            ": " &
            (if Skill_Level = 0 then "Untrained"
             else Trim(Source => Get_Skill_Level_Name(Skill_Level => Skill_Level), Side => Left)) &
            "}");
         <<End_Of_Add_Skills_Loop>>
      end loop Add_Skills_Loop;
      Combo_Box := Get_Widget(pathName => Frame_Name & ".setting.skill");
      Old_Combo_List := To_Unbounded_String(Source => cget(Widgt => Combo_Box, option => "-values"));
      if Length(Source => Old_Combo_List) + 1 /= Length(Source => Combo_List) then
         configure(Widgt => Combo_Box, options => "-values [list" & To_String(Source => Combo_List) & "]");
         Current(ComboBox => Combo_Box, NewIndex => "0");
         Set(SpinBox => Spin_Box, Value => "1");
      else
         Update_Header;
      end if;
      Tcl_Eval(interp => Interp, strng => "UpdateSchoolCost " & Spin_Box & " " & Get(Widgt => Spin_Box));
      Tcl_Eval(interp => Interp, strng => "UpdateSchoolSelectedCost");
      return TCL_OK;
   end Set_School_Skills_Command;

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
      use Tiny_String;

      School_Frame: Ttk_Frame :=
        Get_Widget(pathName => Main_Paned & ".schoolframe", Interp => Interp);
      School_Canvas: constant Tk_Canvas :=
        Get_Widget(pathName => School_Frame & ".canvas", Interp => Interp);
      Combo_Box: constant Ttk_ComboBox :=
        Get_Widget(pathName => School_Canvas & ".school.setting.crew", Interp => Interp);
      Combo_List: Unbounded_String := Null_Unbounded_String;
      Money_Label: constant Ttk_Label :=
        Get_Widget(pathName => School_Canvas & ".school.money", Interp => Interp);
      Money_Index_2: Natural;
   begin
      if Winfo_Get(Widgt => School_Canvas, Info => "exists") = "0" then
         Tcl_EvalFile
           (interp => Get_Context,
            fileName => To_String(Source => Data_Directory) & "ui" & Dir_Separator & "school.tcl");
         Bind(Widgt => School_Frame, Sequence => "<Configure>", Script => "{ResizeCanvas %W.canvas %w %h}");
      elsif Winfo_Get(Widgt => School_Canvas, Info => "ismapped") = "1" and Argc = 1 then
         Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Close_Button);
         Show_Sky_Map(Clear => True);
         return TCL_OK;
      end if;
      Tcl_SetVar(interp => Interp, varName => "gamestate", newValue => "crew");
      Money_Index_2 := Find_Item(Inventory => Player_Ship.Cargo, Proto_Index => Money_Index);
      if Money_Index_2 > 0 then
         configure
           (Widgt => Money_Label,
            options => "-text {You have" &
            Natural'Image
              (Inventory_Container.Element
                 (Container => Player_Ship.Cargo, Index => Money_Index_2)
                 .Amount) &
            " " & To_String(Source => Money_Name) & ".}");
      else
         configure
           (Money_Label,
            "-text {You don't have any " & To_String(Money_Name) &
            " to pay for learning.}");
      end if;
      School_Frame.Name := New_String(School_Canvas & ".school");
      if Argc = 1 then
         Add_Crew_Loop :
         for Member of Player_Ship.Crew loop
            Append(Combo_List, " " & To_String(Source => Member.Name));
         end loop Add_Crew_Loop;
         configure(Combo_Box, "-values [list" & To_String(Combo_List) & "]");
         Current(Combo_Box, "0");
      end if;
      if Set_School_Skills_Command(Client_Data, Interp, Argc, Argv) /=
        TCL_OK then
         return TCL_ERROR;
      end if;
      Tcl.Tk.Ada.Grid.Grid(Close_Button, "-row 0 -column 1");
      configure
        (School_Canvas,
         "-height [expr " & SashPos(Main_Paned, "0") & " - 20] -width " &
         cget(Main_Paned, "-width"));
      Tcl_Eval(Get_Context, "update");
      Canvas_Create
        (School_Canvas, "window", "0 0 -anchor nw -window " & School_Frame);
      Tcl_Eval(Get_Context, "update");
      configure
        (School_Canvas,
         "-scrollregion [list " & BBox(School_Canvas, "all") & "]");
      Show_Screen("schoolframe");
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
        Get_Widget(Main_Paned & ".schoolframe.canvas.school.setting.crew");
      MemberIndex: Positive := 1;
   begin
      for Member of Player_Ship.Crew loop
         exit when Member.Name = To_Bounded_String(Get(Member_Box));
         MemberIndex := MemberIndex + 1;
      end loop;
      return MemberIndex;
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
        Get_Widget(Main_Paned & ".schoolframe.canvas.school.setting.skill");
      SkillIndex: Positive := 1;
      ComboBoxValue: constant String := Get(Skill_Box);
      SkillName: constant Bounded_String :=
        Bounded_Slice
          (To_Bounded_String(ComboBoxValue), 1, Index(ComboBoxValue, ":") - 1);
   begin
      for I in 1 .. Skills_Amount loop
         exit when SkillsData_Container.Element(Skills_List, I).Name =
           SkillName;
         SkillIndex := SkillIndex + 1;
      end loop;
      return SkillIndex;
   end Get_Skill_Index;

   -- ****o* SchoolUI/SchoolUI.Train_Skill_Command
   -- FUNCTION
   -- Train the selected skill
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- TrainSkill
   -- SOURCE
   function Train_Skill_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Train_Skill_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Argc, Argv);
      AmountBox: constant Ttk_SpinBox :=
        Get_Widget
          (Main_Paned & ".schoolframe.canvas.school." &
           Tcl_GetVar(Interp, "traintype") & "box.amount",
           Interp);
   begin
      TrainSkill
        (Get_Member_Index, Skills_Amount_Range(Get_Skill_Index),
         Positive'Value(Get(AmountBox)),
         (if Tcl_GetVar(Interp, "traintype") = "amount" then True else False));
      Update_Messages;
      return
        Show_School_Command
          (ClientData, Interp, 2,
           CArgv.Empty & "TrainSkill" &
           Trim(Positive'Image(Get_Member_Index), Left));
   exception
      when Trade_No_Money =>
         Show_Message
           (Text =>
              "You don't have any " & To_String(Money_Name) &
              " to pay for learning.",
            Title => "Can't train");
         return TCL_OK;
      when Trade_Not_Enough_Money =>
         Show_Message
           (Text =>
              "You don't have enough " & To_String(Money_Name) &
              " to pay for learning this skill.",
            Title => "Can't train");
         return TCL_OK;
   end Train_Skill_Command;

   -- ****o* SchoolUI/SchoolUI.Update_School_Cost_Command
   -- FUNCTION
   -- Update the cost of training
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- UpdateSchoolCost combobox amount
   -- Combobox is the Tk path to the ttk::combobox with the amount of
   -- training sessions, amount is the amount of the requested training
   -- sessions
   -- SOURCE
   function Update_School_Cost_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Update_School_Cost_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      ComboBox: constant Ttk_ComboBox :=
        Get_Widget(CArgv.Arg(Argv, 1), Interp);
      Label: constant Ttk_Label :=
        Get_Widget(Winfo_Get(ComboBox, "parent") & ".cost", Interp);
      Amount, Cost: Natural := 0;
   begin
      Amount := Natural'Value(CArgv.Arg(Argv, 2));
      if Amount < 1 then
         Amount := 1;
      elsif Amount > 100 then
         Amount := 100;
      end if;
      Cost :=
        TrainCost(Get_Member_Index, Skills_Amount_Range(Get_Skill_Index)) *
        Amount;
      configure
        (Label,
         "-text {" & Positive'Image(Cost) & " " & To_String(Money_Name) & "}");
      Tcl_SetResult(Interp, "1");
      return TCL_OK;
   exception
      when Constraint_Error =>
         Tcl_SetResult(Interp, "0");
         return TCL_OK;
   end Update_School_Cost_Command;

   -- ****o* SchoolUI/SchoolUI.Update_School_Selected_Cost_Command
   -- FUNCTION
   -- Update the minimal and maximum values of spinbox with training cost
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- UpdateSchoolSelectedCost
   -- SOURCE
   function Update_School_Selected_Cost_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Update_School_Selected_Cost_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      AmountBox: constant Ttk_SpinBox :=
        Get_Widget
          (Main_Paned & ".schoolframe.canvas.school.costbox.amount", Interp);
      MoneyIndex2: constant Natural :=
        Find_Item(Player_Ship.Cargo, Money_Index);
      Cost: constant Natural :=
        TrainCost(Get_Member_Index, Skills_Amount_Range(Get_Skill_Index));
   begin
      if MoneyIndex2 > 0 and Cost > 0 then
         configure
           (AmountBox,
            "-from" & Positive'Image(Cost) & " -to" &
            Positive'Image
              (Inventory_Container.Element
                 (Container => Player_Ship.Cargo, Index => MoneyIndex2)
                 .Amount));
         Bind
           (AmountBox, "<<Increment>>",
            "{" & AmountBox & " set [expr [" & AmountBox & " get] +" &
            Positive'Image(Cost) & " - 1]}");
         Bind
           (AmountBox, "<<Decrement>>",
            "{" & AmountBox & " set [expr [" & AmountBox & " get] -" &
            Positive'Image(Cost) & " + 1]}");
      else
         configure(AmountBox, "-from 0 -to 0");
         Unbind(AmountBox, "<<Increment>>");
         Unbind(AmountBox, "<<Decrement>>");
      end if;
      Set(AmountBox, Natural'Image(Cost));
      return TCL_OK;
   end Update_School_Selected_Cost_Command;

   procedure Add_Commands is
   begin
      Add_Command("ShowSchool", Show_School_Command'Access);
      Add_Command("TrainSkill", Train_Skill_Command'Access);
      Add_Command("SetSchoolSkills", Set_School_Skills_Command'Access);
      Add_Command("UpdateSchoolCost", Update_School_Cost_Command'Access);
      Add_Command
        ("UpdateSchoolSelectedCost",
         Update_School_Selected_Cost_Command'Access);
   end Add_Commands;

end Bases.SchoolUI;
