-- Copyright (c) 2020-2024 Bartek thindil Jasicki
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

with Ada.Strings;
-- with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C; use Interfaces.C;
with CArgv; use CArgv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
with Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Winfo;
with Bases.Trade; use Bases.Trade;
with CoreUI; use CoreUI;
-- with Dialogs;
-- with Trades;
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
      Import => True,
      Convention => C,
      External_Name => "showSchoolCommand";
      -- ****

   -- ****if* SchoolUI/SchoolUI.Get_Member_Index
   -- FUNCTION
   -- Get the index in the player ship of the currently selected member
   -- RESULT
   -- The index of the currently selected crew member
   -- SOURCE
   function Get_Member_Index return Positive with
      Import => True,
      Convention => C,
      External_Name => "getAdaMemberIndex";
      -- ****

   -- ****if* SchoolUI/SchoolUI.Get_Skill_Index
   -- FUNCTION
   -- Get the index of the currently selected skill
   -- RESULT
   -- The index of the currently selected skill
   -- SOURCE
   function Get_Skill_Index return Positive with
      Import => True,
      Convention => C,
      External_Name => "getAdaSkillIndex";
      -- ****

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
      Import => True,
      Convention => C,
      External_Name => "trainSkillCommand";
      -- ****

--   function Train_Skill_Command
--     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
--      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
--      use Ada.Strings;
--      use Dialogs;
--      use Trades;
--
--      pragma Unreferenced(Argc, Argv);
--      Amount_Box: constant Ttk_SpinBox :=
--        Get_Widget
--          (pathName =>
--             Main_Paned & ".schoolframe.canvas.school." &
--             Tcl_GetVar(interp => Interp, varName => "traintype") &
--             "box.amount",
--           Interp => Interp);
--   begin
--      if Get(Widgt => Amount_Box) = "0" then
--         return TCL_OK;
--      end if;
--      Train_Skill
--        (Member_Index => Get_Member_Index,
--         Skill_Index => Skills_Amount_Range(Get_Skill_Index),
--         Amount => Positive'Value(Get(Widgt => Amount_Box)),
--         Is_Amount =>
--           (if Tcl_GetVar(interp => Interp, varName => "traintype") = "amount"
--            then True
--            else False));
--      Update_Messages;
--      return
--        Show_School_Command
--          (Client_Data => Client_Data, Interp => Interp, Argc => 2,
--           Argv =>
--             CArgv.Empty & "TrainSkill" &
--             Trim(Source => Positive'Image(Get_Member_Index), Side => Left));
--   exception
--      when Trade_No_Money =>
--         Show_Message
--           (Text =>
--              "You don't have any " & To_String(Source => Money_Name) &
--              " to pay for learning.",
--            Title => "Can't train");
--         return TCL_OK;
--      when Trade_Not_Enough_Money =>
--         Show_Message
--           (Text =>
--              "You don't have enough " & To_String(Source => Money_Name) &
--              " to pay for learning this skill.",
--            Title => "Can't train");
--         return TCL_OK;
--   end Train_Skill_Command;

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
      use Tcl.Tk.Ada.Widgets.TtkLabel;
      use Tcl.Tk.Ada.Winfo;

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
