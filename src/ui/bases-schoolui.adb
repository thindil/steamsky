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

with Interfaces.C; use Interfaces.C;
with CArgv; use CArgv;
with Tcl; use Tcl;
with Utils.UI;

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
      Import => True,
      Convention => C,
      External_Name => "updateSchoolCostCommand";
      -- ****

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
      Import => True,
      Convention => C,
      External_Name => "updateSchoolSelectedCostCommand";
      -- ****

   procedure Add_Commands is
      use Utils.UI;
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
