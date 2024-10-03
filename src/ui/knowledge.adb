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

with Interfaces.C;
with CArgv;
with Tcl;
with Utils.UI;

package body Knowledge is

   -- ****o* Knowledge/Knowledge.Show_Knowledge_Command
   -- FUNCTION
   -- Show information about known by player things
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command.
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowKnowledge
   -- SOURCE
   function Show_Knowledge_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "showKnowledgeCommand";
      -- ****

   -- ****o* Knowledge/Knowledge.Knowledge_Max_Min_Command
   -- FUNCTION
   -- Maximize or minimize the selected section of knowledge info
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- KnowledgeMaxMin framename
   -- Framename is name of the frame to maximize or minimize
   -- SOURCE
   function Knowledge_Max_Min_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "knowledgeMaxMinCommand";
      -- ****

   procedure Add_Commands is
      use Utils.UI;
   begin
      Add_Command
        (Name => "ShowKnowledge",
         Ada_Command => Show_Knowledge_Command'Access);
      Add_Command
        (Name => "KnowledgeMaxMin",
         Ada_Command => Knowledge_Max_Min_Command'Access);
   end Add_Commands;

end Knowledge;
