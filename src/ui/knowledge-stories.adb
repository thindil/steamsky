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
with CArgv; use CArgv;
with Tcl; use Tcl;
with Utils.UI;

package body Knowledge.Stories is

   -- ****o* KStories/KStories.Show_Story_Command
   -- FUNCTION
   -- Show the current story information
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowStory
   -- SOURCE
   function Show_Story_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "showStoryCommand";
      -- ****

   -- ****o* KStories/KStories.Show_Story_Location_Command
   -- FUNCTION
   -- Show the current story event on map
   -- PARAMETERS
   -- Client_Data - Custom data send to the command.
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowStoryLocation
   -- SOURCE
   function Show_Story_Location_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "showStoryLocationCommand";
      -- ****

   -- ****o* KStories/KStories.Set_Story_Command
   -- FUNCTION
   -- Set the current story event as the player's ship destination
   -- PARAMETERS
   -- Client_Data - Custom data send to the command.
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetStory
   -- SOURCE
   function Set_Story_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "setStoryCommand";
      -- ****

   procedure Add_Knowledge_Stories_Commands is
      use Utils.UI;
   begin
      Add_Command
        (Name => "ShowStory", Ada_Command => Show_Story_Command'Access);
      Add_Command
        (Name => "ShowStoryLocation",
         Ada_Command => Show_Story_Location_Command'Access);
      Add_Command(Name => "SetStory", Ada_Command => Set_Story_Command'Access);
   end Add_Knowledge_Stories_Commands;

end Knowledge.Stories;
