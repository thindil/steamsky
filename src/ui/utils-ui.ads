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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C; use Interfaces.C;
with CArgv; use CArgv;
with Tcl.Ada;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Config; use Config;

-- ****h* Utils/UUI
-- FUNCTION
-- Provide various code for GUI
-- SOURCE
package Utils.UI is
-- ****

   --## rule off TYPE_INITIAL_VALUES
   -- ****t* UUI/UUI.Travel_Array
   -- FUNCTION
   -- The values returned by Travel_Info function: ETA and fuel usage
   -- SOURCE
   type Travel_Array is array(1 .. 2) of Natural;
   -- ****
   --## rule on TYPE_INITIAL_VALUES

   -- ****t* UUI/UUI.CreateCommands
   -- FUNCTION
   -- Used to add new Tcl commands to interpreter
   -- SOURCE
   package CreateCommands is new Tcl.Ada.Generic_Command
     (ClientData => Integer);
   -- ****

   -- ****o* UUI/UUI.Show_On_Map_Command
   -- FUNCTION
   -- Show the selected point on map
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowOnMap X Y
   -- X is the x coordinate of point to show, Y is the y coordinate of point
   -- to show
   -- SOURCE
   function Show_On_Map_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "showOnMapCommand";
      -- ****

      -- ****o* UUI/UUI.Set_Destination_Command
      -- FUNCTION
      -- Set the selected map point as the player's ship destination
      -- PARAMETERS
      -- Client_Data - Custom data send to the command. Unused
      -- Interp      - Tcl interpreter in which command was executed.
      -- Argc        - Number of arguments passed to the command. Unused
      -- Argv        - Values of arguments passed to the command.
      -- RESULT
      -- This function always return TCL_OK
      -- COMMANDS
      -- SetDestination X Y
      -- X is the x coordinate of point to set, Y is the y coordinate of point
      -- to set
      -- SOURCE
   function Set_Destination_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

      -- ****f* UUI/UUI.Add_Command
      -- FUNCTION
      -- Add the selected command to Tcl interpreter
      -- PARAMETERS
      -- Name        - The name of the command which will be used in Tcl
      -- Ada_Command - Ada function which will be invoked
      -- SOURCE
   procedure Add_Command
     (Name: String; Ada_Command: not null CreateCommands.Tcl_CmdProc) with
      Pre => Name'Length > 0;
      -- ****

      -- ****f* UUI/UUI.Add_Commands
      -- FUNCTION
      -- Add various, UI related Tcl commands
      -- SOURCE
   procedure Add_Commands;
   -- ****

   -- ****f* UUI/UUI.Minutes_To_Date
   -- FUNCTION
   -- Convert minutes to game date and add it to text
   -- PARAMETERS
   -- Minutes  - Amount of minutes to convert
   -- Info_Text - Text to which time info will be added
   -- RESULT
   -- Parameter InfoText
   -- SOURCE
   procedure Minutes_To_Date
     (Minutes: Natural; Info_Text: in out Unbounded_String);
   -- ****

   -- ****f* UUI/UUI.Travel_Info
   -- FUNCTION
   -- Count the ETA and fuel usage for the selected distance
   -- PARAMETERS
   -- Distance - Distance in map fields to destination point
   -- RESULT
   -- The array with two values, the first is estimated time to travel the
   -- distance, the second is the amount of fuel needed to travel the distance.
   -- HISTORY
   -- 9.1 - Changed into function and removed parameters
   -- SOURCE
   function Travel_Info(Distance: Positive) return Travel_Array;
   -- ****

   -- ****f* UUI/UUI.Update_Messages
   -- FUNCTION
   -- Update game messages
   -- SOURCE
   procedure Update_Messages;
   -- ****

   -- ****f* UUI/UUI.Show_Screen
   -- FUNCTION
   -- Remove an old screen from the window and add a new to it
   -- PARAMETERS
   -- NewScreenName - Part of th name of the new Ttk_Frame to add
   -- SOURCE
   procedure Show_Screen(New_Screen_Name: String) with
      Pre => New_Screen_Name'Length > 0;
      -- ****

      -- ****f* UUI/UUI.Delete_Widgets
      -- FUNCTION
      -- Remove widgets from the selected frame
      -- PARAMETERS
      -- Start_Index - The first row from which widgets will be removed
      -- End_Index   - The last row in which widgets will be removed
      -- Frame       - The fram from which widgets will be removed
      -- HISTORY
      -- 5.9 - Added
      -- SOURCE
   procedure Delete_Widgets
     (Start_Index, End_Index: Integer; Frame: Tk_Widget'Class);
      -- ****

   -- ****f* UUI/UUI.Set_Fonts
   -- FUNCTION
   -- Set all the game fonts to the selected size
   -- PARAMETERS
   -- New_Size  - The new size of the selected font's type
   -- Font_Type - The type of the font
   -- HISTORY
   -- 7.1 - Added
   -- SOURCE
   procedure Set_Fonts(New_Size: Positive; Font_Type: Font_Types);
   -- ****

end Utils.UI;
