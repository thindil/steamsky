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

with Tcl.Ada;

-- ****h* Utils/UUI
-- FUNCTION
-- Provide various code for GUI
-- SOURCE
package Utils.UI is
-- ****

   -- ****t* UUI/CreateCommands
   -- FUNCTION
   -- Used to add new Tcl commands to interpreter
   -- SOURCE
   package CreateCommands is new Tcl.Ada.Generic_Command(Integer);
   -- ****

   -- ****f* UUI/ShowMessage
   -- FUNCTION
   -- Show the selected message to a player
   -- PARAMETERS
   -- Text - Text of message to show
   -- SOURCE
   procedure ShowMessage(Text: String);
   -- ****

   -- ****f* UUI/AddCommand
   -- FUNCTION
   -- Add the selected command to Tcl interpreter
   -- PARAMETERS
   -- Name       - The name of the command which will be used in Tcl
   -- AdaCommand - Ada function which will be invoked
   -- SOURCE
   procedure AddCommand
     (Name: String; AdaCommand: not null CreateCommands.Tcl_CmdProc);
     -- ****

   -- ****f* UUI/AddCommands
   -- FUNCTION
   -- Add various, UI related Tcl commands
   -- SOURCE
   procedure AddCommands;
   -- ****

   -- ****f* UUI/TravelInfo
   -- FUNCTION
   -- Add info about travel eta and approx fuel usage
   -- PARAMETERS
   -- InfoText     - Text to which info about travel will be added
   -- Distance     - Distance in map fields to destination point
   -- ShowFuelName - If true, add fuel name to info. Default is false
   -- RESULT
   -- Parameter InfoText
   -- SOURCE
   procedure TravelInfo
     (InfoText: in out Unbounded_String; Distance: Positive;
      ShowFuelName: Boolean := False);
   -- ****

   -- ****f* UUI/UpdateMessages
   -- FUNCTION
   -- Update game messages
   -- SOURCE
   procedure UpdateMessages;
   -- ****

end Utils.UI;
