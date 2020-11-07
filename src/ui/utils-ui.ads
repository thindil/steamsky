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
with Ships; use Ships;

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

   -- ****f* UUI/MinutesToDate
   -- FUNCTION
   -- Convert minutes to game date and add it to text
   -- PARAMETERS
   -- Minutes  - Amount of minutes to convert
   -- InfoText - Text to which time info will be added
   -- RESULT
   -- Parameter InfoText
   -- SOURCE
   procedure MinutesToDate
     (Minutes: Natural; InfoText: in out Unbounded_String);
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

   -- ****f* UUI/ShowScreen
   -- FUNCTION
   -- Remove an old screen from the window and add a new to it
   -- PARAMETERS
   -- NewScreenName - Part of th name of the new Ttk_Frame to add
   -- SOURCE
   procedure ShowScreen(NewScreenName: String);
   -- ****

   -- ****f* UUI/ShowInventoryItemInfo
   -- FUNCTION
   -- Show info about selected item in ship cargo or crew member inventory
   -- PARAMETERS
   -- Parent      - The name of the parent widget
   -- ItemIndex   - Index of item (can be inventory or ship cargo)
   -- MemberIndex - If item is in crew member inventory, crew index of member,
   --               otherwise 0
   -- SOURCE
   procedure ShowInventoryItemInfo
     (Parent: String; ItemIndex: Positive; MemberIndex: Natural) with
      Pre => MemberIndex <= PlayerShip.Crew.Last_Index;
      -- ****

      -- ****f* UUI/ShowInfo
      -- FUNCTION
      -- Show the selected info to a player
      -- PARAMETERS
      -- Text       - Text of info to show
      -- ParentName - Name of the parent widget
      -- SOURCE
   procedure ShowInfo(Text, ParentName: String);
   -- ****

end Utils.UI;
