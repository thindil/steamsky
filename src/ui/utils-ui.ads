-- Copyright (c) 2020-2021 Bartek thindil Jasicki <thindil@laeran.pl>
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
with Tcl.Ada;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Items; use Items;
with Ships; use Ships;

-- ****h* Utils/UUI
-- FUNCTION
-- Provide various code for GUI
-- SOURCE
package Utils.UI is
-- ****

   -- ****t* UUI/UUI.CreateCommands
   -- FUNCTION
   -- Used to add new Tcl commands to interpreter
   -- SOURCE
   package CreateCommands is new Tcl.Ada.Generic_Command(Integer);
   -- ****

   -- ****o* UUI/UUI.Close_Dialog_Command
   -- FUNCTION
   -- Close the selected dialog
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- CloseDialog dialogname
   -- Dialogname is name of the dialog to close
   -- SOURCE
   function Close_Dialog_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

      -- ****f* UUI/UUI.ShowMessage
      -- FUNCTION
      -- Show the selected message to a player
      -- PARAMETERS
      -- Text        - Text of message to show
      -- ParentFrame - The parent frame of the message widget. Default is
      --               the game frame. Can be empty
      -- SOURCE
   procedure ShowMessage
     (Text: String; ParentFrame: String := ".gameframe") with
      Pre => Text'Length > 0 and ParentFrame'Length > 0;
   -- ****

   -- ****f* UUI/UUI.AddCommand
   -- FUNCTION
   -- Add the selected command to Tcl interpreter
   -- PARAMETERS
   -- Name       - The name of the command which will be used in Tcl
   -- AdaCommand - Ada function which will be invoked
   -- SOURCE
   procedure AddCommand
     (Name: String; AdaCommand: not null CreateCommands.Tcl_CmdProc) with
      Pre => Name'Length > 0;
      -- ****

      -- ****f* UUI/UUI.AddCommands
      -- FUNCTION
      -- Add various, UI related Tcl commands
      -- SOURCE
   procedure AddCommands;
   -- ****

   -- ****f* UUI/UUI.MinutesToDate
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

   -- ****f* UUI/UUI.TravelInfo
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

   -- ****f* UUI/UUI.UpdateMessages
   -- FUNCTION
   -- Update game messages
   -- SOURCE
   procedure UpdateMessages;
   -- ****

   -- ****f* UUI/UUI.ShowScreen
   -- FUNCTION
   -- Remove an old screen from the window and add a new to it
   -- PARAMETERS
   -- NewScreenName - Part of th name of the new Ttk_Frame to add
   -- SOURCE
   procedure ShowScreen(NewScreenName: String) with
      Pre => NewScreenName'Length > 0;
   -- ****

   -- ****f* UUI/UUI.ShowInventoryItemInfo
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
      Pre => MemberIndex <= PlayerShip.Crew.Last_Index and Parent'Length > 0;
      -- ****

      -- ****f* UUI/UUI.ShowInfo
      -- FUNCTION
      -- Show the selected info to a player
      -- PARAMETERS
      -- Text       - Text of info to show
      -- ParentName - Name of the parent widget. If empty, then the main game
      --              window will be used as parent for widget. Default value
      --              is .gameframe
      -- SOURCE
   procedure ShowInfo(Text: String; ParentName: String := ".gameframe") with
      Pre => Text'Length > 0 and ParentName'Length > 0;
      -- ****

      -- ****f* UUI/UUI.ShowManipulateItem
      -- FUNCTION
      -- Show the dialog for manipulate items amount in cargo (like selling,
      -- dropping, etc).
      -- PARAMETERS
      -- Title     - Title of the dialog
      -- Command   - Tcl command which will be executed when the player hit
      --             the button Ok
      -- Action    - The name of action which the player is doing (like drop,
      --             sell, ect)
      -- ItemIndex - The index of the item which will be manipulated
      -- MaxAmount - Max amount of the items to manipualate. If zero, use max
      --             amount of items from player ship cargo. Default value is
      --             zero.
      -- SOURCE
   procedure ShowManipulateItem
     (Title, Command, Action: String;
      ItemIndex: Inventory_Container.Extended_Index;
      MaxAmount: Natural := 0) with
      Pre => Title'Length > 0 and Command'Length > 0;
      -- ****

      -- ****f* UUI/UUI.ShowQuestion
      -- FUNCTION
      -- Show the dialog with question to the player
      -- PARAMETERS
      -- Question - The question which will be the player asked for
      -- Result   - The value set for Ok button
      -- In_Game  - The question in show during game
      -- HISTORY
      -- 5.9 - Added
      -- SOURCE
   procedure ShowQuestion
     (Question, Result: String; In_Game: Boolean := True) with
      Pre => Question'Length > 0;
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
     (Start_Index: Natural; End_Index: Positive; Frame: Tk_Widget'Class) with
     Pre => Start_Index < End_Index;
   -- ****

end Utils.UI;
