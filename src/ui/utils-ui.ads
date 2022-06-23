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

with Ada.Containers; use Ada.Containers;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C; use Interfaces.C;
with CArgv; use CArgv;
with Tcl.Ada;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Config; use Config;
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
      Convention => C;
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
     (Minutes: Natural; Info_Text: in out Unbounded_String) with
      Global => null,
      SPARK_Mode;
   -- ****

   -- ****f* UUI/UUI.Travel_Info
   -- FUNCTION
   -- Add info about travel eta and approx fuel usage
   -- PARAMETERS
   -- Info_Text      - Text to which info about travel will be added
   -- Distance       - Distance in map fields to destination point
   -- Show_Fuel_Name - If true, add fuel name to info. Default is false
   -- RESULT
   -- Parameter InfoText
   -- SOURCE
   procedure Travel_Info
     (Info_Text: in out Unbounded_String; Distance: Positive;
      Show_Fuel_Name: Boolean := False) with
      SPARK_Mode;
   -- ****

   -- ****f* UUI/UUI.Update_Messages
   -- FUNCTION
   -- Update game messages
   -- SOURCE
   procedure Update_Messages with
      SPARK_Mode;
   -- ****

   -- ****f* UUI/UUI.Show_Screen
   -- FUNCTION
   -- Remove an old screen from the window and add a new to it
   -- PARAMETERS
   -- NewScreenName - Part of th name of the new Ttk_Frame to add
   -- SOURCE
   procedure Show_Screen(New_Screen_Name: String) with
      SPARK_Mode,
      Pre => New_Screen_Name'Length > 0;
      -- ****

      -- ****f* UUI/UUI.Show_Inventory_Item_Info
      -- FUNCTION
      -- Show info about selected item in ship cargo or crew member inventory
      -- PARAMETERS
      -- Parent           - The name of the parent widget
      -- Item_Index       - Index of item (can be inventory or ship cargo)
      -- Member_Index     - If item is in crew member inventory, crew index of member,
      --                    otherwise 0
      -- Button_1_Text    - The text displayed on the first optional button. If empty,
      --                    the button will not show. Default value is empty.
      -- Button_1_Command - The command for the first optional button. Has meaning
      --                    only if Button_1_Text is set. Default value is empty.
      -- Button_1_Icon    - The image to show on the first optional button. If set,
      --                    Button_1_Text will be used as tooltip. Default value is
      --                    empty.
      -- Button_2_Text    - The text displayed on the second optional button. If empty,
      --                    the button will not show. Default value is empty.
      -- Button_2_Command - The command for the first optional button. Has meaning
      --                    only if Button_2_Text is set. Default value is empty.
      -- Button_2_Icon    - The image to show on the second optional button. If set,
      --                    Button_2_Text will be used as tooltip. Default value is
      --                    empty.
      -- SOURCE
   procedure Show_Inventory_Item_Info
     (Parent: String; Item_Index: Positive; Member_Index: Natural;
      Button_1_Text, Button_1_Command, Button_1_Icon, Button_2_Text,
      Button_2_Command, Button_2_Icon: String := "") with
      SPARK_Mode,
      Pre => Member_Index <= Player_Ship.Crew.Last_Index and Parent'Length > 0;
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
     (Start_Index, End_Index: Integer; Frame: Tk_Widget'Class) with
      SPARK_Mode;
      -- ****

      -- ****f* UUI/UUI.Get_Skill_Marks
      -- FUNCTION
      -- Get the marks for the selected skill for the selected crew member
      -- PARAMETERS
      -- Skill_Index  - The index of the skill which will be checked
      -- Member_Index - The index of the player ship crew member which will be
      --                checked
      -- RESULT
      -- If the crew member don't have the selected skill, empty String, if
      -- have the skill, " +", if the skill is the highest in the crew, " ++"
      -- HISTORY
      -- 6.7 - Added
      -- SOURCE
   function Get_Skill_Marks
     (Skill_Index: Skills_Amount_Range; Member_Index: Positive)
      return String with
      SPARK_Mode,
      Pre => Skill_Index <= Skills_Amount and
      Member_Index <= Player_Ship.Crew.Last_Index;
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
