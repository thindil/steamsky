-- Copyright (c) 2021-2024 Bartek thindil Jasicki
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
with Tcl.Tk.Ada.Widgets.Canvas; use Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.TtkScrollbar; use Tcl.Tk.Ada.Widgets.TtkScrollbar;

-- ****h* Table/Table
-- FUNCTION
-- Provides code for create and manipulate more advanced table widget
-- SOURCE
package Table is
-- ****

   -- ****t* Table/Table.Width_Array
   -- FUNCTION
   -- Used to store width of Table_Widget columns
   -- SOURCE
   type Width_Array is array(Positive range <>) of Positive;
   -- ****

   --## rule off REDUCEABLE_SCOPE
   -- ****d* Table/Table.No_Width_Array
   -- FUNCTION
   -- Empty table width array
   -- SOURCE
   No_Width_Array: constant Width_Array := (1 => 1);
   -- ****
   --## rule on REDUCEABLE_SCOPE

   -- ****s* Table/Table.Table_Widget
   -- FUNCTION
   -- Store data for each created table
   -- PARAMETERS
   -- Canvas             - Tk_Canvas which is used as table
   -- Columns_Width      - The array with the width for each column in the table
   -- Row                - The current row of the table
   -- Row_Height         - The height of each row
   -- Scrollbar          - The vertical Ttk_Scrollbar associated with the table
   -- SOURCE
   type Table_Widget(Amount: Positive) is record
      Canvas: Tk_Canvas;
      Columns_Width: Width_Array(1 .. Amount) := (others => 1);
      Row: Positive := 1;
      Row_Height: Positive := 1;
      Scrollbar: Ttk_Scrollbar;
   end record;
   -- ****

   --## rule off REDUCEABLE_SCOPE
   -- ****d* Table/Table.Empty_Table
   -- FUNCTION
   -- Empty table widget with default values and one column
   -- SOURCE
   Empty_Table: constant Table_Widget := (Amount => 1, others => <>);
   -- ****
   --## rule on REDUCEABLE_SCOPE

   -- ****t* Table/Table.Headers_Array
   -- FUNCTION
   -- Used to store the titles for columns in the selected table
   -- SOURCE
   type Headers_Array is array(Positive range <>) of Unbounded_String;
   -- ****

   --## rule off REDUCEABLE_SCOPE
   -- ****d* Table/Table.No_Headers
   -- FUNCTION
   -- Array of headers with one empty header
   -- SOURCE
   No_Headers: constant Headers_Array := (1 => Null_Unbounded_String);
   -- ****
   --## rule on REDUCEABLE_SCOPE

   -- ****f* Table/Table.Create_Table
   -- FUNCTION
   -- Create a new table and columns headers in it
   -- PARAMETERS
   -- Parent    - The Tk path for the parent widget
   -- Headers   - The titles for the table headers
   -- Scrollbar - Ttk_Scrollbar associated with the table. If empty
   --             then create a new scrollbars. Default value is empty.
   -- Command   - The Tcl command executed when the player press the table
   --             header. If empty, no command is executed. Default value is
   --             empty
   -- Tooltip   - The tooltip show when the player hover mouse over the table
   --             header. Can be empty. Default value is empty
   -- RESULT
   -- The newly created Table_Widget
   -- HISTORY
   -- 5.7 - Added
   -- 6.4 - Added Command parameter
   -- 7.1 - Renamed to Create_Table
   -- SOURCE
   function Create_Table
     (Parent: String; Headers: Headers_Array;
      Scrollbar: Ttk_Scrollbar := Get_Widget(pathName => ".");
      Command, Tooltip_Text: String := "") return Table_Widget with
      Pre => Parent'Length > 0 and Headers'Length > 0,
      Post => Create_Table'Result.Row_Height > 1;
      -- ****

      --## rule off LOCAL_HIDING
      -- ****f* Table/Table.Clear_Table
      -- FUNCTION
      -- Clear data from the table
      -- PARAMETERS
      -- Table - The Table_Widget which will be cleared
      -- OUTPUT
      -- Cleared Table parameter Table_Widget
      -- HISTORY
      -- 5.7 - Added
      -- 7.1 - Renamed to Clear_Table
      -- SOURCE
   procedure Clear_Table(Table: in out Table_Widget) with
      Pre => Table.Row_Height > 1;
      -- ****

      -- ****f* Table/Table.Add_Button
      -- FUNCTION
      -- Add button item to the selected Table_Widget
      -- PARAMETERS
      -- Table    - The Table_Widget in which button will be added
      -- Text     - The text displayed on the button
      -- Tooltip  - The tooltip show when user hover mouse over button
      -- Command  - Tcl command which will be executed when button was clicked
      -- Column   - The column in which the button will be added
      -- New_Row  - If True, increase current number of row in the Table_Widget.
      --            Default value is False.
      -- Color    - The color of the text on button which will be added. If empty,
      --            use default interface color. Default value is empty.
      -- OUTPUT
      -- Updated Table parameter Table_Widget
      -- HISTORY
      -- 5.7 - Added
      -- 7.1 - Renamed to Add_Button
      -- SOURCE
   procedure Add_Button
     (Table: in out Table_Widget; Text, Tooltip, Command: String;
      Column: Positive; New_Row: Boolean := False; Color: String := "") with
      Pre => Table.Row_Height > 1 and Command'Length > 0;
   -- ****

      -- ****f* Table/Table.Update_Table
      -- FUNCTION
      -- Update size and coordinates of all elements in the selected table
      -- PARAMETERS
      -- Table      - The Table_Widget which elements will be resized if needed
      -- Grab_Focus - If true, grab the keyboard focus for the table
      -- HISTORY
      -- 5.7 - Added
      -- 6.7 - Added option to set focus on table
      -- 7.1 - Renamed to Update_Table
      -- SOURCE
   procedure Update_Table
     (Table: Table_Widget; Grab_Focus: Boolean := True) with
      Pre => Table.Row_Height > 1;
      -- ****

      -- ****f* Table/Table.Add_Progress_Bar
      -- FUNCTION
      -- Add progress bar item to the selected Table_Widget
      -- PARAMETERS
      -- Table         - The Table_Widget in which progress bar will be added
      -- Value         - The current value of the progress bar
      -- Max_Value     - The maximum value of the progress bar
      -- Tooltip       - The tooltip show when user hover mouse over progress bar
      -- Command       - Tcl command which will be executed when the row in which the
      --                 the progress bar is was clicked
      -- Column        - The column in which the progress bar will be added
      -- New_Row       - If True, increase current number of row in the Table_Widget.
      --                 Default value is False.
      -- Invert_Colors - Invert colors of the progress bar (small amount green, max
      --                 red instead of small amount red and max green)
      -- OUTPUT
      -- Updated Table parameter Table_Widget
      -- HISTORY
      -- 5.7 - Added
      -- 7.1 - Renamed to Add_Progress_Bar
      -- SOURCE
   procedure Add_Progress_Bar
     (Table: in out Table_Widget; Value: Natural; Max_Value: Positive;
      Tooltip, Command: String; Column: Positive;
      New_Row, Invert_Colors: Boolean := False) with
      Pre => Table.Row_Height > 1 and Value <= Max_Value;
      -- ****

      -- ****f* Table/Table.Add_Pagination
      -- FUNCTION
      -- Add pagination buttons to the bottom of the table
      -- PARAMETERS
      -- Table            - The Table_Widget to which buttons will be added
      -- Previous_Command - The Tcl command which will be executed by the previous
      --                    button. If empty, button will not be shown. Default
      --                    value is empty
      -- Next_Command     - The Tcl command which will be executed by the next
      --                    button. If empty, button will not be shown. Default
      --                    value is empty
      -- HISTORY
      -- 5.9 - Added
      -- 7.1 - Renamed to Add_Pagination
      -- 7.6 - Added default values for Previous_Command and Next_Command
      -- SOURCE
   procedure Add_Pagination
     (Table: Table_Widget; Previous_Command, Next_Command: String := "") with
      Pre => Table.Row_Height > 1;
      -- ****

      -- ****f* Table/Table.Get_Column_Number
      -- FUNCTION
      -- Get the number of the Table_Widget column for the selected X axis
      -- coordinate
      -- PARAMETERS
      -- Table      - The Table_Widget which column will be taken
      -- X_Position - The X axis coordinate from which the column will be count
      -- RESULT
      -- The number of the column for the selected coordinate. The number starts
      -- from 1.
      -- HISTORY
      -- 6.4 - Added
      -- SOURCE
   function Get_Column_Number
     (Table: Table_Widget; X_Position: Natural) return Positive;
     -- ****

   -- ****f* Table/Tabel.Add_Commands
   -- FUNCTION
   -- Add Tcl commands related to the Table_Widget
   -- HISTORY
   -- 6.6 - Added
   -- 7.1 - Renamed to Add_Commands
   -- SOURCE
   procedure Add_Commands;
   -- ****

-- Temporary code to interact with Nim

   --## rule off TYPE_INITIAL_VALUES
   type Nim_Width is array(0 .. 10) of Integer;
   --## rule on TYPE_INITIAL_VALUES

end Table;
