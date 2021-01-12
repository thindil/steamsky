-- Copyright (c) 2021 Bartek thindil Jasicki <thindil@laeran.pl>
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

   -- ****s* Table/Table.Table_Widget
   -- FUNCTION
   -- Store data for each created table
   -- PARAMETERS
   -- Canvas        - Tk_Canvas which is used as table
   -- Columns_Width - The array with the width for each column in the table
   -- Row           - The current row of the table
   -- Row_Height    - The height of each row
   -- SOURCE
   type Table_Widget(Amount: Positive) is record
      Canvas: Tk_Canvas;
      Columns_Width: Width_Array(1 .. Amount) := (others => 1);
      Row: Positive := 1;
      Row_Height: Positive := 1;
   end record;
   -- ****

   -- ****t* Table/Table.Headers_Array
   -- FUNCTION
   -- Used to store the titles for columns in the selected table
   -- SOURCE
   type Headers_Array is array(Positive range <>) of Unbounded_String;
   -- ****

   -- ****f* Table/Table.CreateTable
   -- FUNCTION
   -- Create a new table and columns headers in it
   -- PARAMETERS
   -- Parent  - The Tk path for the parent widget
   -- Headers - The titles for the table headers
   -- RESULT
   -- The newly created Table_Widget
   -- HISTORY
   -- 5.7 - Added
   -- SOURCE
   function CreateTable
     (Parent: String; Headers: Headers_Array) return Table_Widget;
   -- ****

     -- ****f* Table/Table.ClearTable
     -- FUNCTION
     -- Clear data from the table
     -- PARAMETERS
     -- Table - The Table_Widget which will be cleared
     -- OUTPUT
     -- Cleared Table parameter Table_Widget
     -- HISTORY
     -- 5.7 - Added
     -- SOURCE
   procedure ClearTable(Table: in out Table_Widget);
   -- ****

end Table;
