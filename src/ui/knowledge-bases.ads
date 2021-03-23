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

-- ****h* Knowledge/KBases
-- FUNCTION
-- Provide code to show the list of known bases to the player
-- SOURCE
package Knowledge.Bases is
-- ****

   -- ****f* KBases/KBases.UpdateBasesList
   -- FUNCTION
   -- Update and show list of known bases
   -- PARAMETERS
   -- BaseName    - Name of the base to find on list
   -- Start_Index - Base index from which start showing bases
   -- SOURCE
   procedure UpdateBasesList(BaseName: String := ""; Start_Index: Positive := 1);
   -- ****

   -- ****f* KBases/KBases.AddCommands
   -- FUNCTION
   -- Add Tcl commands related to the list of known bases
   -- SOURCE
   procedure AddCommands;
   -- ****

end Knowledge.Bases;
