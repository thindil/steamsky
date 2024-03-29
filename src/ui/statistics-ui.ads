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

-- ****h* Statistics/SUI
-- FUNCTION
-- Provide code to show the game statistics to the player
-- SOURCE
package Statistics.UI is
-- ****

   -- ****f* SUI/SUI.Show_Statistics
   -- FUNCTION
   -- Show the game statistics to the player
   -- PARAMETERS
   -- Refresh - If true, refresh the view instead of back to the game map
   -- SOURCE
   procedure Show_Statistics(Refresh: Boolean := False);
   -- ****

   -- ****f* SUI/SUI.Add_Commands
   -- FUNCTION
   -- Add Tcl commands related to the game statistics
   -- SOURCE
   procedure Add_Commands;
   -- ****

end Statistics.UI;
