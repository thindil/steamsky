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

-- ****h* Utils.UI/Utils
-- FUNCTION
-- Provide various code for GUI
-- SOURCE
package Utils.UI is
-- ****

   -- ****f* Utils.UI/ShowMessage
   -- FUNCTION
   -- Show the selected message to a player
   -- PARAMETERS
   -- Text - Text of message to show
   -- Icon - Tk message icon name: error, info, question or warning
   -- SOURCE
   procedure ShowMessage(Text, Icon: String);
   -- ****

end Utils.UI;
