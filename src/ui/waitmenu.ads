-- Copyright (c) 2020-2024 Bartek thindil Jasicki
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

-- ****h* WaitMenu/WaitMenu
-- FUNCTION
-- Provide code for wait orders
-- SOURCE
package WaitMenu is
-- ****

   -- ****f* WaitMenu/WaitMenu.Add_Commands
   -- FUNCTION
   -- Add Tcl commands related to the waiting
   -- SOURCE
   procedure Add_Commands with
      Import => True,
      Convention => C,
      External_Name => "addAdaWaitCommands";
   -- ****

end WaitMenu;
