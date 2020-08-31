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

-- ****h* SUI2/SUModules
-- FUNCTION
-- Provide code to show information about the player ship modules
-- SOURCE
package Ships.UI.Modules is
-- ****

   -- ****f* SUModules/ShowModuleOptions
   -- FUNCTION
   -- Show available options for the selected module
   -- PARAMETERS
   -- ModuleIndex - Index of the player ship module which options will be show
   -- SOURCE
   procedure ShowModuleOptions(ModuleIndex: Positive);
   -- ****

   -- ****f* SUModules/AddCommands
   -- FUNCTION
   -- Add Tcl commands related to the player's ship modules information
   -- SOURCE
   procedure AddCommands;
   -- ****

end Ships.UI.Modules;
