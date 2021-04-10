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

with Tcl.Tk.Ada.Widgets.TtkPanedWindow; use Tcl.Tk.Ada.Widgets.TtkPanedWindow;

-- ****h* CoreUI/CoreUI
-- FUNCTION
-- Provide some constants and variables common for the game UI
-- SOURCE
package CoreUI is
-- ****

   -- ****d* CoreUI/CoreUI.Paned
   -- FUNCTION
   -- The main Tk paned widget of the game
   -- SOURCE
   Main_Paned: Ttk_PanedWindow;
   -- ****

end CoreUI;
