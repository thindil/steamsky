--    Copyright 2020 Bartek thindil Jasicki
--
--    This file is part of Steam Sky.
--
--    Steam Sky is free software: you can redistribute it and/or modify
--    it under the terms of the GNU General Public License as published by
--    the Free Software Foundation, either version 3 of the License, or
--    (at your option) any later version.
--
--    Steam Sky is distributed in the hope that it will be useful,
--    but WITHOUT ANY WARRANTY; without even the implied warranty of
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--    GNU General Public License for more details.
--
--    You should have received a copy of the GNU General Public License
--    along with Steam Sky.  If not, see <http://www.gnu.org/licenses/>.

with Ada.Exceptions; use Ada.Exceptions;
with Gtk.Dialog; use Gtk.Dialog;
with Gtk.Window; use Gtk.Window;

-- ****h* Steamsky/ErrorDialog
-- FUNCTION
-- Provide code for handle exceptions
-- SOURCE
package ErrorDialog is
-- ****

   -- ****v* ErrorDialog/ErrorDialog
   -- FUNCTION
   -- Gtk dialog used to show information about the crash of the game.
   -- SOURCE
   ErrorDialog: Gtk_Dialog;
   -- ****

   -- ****f* ErrorDialog/CreateErrorUI
   -- FUNCTION
   -- Create error reporting UI
   -- SOURCE
   procedure CreateErrorUI(Parent: Gtk_Window);
   -- ****

   -- ****f* ErrorDialog/SaveException
   -- FUNCTION
   -- Save data exception to file
   -- PARAMETERS
   -- An_Exception    - An exception which was occurred
   -- PrintToTerminal - If true, print info about exception to terminal
   -- SOURCE
   procedure SaveException
     (An_Exception: Exception_Occurrence; PrintToTerminal: Boolean);
   -- ****

   -- ****f* ErrorDialog/On_Exception
   -- FUNCTION
   -- Handle GUI exceptions
   -- PARAMETERS
   -- An_Exception - An exception which was occurred
   -- SOURCE
   procedure On_Exception(An_Exception: Exception_Occurrence);
   -- ****

end ErrorDialog;
