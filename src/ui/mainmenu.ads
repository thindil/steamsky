--    Copyright 2018 Bartek thindil Jasicki
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions; use Ada.Exceptions;

package MainMenu is

   procedure CreateMainMenu; -- Create main menu and show it
   procedure UpdateGoalButton
     (Message: String); -- Update label on character goal button
   procedure ShowMainMenu; -- Show main menu window
   procedure SaveException
     (An_Exception: Exception_Occurrence;
      PrintToTerminal: Boolean); -- Save data exception to file
   procedure On_Exception
     (An_Exception: Exception_Occurrence); -- Handle GUI exceptions

end MainMenu;
