--    Copyright 2018-2020 Bartek thindil Jasicki
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

-- ****h* Steamsky/MainMenu
-- FUNCTION
-- Provides code for main menu UI
-- SOURCE
package MainMenu is
-- ****

   -- ****f* MainMenu/CreateMainMenu
   -- FUNCTION
   -- Create main menu and show it
   -- SOURCE
   procedure CreateMainMenu;
   -- ****

   -- ****f* MainMenu/UpdateGoalButton
   -- FUNCTION
   -- Update label on character goal button
   -- PARAMETERS
   -- Message - New label for select goal button
   -- SOURCE
   procedure UpdateGoalButton(Message: String) with
      Pre => Message'Length > 0;
      -- ****

      -- ****f* MainMenu/ShowMainMenu
      -- FUNCTION
      -- Show main menu window
      -- SOURCE
   procedure ShowMainMenu;
   -- ****

end MainMenu;
