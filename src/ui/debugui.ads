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

-- ****h* DebugUI/DebugUI
-- FUNCTION
-- Provide code to modify the game data
-- SOURCE
package DebugUI is
-- ****

   -- ****f* DebugUI/DebugUI.Show_Debug_Ui
   -- FUNCTION
   -- Show debug ui to the player
   -- SOURCE
   procedure Show_Debug_Ui with
      Convention => C,
      Import => True,
      External_Name => "showDebugUi";
   -- ****

end DebugUI;
