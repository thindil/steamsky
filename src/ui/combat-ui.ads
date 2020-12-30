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

-- ****h* Combat/CUI
-- FUNCTION
-- Provide code to show combat UI to the player
-- SOURCE
package Combat.UI is
-- ****

   -- ****f* CUI/CUI.ShowCombatUI
   -- FUNCTION
   -- Show combat UI to the player
   -- PARAMETERS
   -- NewCombat - If true, start the new combat. Default value is true
   -- SOURCE
   procedure ShowCombatUI(NewCombat: Boolean := True);
   -- ****

end Combat.UI;
