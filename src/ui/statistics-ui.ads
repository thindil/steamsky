--    Copyright 2018-2019 Bartek thindil Jasicki
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

with Gtkada.Builder; use Gtkada.Builder;

package Statistics.UI is

-- ****f* Statistics.UI/HideStatistics;
-- FUNCTION
-- Hide stats and show sky map or main menu
-- SOURCE
   procedure HideStatistics;
-- ****
-- ****f* Statistics.UI/CreateStatsUI
-- FUNCTION
-- Create infterace for show game statistics
-- SOURCE
   procedure CreateStatsUI(NewBuilder: Gtkada_Builder);
-- ****
-- ****f* Statistics.UI/ShowStatsUI;
-- FUNCTION
-- Show interface for show game statistics
-- SOURCE
   procedure ShowStatsUI;
-- ****
-- ****f* Statistics.UI/UpdateGoalsButton
-- FUNCTION
-- Update label on character goal button
-- SOURCE
   procedure UpdateGoalsButton(Message: String) with
      Pre => Message'Length > 0;
-- ****

end Statistics.UI;
