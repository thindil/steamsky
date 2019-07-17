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

package Events.UI is

-- ****f* Events.UI/CreateEventsUI
-- FUNCTION
-- Create infterace for show known events
-- SOURCE
   procedure CreateEventsUI(NewBuilder: Gtkada_Builder);
-- ****
-- ****f* Events.UI/ShowEventsUI;
-- FUNCTION
-- Show interface for show known events
-- SOURCE
   procedure ShowEventsUI;
-- ****

end Events.UI;
