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

package Ships.Cargo.UI is

-- ****f* Ships.Cargo.UI/CreateCargoUI
-- FUNCTION
-- Create infterace for show player ship cargo
-- SOURCE
   procedure CreateCargoUI(NewBuilder: Gtkada_Builder);
-- ****
-- ****f* Ships.Cargo.UI/ShowCargoUI;
-- FUNCTION
-- Show interface for show player ship cargo
-- SOURCE
   procedure ShowCargoUI;
-- ****

end Ships.Cargo.UI;
