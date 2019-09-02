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

-- ****h* Steamsky/Ships.UI
-- FUNCTION
-- Provides code for player ship information UI
-- SOURCE
package Ships.UI is
-- ****

   -- ****f* Ships.UI/CreateShipUI
   -- FUNCTION
   -- Create infterace for show player ship info
   -- SOURCE
   procedure CreateShipUI;
   -- ****

   -- ****f* Ships.UI/ShowShipUI
   -- FUNCTION
   -- Show interface for show player ship info
   -- SOURCE
   procedure ShowShipUI;
   -- ****

private

   -- ****v* Ships.UI/ModuleIndex
   -- FUNCTION
   -- Index of selected module
   -- SOURCE
   ModuleIndex: Positive;
   -- ****

   -- ****f* Ships.UI/ShowModuleOptions
   -- FUNCTION
   -- Show options for selected module
   -- SOURCE
   procedure ShowModuleOptions;
   -- ****

   -- ****f* Ships.UI/ShowShipInfo
   -- FUNCTION
   -- Show general info about player's ship
   -- SOURCE
   procedure ShowShipInfo;
   -- ****

end Ships.UI;
