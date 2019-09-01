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

-- ****h* Steamsky/GameOptions
-- FUNCTION
-- Provides code for game options UI
-- SOURCE
package GameOptions is
-- ****

   -- ****f* GameOptions/CreateGameOptions
   -- FUNCTION
   -- Create infterace for game options
   -- SOURCE
   procedure CreateGameOptions;
   -- ****

   -- ****f* GameOptions/ShowGameOptions
   -- FUNCTION
   -- Show interface for game options
   -- SOURCE
   procedure ShowGameOptions;
   -- ****

   -- ****f* GameOptions/CloseOptions
   -- FUNCTION
   -- Close options, save them and back to sky map view
   -- PARAMETERS
   -- Object - Gtkada_Builder used to create UI
   -- SOURCE
   procedure CloseOptions(Object: access Gtkada_Builder_Record'Class);
   -- ****

end GameOptions;
