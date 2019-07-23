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

-- ****h* Steamsky/Trades.UI
-- FUNCTION
-- Provides code for trading with bases and ships UI
-- SOURCE
package Trades.UI is
-- ****

   -- ****f* Trades.UI/CreateTradeUI
   -- FUNCTION
   -- Create infterace for trades
   -- PARAMETERS
   -- NewBuilder - Gtkada_Builder used to create UI
   -- SOURCE
   procedure CreateTradeUI(NewBuilder: Gtkada_Builder);
   -- ****
   -- ****f* Trades.UI/ShowTradeUI;
   -- FUNCTION
   -- Show interface for trades
   -- SOURCE
   procedure ShowTradeUI;
   -- ****

end Trades.UI;
