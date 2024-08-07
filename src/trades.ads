--    Copyright 2017-2024 Bartek thindil Jasicki
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

with Bases; use Bases;

-- ****h* Trades/Trades
-- FUNCTION
-- Provides code for trading with ships and bases
-- SOURCE
package Trades is
-- ****

   -- ****v* Trades/Trades.Trader_Cargo
   -- FUNCTION
   -- List of all cargo in trader ship
   -- SOURCE
   Trader_Cargo: BaseCargo_Container.Vector (Capacity => 32);
   -- ****

end Trades;
