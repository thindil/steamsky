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

   -- ****e* Trades/Trades.Trade_Cant_Buy
   -- FUNCTION
   -- Raised when items is not available to buy
   -- SOURCE
   Trade_Cant_Buy: exception;
   -- ****

   -- ****e* Trades/Trades.Trade_No_Free_Cargo
   -- FUNCTION
   -- Raised when no enough free cargo in ship
   -- SOURCE
   Trade_No_Free_Cargo: exception;
   -- ****

   -- ****e* Trades/Trades.Trade_No_Money
   -- FUNCTION
   -- Raised when player don't have money
   -- SOURCE
   Trade_No_Money: exception;
   -- ****

   -- ****e* Trades/Trades.Trade_Not_Enough_Money
   -- FUNCTION
   -- Raised when player don't have enough money
   -- SOURCE
   Trade_Not_Enough_Money: exception;
   -- ****

   -- ****e* Trades/Trades.Trade_No_Money_In_Base
   -- FUNCTION
   -- Raised when base don't have enough money for buy item
   -- SOURCE
   Trade_No_Money_In_Base: exception;
   -- ****

   -- ****e* Trades/Trades.Trade_No_Trader
   -- FUNCTION
   -- Raised when no one is assigned to talk in bases duty
   -- SOURCE
   Trade_No_Trader: exception;
   -- ****

end Trades;
