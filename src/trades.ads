--    Copyright 2017-2019 Bartek thindil Jasicki
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Bases; use Bases;
with Ships; use Ships;

-- ****h* Steamsky/Trades
-- FUNCTION
-- Provides code for trading with ships and bases
-- SOURCE
package Trades is
-- ****

   -- ****v* Trades/TraderCargo
   -- FUNCTION
   -- List of all cargo in trader ship
   -- SOURCE
   TraderCargo: BaseCargo_Container.Vector;
   -- ****
   -- ****e* Trades/Trade_Cant_Buy
   -- FUNCTION
   -- Raised when items is not available to buy
   -- SOURCE
   Trade_Cant_Buy: exception;
   -- ****
   -- ****e* Trades/Trade_Not_For_Sale_Now
   -- FUNCTION
   -- Raised when no items available at this time for sale
   -- SOURCE
   Trade_Not_For_Sale_Now: exception;
   -- ****
   -- ****e* Trades/Trade_Buying_Too_Much
   -- FUNCTION
   -- Raised when player trying buy more than can
   -- SOURCE
   Trade_Buying_Too_Much: exception;
   -- ****
   -- ****e* Trades/Trade_No_Free_Cargo
   -- FUNCTION
   -- Raised when no enough free cargo in ship
   -- SOURCE
   Trade_No_Free_Cargo: exception;
   -- ****
   -- ****e* Trades/Trade_No_Money
   -- FUNCTION
   -- Raised when player don't have money
   -- SOURCE
   Trade_No_Money: exception;
   -- ****
   -- ****e* Trades/Trade_Not_Enough_Money
   -- FUNCTION
   -- Raised when player don't have enough money
   -- SOURCE
   Trade_Not_Enough_Money: exception;
   -- ****
   -- ****e* Trades/Trade_Invalid_Amount
   -- FUNCTION
   -- Raised when player enter invalid amount
   -- SOURCE
   Trade_Invalid_Amount: exception;
   -- ****
   -- ****e* Trades/Trade_Too_Much_For_Sale
   -- FUNCTION
   -- Raised when player try sell more than have
   -- SOURCE
   Trade_Too_Much_For_Sale: exception;
   -- ****
   -- ****e* Trades/Trade_No_Money_In_Base
   -- FUNCTION
   -- Raised when base don't have enough money for buy item
   -- SOURCE
   Trade_No_Money_In_Base: exception;
   -- ****
   -- ****e* Trades/Trade_No_Trader
   -- FUNCTION
   -- Raised when no one is assigned to talk in bases duty
   -- SOURCE
   Trade_No_Trader: exception;
   -- ****

   -- ****f* Trades/BuyItems
   -- FUNCTION
   -- Buy items from bases or trader
   -- PARAMETERS
   -- BaseItemIndex - Base or ship cargo index of item to buy
   -- Amount        - Amount of items to buy
   -- SOURCE
   procedure BuyItems(BaseItemIndex: Positive; Amount: String);
   -- ****
   -- ****f* Trades/SellItems
   -- FUNCTION
   -- Sell items from bases or trader
   -- PARAMETERS
   -- ItemIndex - Player ship cargo index of item to sell
   -- Amount    - Amount of items to sell
   -- SOURCE
   procedure SellItems(ItemIndex: Positive; Amount: String);
   -- ****
   -- ****f* Trades/GenerateTraderCargo
   -- FUNCTION
   -- Generate list of cargo to trade
   -- PARAMETERS
   -- ProtoIndex - Index of prototype ship which will be used to generate
   --              cargo
   -- SOURCE
   procedure GenerateTraderCargo(ProtoIndex: Unbounded_String) with
      Pre => ProtoShips_Container.Contains(ProtoShips_List, ProtoIndex);
      -- ****

end Trades;
