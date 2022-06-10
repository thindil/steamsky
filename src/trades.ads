--    Copyright 2017-2022 Bartek thindil Jasicki
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
with Items; use Items;
with Ships; use Ships;

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

   -- ****e* Trades/Trades.Trade_Not_For_Sale_Now
   -- FUNCTION
   -- Raised when no items available at this time for sale
   -- SOURCE
   Trade_Not_For_Sale_Now: exception;
   -- ****

   -- ****e* Trades/Trades.Trade_Buying_Too_Much
   -- FUNCTION
   -- Raised when player trying buy more than can
   -- SOURCE
   Trade_Buying_Too_Much: exception;
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

   -- ****e* Trades/Trades.Trade_Invalid_Amount
   -- FUNCTION
   -- Raised when player enter invalid amount
   -- SOURCE
   Trade_Invalid_Amount: exception;
   -- ****

   -- ****e* Trades/Trades.Trade_Too_Much_For_Sale
   -- FUNCTION
   -- Raised when player try sell more than have
   -- SOURCE
   Trade_Too_Much_For_Sale: exception;
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

   -- ****f* Trades/Trades.Buy_Items
   -- FUNCTION
   -- Buy items from bases or trader
   -- PARAMETERS
   -- Base_Item_Index - Base or ship cargo index of item to buy
   -- Amount          - Amount of items to buy
   -- SOURCE
   procedure Buy_Items
     (Base_Item_Index: BaseCargo_Container.Extended_Index; Amount: String) with
      Pre => Amount'Length > 0,
      Test_Case => (Name => "Test_BuyItems", Mode => Nominal);
      -- ****

      -- ****f* Trades/Trades.Sell_Items
      -- FUNCTION
      -- Sell items from bases or trader
      -- PARAMETERS
      -- Item_Index - Player ship cargo index of item to sell
      -- Amount     - Amount of items to sell
      -- SOURCE
   procedure Sell_Items
     (Item_Index: Inventory_Container.Extended_Index; Amount: String) with
      Pre => Item_Index in
        Inventory_Container.First_Index(Container => Player_Ship.Cargo) ..
              Inventory_Container.Last_Index
                (Container => Player_Ship.Cargo) and
      Amount'Length > 0,
      Test_Case => (Name => "Test_SellItems", Mode => Nominal);
      -- ****

      -- ****f* Trades/Trades.Generate_Trader_Cargo
      -- FUNCTION
      -- Generate list of cargo to trade
      -- PARAMETERS
      -- Proto_Index - Index of prototype ship which will be used to generate
      --               cargo
      -- SOURCE
   procedure Generate_Trader_Cargo
     (Proto_Index: Proto_Ships_Container.Extended_Index) with
      Pre => Proto_Index <= Proto_Ships_List.Last_Index,
      Test_Case => (Name => "Test_GenerateTraderCargo", Mode => Nominal);
      -- ****

end Trades;
