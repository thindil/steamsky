--    Copyright 2017 Bartek thindil Jasicki
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

package Bases.Trade is

   procedure BuyItems
     (BaseItemIndex: Positive;
      Amount: String); -- Buy items from bases
   procedure SellItems
     (ItemIndex: Positive;
      Amount: String); -- Sell items from bases
   procedure HireRecruit
     (RecruitIndex: Positive); -- Hire selected recruit from bases
   procedure BuyRecipe(RecipeIndex: Positive); -- Buy new crafting recipe
   procedure HealWounded
     (MemberIndex: Natural); -- Heals wounded crew members in bases
   procedure HealCost
     (Cost, Time: in out Natural;
      MemberIndex: Natural); -- Count cost of healing action
   Trade_Cant_Buy: exception; -- Raised when items is not available to buy
   Trade_Not_For_Sale_Now: exception; -- Raised when no items available at this time for sale
   Trade_Buying_Too_Much: exception; -- Raised when player trying buy more than can
   Trade_No_Free_Cargo: exception; -- Raised when no enough free cargo in ship
   Trade_No_Money: exception; -- Raised when player don't have money
   Trade_Not_Enough_Money: exception; -- Raised when player don't have enough money
   Trade_Invalid_Amount: exception; -- Raised when player enter invalid amount
   Trade_Too_Much_For_Sale: exception; -- Raised when player try sell more than have
   Trade_No_Money_In_Base: exception; -- Raised when base don't have enough money for buy item
   Trade_Already_Known: exception; -- Raised when player known selected recipe
   Trade_Cant_Heal: exception; -- Raised when no crew members are wounded

end Bases.Trade;
