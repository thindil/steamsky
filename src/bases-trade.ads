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

   function BuyItems
     (BaseItemIndex: Positive;
      Amount: String)
     return String; -- Buy items from bases, returns empty string if all ok
   function SellItems
     (ItemIndex: Positive;
      Amount: String)
     return String; -- Sell items from bases, returns empty string if all ok
   function HireRecruit
     (RecruitIndex: Positive)
     return String; -- Hire selected recruit from bases, returns empty string if all ok
   function BuyRecipe
     (RecipeIndex: Positive)
     return String; -- Buy new crafting recipe, returns empty string if all ok
   function HealWounded
     (MemberIndex: Natural)
     return String; -- Heals wounded crew members in bases, returns empty string if all ok
   procedure HealCost
     (Cost, Time: in out Natural;
      MemberIndex: Natural); -- Count cost of healing action

end Bases.Trade;
