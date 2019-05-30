--    Copyright 2017-2018 Bartek thindil Jasicki
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

   procedure HireRecruit
     (RecruitIndex, Cost: Positive; DailyPayment, TradePayment: Natural;
      ContractLenght: Integer); -- Hire selected recruit from bases
   procedure BuyRecipe(RecipeIndex: Positive); -- Buy new crafting recipe
   procedure HealWounded
     (MemberIndex: Natural); -- Heals wounded crew members in bases
   procedure HealCost
     (Cost, Time: in out Natural;
      MemberIndex: Natural); -- Count cost of healing action
   function TrainCost
     (MemberIndex, SkillIndex: Positive)
      return Natural; -- Count cost of training action
   procedure TrainSkill
     (MemberIndex, SkillIndex: Positive); -- Train selected skill
   Trade_Already_Known: exception; -- Raised when player known selected recipe
   Trade_Cant_Heal: exception; -- Raised when no crew members are wounded
   Trade_Cant_Train: exception; -- Raised when skill is maxed and can't be trained

end Bases.Trade;
