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

package Bases.Trade is

-- ****v* Bases.Trade/Trade_Already_Known
-- FUNCTION
-- Raised when player known selected recipe
-- SOURCE
   Trade_Already_Known: exception;
-- ****
-- ****v* Bases.Trade/Trade_Cant_Heal
-- FUNCTION
-- Raised when no crew members are wounded
-- SOURCE
   Trade_Cant_Heal: exception;
-- ****
-- ****v* Bases.Trade/Trade_Cant_Train
-- FUNCTION
-- Raised when skill is maxed and can't be trained
-- SOURCE
   Trade_Cant_Train: exception;
-- ****

-- ****f* Bases.Trade/HireRecruit
-- FUNCTION
-- Hire selected recruit from bases
-- SOURCE
   procedure HireRecruit
     (RecruitIndex, Cost: Positive; DailyPayment, TradePayment: Natural;
      ContractLenght: Integer);
-- ****
-- ****f* Bases.Trade/BuyRecipe
-- FUNCTION
-- Buy new crafting recipe
-- SOURCE
   procedure BuyRecipe(RecipeIndex: Unbounded_String);
-- ****
-- ****f* Bases.Trade/HealWounded
-- FUNCTION
-- Heals wounded crew members in bases
-- SOURCE
   procedure HealWounded(MemberIndex: Natural);
-- ****
-- ****f* Bases.Trade/HealCost
-- FUNCTION
-- Count cost of healing action
-- SOURCE
   procedure HealCost(Cost, Time: in out Natural; MemberIndex: Natural);
-- ****
-- ****f* Bases.Trade/TrainCost
-- FUNCTION
-- Count cost of training action
-- SOURCE
   function TrainCost(MemberIndex, SkillIndex: Positive) return Natural;
-- ****
-- ****f* Bases.Trade/TrainSkill
-- FUNCTION
-- Train selected skill
-- SOURCE
   procedure TrainSkill(MemberIndex, SkillIndex: Positive) with
      Pre =>
      (MemberIndex <= PlayerShip.Crew.Last_Index and
       SkillIndex < Skills_List.Last_Index);
-- ****

end Bases.Trade;
