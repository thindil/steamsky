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

   -- Raised when player known selected recipe
   Trade_Already_Known: exception;
   -- Raised when no crew members are wounded
   Trade_Cant_Heal: exception;
   -- Raised when skill is maxed and can't be trained
   Trade_Cant_Train: exception;

   -- Hire selected recruit from bases
   procedure HireRecruit
     (RecruitIndex, Cost: Positive; DailyPayment, TradePayment: Natural;
      ContractLenght: Integer);
   -- Buy new crafting recipe
   procedure BuyRecipe(RecipeIndex: Unbounded_String);
   -- Heals wounded crew members in bases
   procedure HealWounded(MemberIndex: Natural);
   -- Count cost of healing action
   procedure HealCost(Cost, Time: in out Natural; MemberIndex: Natural);
   -- Count cost of training action
   function TrainCost(MemberIndex, SkillIndex: Positive) return Natural;
   -- Train selected skill
   procedure TrainSkill(MemberIndex, SkillIndex: Positive) with
      Pre =>
      (MemberIndex <= PlayerShip.Crew.Last_Index and
       SkillIndex < Skills_List.Last_Index);

end Bases.Trade;
