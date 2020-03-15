--    Copyright 2017-2020 Bartek thindil Jasicki
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

-- ****h* Steamsky/Bases.Trade
-- FUNCTION
-- Provide code for hiring recruits, buying recipes, heal and train crew
-- members in bases.
-- SOURCE
package Bases.Trade is
-- ****

   -- ****e* Bases.Trade/Trade_Already_Known
   -- FUNCTION
   -- Raised when player known selected recipe
   -- SOURCE
   Trade_Already_Known: exception;
   -- ****
   -- ****e* Bases.Trade/Trade_Cant_Heal
   -- FUNCTION
   -- Raised when no crew members are wounded
   -- SOURCE
   Trade_Cant_Heal: exception;
   -- ****
   -- ****e* Bases.Trade/Trade_Cant_Train
   -- FUNCTION
   -- Raised when skill is maxed and can't be trained
   -- SOURCE
   Trade_Cant_Train: exception;
   -- ****

   -- ****f* Bases.Trade/HireRecruit
   -- FUNCTION
   -- Hire selected recruit from bases and add him/her to player ship crew
   -- PARAMETERS
   -- RecruitIndex   - Index of recruit, from base recruits list to hire
   -- Cost           - Cost of hire of selected recruit
   -- DailyPayment   - Daily payment of selected recruit
   -- TradePayment   - Percent of earnings from each trade which this recruit
   --                  will take
   -- ContractLength - Length of the contract with this recruit in days. 0
   --                  means infinite contract
   -- SOURCE
   procedure HireRecruit
     (RecruitIndex, Cost: Positive; DailyPayment, TradePayment: Natural;
      ContractLenght: Integer);
   -- ****
   -- ****f* Bases.Trade/BuyRecipe
   -- FUNCTION
   -- Buy new crafting recipe
   -- PARAMETERS
   -- RecipeIndex - Index of the recipe from base recipes list to buy
   -- SOURCE
   procedure BuyRecipe(RecipeIndex: Unbounded_String) with
      Pre => (RecipeIndex /= Null_Unbounded_String);
      -- ****
      -- ****f* Bases.Trade/HealWounded
      -- FUNCTION
      -- Heals wounded crew members in bases
      -- PARAMETERS
      -- MemberIndex - Index of player ship crew member to heal or 0 for heal
      --               all wounded crew members
      -- SOURCE
   procedure HealWounded(MemberIndex: Crew_Container.Extended_Index) with
      Pre => (MemberIndex <= PlayerShip.Crew.Last_Index);
      -- ****
      -- ****f* Bases.Trade/HealCost
      -- FUNCTION
      -- Count cost of healing action
      -- PARAMETERS
      -- Cost        - Overall cost of heal wounded player ship crew member(s)
      -- Time        - Time needed to heal wounded player ship crew member(s)
      -- MemberIndex - Index of player ship crew member to heal or 0 for heal
      --               all wounded crew members
      -- RESULT
      -- Parameters Cost and Time
      -- SOURCE
   procedure HealCost
     (Cost, Time: in out Natural;
      MemberIndex: Crew_Container.Extended_Index) with
      Pre => (MemberIndex <= PlayerShip.Crew.Last_Index);
      -- ****
      -- ****f* Bases.Trade/TrainCost
      -- FUNCTION
      -- Count cost of training action
      -- PARAMETERS
      -- MemberIndex - Index of player ship crew member which will be training
      -- SkillIndex  - Index of skill of selected crew member which will be
      --               training
      -- RESULT
      -- Overall cost of training selected skill by selected crew member
      -- SOURCE
   function TrainCost(MemberIndex, SkillIndex: Positive) return Natural with
      Pre =>
      (MemberIndex <= PlayerShip.Crew.Last_Index and
       SkillIndex <= Skills_List.Last_Index);
      -- ****
      -- ****f* Bases.Trade/TrainSkill
      -- FUNCTION
      -- Train selected skill
      -- PARAMETERS
      -- MemberIndex - Index of playership crew member which train
      -- SkillIndex  - Index of skill of selected crew member to train
      -- SOURCE
   procedure TrainSkill(MemberIndex, SkillIndex: Positive) with
      Pre =>
      (MemberIndex <= PlayerShip.Crew.Last_Index and
       SkillIndex <= Skills_List.Last_Index);
      -- ****

end Bases.Trade;
