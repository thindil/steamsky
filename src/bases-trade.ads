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

-- ****h* Bases/BTrade
-- FUNCTION
-- Provide code for hiring recruits, buying recipes, heal and train crew
-- members in bases.
-- SOURCE
package Bases.Trade is
-- ****

   -- ****e* BTrade/BTrade.Trade_Already_Known
   -- FUNCTION
   -- Raised when player known selected recipe
   -- SOURCE
   Trade_Already_Known: exception;
   -- ****

   -- ****e* BTrade/BTrade.Trade_Cant_Heal
   -- FUNCTION
   -- Raised when no crew members are wounded
   -- SOURCE
   Trade_Cant_Heal: exception;
   -- ****

   -- ****f* BTrade/BTrade.Hire_Recruit
   -- FUNCTION
   -- Hire selected recruit from bases and add him/her to player ship crew
   -- PARAMETERS
   -- Recruit_Index   - Index of recruit, from base recruits list to hire
   -- Cost            - Cost of hire of selected recruit
   -- Daily_Payment   - Daily payment of selected recruit
   -- Trade_Payment   - Percent of earnings from each trade which this recruit
   --                  will take
   -- Contract_Length - Length of the contract with this recruit in days. 0
   --                  means infinite contract
   -- SOURCE
   procedure Hire_Recruit
     (Recruit_Index: Recruit_Container.Extended_Index; Cost: Positive;
      Daily_Payment, TradePayment: Natural; Contract_Length: Integer) with
      Test_Case => (Name => "Test_HireRecruit", Mode => Robustness);
      -- ****

      -- ****f* BTrade/BTrade.Buy_Recipe
      -- FUNCTION
      -- Buy new crafting recipe
      -- PARAMETERS
      -- Recipe_Index - Index of the recipe from base recipes list to buy
      -- SOURCE
   procedure Buy_Recipe(Recipe_Index: Tiny_String.Bounded_String) with
      Pre => Tiny_String.Length(Source => Recipe_Index) > 0,
      Test_Case => (Name => "Test_BuyRecipe", Mode => Nominal);
      -- ****

      -- ****f* BTrade/BTrade.Heal_Wounded
      -- FUNCTION
      -- Heals wounded crew members in bases
      -- PARAMETERS
      -- Member_Index - Index of player ship crew member to heal or 0 for heal
      --               all wounded crew members
      -- SOURCE
   procedure Heal_Wounded(Member_Index: Crew_Container.Extended_Index) with
      Pre => (Member_Index <= Player_Ship.Crew.Last_Index),
      Test_Case => (Name => "Test_HealWounded", Mode => Nominal);
      -- ****

      -- ****f* BTrade/BTrade.Heal_Cost
      -- FUNCTION
      -- Count cost of healing action
      -- PARAMETERS
      -- Cost         - Overall cost of heal wounded player ship crew member(s)
      -- Time         - Time needed to heal wounded player ship crew member(s)
      -- Member_Index - Index of player ship crew member to heal or 0 for heal
      --                all wounded crew members
      -- RESULT
      -- Parameters Cost and Time
      -- SOURCE
   procedure Heal_Cost
     (Cost, Time: in out Natural;
      Member_Index: Crew_Container.Extended_Index) with
      Pre => Member_Index <= Player_Ship.Crew.Last_Index,
      Post => Cost > 0 and Time > 0,
      Test_Case => (Name => "Test_HealCost", Mode => Nominal);
      -- ****

      -- ****f* BTrade/BTrade.Train_Cost
      -- FUNCTION
      -- Count cost of training action
      -- PARAMETERS
      -- Member_Index - Index of player ship crew member which will be training
      -- Skill_Index  - Index of skill of selected crew member which will be
      --                training
      -- RESULT
      -- Overall cost of training selected skill by selected crew member.
      -- Return 0 if the skill can't be trained because is maxed.
      -- SOURCE
   function Train_Cost
     (Member_Index: Crew_Container.Extended_Index;
      Skill_Index: Skills_Container.Extended_Index) return Natural with
      Pre => Member_Index in
        Player_Ship.Crew.First_Index .. Player_Ship.Crew.Last_Index and
      Skill_Index in 1 .. Skills_Amount,
      Test_Case => (Name => "Test_TrainCost", Mode => Nominal);
      -- ****

      -- ****f* BTrade/BTrade.Train_Skill
      -- FUNCTION
      -- Train selected skill
      -- PARAMETERS
      -- Member_Index - Index of Player_Ship crew member which train
      -- Skill_Index  - Index of skill of selected crew member to train
      -- Amount       - How many times train or how many money spend on training
      -- Is_Amount    - If true, Amount variable is how many times train,
      --                otherwise it is amount of money to spend
      -- SOURCE
   procedure Train_Skill
     (Member_Index: Crew_Container.Extended_Index;
      Skill_Index: Skills_Container.Extended_Index; Amount: Positive;
      Is_Amount: Boolean := True) with
      Pre => Member_Index in
        Player_Ship.Crew.First_Index .. Player_Ship.Crew.Last_Index and
      Skill_Index in 1 .. Skills_Amount,
      Test_Case => (Name => "Test_TrainSkill", Mode => Nominal);
      -- ****

end Bases.Trade;
