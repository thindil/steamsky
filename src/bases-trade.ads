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

-- ****h* Bases/BTrade
-- FUNCTION
-- Provide code for hiring recruits, buying recipes, heal and train crew
-- members in bases.
-- SOURCE
package Bases.Trade is
-- ****

   -- ****f* BTrade/BTrade.Buy_Recipe
   -- FUNCTION
   -- Buy new crafting recipe
   -- PARAMETERS
   -- Recipe_Index - Index of the recipe from base recipes list to buy
   -- SOURCE
   procedure Buy_Recipe(Recipe_Index: Tiny_String.Bounded_String) with
      Pre => Tiny_String.Length(Source => Recipe_Index) > 0;
      -- ****

      -- ****f* BTrade/BTrade.Heal_Wounded
      -- FUNCTION
      -- Heals wounded crew members in bases
      -- PARAMETERS
      -- Member_Index - Index of player ship crew member to heal or 0 for heal
      --               all wounded crew members
      -- SOURCE
   procedure Heal_Wounded(Member_Index: Crew_Container.Extended_Index) with
      Pre => Member_Index <= Player_Ship.Crew.Last_Index;
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
      Import => True,
      Convention => C,
      External_Name => "healAdaCost",
      Pre => Member_Index <= Player_Ship.Crew.Last_Index,
      Post => Cost > 0 and Time > 0;
      -- ****

end Bases.Trade;
