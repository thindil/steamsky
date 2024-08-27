--    Copyright 2016-2024 Bartek thindil Jasicki
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

with ShipModules; use ShipModules;
with Game; use Game;

-- ****h* Crafts/Crafts
-- FUNCTION
-- Provide code for crafting
-- SOURCE
package Crafts is
-- ****

   --## rule off TYPE_INITIAL_VALUES
   -- ****s* Crafts/Crafts.Craft_Data
   -- FUNCTION
   -- Data structure for recipes
   -- PARAMETERS
   -- Material_Types   - Types of material needed for recipe
   -- Material_Amounts - Amounts of material needed for recipe
   -- Result_Index     - Prototype index of crafted item
   -- Result_Amount    - Amount of products
   -- Workplace        - Ship module needed for crafting
   -- Skill            - Skill used in crafting item
   -- Time             - Minutes needed for finish recipe
   -- Difficulty       - How difficult is recipe to discover
   -- Tool             - Type of tool used to craft item
   -- Reputation       - Minimal reputation in base needed to buy that recipe
   -- Tool_Quality     - Minimal quality of tool needed to craft that recipe
   -- SOURCE
   type Craft_Data is record
      Material_Types: TinyString_Container.Vector;
      Material_Amounts: Positive_Container.Vector;
      Result_Index: Natural;
      Result_Amount: Natural := 0;
      Workplace: Module_Type;
      Skill: SkillsData_Container.Extended_Index;
      Time: Positive := 1;
      Difficulty: Positive := 1;
      Tool: Tiny_String.Bounded_String;
      Reputation: Reputation_Range;
      Tool_Quality: Positive := 1;
   end record;
   -- ****
   --## rule on TYPE_INITIAL_VALUES

end Crafts;
