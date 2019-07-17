--    Copyright 2016-2019 Bartek thindil Jasicki
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

with Ada.Containers.Hashed_Maps; use Ada.Containers;
with Ada.Strings.Unbounded.Hash;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with DOM.Readers; use DOM.Readers;
with ShipModules; use ShipModules;
with Game; use Game;
with Ships; use Ships;

package Crafts is

-- ****t* Crafts/Craft_Data
-- FUNCTION
-- Data structure for recipes
-- SOURCE
   type Craft_Data is record
      MaterialTypes: UnboundedString_Container
        .Vector; -- Types of material needed for recipe
      MaterialAmounts: Positive_Container
        .Vector; -- Amounts of material needed for recipe
      ResultIndex: Unbounded_String; -- Prototype index of crafted item
      ResultAmount: Natural; -- Amount of products
      Workplace: ModuleType; -- Ship module needed for crafting
      Skill: Positive; -- Skill used in crafting item
      Time: Positive; -- Minutes needed for finish recipe
      Difficulty: Positive; -- How difficult is recipe to discover
      BaseType: Natural; -- Sky base type in which recipe can be bought
      Tool: Unbounded_String; -- Type of tool used to craft item
   end record;
-- ****

-- ****t* Crafts/Recipes_Container
-- SOURCE
   package Recipes_Container is new Hashed_Maps(Unbounded_String, Craft_Data,

      Ada.Strings.Unbounded.Hash, "=");
-- FUNCTION
-- List of recipes available in game
-- ****
-- ****v* Crafts/Recipes_List
-- SOURCE
   Recipes_List: Recipes_Container.Map;
-- FUNCTION
-- List of all know by player recipes
-- ****
-- ****v* Crafts/Known_Recipes
-- SOURCE
   Known_Recipes: UnboundedString_Container.Vector;
-- FUNCTION
-- Raised when no materials needed for selected recipe
-- ****
-- ****v* Crafts/Crafting_No_Materials
-- SOURCE
   Crafting_No_Materials: exception;
-- FUNCTION
-- Raised when no tool needed for selected recipe
-- ****
-- ****v* Crafts/Crafting_No_Tools
-- SOURCE
   Crafting_No_Tools: exception;
-- FUNCTION
-- Raised when no workshop needed for selected recipe
-- ****
-- ****v* Crafts/Crafting_No_Workshop
-- SOURCE
   Crafting_No_Workshop: exception;
-- ****

-- ****f* Crafts/LoadRecipes
-- FUNCTION
-- Load recipes from files
-- SOURCE
   procedure LoadRecipes(Reader: Tree_Reader);
-- ****
-- ****f* Crafts/Manufacturing
-- FUNCTION
-- Craft selected items
-- SOURCE
   procedure Manufacturing(Minutes: Positive);
-- ****
-- ****f* Crafts/CheckRecipe
-- FUNCTION
-- Check if player have all requirements for selected recipe, return max amount of items which can be craft
-- SOURCE
   function CheckRecipe(RecipeIndex: Unbounded_String) return Positive with
      Pre => RecipeIndex /= Null_Unbounded_String;
-- ****
-- ****f* Crafts/SetRecipe
-- FUNCTION
-- Set crafting recipe for selected workshop
-- SOURCE
   procedure SetRecipe
     (Workshop, Amount: Positive; RecipeIndex: Unbounded_String) with
      Pre =>
      (Workshop <= PlayerShip.Modules.Last_Index and
       RecipeIndex /= Null_Unbounded_String);
-- ****

end Crafts;
