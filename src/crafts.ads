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

   -- Data structure for recipes
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
   package Recipes_Container is new Hashed_Maps(Unbounded_String, Craft_Data,
      Ada.Strings.Unbounded.Hash, "=");
   -- List of recipes available in game
   Recipes_List: Recipes_Container.Map;
   -- List of all know by player recipes
   Known_Recipes: UnboundedString_Container.Vector;
   -- Raised when no materials needed for selected recipe
   Crafting_No_Materials: exception;
   -- Raised when no tool needed for selected recipe
   Crafting_No_Tools: exception;
   -- Raised when no workshop needed for selected recipe
   Crafting_No_Workshop: exception;

   -- Load recipes from files
   procedure LoadRecipes(Reader: Tree_Reader);
   -- Craft selected items
   procedure Manufacturing(Minutes: Positive);
   -- Check if player have all requirements for selected recipe, return max amount of items which can be craft
   function CheckRecipe(RecipeIndex: Unbounded_String) return Positive with
      Pre => RecipeIndex /= Null_Unbounded_String;
      -- Set crafting recipe for selected workshop
   procedure SetRecipe
     (Workshop, Amount: Positive; RecipeIndex: Unbounded_String) with
      Pre =>
      (Workshop <= PlayerShip.Modules.Last_Index and
       RecipeIndex /= Null_Unbounded_String);

end Crafts;
