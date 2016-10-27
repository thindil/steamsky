--    Copyright 2016 Bartek thindil Jasicki
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

with Ada.Containers.Vectors; use Ada.Containers;
with Items; use Items;
with ShipModules; use ShipModules;
with Game; use Game;

package Crafts is

    package MaterialTypes_Container is new Vectors(Positive, Items_Types);
    type Craft_Data is -- Data structure for recipes
        record
            MaterialTypes : MaterialTypes_Container.Vector; -- Types of material needed for recipe
            MaterialAmounts : Positive_Container.Vector; -- Amounts of material needed for recipe
            ResultIndex : Positive; -- Prototype index of crafted item
            ResultAmount : Positive; -- Amount of products
            Workplace : ModuleType; -- Ship module needed for crafting
            Skill : Positive; -- Skill used in crafting item
        end record;
    package Recipes_Container is new Vectors(Positive, Craft_Data);
    Recipes_List : Recipes_Container.Vector; -- List of recipes available in game 

    function LoadRecipes return Boolean; -- Load recipes from file, return False if file not exists
    procedure Manufacturing(Minutes : Positive); -- Craft selected items
    procedure SetRecipe(RecipeIndex, ModuleIndex : Positive); -- Set recipe to craft

end Crafts;
