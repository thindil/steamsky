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
with Terminal_Interface.Curses; use Terminal_Interface.Curses;
with Items; use Items;
with ShipModules; use ShipModules;
with Game; use Game;

package Crafts is

    package MaterialTypes_Container is new Vectors(Positive, Items_Types);
    package MaterialAmounts_Container is new Vectors(Positive, Positive);
    type Craft_Data is -- Data structure for recipes
        record
            MaterialTypes : MaterialTypes_Container.Vector; -- Types of material needed for recipe
            MaterialAmounts : MaterialAmounts_Container.Vector; -- Amounts of material needed for recipe
            ResultIndex : Positive; -- Prototype index of crafted item
            ResultAmount : Positive; -- Amount of products
            Workplace : ModuleType; -- Ship module needed for crafting
        end record;
    package Recipes_Container is new Vectors(Positive, Craft_Data);
    Recipes_List : Recipes_Container.Vector; -- List of recipes available in game 

    function LoadRecipes return Boolean; -- Load recipes from file, return False if file not exists
    procedure ShowCraft(Key : Key_Code); -- Show crafting screen
    function CraftKeys(Key : Key_Code) return GameStates; -- Handle keys in craft screen

end Crafts;
