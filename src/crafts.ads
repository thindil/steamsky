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

with Prototypes; use Prototypes;
with Ships; use Ships;

package Crafts is

    type Craft_Data is -- Data structure for recipes
        record
            MaterialType : Items_Types; -- Type of material needed for recipe
            MaterialAmount : Positive; -- Amount of material needed for recipe
            ResultIndex : Positive; -- Prototype index of crafted item
            ResultAmount : Positive; -- Amount of products
            Workplace : ModuleType; -- Ship module needed for crafting
        end record;
    Recipes : constant array(1..2) of Craft_Data := ((MaterialType =>
    FoodMaterial, MaterialAmount => 1, ResultIndex => 2, ResultAmount => 1,
    Workplace => ALCHEMY_LAB), (MaterialType => FuelMaterial, MaterialAmount =>
    1, ResultIndex => 1, ResultAmount => 5, Workplace => ALCHEMY_LAB));

    procedure ShowCraft; -- Show crafting screen

end Crafts;
