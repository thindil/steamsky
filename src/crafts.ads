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

with Interfaces.C.Strings; use Interfaces.C.Strings;
with ShipModules; use ShipModules;
with Game; use Game;
with Ships; use Ships;

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

   -- ****f* Crafts/Crafts.Set_Recipe_Data
   -- FUNCTION
   -- Set crafting data for selected recipe
   -- PARAMETERS
   -- Recipe_Index - Index of recipe from Recipes_List or full name of recipe
   --                for deconstructing
   -- RESULT
   -- Crafting data for selected recipe
   -- SOURCE
   function Set_Recipe_Data
     (Recipe_Index: Tiny_String.Bounded_String) return Craft_Data;
   -- ****

      -- ****f* Crafts/Crafts.Check_Recipe
      -- FUNCTION
      -- Check if player have all requirements for selected recipe
      -- PARAMETERS
      -- Recipe_Index - Index of the prototype recipe to check or if deconstruct
      --                existing item, "Study " + item name.
      -- RESULT
      -- Max amount of items which can be craft
      -- SOURCE
   function Check_Recipe
     (Recipe_Index: Tiny_String.Bounded_String) return Positive with
      Pre => Tiny_String.Length(Source => Recipe_Index) > 0;
      -- ****

      -- ****f* Crafts/Crafts.Set_Recipe
      -- FUNCTION
      -- Set crafting recipe for selected workshop
      -- PARAMETERS
      -- Workshop     - Index of player ship module (workplace) to which
      --                selected recipe will be set
      -- Amount       - How many times the recipe will be crafted
      -- Recipe_Index - Index of the prototype recipe to check or if deconstruct
      --                existing item, "Study " + item name.
      -- SOURCE
   procedure Set_Recipe
     (Workshop, Amount: Positive;
      Recipe_Index: Tiny_String.Bounded_String) with
      Pre => Workshop <= Player_Ship.Modules.Last_Index and
      Tiny_String.Length(Source => Recipe_Index) > 0;
      -- ****

-- Temporary code to interact with Nim

   --## rule off TYPE_INITIAL_VALUES
   type Material_Types_Array is array(0 .. 4) of chars_ptr;
   type Material_Amounts_Array is array(0 .. 4) of Integer;
   type Craft_Nim_Data is record
      Material_Types: Material_Types_Array;
      Material_Amounts: Material_Amounts_Array;
      Result_Index: Integer;
      Result_Amount: Integer;
      Workplace: Integer;
      Skill: Integer;
      Time: Positive := 1;
      Difficulty: Positive := 1;
      Tool: chars_ptr;
      Reputation: Integer;
      Tool_Quality: Positive := 1;
   end record;
   --## rule on TYPE_INITIAL_VALUES

   function Get_Recipe
     (Recipe_Index: Tiny_String.Bounded_String) return Craft_Data;

   function Get_Recipes_Amount return Positive with
      Import => True,
      Convention => C,
      External_Name => "getAdaRecipesAmount";

   function Is_Known_Recipe
     (Recipe_Index: Tiny_String.Bounded_String) return Boolean;

   function Get_Known_Recipe(Index: Integer) return Tiny_String.Bounded_String;

   function Get_Known_Recipes_Amount return Natural with
      Import => True,
      Convention => C,
      External_Name => "getAdaKnownRecipesAmount";

end Crafts;
