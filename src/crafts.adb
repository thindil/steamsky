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

package body Crafts is

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

   function Convert_Recipe_From_Nim
     (Crafting_Data: Craft_Nim_Data) return Craft_Data is
      use Tiny_String;

      --## rule off IMPROPER_INITIALIZATION
      Temp_Record: Craft_Data;
      Temp_Materials: TinyString_Container.Vector;
      Temp_Amount: Positive_Container.Vector;
   begin
      Temp_Record :=
        (Material_Types => Temp_Materials, Material_Amounts => Temp_Amount,
         Result_Index => Crafting_Data.Result_Index,
         Result_Amount => Crafting_Data.Result_Amount,
         Workplace => Module_Type'Val(Crafting_Data.Workplace),
         Skill => SkillsData_Container.Extended_Index(Crafting_Data.Skill),
         Time => Crafting_Data.Time, Difficulty => Crafting_Data.Difficulty,
         Tool =>
           To_Bounded_String
             (Source =>
                Interfaces.C.Strings.Value(Item => Crafting_Data.Tool)),
         Reputation => Crafting_Data.Reputation,
         Tool_Quality => Crafting_Data.Tool_Quality);
      --## rule on IMPROPER_INITIALIZATION
      Load_Materials_Loop :
      for I in Crafting_Data.Material_Types'Range loop
         exit Load_Materials_Loop when Crafting_Data.Material_Amounts(I) = 0;
         Temp_Record.Material_Types.Append
           (New_Item =>
              To_Bounded_String
                (Source => Value(Item => Crafting_Data.Material_Types(I))));
         Temp_Record.Material_Amounts.Append
           (New_Item => Crafting_Data.Material_Amounts(I));
      end loop Load_Materials_Loop;
      return Temp_Record;
   end Convert_Recipe_From_Nim;

   function Set_Recipe_Data
     (Recipe_Index: Tiny_String.Bounded_String) return Craft_Data is
      use Tiny_String;

      --## rule off IMPROPER_INITIALIZATION
      Recipe: Craft_Data;
      Temp_Nim_Record: Craft_Nim_Data;
      --## rule on IMPROPER_INITIALIZATION
      procedure Set_Ada_Recipe_Data
        (C_Index: chars_ptr; Ada_Craft: out Craft_Nim_Data) with
         Import => True,
         Convention => C,
         External_Name => "setAdaRecipeData";
   begin
      Set_Ada_Recipe_Data
        (C_Index => New_String(Str => To_String(Source => Recipe_Index)),
         Ada_Craft => Temp_Nim_Record);
      --## rule off IMPROPER_INITIALIZATION
      Recipe := Convert_Recipe_From_Nim(Crafting_Data => Temp_Nim_Record);
      return Recipe;
   end Set_Recipe_Data;

   procedure Set_Recipe
     (Workshop, Amount: Positive; Recipe_Index: Tiny_String.Bounded_String) is
      procedure Set_Ada_Recipe(W, A: Integer; R_Index: chars_ptr) with
         Import => True,
         Convention => C,
         External_Name => "setAdaRecipe";
   begin
      Set_Ship_In_Nim;
      Set_Ada_Recipe
        (W => Workshop, A => Amount,
         R_Index =>
           New_String(Str => Tiny_String.To_String(Source => Recipe_Index)));
      Get_Ship_From_Nim(Ship => Player_Ship);
   end Set_Recipe;

   function Get_Recipe
     (Recipe_Index: Tiny_String.Bounded_String) return Craft_Data is
      Nim_Recipe: Craft_Nim_Data;
      procedure Get_Ada_Craft
        (C_Index: chars_ptr; Ada_Craft: out Craft_Nim_Data) with
         Import => True,
         Convention => C,
         External_Name => "getAdaCraftData";
   begin
      Get_Ada_Craft
        (C_Index =>
           New_String(Str => Tiny_String.To_String(Source => Recipe_Index)),
         Ada_Craft => Nim_Recipe);
      return Convert_Recipe_From_Nim(Crafting_Data => Nim_Recipe);
   end Get_Recipe;

   function Is_Known_Recipe
     (Recipe_Index: Tiny_String.Bounded_String) return Boolean is
      function Is_Ada_Known_Recipe(R_Index: chars_ptr) return Integer with
         Import => True,
         Convention => C,
         External_Name => "isAdaKnownRecipe";
   begin
      if Is_Ada_Known_Recipe
          (R_Index =>
             New_String
               (Str => Tiny_String.To_String(Source => Recipe_Index))) =
        1 then
         return True;
      end if;
      return False;
   end Is_Known_Recipe;

end Crafts;
