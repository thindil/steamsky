--    Copyright 2019-2023 Bartek thindil Jasicki
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

--## rule off REDUCEABLE_SCOPE
with Interfaces.C.Strings; use Interfaces.C.Strings;
--## rule on REDUCEABLE_SCOPE
with Bases;

package body BasesTypes is

   procedure Load_Bases_Types is
      use Interfaces.C;
      use Tiny_String;

      --## rule off TYPE_INITIAL_VALUES
      type Ada_Bases_Types is array(0 .. 10) of chars_ptr;
      --## rule on TYPE_INITIAL_VALUES
      A_Bases_Types: Ada_Bases_Types;
      procedure Get_Ada_Bases_Types(B_Types: out Ada_Bases_Types) with
         Import => True,
         Convention => C,
         External_Name => "getAdaBasesTypes";
   begin
      Get_Ada_Bases_Types(B_Types => A_Bases_Types);
      Set_Bases_Types_Loop :
      for I in A_Bases_Types'Range loop
         Bases_Types(I) :=
           To_Bounded_String(Source => Value(Item => A_Bases_Types(I)));
      end loop Set_Bases_Types_Loop;
   end Load_Bases_Types;

   function Is_Buyable
     (Base_Type: Tiny_String.Bounded_String; Item_Index: Positive;
      Check_Flag: Boolean := True; Base_Index: Extended_Base_Range := 0)
      return Boolean is
      use Bases;
      use Tiny_String;

      function Is_Ada_Buyable
        (B_Type: chars_ptr;
         I_Index, C_Flags, B_Index, Rep_Level, Rep_Experience: Integer)
         return Integer with
         Import => True,
         Convention => C,
         External_Name => "isAdaBuyable";

   begin
      if Is_Ada_Buyable
          (B_Type => New_String(Str => To_String(Source => Base_Type)),
           I_Index => Item_Index, C_Flags => (if Check_Flag then 1 else 0),
           B_Index => Base_Index,
           Rep_Level =>
             (if Base_Index > 0 then Sky_Bases(Base_Index).Reputation.Level
              else 0),
           Rep_Experience =>
             (if Base_Index > 0 then
                Sky_Bases(Base_Index).Reputation.Experience
              else 0)) =
        1 then
         return True;
      end if;
      return False;
   end Is_Buyable;

   function Get_Price
     (Base_Type: Tiny_String.Bounded_String; Item_Index: Positive)
      return Natural is
      function Get_Ada_Price
        (B_Type: chars_ptr; I_Index: Positive) return Natural with
         Import => True,
         Convention => C,
         External_Name => "getAdaPrice";
   begin
      return
        Get_Ada_Price
          (B_Type =>
             New_String(Str => Tiny_String.To_String(Source => Base_Type)),
           I_Index => Item_Index);
   end Get_Price;

   function Has_Flag
     (Base_Type: Tiny_String.Bounded_String; Flag: String) return Boolean is
      function Has_Ada_Flag
        (B_Type, Flag_To_Check: chars_ptr) return Integer with
         Import => True,
         Convention => C,
         External_Name => "hasAdaFlag";
   begin
      if Has_Ada_Flag
          (B_Type =>
             New_String(Str => Tiny_String.To_String(Source => Base_Type)),
           Flag_To_Check => New_String(Str => Flag)) =
        0 then
         return False;
      end if;
      return True;
   end Has_Flag;

   function Get_Base_Type_Name
     (Base_Type: Tiny_String.Bounded_String) return String is
      function Get_Ada_Base_Type_Name(B_Type: chars_ptr) return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "getAdaBaseTypeName";
   begin
      return
        Value
          (Item =>
             Get_Ada_Base_Type_Name
               (B_Type =>
                  New_String
                    (Str => Tiny_String.To_String(Source => Base_Type))));
   end Get_Base_Type_Name;

   function Has_Recipe
     (Base_Type: Tiny_String.Bounded_String; Recipe: String) return Boolean is
      function Has_Ada_Recipe
        (B_Type, Recipe_To_Check: chars_ptr) return Integer with
         Import => True,
         Convention => C,
         External_Name => "hasAdaRecipe";
   begin
      if Has_Ada_Recipe
          (B_Type =>
             New_String(Str => Tiny_String.To_String(Source => Base_Type)),
           Recipe_To_Check => New_String(Str => Recipe)) =
        0 then
         return False;
      end if;
      return True;
   end Has_Recipe;

   function Get_Base_Type_Color
     (Base_Type: Tiny_String.Bounded_String) return String is
      function Get_Ada_Base_Type_Color(B_Type: chars_ptr) return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "getAdaBaseTypeColor";
   begin
      return
        Value
          (Item =>
             Get_Ada_Base_Type_Color
               (B_Type =>
                  New_String
                    (Str => Tiny_String.To_String(Source => Base_Type))));
   end Get_Base_Type_Color;

   function Get_Base_Type_Description
     (Base_Type: Tiny_String.Bounded_String) return String is
      function Get_Ada_Base_Type_Description
        (B_Type: chars_ptr) return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "getAdaBaseTypeDescription";
   begin
      return
        Value
          (Item =>
             Get_Ada_Base_Type_Description
               (B_Type =>
                  New_String
                    (Str => Tiny_String.To_String(Source => Base_Type))));
   end Get_Base_Type_Description;

end BasesTypes;
