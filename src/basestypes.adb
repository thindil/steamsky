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

with Ada.Strings;
with Ada.Strings.Fixed;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Bases;

package body BasesTypes is

   procedure Load_Bases_Types(File_Name: String) is
      use Ada.Strings;
      use Ada.Strings.Fixed;
      use Interfaces.C;
      use Tiny_String;

      --## rule off IMPROPER_INITIALIZATION
      Temp_Record: Base_Type_Data;
      --## rule off TYPE_INITIAL_VALUES
      type Base_Type_Nim_Data is record
         Name: chars_ptr;
         Color: chars_ptr;
         Description: chars_ptr;
      end record;
      type Ada_Bases_Types is array(0 .. 10) of chars_ptr;
      --## rule on TYPE_INITIAL_VALUES
      Temp_Nim_Record: Base_Type_Nim_Data;
      Trade: Prices_Array;
      --## rule on IMPROPER_INITIALIZATION
      Index, Index2: Natural := 0;
      Base_Data: Unbounded_String := Null_Unbounded_String;
      A_Bases_Types: Ada_Bases_Types;
      procedure Load_Ada_Bases_Types(Name: chars_ptr) with
         Import => True,
         Convention => C,
         External_Name => "loadAdaBasesTypes";
      procedure Get_Ada_Base_Type
        (Base_Index: chars_ptr; Ada_Base_Type: out Base_Type_Nim_Data) with
         Import => True,
         Convention => C,
         External_Name => "getAdaBaseType";
      function Get_Ada_Base_Data
        (Base_Index: chars_ptr; Item_Index: Integer; Data_Type: chars_ptr)
         return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "getAdaBaseData";
      function Get_Ada_Base_Trade
        (Base_Index: chars_ptr; Trade_Index: Integer; Nim_Trade: Prices_Array)
         return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "getAdaBaseTrade";
      procedure Get_Ada_Bases_Types(B_Types: out Ada_Bases_Types) with
         Import => True,
         Convention => C,
         External_Name => "getAdaBasesTypes";
   begin
      Load_Ada_Bases_Types(Name => New_String(Str => File_Name));
      Load_Bases_Types_Loop :
      loop
         Get_Ada_Base_Type
           (Base_Index => New_String(Str => Index'Img),
            Ada_Base_Type => Temp_Nim_Record);
         exit Load_Bases_Types_Loop when Strlen(Item => Temp_Nim_Record.Name) =
           0;
         Temp_Record.Name :=
           To_Unbounded_String(Source => Value(Item => Temp_Nim_Record.Name));
         Temp_Record.Color := Value(Item => Temp_Nim_Record.Color);
         Temp_Record.Description :=
           To_Unbounded_String
             (Source => Value(Item => Temp_Nim_Record.Description));
         Temp_Record.Recipes.Clear;
         Index2 := 0;
         Load_Base_Recipes_Loop :
         loop
            Base_Data :=
              To_Unbounded_String
                (Source =>
                   Value
                     (Item =>
                        Get_Ada_Base_Data
                          (Base_Index => New_String(Str => Index'Img),
                           Item_Index => Index2,
                           Data_Type => New_String(Str => "recipe"))));
            exit Load_Base_Recipes_Loop when Length(Source => Base_Data) = 0;
            Temp_Record.Recipes.Append(New_Item => Base_Data);
            Index2 := Index2 + 1;
         end loop Load_Base_Recipes_Loop;
         Index2 := 0;
         Temp_Record.Flags.Clear;
         Load_Base_Flags_Loop :
         loop
            Base_Data :=
              To_Unbounded_String
                (Source =>
                   Value
                     (Item =>
                        Get_Ada_Base_Data
                          (Base_Index => New_String(Str => Index'Img),
                           Item_Index => Index2,
                           Data_Type => New_String(Str => "flag"))));
            exit Load_Base_Flags_Loop when Length(Source => Base_Data) = 0;
            Temp_Record.Flags.Append(New_Item => Base_Data);
            Index2 := Index2 + 1;
         end loop Load_Base_Flags_Loop;
         Index2 := 1;
         Temp_Record.Trades.Clear;
         Load_Base_Trades_Loop :
         loop
            Base_Data :=
              To_Unbounded_String
                (Source =>
                   Value
                     (Item =>
                        Get_Ada_Base_Trade
                          (Base_Index => New_String(Str => Index'Img),
                           Trade_Index => Index2, Nim_Trade => Trade)));
            exit Load_Base_Trades_Loop when Length(Source => Base_Data) = 0;
            Temp_Record.Trades.Include
              (Key =>
                 To_Bounded_String(Source => To_String(Source => Base_Data)),
               New_Item => Trade);
            Index2 := Index2 + 1;
         end loop Load_Base_Trades_Loop;
         BasesTypes_Container.Include
           (Container => Bases_Types_List,
            Key =>
              To_Bounded_String
                (Source => Trim(Source => Index'Img, Side => Left)),
            New_Item => Temp_Record);
         Index := Index + 1;
      end loop Load_Bases_Types_Loop;
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
