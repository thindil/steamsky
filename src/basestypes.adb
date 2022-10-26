--    Copyright 2019-2022 Bartek thindil Jasicki
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

with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Interfaces.C.Strings;
with Bases;

package body BasesTypes is

   procedure Load_Bases_Types(File_Name: String) is
      use Interfaces.C;
      use Interfaces.C.Strings;
      use Tiny_String;

      --## rule off IMPROPER_INITIALIZATION
      Temp_Record: Base_Type_Data;
      type Base_Type_Nim_Data is record
         Name: chars_ptr;
         Color: chars_ptr;
         Description: chars_ptr;
      end record;
      Temp_Nim_Record: Base_Type_Nim_Data;
      Trade: Prices_Array;
      --## rule on IMPROPER_INITIALIZATION
      Index, Index2: Natural := 0;
      Base_Data: Unbounded_String := Null_Unbounded_String;
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
        (Base_Index: chars_ptr; Trade_Index: Integer; Trade: Prices_Array)
         return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "getAdaBaseTrade";
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
                   (Value
                      (Item =>
                         Get_Ada_Base_Data
                           (Base_Index => New_String(Str => Index'Img),
                            Item_Index => Index2,
                            Data_Type => New_String(Str => "recipe")))));
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
                   (Value
                      (Item =>
                         Get_Ada_Base_Data
                           (Base_Index => New_String(Str => Index'Img),
                            Item_Index => Index2,
                            Data_Type => New_String(Str => "flag")))));
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
                   (Value
                      (Item =>
                         Get_Ada_Base_Trade
                           (Base_Index => New_String(Str => Index'Img),
                            Trade_Index => Index2, Trade => Trade))));
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
   end Load_Bases_Types;

   function Is_Buyable
     (Base_Type: Tiny_String.Bounded_String;
      Item_Index: Objects_Container.Extended_Index;
      Check_Flag: Boolean := True; Base_Index: Extended_Base_Range := 0)
      return Boolean is
      use Bases;
      use Tiny_String;

   begin
      if Base_Index > 0
        and then Sky_Bases(Base_Index).Reputation.Level <
          Objects_Container.Element
            (Container => Items_List, Index => Item_Index)
            .Reputation then
         return False;
      end if;
      if Check_Flag
        and then
        (Bases_Types_List(Base_Type).Flags.Contains
           (Item => To_Unbounded_String(Source => "blackmarket")) and
         Get_Price(Base_Type => Base_Type, Item_Index => Item_Index) > 0) then
         return True;
      end if;
      if not Bases_Types_List(Base_Type).Trades.Contains
          (Key =>
             To_Bounded_String
               (Source =>
                  Trim
                    (Source => Positive'Image(Item_Index), Side => Left))) then
         return False;
      end if;
      if Bases_Types_List(Base_Type).Trades
          (To_Bounded_String
             (Source =>
                Trim(Source => Positive'Image(Item_Index), Side => Left)))
          (1) =
        0 then
         return False;
      end if;
      return True;
   end Is_Buyable;

   function Get_Price
     (Base_Type: Tiny_String.Bounded_String;
      Item_Index: Objects_Container.Extended_Index) return Natural is
      New_Item_Index: constant Tiny_String.Bounded_String :=
        Tiny_String.To_Bounded_String
          (Source => Trim(Source => Positive'Image(Item_Index), Side => Left));
   begin
      if Objects_Container.Element
          (Container => Items_List, Index => Item_Index)
          .Price =
        0 then
         return 0;
      end if;
      if Bases_Types_List(Base_Type).Trades.Contains
          (Key => New_Item_Index) then
         if Bases_Types_List(Base_Type).Trades(New_Item_Index)(1) > 0 then
            return Bases_Types_List(Base_Type).Trades(New_Item_Index)(1);
         elsif Bases_Types_List(Base_Type).Trades(New_Item_Index)(2) > 0 then
            return Bases_Types_List(Base_Type).Trades(New_Item_Index)(2);
         end if;
      end if;
      return
        Objects_Container.Element(Container => Items_List, Index => Item_Index)
          .Price;
   end Get_Price;

end BasesTypes;
