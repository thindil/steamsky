--    Copyright 2017-2024 Bartek thindil Jasicki
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Trades; use Trades;
with Maps; use Maps;

package body Bases.Trade is

   procedure Buy_Recipe(Recipe_Index: Tiny_String.Bounded_String) is
      use Interfaces.C;
      use Tiny_String;

      Base_Index: constant Extended_Base_Range :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      Result: chars_ptr;
      Ada_Result, Exception_Name: Unbounded_String := Null_Unbounded_String;
      Space_Index: Natural := 0;
      function Buy_Ada_Recipe(R_Index: chars_ptr) return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "buyAdaRecipe";
      Trade_Already_Known: exception;
   begin
      Set_Ship_In_Nim;
      Get_Base_Cargo(Base_Index => Base_Index);
      Get_Game_Date;
      Result :=
        Buy_Ada_Recipe
          (R_Index => New_String(Str => To_String(Source => Recipe_Index)));
      if Strlen(Item => Result) > 0 then
         Ada_Result := To_Unbounded_String(Source => Value(Item => Result));
         Space_Index := Index(Source => Ada_Result, Pattern => " ");
         if Space_Index > 0 then
            Exception_Name :=
              Unbounded_Slice
                (Source => Ada_Result, Low => 1, High => Space_Index - 1);
         end if;
         if Exception_Name =
           To_Unbounded_String(Source => "NoTraderError") then
            raise Trade_No_Trader;
         elsif Exception_Name =
           To_Unbounded_String(Source => "CantBuyError") then
            raise Trade_Cant_Buy;
         elsif Exception_Name =
           To_Unbounded_String(Source => "AlreadyKnownError") then
            raise Trade_Already_Known;
         end if;
      end if;
      Get_Ship_From_Nim(Ship => Player_Ship);
      Set_Base_Cargo(Base_Index => Base_Index);
      Set_Game_Date;
   end Buy_Recipe;

   procedure Heal_Wounded(Member_Index: Crew_Container.Extended_Index) is
      use Interfaces.C;

      Base_Index: constant Extended_Base_Range :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      Result: chars_ptr;
      Ada_Result, Exception_Name: Unbounded_String := Null_Unbounded_String;
      Space_Index: Natural := 0;
      function Heal_Ada_Wounded(M_Index: Integer) return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "healAdaWounded";
      Trade_Cant_Heal: exception;
   begin
      Set_Ship_In_Nim;
      Get_Base_Cargo(Base_Index => Base_Index);
      Get_Game_Date;
      Result := Heal_Ada_Wounded(M_Index => Member_Index);
      if Strlen(Item => Result) > 0 then
         Ada_Result := To_Unbounded_String(Source => Value(Item => Result));
         Space_Index := Index(Source => Ada_Result, Pattern => " ");
         if Space_Index > 0 then
            Exception_Name :=
              Unbounded_Slice
                (Source => Ada_Result, Low => 1, High => Space_Index - 1);
         end if;
         if Exception_Name =
           To_Unbounded_String(Source => "NoTraderError") then
            raise Trade_No_Trader;
         elsif Exception_Name =
           To_Unbounded_String(Source => "CantHealError") then
            raise Trade_Cant_Heal;
         end if;
      end if;
      Get_Ship_From_Nim(Ship => Player_Ship);
      Set_Base_Cargo(Base_Index => Base_Index);
      Set_Game_Date;
   end Heal_Wounded;

end Bases.Trade;
