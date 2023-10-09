--    Copyright 2017-2023 Bartek thindil Jasicki
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
with Game; use Game;
with Maps; use Maps;

package body Trades is

   procedure Buy_Items
     (Base_Item_Index: BaseCargo_Container.Extended_Index; Amount: String) is
      use Interfaces.C;
      Base_Index: constant Extended_Base_Range :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      Result: chars_ptr;
      Ada_Result, Exception_Name: Unbounded_String := Null_Unbounded_String;
      Space_Index: Natural := 0;
      function Buy_Ada_Items
        (B_Item_Index: Natural; A: chars_ptr) return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "buyAdaItems";
   begin
      Set_Ship_In_Nim;
      Get_Base_Cargo(Base_Index => Base_Index);
      Result :=
        Buy_Ada_Items
          (B_Item_Index => Base_Item_Index, A => New_String(Str => Amount));
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
           To_Unbounded_String(Source => "NoFreeCargoError") then
            raise Trade_No_Free_Cargo;
         elsif Exception_Name =
           To_Unbounded_String(Source => "NoMoneyError") then
            raise Trade_No_Money
              with Slice
                (Source => Ada_Result, Low => Space_Index + 1,
                 High => Length(Source => Ada_Result));
         elsif Exception_Name =
           To_Unbounded_String(Source => "NotEnoughMoneyError") then
            raise Trade_Not_Enough_Money
              with Slice
                (Source => Ada_Result, Low => Space_Index + 1,
                 High => Length(Source => Ada_Result));
         end if;
      end if;
      Get_Ship_From_Nim(Ship => Player_Ship);
      Set_Base_Cargo(Base_Index => Base_Index);
   end Buy_Items;

   procedure Sell_Items
     (Item_Index: Inventory_Container.Extended_Index; Amount: String) is
      use Interfaces.C;

      Base_Index: constant Extended_Base_Range :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      Result: chars_ptr;
      Ada_Result, Exception_Name: Unbounded_String := Null_Unbounded_String;
      Space_Index: Natural := 0;
      function Sell_Ada_Items
        (I_Index: Natural; A: chars_ptr) return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "sellAdaItems";
   begin
      Set_Ship_In_Nim;
      Get_Base_Cargo(Base_Index => Base_Index);
      Result :=
        Sell_Ada_Items(I_Index => Item_Index, A => New_String(Str => Amount));
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
           To_Unbounded_String(Source => "NoFreeCargoError") then
            raise Trade_No_Free_Cargo;
         elsif Exception_Name =
           To_Unbounded_String(Source => "NoMoneyInBaseError") then
            raise Trade_No_Money_In_Base
              with Slice
                (Source => Ada_Result, Low => Space_Index + 1,
                 High => Length(Source => Ada_Result));
         end if;
      end if;
      Get_Ship_From_Nim(Ship => Player_Ship);
      Set_Base_Cargo(Base_Index => Base_Index);
   end Sell_Items;

   procedure Generate_Trader_Cargo(Proto_Index: Positive) is
      procedure Generate_Ada_Trader_Cargo(P_Index: Positive) with
         Import => True,
         Convention => C,
         External_Name => "generateAdaTraderCargo";
   begin
      BaseCargo_Container.Clear(Container => Trader_Cargo);
      Generate_Ada_Trader_Cargo(P_Index => Proto_Index);
      Set_Base_Cargo(Base_Index => 0);
   end Generate_Trader_Cargo;

end Trades;
