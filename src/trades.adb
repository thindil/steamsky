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

with Interfaces.C.Strings;
with Bases.Cargo;
with Crew;
with Events;
with Game; use Game;
with Maps; use Maps;
with Messages;
with Ships.Cargo;
with Ships.Crew;

package body Trades is

   procedure Buy_Items
     (Base_Item_Index: BaseCargo_Container.Extended_Index; Amount: String) is
      use Bases.Cargo;
      use Crew;
      use Events;
      use Messages;
      use Ships.Cargo;
      use Ships.Crew;
      use Tiny_String;

      Buy_Amount, Price: Positive;
      Base_Index: constant Extended_Base_Range :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      Cost: Natural;
      Money_Index_2: Inventory_Container.Extended_Index;
      Event_Index: constant Natural :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Event_Index;
      Item_Name: Bounded_String;
      Trader_Index: constant Crew_Container.Extended_Index :=
        Find_Member(Order => TALK);
      Item_Index: Natural;
      Item: Base_Cargo := (others => <>);
   begin
      Buy_Amount := Positive'Value(Amount);
      if Trader_Index = 0 then
         raise Trade_No_Trader;
      end if;
      if Base_Index > 0 then
         Item_Index :=
           BaseCargo_Container.Element
             (Container => Sky_Bases(Base_Index).Cargo,
              Index => Base_Item_Index)
             .Proto_Index;
         Item_Name := Get_Proto_Item(Index => Item_Index).Name;
         Price :=
           BaseCargo_Container.Element
             (Container => Sky_Bases(Base_Index).Cargo,
              Index => Base_Item_Index)
             .Price;
         if Event_Index > 0
           and then
           (Get_Event(Index => Event_Index).E_Type = DOUBLEPRICE and
            Get_Event(Index => Event_Index).Item_Index = Item_Index) then
            Price := Price * 2;
         end if;
      else
         Item_Index :=
           BaseCargo_Container.Element
             (Container => Trader_Cargo, Index => Base_Item_Index)
             .Proto_Index;
         Item_Name := Get_Proto_Item(Index => Item_Index).Name;
         if BaseCargo_Container.Element
             (Container => Trader_Cargo, Index => Base_Item_Index)
             .Amount <
           Buy_Amount then
            raise Trade_Buying_Too_Much with To_String(Source => Item_Name);
         end if;
         Price :=
           BaseCargo_Container.Element
             (Container => Trader_Cargo, Index => Base_Item_Index)
             .Price;
      end if;
      Cost := Buy_Amount * Price;
      Count_Price(Price => Cost, Trader_Index => Trader_Index);
      Money_Index_2 :=
        Find_Item(Inventory => Player_Ship.Cargo, Proto_Index => Money_Index);
      --## rule off SIMPLIFIABLE_EXPRESSIONS
      if Free_Cargo
          (Amount =>
             Cost -
             (Get_Proto_Item(Index => Item_Index).Weight * Buy_Amount)) <
        0 then
         raise Trade_No_Free_Cargo;
      end if;
      --## rule on SIMPLIFIABLE_EXPRESSIONS
      if Money_Index_2 = 0 then
         raise Trade_No_Money with To_String(Source => Item_Name);
      end if;
      if Cost >
        Inventory_Container.Element
          (Container => Player_Ship.Cargo, Index => Money_Index_2)
          .Amount then
         raise Trade_Not_Enough_Money with To_String(Source => Item_Name);
      end if;
      Update_Cargo
        (Ship => Player_Ship, Cargo_Index => Money_Index_2, Amount => -Cost);
      if Base_Index > 0 then
         Update_Base_Cargo(Proto_Index => Money_Index, Amount => Cost);
      else
         Item :=
           BaseCargo_Container.Element(Container => Trader_Cargo, Index => 1);
         Item.Amount := Item.Amount + Cost; --## rule line off ASSIGNMENTS
         BaseCargo_Container.Replace_Element
           (Container => Trader_Cargo, Index => 1, New_Item => Item);
      end if;
      if Base_Index > 0 then
         Update_Cargo
           (Ship => Player_Ship, Proto_Index => Item_Index,
            Amount => Buy_Amount,
            Durability =>
              BaseCargo_Container.Element
                (Container => Sky_Bases(Base_Index).Cargo,
                 Index => Base_Item_Index)
                .Durability,
            Price => Price);
         Update_Base_Cargo
           (Cargo_Index => Base_Item_Index, Amount => -Buy_Amount,
            Durability =>
              BaseCargo_Container.Element
                (Container => Sky_Bases(Base_Index).Cargo,
                 Index => Base_Item_Index)
                .Durability);
         Gain_Rep(Base_Index => Base_Index, Points => 1);
      else
         Update_Cargo
           (Ship => Player_Ship, Proto_Index => Item_Index,
            Amount => Buy_Amount,
            Durability =>
              BaseCargo_Container.Element
                (Container => Trader_Cargo, Index => Base_Item_Index)
                .Durability,
            Price => Price);
         Item :=
           BaseCargo_Container.Element
             (Container => Trader_Cargo, Index => Base_Item_Index);
         --## rule off ASSIGNMENTS
         Item.Amount := Item.Amount - Buy_Amount;
         --## rule on ASSIGNMENTS
         if Item.Amount = 0 then
            BaseCargo_Container.Delete
              (Container => Trader_Cargo, Index => Base_Item_Index);
         else
            BaseCargo_Container.Replace_Element
              (Container => Trader_Cargo, Index => Base_Item_Index,
               New_Item => Item);
         end if;
      end if;
      Gain_Exp
        (Amount => 1, Skill_Number => Talking_Skill,
         Crew_Index => Trader_Index);
      Show_Log_Block :
      declare
         --## rule off SIMPLIFIABLE_EXPRESSIONS
         Gain: constant Integer := (Buy_Amount * Price) - Cost;
         --## rule on SIMPLIFIABLE_EXPRESSIONS
         Event: Event_Data := (others => <>);
      begin
         Add_Message
           (Message =>
              "You bought" & Positive'Image(Buy_Amount) & " " &
              To_String(Source => Item_Name) & " for" & Positive'Image(Cost) &
              " " & To_String(Source => Money_Name) & "." &
              (if Gain = 0 then ""
               else " You " & (if Gain > 0 then "gain" else "lost") &
                 Integer'Image(abs Gain) & " " &
                 To_String(Source => Money_Name) &
                 " compared to the base price."),
            M_Type => TRADEMESSAGE);
         if Base_Index = 0 and Event_Index > 0 then
            Event := Get_Event(Index => Event_Index);
            Event.Time := Event.Time + 5; --## rule line off ASSIGNMENTS
            Get_Ada_Event
              (Index => Event_Index, X => Event.Sky_X, Y => Event.Sky_Y,
               Time => Event.Time, Data => Event.Ship_Index,
               E_Type => Events_Types'Pos(Event.E_Type));
         end if;
      end Show_Log_Block;
      Update_Game(Minutes => 5);
   exception
      when Constraint_Error =>
         raise Trade_Invalid_Amount;
   end Buy_Items;

   procedure Sell_Items
     (Item_Index: Inventory_Container.Extended_Index; Amount: String) is
      use Interfaces.C;
      use Interfaces.C.Strings;

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
