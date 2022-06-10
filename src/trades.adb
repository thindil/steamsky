--    Copyright 2017-2022 Bartek thindil Jasicki
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

with Ada.Containers; use Ada.Containers;
with Maps; use Maps;
with Messages; use Messages;
with Ships.Cargo; use Ships.Cargo;
with Ships.Crew; use Ships.Crew;
with Events; use Events;
with Game; use Game;
with Crew; use Crew;
with Utils; use Utils;
with Bases.Cargo; use Bases.Cargo;
with BasesTypes; use BasesTypes;

package body Trades is

   procedure Buy_Items
     (Base_Item_Index: BaseCargo_Container.Extended_Index; Amount: String) is
      use Tiny_String;

      Buy_Amount, Price: Positive;
      Base_Index: constant Extended_Base_Range :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      Cost: Natural;
      Money_Index_2: Inventory_Container.Extended_Index;
      Event_Index: constant Events_Container.Extended_Index :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Event_Index;
      Item_Name: Bounded_String;
      Trader_Index: constant Crew_Container.Extended_Index := Find_Member(Order => TALK);
      Item_Index: Objects_Container.Extended_Index;
      Item: Base_Cargo;
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
         Item_Name :=
           Objects_Container.Element
             (Container => Items_List, Index => Item_Index)
             .Name;
         Price :=
           BaseCargo_Container.Element
             (Container => Sky_Bases(Base_Index).Cargo,
              Index => Base_Item_Index)
             .Price;
         if Event_Index > 0
           and then
           (Events_List(Event_Index).E_Type = DOUBLEPRICE and
            Events_List(Event_Index).Item_Index = Item_Index) then
            Price := Price * 2;
         end if;
      else
         Item_Index :=
           BaseCargo_Container.Element
             (Container => Trader_Cargo, Index => Base_Item_Index)
             .Proto_Index;
         Item_Name :=
           Objects_Container.Element
             (Container => Items_List, Index => Item_Index)
             .Name;
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
      Money_Index_2 := Find_Item(Inventory => Player_Ship.Cargo, Proto_Index => Money_Index);
      if Free_Cargo
          (Amount => Cost -
           (Objects_Container.Element
              (Container => Items_List, Index => Item_Index)
              .Weight *
            Buy_Amount)) <
        0 then
         raise Trade_No_Free_Cargo;
      end if;
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
        (Ship => Player_Ship, Cargo_Index => Money_Index_2, Amount => -(Cost));
      if Base_Index > 0 then
         Update_Base_Cargo(Proto_Index => Money_Index, Amount => Cost);
      else
         Item :=
           BaseCargo_Container.Element(Container => Trader_Cargo, Index => 1);
         Item.Amount := Item.Amount + Cost;
         BaseCargo_Container.Replace_Element
           (Container => Trader_Cargo, Index => 1, New_Item => Item);
      end if;
      if Base_Index > 0 then
         Update_Cargo
           (Ship => Player_Ship, Proto_Index => Item_Index, Amount => Buy_Amount,
            Durability =>
              BaseCargo_Container.Element
                (Container => Sky_Bases(Base_Index).Cargo,
                 Index => Base_Item_Index)
                .Durability,
            Price => Price);
         Update_Base_Cargo
           (Cargo_Index => Base_Item_Index, Amount => -(Buy_Amount),
            Durability =>
              BaseCargo_Container.Element
                (Container => Sky_Bases(Base_Index).Cargo,
                 Index => Base_Item_Index)
                .Durability);
         Gain_Rep(Base_Index => Base_Index, Points => 1);
      else
         Update_Cargo
           (Ship => Player_Ship, Proto_Index => Item_Index, Amount => Buy_Amount,
            Durability =>
              BaseCargo_Container.Element
                (Container => Trader_Cargo, Index => Base_Item_Index)
                .Durability,
            Price => Price);
         Item :=
           BaseCargo_Container.Element
             (Container => Trader_Cargo, Index => Base_Item_Index);
         Item.Amount := Item.Amount - Buy_Amount;
         if Item.Amount = 0 then
            BaseCargo_Container.Delete
              (Container => Trader_Cargo, Index => Base_Item_Index);
         else
            BaseCargo_Container.Replace_Element
              (Container => Trader_Cargo, Index => Base_Item_Index,
               New_Item => Item);
         end if;
      end if;
      Gain_Exp(1, Talking_Skill, Trader_Index);
      Show_Log_Block :
      declare
         Gain: constant Integer := (Buy_Amount * Price) - Cost;
      begin
         Add_Message
           ("You bought" & Positive'Image(Buy_Amount) & " " &
            To_String(Item_Name) & " for" & Positive'Image(Cost) & " " &
            To_String(Money_Name) & "." &
            (if Gain = 0 then ""
             else " You " & (if Gain > 0 then "gain" else "lost") &
               Integer'Image(abs (Gain)) & " " & To_String(Money_Name) &
               " compared to the base price."),
            TRADEMESSAGE);
      end Show_Log_Block;
      if Base_Index = 0 and Event_Index > 0 then
         Events_List(Event_Index).Time := Events_List(Event_Index).Time + 5;
      end if;
      Update_Game(5);
   exception
      when Constraint_Error =>
         raise Trade_Invalid_Amount;
   end Buy_Items;

   procedure Sell_Items
     (Item_Index: Inventory_Container.Extended_Index; Amount: String) is
      use Tiny_String;

      SellAmount: Positive;
      BaseIndex: constant Extended_Base_Range :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      ProtoIndex: constant Objects_Container.Extended_Index :=
        Inventory_Container.Element
          (Container => Player_Ship.Cargo, Index => Item_Index)
          .Proto_Index;
      ItemName: constant String :=
        To_String
          (Objects_Container.Element
             (Container => Items_List, Index => ProtoIndex)
             .Name);
      Price: Positive;
      EventIndex: constant Events_Container.Extended_Index :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Event_Index;
      Base_Item_Index: Natural := 0;
      CargoAdded: Boolean := False;
      TraderIndex: constant Crew_Container.Extended_Index := Find_Member(TALK);
      Profit: Integer;
      Item: Base_Cargo;
   begin
      SellAmount := Positive'Value(Amount);
      if TraderIndex = 0 then
         raise Trade_No_Trader;
      end if;
      if BaseIndex > 0 then
         Base_Item_Index := Find_Base_Cargo(ProtoIndex);
      else
         Find_Base_Index_Loop :
         for I in
           BaseCargo_Container.First_Index(Container => Trader_Cargo) ..
             BaseCargo_Container.Last_Index(Container => Trader_Cargo) loop
            if BaseCargo_Container.Element
                (Container => Trader_Cargo, Index => I)
                .Proto_Index =
              ProtoIndex then
               Base_Item_Index := I;
               exit Find_Base_Index_Loop;
            end if;
         end loop Find_Base_Index_Loop;
      end if;
      if Base_Item_Index = 0 then
         Price := Get_Price(Sky_Bases(BaseIndex).Base_Type, ProtoIndex);
      else
         Price :=
           (if BaseIndex > 0 then
              BaseCargo_Container.Element
                (Container => Sky_Bases(BaseIndex).Cargo,
                 Index => Base_Item_Index)
                .Price
            else BaseCargo_Container.Element
                (Container => Trader_Cargo, Index => Base_Item_Index)
                .Price);
      end if;
      if EventIndex > 0 and then Events_List(EventIndex).E_Type = DOUBLEPRICE
        and then Events_List(EventIndex).Item_Index = ProtoIndex then
         Price := Price * 2;
      end if;
      Profit := Price * SellAmount;
      if Inventory_Container.Element
          (Container => Player_Ship.Cargo, Index => Item_Index)
          .Durability <
        100 then
         Profit :=
           Positive
             (Float'Floor
                (Float(Profit) *
                 (Float
                    (Inventory_Container.Element
                       (Container => Player_Ship.Cargo, Index => Item_Index)
                       .Durability) /
                  100.0)));
      end if;
      Count_Price(Profit, TraderIndex, False);
      Pay_Trade_Profit_Loop :
      for I in Player_Ship.Crew.Iterate loop
         if Player_Ship.Crew(I).Payment(2) = 0 then
            goto End_Of_Loop;
         end if;
         if Profit < 1 then
            Update_Morale
              (Player_Ship, Crew_Container.To_Index(I), Get_Random(-25, -5));
            Add_Message
              (To_String(Player_Ship.Crew(I).Name) &
               " is sad because doesn't get own part of profit.",
               TRADEMESSAGE, RED);
            Profit := 0;
            goto End_Of_Loop;
         end if;
         Profit :=
           Profit -
           Positive
             (Float'Ceiling
                (Float(Profit) *
                 (Float(Player_Ship.Crew(I).Payment(2)) / 100.0)));
         if Profit < 1 then
            if Profit < 0 then
               Update_Morale
                 (Player_Ship, Crew_Container.To_Index(I),
                  Get_Random(-12, -2));
               Add_Message
                 (To_String(Player_Ship.Crew(I).Name) &
                  " is sad because doesn't get own part of profit.",
                  TRADEMESSAGE, RED);
            end if;
            Profit := 0;
         end if;
         <<End_Of_Loop>>
      end loop Pay_Trade_Profit_Loop;
      if Free_Cargo
          ((Objects_Container.Element
              (Container => Items_List, Index => ProtoIndex)
              .Weight *
            SellAmount) -
           Profit) <
        0 then
         raise Trade_No_Free_Cargo;
      end if;
      if BaseIndex > 0 then
         if Profit >
           BaseCargo_Container.Element
             (Container => Sky_Bases(BaseIndex).Cargo, Index => 1)
             .Amount then
            raise Trade_No_Money_In_Base with ItemName;
         end if;
         Update_Base_Cargo
           (ProtoIndex, SellAmount,
            Inventory_Container.Element
              (Container => Player_Ship.Cargo, Index => Item_Index)
              .Durability);
      else
         if Profit >
           BaseCargo_Container.Element(Container => Trader_Cargo, Index => 1)
             .Amount then
            raise Trade_No_Money_In_Base with ItemName;
         end if;
         Update_Trader_Cargo_Loop :
         for I in
           BaseCargo_Container.First_Index(Container => Trader_Cargo) ..
             BaseCargo_Container.Last_Index(Container => Trader_Cargo) loop
            Item :=
              BaseCargo_Container.Element
                (Container => Trader_Cargo, Index => I);
            if Item.Proto_Index = ProtoIndex and
              Item.Durability =
                Inventory_Container.Element
                  (Container => Player_Ship.Cargo, Index => Item_Index)
                  .Durability then
               Item.Amount := Item.Amount + SellAmount;
               BaseCargo_Container.Replace_Element
                 (Container => Trader_Cargo, Index => I, New_Item => Item);
               CargoAdded := True;
               exit Update_Trader_Cargo_Loop;
            end if;
         end loop Update_Trader_Cargo_Loop;
         if not CargoAdded then
            BaseCargo_Container.Append
              (Container => Trader_Cargo,
               New_Item =>
                 (Proto_Index => ProtoIndex, Amount => SellAmount,
                  Durability =>
                    Inventory_Container.Element
                      (Container => Player_Ship.Cargo, Index => Item_Index)
                      .Durability,
                  Price =>
                    Objects_Container.Element
                      (Container => Items_List, Index => ProtoIndex)
                      .Price));
         end if;
      end if;
      Update_Cargo
        (Ship => Player_Ship, Cargo_Index => Item_Index,
         Amount => (0 - SellAmount),
         Price =>
           Inventory_Container.Element
             (Container => Player_Ship.Cargo, Index => Item_Index)
             .Price);
      Update_Cargo(Player_Ship, Money_Index, Profit);
      if BaseIndex > 0 then
         Update_Base_Cargo(Money_Index, -(Profit));
         Gain_Rep(BaseIndex, 1);
         if Objects_Container.Element
             (Container => Items_List, Index => ProtoIndex)
             .Reputation >
           Sky_Bases(BaseIndex).Reputation.Level then
            Gain_Rep(BaseIndex, 1);
         end if;
      else
         Item :=
           BaseCargo_Container.Element(Container => Trader_Cargo, Index => 1);
         Item.Amount := Item.Amount - Profit;
         BaseCargo_Container.Replace_Element
           (Container => Trader_Cargo, Index => 1, New_Item => Item);
      end if;
      Gain_Exp(1, Talking_Skill, TraderIndex);
      Show_Log_Block :
      declare
         Gain: constant Integer := Profit - (SellAmount * Price);
      begin
         Add_Message
           ("You sold" & Positive'Image(SellAmount) & " " & ItemName & " for" &
            Positive'Image(Profit) & " " & To_String(Money_Name) & "." &
            (if Gain = 0 then ""
             else " You " & (if Gain > 0 then "gain" else "lost") &
               Integer'Image(abs (Gain)) & " " & To_String(Money_Name) &
               " compared to the base price."),
            TRADEMESSAGE);
      end Show_Log_Block;
      if BaseIndex = 0 and EventIndex > 0 then
         Events_List(EventIndex).Time := Events_List(EventIndex).Time + 5;
      end if;
      Update_Game(5);
   exception
      when Constraint_Error =>
         raise Trade_Invalid_Amount;
   end Sell_Items;

   procedure Generate_Trader_Cargo
     (Proto_Index: Proto_Ships_Container.Extended_Index) is
      use Tiny_String;

      TraderShip: Ship_Record :=
        Create_Ship
          (Proto_Index, Null_Bounded_String, Player_Ship.Sky_X,
           Player_Ship.Sky_Y, FULL_STOP);
      CargoAmount: Natural range 0 .. 10 :=
        (if TraderShip.Crew.Length < 5 then Get_Random(1, 3)
         elsif TraderShip.Crew.Length < 10 then Get_Random(1, 5)
         else Get_Random(1, 10));
      CargoItemIndex, ItemIndex: Inventory_Container.Extended_Index;
      ItemAmount: Positive range 1 .. 1_000;
      NewItemIndex: Objects_Container.Extended_Index;
      Item: Inventory_Data;
      TraderItem: Base_Cargo;
   begin
      BaseCargo_Container.Clear(Container => Trader_Cargo);
      Add_Items_To_Cargo_Loop :
      for Item of TraderShip.Cargo loop
         BaseCargo_Container.Append
           (Container => Trader_Cargo,
            New_Item =>
              (Proto_Index => Item.Proto_Index, Amount => Item.Amount,
               Durability => 100,
               Price =>
                 Objects_Container.Element
                   (Container => Items_List, Index => Item.Proto_Index)
                   .Price));
      end loop Add_Items_To_Cargo_Loop;
      Generate_Cargo_Loop :
      while CargoAmount > 0 loop
         ItemAmount :=
           (if TraderShip.Crew.Length < 5 then Get_Random(1, 100)
            elsif TraderShip.Crew.Length < 10 then Get_Random(1, 500)
            else Get_Random(1, 1_000));
         ItemIndex :=
           Get_Random
             (1, Positive(Objects_Container.Length(Container => Items_List)));
         Find_Item_Index_Loop :
         for I in
           Objects_Container.First_Index(Container => Items_List) ..
             Objects_Container.Last_Index(Container => Items_List) loop
            ItemIndex := ItemIndex - 1;
            if ItemIndex = 0 then
               NewItemIndex := I;
               exit Find_Item_Index_Loop;
            end if;
         end loop Find_Item_Index_Loop;
         CargoItemIndex := Find_Item(TraderShip.Cargo, NewItemIndex);
         if CargoItemIndex > 0 then
            TraderItem :=
              BaseCargo_Container.Element
                (Container => Trader_Cargo, Index => CargoItemIndex);
            TraderItem.Amount := TraderItem.Amount + ItemAmount;
            BaseCargo_Container.Replace_Element
              (Container => Trader_Cargo, Index => CargoItemIndex,
               New_Item => TraderItem);
            Item :=
              Inventory_Container.Element
                (Container => TraderShip.Cargo, Index => CargoItemIndex);
            Item.Amount := Item.Amount + ItemAmount;
            Inventory_Container.Replace_Element
              (Container => TraderShip.Cargo, Index => CargoItemIndex,
               New_Item => Item);
         else
            if Free_Cargo
                (0 -
                 (Objects_Container.Element
                    (Container => Items_List, Index => NewItemIndex)
                    .Weight *
                  ItemAmount)) >
              -1 then
               BaseCargo_Container.Append
                 (Container => Trader_Cargo,
                  New_Item =>
                    (Proto_Index => NewItemIndex, Amount => ItemAmount,
                     Durability => 100,
                     Price =>
                       Objects_Container.Element
                         (Container => Items_List, Index => NewItemIndex)
                         .Price));
               Inventory_Container.Append
                 (Container => TraderShip.Cargo,
                  New_Item =>
                    (Proto_Index => NewItemIndex, Amount => ItemAmount,
                     Durability => 100, Name => Null_Bounded_String,
                     Price => 0));
            else
               CargoAmount := 1;
            end if;
         end if;
         CargoAmount := CargoAmount - 1;
      end loop Generate_Cargo_Loop;
   end Generate_Trader_Cargo;

end Trades;
