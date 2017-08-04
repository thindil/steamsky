--    Copyright 2017 Bartek thindil Jasicki
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
with Maps; use Maps;
with Messages; use Messages;
with Items; use Items;
with Ships; use Ships;
with Ships.Cargo; use Ships.Cargo;
with Ships.Crew; use Ships.Crew;
with Events; use Events;
with Bases; use Bases;
with Crew; use Crew;
with Game; use Game;

package body Trades is

   procedure BuyItems(BaseItemIndex: Positive; Amount: String) is
      BuyAmount, TraderIndex, Price, ProtoMoneyIndex: Positive;
      BaseIndex: constant Positive :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      BaseType: constant Positive :=
        Bases_Types'Pos(SkyBases(BaseIndex).BaseType) + 1;
      Cost, MoneyIndex2: Natural;
      EventIndex: constant Natural :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex;
      ItemIndex: constant Positive :=
        SkyBases(BaseIndex).Cargo(BaseItemIndex).ProtoIndex;
      ItemName: constant String := To_String(Items_List(ItemIndex).Name);
   begin
      BuyAmount := Positive'Value(Amount);
      if not Items_List(ItemIndex).Buyable(BaseType) then
         raise Trade_Cant_Buy with ItemName;
      end if;
      if SkyBases(BaseIndex).Cargo(BaseItemIndex).Amount = 0 then
         raise Trade_Not_For_Sale_Now with ItemName;
      elsif SkyBases(BaseIndex).Cargo(BaseItemIndex).Amount < BuyAmount then
         raise Trade_Buying_Too_Much with ItemName;
      end if;
      TraderIndex := FindMember(Talk);
      Price := SkyBases(BaseIndex).Cargo(BaseItemIndex).Price;
      if EventIndex > 0 then
         if Events_List(EventIndex).EType = DoublePrice and
           Events_List(EventIndex).Data = ItemIndex then
            Price := Price * 2;
         end if;
      end if;
      Cost := BuyAmount * Price;
      CountPrice(Cost, TraderIndex);
      ProtoMoneyIndex := FindProtoItem(MoneyIndex);
      MoneyIndex2 := FindCargo(ProtoMoneyIndex);
      if FreeCargo(Cost - (Items_List(ItemIndex).Weight * BuyAmount)) < 0 then
         raise Trade_No_Free_Cargo;
      end if;
      if MoneyIndex2 = 0 then
         raise Trade_No_Money with ItemName;
      end if;
      if Cost > PlayerShip.Cargo(MoneyIndex2).Amount then
         raise Trade_Not_Enough_Money with ItemName;
      end if;
      UpdateCargo
        (Ship => PlayerShip,
         CargoIndex => MoneyIndex2,
         Amount => (0 - Cost));
      UpdateBaseCargo(ProtoMoneyIndex, Cost);
      UpdateCargo
        (PlayerShip,
         ItemIndex,
         BuyAmount,
         SkyBases(BaseIndex).Cargo(BaseItemIndex).Durability);
      UpdateBaseCargo
        (CargoIndex => BaseItemIndex,
         Amount => (0 - BuyAmount),
         Durability =>
           SkyBases(BaseIndex).Cargo.Element(BaseItemIndex).Durability);
      GainExp(1, 4, TraderIndex);
      GainRep(BaseIndex, 1);
      AddMessage
        ("You bought" &
         Positive'Image(BuyAmount) &
         " " &
         ItemName &
         " for" &
         Positive'Image(Cost) &
         " " &
         To_String(MoneyName) &
         ".",
         TradeMessage);
      UpdateGame(5);
   exception
      when Constraint_Error =>
         raise Trade_Invalid_Amount;
   end BuyItems;

   procedure SellItems(ItemIndex: Positive; Amount: String) is
      SellAmount, TraderIndex: Positive;
      BaseIndex: constant Positive :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      BaseType: constant Positive :=
        Bases_Types'Pos(SkyBases(BaseIndex).BaseType) + 1;
      ProtoIndex: constant Positive := PlayerShip.Cargo(ItemIndex).ProtoIndex;
      ItemName: constant String := To_String(Items_List(ProtoIndex).Name);
      Profit, Price: Positive;
      EventIndex: constant Natural :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex;
      MoneyIndex2: constant Positive := FindProtoItem(MoneyIndex);
      BaseItemIndex: constant Natural := FindBaseCargo(ProtoIndex);
   begin
      SellAmount := Positive'Value(Amount);
      if PlayerShip.Cargo(ItemIndex).Amount < SellAmount then
         raise Trade_Too_Much_For_Sale with ItemName;
      end if;
      TraderIndex := FindMember(Talk);
      if BaseItemIndex = 0 then
         Price := Items_List(ProtoIndex).Prices(BaseType);
      else
         Price := SkyBases(BaseIndex).Cargo(BaseItemIndex).Price;
      end if;
      if EventIndex > 0 then
         if Events_List(EventIndex).EType = DoublePrice and
           Events_List(EventIndex).Data = ProtoIndex then
            Price := Price * 2;
         end if;
      end if;
      Profit := Price * SellAmount;
      if PlayerShip.Cargo(ItemIndex).Durability < 100 then
         Profit :=
           Positive
             (Float'Floor
                (Float(Profit) *
                 (Float(PlayerShip.Cargo(ItemIndex).Durability) / 100.0)));
      end if;
      CountPrice(Profit, TraderIndex, False);
      if FreeCargo((Items_List(ProtoIndex).Weight * SellAmount) - Profit) <
        0 then
         raise Trade_No_Free_Cargo;
      end if;
      if Profit > SkyBases(BaseIndex).Cargo(1).Amount then
         raise Trade_No_Money_In_Base with ItemName;
      end if;
      UpdateBaseCargo
        (ProtoIndex,
         SellAmount,
         PlayerShip.Cargo.Element(ItemIndex).Durability);
      UpdateCargo
        (Ship => PlayerShip,
         CargoIndex => ItemIndex,
         Amount => (0 - SellAmount),
         Durability => PlayerShip.Cargo.Element(ItemIndex).Durability);
      UpdateCargo(PlayerShip, MoneyIndex2, Profit);
      UpdateBaseCargo(MoneyIndex2, (0 - Profit));
      GainExp(1, 4, TraderIndex);
      GainRep(BaseIndex, 1);
      AddMessage
        ("You sold" &
         Positive'Image(SellAmount) &
         " " &
         ItemName &
         " for" &
         Positive'Image(Profit) &
         " " &
         To_String(MoneyName) &
         ".",
         TradeMessage);
      UpdateGame(5);
   exception
      when Constraint_Error =>
         raise Trade_Invalid_Amount;
   end SellItems;

end Trades;
