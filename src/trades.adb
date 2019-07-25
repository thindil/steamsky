--    Copyright 2017-2019 Bartek thindil Jasicki
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
with Items; use Items;
with Ships.Cargo; use Ships.Cargo;
with Ships.Crew; use Ships.Crew;
with Events; use Events;
with Crew; use Crew;
with Game; use Game;
with Utils; use Utils;
with Bases.Cargo; use Bases.Cargo;

package body Trades is

   procedure BuyItems(BaseItemIndex: Positive; Amount: String) is
      BuyAmount, Price: Positive;
      BaseIndex: constant Natural :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      Cost, MoneyIndex2: Natural;
      EventIndex: constant Natural :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex;
      ItemName, ItemIndex: Unbounded_String;
      TraderIndex: constant Natural := FindMember(Talk);
   begin
      BuyAmount := Positive'Value(Amount);
      if TraderIndex = 0 then
         raise Trade_No_Trader;
      end if;
      if BaseIndex > 0 then
         ItemIndex := SkyBases(BaseIndex).Cargo(BaseItemIndex).ProtoIndex;
         ItemName := Items_List(ItemIndex).Name;
         Price := SkyBases(BaseIndex).Cargo(BaseItemIndex).Price;
         if EventIndex > 0
           and then
           (Events_List(EventIndex).EType = DoublePrice and
            Events_List(EventIndex).ItemIndex = ItemIndex) then
            Price := Price * 2;
         end if;
      else
         ItemIndex := TraderCargo(BaseItemIndex).ProtoIndex;
         ItemName := Items_List(ItemIndex).Name;
         if TraderCargo(BaseItemIndex).Amount < BuyAmount then
            raise Trade_Buying_Too_Much with To_String(ItemName);
         end if;
         Price := TraderCargo(BaseItemIndex).Price;
      end if;
      Cost := BuyAmount * Price;
      CountPrice(Cost, TraderIndex);
      MoneyIndex2 := FindItem(PlayerShip.Cargo, MoneyIndex);
      if FreeCargo(Cost - (Items_List(ItemIndex).Weight * BuyAmount)) < 0 then
         raise Trade_No_Free_Cargo;
      end if;
      if MoneyIndex2 = 0 then
         raise Trade_No_Money with To_String(ItemName);
      end if;
      if Cost > PlayerShip.Cargo(MoneyIndex2).Amount then
         raise Trade_Not_Enough_Money with To_String(ItemName);
      end if;
      UpdateCargo
        (Ship => PlayerShip, CargoIndex => MoneyIndex2, Amount => (0 - Cost));
      if BaseIndex > 0 then
         UpdateBaseCargo(MoneyIndex, Cost);
      else
         TraderCargo(1).Amount := TraderCargo(1).Amount + Cost;
      end if;
      if BaseIndex > 0 then
         UpdateCargo
           (Ship => PlayerShip, ProtoIndex => ItemIndex, Amount => BuyAmount,
            Durability => SkyBases(BaseIndex).Cargo(BaseItemIndex).Durability,
            Price => Price);
         UpdateBaseCargo
           (CargoIndex => BaseItemIndex, Amount => (0 - BuyAmount),
            Durability =>
              SkyBases(BaseIndex).Cargo.Element(BaseItemIndex).Durability);
         GainRep(BaseIndex, 1);
      else
         UpdateCargo
           (Ship => PlayerShip, ProtoIndex => ItemIndex, Amount => BuyAmount,
            Durability => TraderCargo(BaseItemIndex).Durability,
            Price => Price);
         TraderCargo(BaseItemIndex).Amount :=
           TraderCargo(BaseItemIndex).Amount - BuyAmount;
         if TraderCargo(BaseItemIndex).Amount = 0 then
            TraderCargo.Delete(Index => BaseItemIndex);
         end if;
      end if;
      GainExp(1, TalkingSkill, TraderIndex);
      AddMessage
        ("You bought" & Positive'Image(BuyAmount) & " " & To_String(ItemName) &
         " for" & Positive'Image(Cost) & " " & To_String(MoneyName) & ".",
         TradeMessage);
      if BaseIndex = 0 and EventIndex > 0 then
         Events_List(EventIndex).Time := Events_List(EventIndex).Time + 5;
      end if;
      UpdateGame(5);
   exception
      when Constraint_Error =>
         raise Trade_Invalid_Amount;
   end BuyItems;

   procedure SellItems(ItemIndex: Positive; Amount: String) is
      SellAmount: Positive;
      BaseIndex: constant Natural :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      ProtoIndex: constant Unbounded_String :=
        PlayerShip.Cargo(ItemIndex).ProtoIndex;
      ItemName: constant String := To_String(Items_List(ProtoIndex).Name);
      Profit, Price, BaseType: Positive;
      EventIndex: constant Natural :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex;
      BaseItemIndex: Natural := 0;
      CargoAdded: Boolean := False;
      TraderIndex: constant Natural := FindMember(Talk);
   begin
      SellAmount := Positive'Value(Amount);
      if TraderIndex = 0 then
         raise Trade_No_Trader;
      end if;
      if BaseIndex > 0 then
         BaseType := Bases_Types'Pos(SkyBases(BaseIndex).BaseType) + 1;
         BaseItemIndex := FindBaseCargo(ProtoIndex);
      else
         BaseType := 1;
         for I in TraderCargo.Iterate loop
            if TraderCargo(I).ProtoIndex = ProtoIndex then
               BaseItemIndex := BaseCargo_Container.To_Index(I);
               exit;
            end if;
         end loop;
      end if;
      if BaseItemIndex = 0 then
         Price := Items_List(ProtoIndex).Prices(BaseType);
      else
         if BaseIndex > 0 then
            Price := SkyBases(BaseIndex).Cargo(BaseItemIndex).Price;
         else
            Price := TraderCargo(BaseItemIndex).Price;
         end if;
      end if;
      if EventIndex > 0 and then Events_List(EventIndex).EType = DoublePrice
        and then Events_List(EventIndex).ItemIndex = ProtoIndex then
         Price := Price * 2;
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
      for Member of PlayerShip.Crew loop
         if Member.Payment(2) > 0 then
            Profit :=
              Profit -
              Positive
                (Float'Floor
                   (Float(Profit) * (Float(Member.Payment(2)) / 100.0)));
         end if;
      end loop;
      if FreeCargo((Items_List(ProtoIndex).Weight * SellAmount) - Profit) <
        0 then
         raise Trade_No_Free_Cargo;
      end if;
      if BaseIndex > 0 then
         if Profit > SkyBases(BaseIndex).Cargo(1).Amount then
            raise Trade_No_Money_In_Base with ItemName;
         end if;
         UpdateBaseCargo
           (ProtoIndex, SellAmount,
            PlayerShip.Cargo.Element(ItemIndex).Durability);
      else
         if Profit > TraderCargo(1).Amount then
            raise Trade_No_Money_In_Base with ItemName;
         end if;
         for I in TraderCargo.Iterate loop
            if TraderCargo(I).ProtoIndex = ProtoIndex and
              TraderCargo(I).Durability =
                PlayerShip.Cargo(ItemIndex).Durability then
               TraderCargo(I).Amount := TraderCargo(I).Amount + SellAmount;
               CargoAdded := True;
               exit;
            end if;
         end loop;
         if not CargoAdded then
            BaseType := GetRandom(1, 4);
            TraderCargo.Append
              (New_Item =>
                 (ProtoIndex => ProtoIndex, Amount => SellAmount,
                  Durability => PlayerShip.Cargo(ItemIndex).Durability,
                  Price => Items_List(ProtoIndex).Prices(BaseType)));
         end if;
      end if;
      UpdateCargo
        (Ship => PlayerShip, CargoIndex => ItemIndex,
         Amount => (0 - SellAmount),
         Price => PlayerShip.Cargo.Element(ItemIndex).Price);
      UpdateCargo(PlayerShip, MoneyIndex, Profit);
      if BaseIndex > 0 then
         UpdateBaseCargo(MoneyIndex, (0 - Profit));
         GainRep(BaseIndex, 1);
      else
         TraderCargo(1).Amount := TraderCargo(1).Amount - Profit;
      end if;
      GainExp(1, TalkingSkill, TraderIndex);
      AddMessage
        ("You sold" & Positive'Image(SellAmount) & " " & ItemName & " for" &
         Positive'Image(Profit) & " " & To_String(MoneyName) & ".",
         TradeMessage);
      if BaseIndex = 0 and EventIndex > 0 then
         Events_List(EventIndex).Time := Events_List(EventIndex).Time + 5;
      end if;
      UpdateGame(5);
   exception
      when Constraint_Error =>
         raise Trade_Invalid_Amount;
   end SellItems;

   procedure GenerateTraderCargo(ProtoIndex: Unbounded_String) is
      TraderShip: ShipRecord :=
        CreateShip
          (ProtoIndex, Null_Unbounded_String, PlayerShip.SkyX, PlayerShip.SkyY,
           FULL_STOP);
      BaseType, CargoAmount, CargoItemIndex, ItemIndex: Natural;
      ItemAmount: Positive;
      NewItemIndex: Unbounded_String;
   begin
      TraderCargo.Clear;
      for Item of TraderShip.Cargo loop
         BaseType := GetRandom(1, 4);
         TraderCargo.Append
           (New_Item =>
              (ProtoIndex => Item.ProtoIndex, Amount => Item.Amount,
               Durability => 100,
               Price => Items_List(Item.ProtoIndex).Prices(BaseType)));
      end loop;
      if TraderShip.Crew.Length < 5 then
         CargoAmount := GetRandom(1, 3);
      elsif TraderShip.Crew.Length < 10 then
         CargoAmount := GetRandom(1, 5);
      else
         CargoAmount := GetRandom(1, 10);
      end if;
      while CargoAmount > 0 loop
         if TraderShip.Crew.Length < 5 then
            ItemAmount := GetRandom(1, 100);
         elsif TraderShip.Crew.Length < 10 then
            ItemAmount := GetRandom(1, 500);
         else
            ItemAmount := GetRandom(1, 1000);
         end if;
         ItemIndex := GetRandom(1, Positive(Items_List.Length));
         for I in Items_List.Iterate loop
            ItemIndex := ItemIndex - 1;
            if ItemIndex = 0 then
               NewItemIndex := Objects_Container.Key(I);
               exit;
            end if;
         end loop;
         CargoItemIndex := FindItem(TraderShip.Cargo, NewItemIndex);
         if CargoItemIndex > 0 then
            TraderCargo(CargoItemIndex).Amount :=
              TraderCargo(CargoItemIndex).Amount + ItemAmount;
            TraderShip.Cargo(CargoItemIndex).Amount :=
              TraderShip.Cargo(CargoItemIndex).Amount + ItemAmount;
         else
            if FreeCargo(0 - (Items_List(NewItemIndex).Weight * ItemAmount)) >
              -1 then
               BaseType := GetRandom(1, 4);
               TraderCargo.Append
                 (New_Item =>
                    (ProtoIndex => NewItemIndex, Amount => ItemAmount,
                     Durability => 100,
                     Price => Items_List(NewItemIndex).Prices(BaseType)));
               TraderShip.Cargo.Append
                 (New_Item =>
                    (ProtoIndex => NewItemIndex, Amount => ItemAmount,
                     Durability => 100, Name => Null_Unbounded_String,
                     Price => 0));
            else
               CargoAmount := 1;
            end if;
         end if;
         CargoAmount := CargoAmount - 1;
      end loop;
   end GenerateTraderCargo;

end Trades;
