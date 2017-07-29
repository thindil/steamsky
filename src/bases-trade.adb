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

with Maps; use Maps;
with Messages; use Messages;
with Items; use Items;
with Ships; use Ships;
with Ships.Cargo; use Ships.Cargo;
with Ships.Crew; use Ships.Crew;
with Events; use Events;
with Crafts; use Crafts;

package body Bases.Trade is

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
      Price := Items_List(ItemIndex).Prices(BaseType);
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
      UpdateCargo(PlayerShip, ProtoMoneyIndex, (0 - Cost));
      UpdateBaseCargo(ProtoMoneyIndex, Cost);
      UpdateCargo
        (PlayerShip,
         ItemIndex,
         BuyAmount,
         SkyBases(BaseIndex).Cargo(BaseItemIndex).Durability);
      UpdateBaseCargo
        (ItemIndex,
         (0 - BuyAmount),
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
   begin
      SellAmount := Positive'Value(Amount);
      if PlayerShip.Cargo(ItemIndex).Amount < SellAmount then
         raise Trade_Too_Much_For_Sale with ItemName;
      end if;
      TraderIndex := FindMember(Talk);
      Price := Items_List(ProtoIndex).Prices(BaseType);
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
      if PlayerShip.Cargo(ItemIndex).Durability = 100 then
         for Item of SkyBases(BaseIndex).Cargo loop
            if Item.ProtoIndex = ProtoIndex then
               Item.Amount := Item.Amount + SellAmount;
               exit;
            end if;
         end loop;
      end if;
      UpdateBaseCargo
        (ProtoIndex,
         SellAmount,
         PlayerShip.Cargo.Element(ItemIndex).Durability);
      UpdateCargo
        (PlayerShip,
         ProtoIndex,
         (0 - SellAmount),
         PlayerShip.Cargo.Element(ItemIndex).Durability);
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

   procedure HireRecruit(RecruitIndex: Positive) is
      BaseIndex: constant Positive :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      MoneyIndex2, Price: Natural;
      Recruit: constant Recruit_Data :=
        SkyBases(BaseIndex).Recruits(RecruitIndex);
      TraderIndex, ProtoMoneyIndex: Positive;
   begin
      ProtoMoneyIndex := FindProtoItem(MoneyIndex);
      MoneyIndex2 := FindCargo(ProtoMoneyIndex);
      if MoneyIndex2 = 0 then
         raise Trade_No_Money;
      end if;
      TraderIndex := FindMember(Talk);
      Price := Recruit.Price;
      CountPrice(Price, TraderIndex);
      if PlayerShip.Cargo(MoneyIndex2).Amount < Price then
         raise Trade_Not_Enough_Money with To_String(Recruit.Name);
      end if;
      PlayerShip.Crew.Append
      (New_Item =>
         (Name => Recruit.Name,
          Gender => Recruit.Gender,
          Health => 100,
          Tired => 0,
          Skills => Recruit.Skills,
          Hunger => 0,
          Thirst => 0,
          Order => Rest,
          PreviousOrder => Rest,
          OrderTime => 15,
          Orders => (others => 0)));
      UpdateCargo(PlayerShip, ProtoMoneyIndex, (0 - Price));
      GainExp(1, 4, TraderIndex);
      GainRep(BaseIndex, 1);
      AddMessage
        ("You hired " &
         To_String(Recruit.Name) &
         " for" &
         Positive'Image(Price) &
         " " &
         To_String(MoneyName) &
         ".",
         TradeMessage);
      SkyBases(BaseIndex).Recruits.Delete(Index => RecruitIndex, Count => 1);
      SkyBases(BaseIndex).Population := SkyBases(BaseIndex).Population - 1;
      UpdateGame(5);
   end HireRecruit;

   procedure BuyRecipe(RecipeIndex: Positive) is
      BaseIndex: constant Positive :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      Cost, MoneyIndex2: Natural;
      RecipeName: constant String :=
        To_String(Items_List(Recipes_List(RecipeIndex).ResultIndex).Name);
      BaseType: constant Positive :=
        Bases_Types'Pos(SkyBases(BaseIndex).BaseType) + 1;
      TraderIndex, ProtoMoneyIndex: Positive;
   begin
      if BaseType /= Recipes_List(RecipeIndex).BaseType then
         raise Trade_Cant_Buy;
      end if;
      if Known_Recipes.Find_Index(Item => RecipeIndex) /=
        Positive_Container.No_Index then
         raise Trade_Already_Known;
      end if;
      TraderIndex := FindMember(Talk);
      if Items_List(Recipes_List(RecipeIndex).ResultIndex).Prices(BaseType) >
        0 then
         Cost :=
           Items_List(Recipes_List(RecipeIndex).ResultIndex).Prices(BaseType) *
           Recipes_List(RecipeIndex).Difficulty *
           100;
      else
         Cost := Recipes_List(RecipeIndex).Difficulty * 100;
      end if;
      CountPrice(Cost, TraderIndex);
      ProtoMoneyIndex := FindProtoItem(MoneyIndex);
      MoneyIndex2 := FindCargo(ProtoMoneyIndex);
      if MoneyIndex2 = 0 then
         raise Trade_No_Money with RecipeName;
      end if;
      if Cost > PlayerShip.Cargo(MoneyIndex2).Amount then
         raise Trade_Not_Enough_Money with RecipeName;
      end if;
      UpdateCargo(PlayerShip, ProtoMoneyIndex, (0 - Cost));
      UpdateBaseCargo(ProtoMoneyIndex, Cost);
      Known_Recipes.Append(New_Item => RecipeIndex);
      AddMessage
        ("You bought recipe for " &
         RecipeName &
         " for" &
         Positive'Image(Cost) &
         " of " &
         To_String(MoneyName) &
         ".",
         TradeMessage);
      GainExp(1, 4, TraderIndex);
      GainRep(BaseIndex, 1);
      UpdateGame(5);
   end BuyRecipe;

   procedure HealWounded(MemberIndex: Natural) is
      BaseIndex: constant Positive :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      Cost, Time, MoneyIndex2: Natural := 0;
      TraderIndex, ProtoMoneyIndex: Positive;
      Message: Unbounded_String := Null_Unbounded_String;
   begin
      HealCost(Cost, Time, MemberIndex);
      if Cost = 0 then
         raise Trade_Cant_Heal;
      end if;
      ProtoMoneyIndex := FindProtoItem(MoneyIndex);
      MoneyIndex2 := FindCargo(ProtoMoneyIndex);
      if MoneyIndex2 = 0 then
         raise Trade_No_Money;
      end if;
      TraderIndex := FindMember(Talk);
      CountPrice(Cost, TraderIndex);
      if PlayerShip.Cargo(MoneyIndex2).Amount < Cost then
         raise Trade_Not_Enough_Money;
      end if;
      if MemberIndex > 0 then
         PlayerShip.Crew(MemberIndex).Health := 100;
         AddMessage
           ("You bought healing " &
            To_String(PlayerShip.Crew(MemberIndex).Name) &
            " for" &
            Positive'Image(Cost) &
            " " &
            To_String(MoneyName) &
            ".",
            TradeMessage);
         Message :=
           To_Unbounded_String(GiveOrders(MemberIndex, Rest, 0, False));
      else
         for I in PlayerShip.Crew.Iterate loop
            if PlayerShip.Crew(I).Health < 100 then
               PlayerShip.Crew(I).Health := 100;
               Message :=
                 To_Unbounded_String
                   (GiveOrders(Crew_Container.To_Index(I), Rest, 0, False));
            end if;
         end loop;
         AddMessage
           ("You bought healing all wounded crew members for" &
            Positive'Image(Cost) &
            " " &
            To_String(MoneyName) &
            ".",
            TradeMessage);
      end if;
      if Length(Message) > 0 then
         AddMessage(To_String(Message), OrderMessage, 3);
      end if;
      UpdateCargo(PlayerShip, ProtoMoneyIndex, (0 - Cost));
      UpdateBaseCargo(ProtoMoneyIndex, Cost);
      GainExp(1, 4, TraderIndex);
      GainRep(BaseIndex, 1);
      UpdateGame(Time);
   end HealWounded;

   procedure HealCost(Cost, Time: in out Natural; MemberIndex: Natural) is
      BaseType: constant Positive :=
        Bases_Types'Pos
          (SkyBases(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex)
             .BaseType) +
        1;
   begin
      if MemberIndex > 0 then
         Time := 5 * (100 - PlayerShip.Crew(MemberIndex).Health);
      else
         for Member of PlayerShip.Crew loop
            if Member.Health < 100 then
               Time := Time + (5 * (100 - Member.Health));
            end if;
         end loop;
      end if;
      Cost :=
        Time *
        Items_List(FindProtoItem(ItemType => HealingTools)).Prices(BaseType);
      if Time = 0 then
         Time := 1;
      end if;
   end HealCost;

end Bases.Trade;
