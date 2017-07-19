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

   function BuyItems(ItemIndex: Positive; Amount: String) return String is
      BuyAmount, TraderIndex, Price, ProtoMoneyIndex, BaseItemIndex: Positive;
      BaseIndex: constant Positive :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      BaseType: constant Positive :=
        Bases_Types'Pos(SkyBases(BaseIndex).BaseType) + 1;
      ItemName: constant String := To_String(Items_List(ItemIndex).Name);
      Cost, MoneyIndex2: Natural;
      EventIndex: constant Natural :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex;
   begin
      BuyAmount := Positive'Value(Amount);
      if not Items_List(ItemIndex).Buyable(BaseType) then
         return "You can't buy " & ItemName & " in this base.";
      end if;
      for I in SkyBases(BaseIndex).Cargo.Iterate loop
         if SkyBases(BaseIndex).Cargo(I).ProtoIndex = ItemIndex then
            BaseItemIndex := BaseCargo_Container.To_Index(I);
            exit;
         end if;
      end loop;
      if SkyBases(BaseIndex).Cargo(BaseItemIndex).Amount = 0 then
         return "You can't buy " & ItemName & " in this base at this moment.";
      elsif SkyBases(BaseIndex).Cargo(BaseItemIndex).Amount < BuyAmount then
         return "Base don't have that much " & ItemName & " to sell.";
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
         return "You don't have that much free space in your ship cargo.";
      end if;
      if MoneyIndex2 = 0 then
         return "You don't have " &
           To_String(MoneyName) &
           " to buy " &
           ItemName &
           ".";
      end if;
      if Cost > PlayerShip.Cargo(MoneyIndex2).Amount then
         return "You don't have enough " &
           To_String(MoneyName) &
           " to buy so much " &
           ItemName &
           ".";
      end if;
      UpdateCargo(PlayerShip, ProtoMoneyIndex, (0 - Cost));
      SkyBases(BaseIndex).Cargo(1).Amount :=
        SkyBases(BaseIndex).Cargo(1).Amount + Cost;
      UpdateCargo(PlayerShip, ItemIndex, BuyAmount);
      SkyBases(BaseIndex).Cargo(BaseItemIndex).Amount :=
        SkyBases(BaseIndex).Cargo(BaseItemIndex).Amount - BuyAmount;
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
      return "";
   exception
      when Constraint_Error =>
         return "You entered invalid amount to buy.";
   end BuyItems;

   function SellItems(ItemIndex: Positive; Amount: String) return String is
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
   begin
      SellAmount := Positive'Value(Amount);
      if PlayerShip.Cargo(ItemIndex).Amount < SellAmount then
         return "You dont have that much " & ItemName & " in ship cargo.";
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
         return "You don't have enough free cargo space in your ship for " &
           To_String(MoneyName) &
           ".";
      end if;
      if Profit > SkyBases(BaseIndex).Cargo(1).Amount then
         return "You can't sell so much " &
           ItemName &
           " because base don't have that much " &
           To_String(MoneyName) &
           " to buy it.";
      end if;
      if PlayerShip.Cargo(ItemIndex).Durability = 100 then
         for Item of SkyBases(BaseIndex).Cargo loop
            if Item.ProtoIndex = ProtoIndex then
               Item.Amount := Item.Amount + SellAmount;
               exit;
            end if;
         end loop;
      end if;
      UpdateCargo
        (PlayerShip,
         ProtoIndex,
         (0 - SellAmount),
         PlayerShip.Cargo.Element(ItemIndex).Durability);
      UpdateCargo(PlayerShip, FindProtoItem(MoneyIndex), Profit);
      SkyBases(BaseIndex).Cargo(1).Amount :=
        SkyBases(BaseIndex).Cargo(1).Amount - Profit;
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
      return "";
   exception
      when Constraint_Error =>
         return "You entered invalid amount to sell.";
   end SellItems;

   function HireRecruit(RecruitIndex: Positive) return String is
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
         return "You don't have " & To_String(MoneyName) & " to hire anyone.";
      end if;
      TraderIndex := FindMember(Talk);
      Price := Recruit.Price;
      CountPrice(Price, TraderIndex);
      if PlayerShip.Cargo(MoneyIndex2).Amount < Price then
         return "You don't have enough " &
           To_String(MoneyName) &
           " to hire " &
           To_String(Recruit.Name) &
           ".";
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
      return "";
   end HireRecruit;

   function BuyRecipe(RecipeIndex: Positive) return String is
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
         return "You can't buy this recipe in this base.";
      end if;
      if Known_Recipes.Find_Index(Item => RecipeIndex) /=
        Positive_Container.No_Index then
         return "You already known this recipe.";
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
         return "You don't have " &
           To_String(MoneyName) &
           " to buy recipe for " &
           RecipeName &
           ".";
      end if;
      if Cost > PlayerShip.Cargo(MoneyIndex2).Amount then
         return "You don't have enough" &
           To_String(MoneyName) &
           "  to buy recipe for " &
           RecipeName &
           ".";
      end if;
      UpdateCargo(PlayerShip, ProtoMoneyIndex, (0 - Cost));
      SkyBases(BaseIndex).Cargo(1).Amount :=
        SkyBases(BaseIndex).Cargo(1).Amount + Cost;
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
      return "";
   end BuyRecipe;

   function HealWounded(MemberIndex: Natural) return String is
      BaseIndex: constant Positive :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      Cost, Time, MoneyIndex2: Natural := 0;
      TraderIndex, ProtoMoneyIndex: Positive;
   begin
      HealCost(Cost, Time, MemberIndex);
      if Cost = 0 then
         return "You don't have anyone to heal.";
      end if;
      ProtoMoneyIndex := FindProtoItem(MoneyIndex);
      MoneyIndex2 := FindCargo(ProtoMoneyIndex);
      if MoneyIndex2 = 0 then
         return "You don't have " &
           To_String(MoneyName) &
           " to pay for healing wounded crew members.";
      end if;
      TraderIndex := FindMember(Talk);
      CountPrice(Cost, TraderIndex);
      if PlayerShip.Cargo(MoneyIndex2).Amount < Cost then
         return "You don't have enough " &
           To_String(MoneyName) &
           " to pay for healing wounded crew members.";
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
         GiveOrders(MemberIndex, Rest, 0, False);
      else
         for I in PlayerShip.Crew.Iterate loop
            if PlayerShip.Crew(I).Health < 100 then
               PlayerShip.Crew(I).Health := 100;
               GiveOrders(Crew_Container.To_Index(I), Rest, 0, False);
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
      UpdateCargo(PlayerShip, ProtoMoneyIndex, (0 - Cost));
      SkyBases(BaseIndex).Cargo(1).Amount :=
        SkyBases(BaseIndex).Cargo(1).Amount + Cost;
      GainExp(1, 4, TraderIndex);
      GainRep(BaseIndex, 1);
      UpdateGame(Time);
      return "";
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
