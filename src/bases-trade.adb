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
with Crafts; use Crafts;
with Trades; use Trades;

package body Bases.Trade is

   procedure HireRecruit(RecruitIndex: Positive) is
      BaseIndex: constant Positive :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      MoneyIndex2, Price: Natural;
      Recruit: constant Recruit_Data :=
        SkyBases(BaseIndex).Recruits(RecruitIndex);
      TraderIndex: Positive;
      Inventory: Inventory_Container.Vector;
   begin
      MoneyIndex2 := FindItem(PlayerShip.Cargo, FindProtoItem(MoneyIndex));
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
          Orders => (others => 0),
          Attributes => Recruit.Attributes,
          Inventory => Inventory));
      UpdateCargo
        (Ship => PlayerShip,
         CargoIndex => MoneyIndex2,
         Amount => (0 - Price));
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
      MoneyIndex2 := FindItem(PlayerShip.Cargo, ProtoMoneyIndex);
      if MoneyIndex2 = 0 then
         raise Trade_No_Money with RecipeName;
      end if;
      if Cost > PlayerShip.Cargo(MoneyIndex2).Amount then
         raise Trade_Not_Enough_Money with RecipeName;
      end if;
      UpdateCargo
        (Ship => PlayerShip,
         CargoIndex => MoneyIndex2,
         Amount => (0 - Cost));
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
   begin
      HealCost(Cost, Time, MemberIndex);
      if Cost = 0 then
         raise Trade_Cant_Heal;
      end if;
      ProtoMoneyIndex := FindProtoItem(MoneyIndex);
      MoneyIndex2 := FindItem(PlayerShip.Cargo, ProtoMoneyIndex);
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
      UpdateCargo
        (Ship => PlayerShip,
         CargoIndex => MoneyIndex2,
         Amount => (0 - Cost));
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
