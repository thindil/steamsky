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

with Maps; use Maps;
with Messages; use Messages;
with Items; use Items;
with Ships; use Ships;
with Ships.Cargo; use Ships.Cargo;
with Ships.Crew; use Ships.Crew;
with Crafts; use Crafts;
with Trades; use Trades;
with Utils; use Utils;
with Bases.Cargo; use Bases.Cargo;
with Factions; use Factions;

package body Bases.Trade is

   function CheckMoney(Price: Positive;
      Message: String := "") return Positive is
      MoneyIndex2: constant Natural :=
        FindItem(PlayerShip.Cargo, FindProtoItem(MoneyIndex));
   begin
      if MoneyIndex2 = 0 then
         if Message /= "" then
            raise Trade_No_Money with Message;
         else
            raise Trade_No_Money;
         end if;
      end if;
      if PlayerShip.Cargo(MoneyIndex2).Amount < Price then
         if Message /= "" then
            raise Trade_Not_Enough_Money with Message;
         else
            raise Trade_Not_Enough_Money;
         end if;
      end if;
      return MoneyIndex2;
   end CheckMoney;

   procedure HireRecruit(RecruitIndex, Cost: Positive;
      DailyPayment, TradePayment: Natural; ContractLenght: Integer) is
      BaseIndex: constant Positive :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      MoneyIndex2, Price: Natural;
      Recruit: constant Recruit_Data :=
        SkyBases(BaseIndex).Recruits(RecruitIndex);
      TraderIndex, Morale: Positive;
      Inventory: Inventory_Container.Vector;
   begin
      TraderIndex := FindMember(Talk);
      Price := Cost;
      CountPrice(Price, TraderIndex);
      MoneyIndex2 := CheckMoney(Price, To_String(Recruit.Name));
      for Item of Recruit.Inventory loop
         Inventory.Append
           (New_Item =>
              (ProtoIndex => Item, Amount => 1, Name => Null_Unbounded_String,
               Durability => 100));
      end loop;
      if Factions_List(SkyBases(BaseIndex).Owner).Flags.Contains
          (To_Unbounded_String("nomorale")) then
         Morale := 50;
      else
         Morale := 50 + SkyBases(BaseIndex).Reputation(1);
         if Morale > 100 then
            Morale := 100;
         end if;
      end if;
      PlayerShip.Crew.Append
        (New_Item =>
           (Name => Recruit.Name, Gender => Recruit.Gender, Health => 100,
            Tired => 0, Skills => Recruit.Skills, Hunger => 0, Thirst => 0,
            Order => Rest, PreviousOrder => Rest, OrderTime => 15,
            Orders => (others => 0), Attributes => Recruit.Attributes,
            Inventory => Inventory, Equipment => Recruit.Equipment,
            Payment => (DailyPayment, TradePayment),
            ContractLength => ContractLenght, Morale => (Morale, 0),
            Loyalty => Morale, HomeBase => Recruit.HomeBase,
            Faction => Recruit.Faction));
      UpdateCargo
        (Ship => PlayerShip, CargoIndex => MoneyIndex2, Amount => (0 - Price));
      GainExp(1, TalkingSkill, TraderIndex);
      GainRep(BaseIndex, 1);
      AddMessage
        ("You hired " & To_String(Recruit.Name) & " for" &
         Positive'Image(Price) & " " & To_String(MoneyName) & ".",
         TradeMessage);
      SkyBases(BaseIndex).Recruits.Delete(Index => RecruitIndex);
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
           Recipes_List(RecipeIndex).Difficulty * 100;
      else
         Cost := Recipes_List(RecipeIndex).Difficulty * 100;
      end if;
      CountPrice(Cost, TraderIndex);
      ProtoMoneyIndex := FindProtoItem(MoneyIndex);
      MoneyIndex2 := CheckMoney(Cost, RecipeName);
      UpdateCargo
        (Ship => PlayerShip, CargoIndex => MoneyIndex2, Amount => (0 - Cost));
      UpdateBaseCargo(ProtoMoneyIndex, Cost);
      Known_Recipes.Append(New_Item => RecipeIndex);
      AddMessage
        ("You bought recipe for " & RecipeName & " for" &
         Positive'Image(Cost) & " of " & To_String(MoneyName) & ".",
         TradeMessage);
      GainExp(1, TalkingSkill, TraderIndex);
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
      TraderIndex := FindMember(Talk);
      MoneyIndex2 := CheckMoney(Cost);
      if MemberIndex > 0 then
         PlayerShip.Crew(MemberIndex).Health := 100;
         AddMessage
           ("You bought healing " &
            To_String(PlayerShip.Crew(MemberIndex).Name) & " for" &
            Positive'Image(Cost) & " " & To_String(MoneyName) & ".",
            TradeMessage);
         GiveOrders(PlayerShip, MemberIndex, Rest, 0, False);
      else
         for I in PlayerShip.Crew.Iterate loop
            if PlayerShip.Crew(I).Health < 100 then
               PlayerShip.Crew(I).Health := 100;
               GiveOrders
                 (PlayerShip, Crew_Container.To_Index(I), Rest, 0, False);
            end if;
         end loop;
         AddMessage
           ("You bought healing all wounded crew members for" &
            Positive'Image(Cost) & " " & To_String(MoneyName) & ".",
            TradeMessage);
      end if;
      UpdateCargo
        (Ship => PlayerShip, CargoIndex => MoneyIndex2, Amount => (0 - Cost));
      UpdateBaseCargo(ProtoMoneyIndex, Cost);
      GainExp(1, TalkingSkill, TraderIndex);
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
         Cost :=
           (5 * (100 - PlayerShip.Crew(MemberIndex).Health)) *
           Items_List
             (FindProtoItem
                (ItemType =>
                   Factions_List(PlayerShip.Crew(MemberIndex).Faction)
                     .HealingTools))
             .Prices
             (BaseType);
      else
         for Member of PlayerShip.Crew loop
            if Member.Health < 100 then
               Time := Time + (5 * (100 - Member.Health));
               Cost :=
                 Cost +
                 ((5 * (100 - Member.Health)) *
                  Items_List
                    (FindProtoItem
                       (ItemType =>
                          Factions_List(Member.Faction).HealingTools))
                    .Prices
                    (BaseType));
            end if;
         end loop;
      end if;
      CountPrice(Cost, FindMember(Talk));
      if Time = 0 then
         Time := 1;
      end if;
   end HealCost;

   function TrainCost(MemberIndex, SkillIndex: Positive) return Natural is
      Cost: Positive := 100;
   begin
      for Skill of PlayerShip.Crew(MemberIndex).Skills loop
         if Skill(1) = SkillIndex then
            if Skill(2) < 100 then
               Cost := (Skill(2) + 1) * 100;
               exit;
            end if;
            return 0;
         end if;
      end loop;
      CountPrice(Cost, FindMember(Talk));
      return Cost;
   end TrainCost;

   procedure TrainSkill(MemberIndex, SkillIndex: Positive) is
      Cost: constant Natural := TrainCost(MemberIndex, SkillIndex);
      MoneyIndex2, TraderIndex: Natural;
      ProtoMoneyIndex, GainedExp: Positive;
      BaseIndex: constant Positive :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
   begin
      if Cost = 0 then
         raise Trade_Cant_Train;
      end if;
      ProtoMoneyIndex := FindProtoItem(MoneyIndex);
      MoneyIndex2 := CheckMoney(Cost);
      AddMessage
        ("You bought training session in " &
         To_String(Skills_List(SkillIndex).Name) & " for " &
         To_String(PlayerShip.Crew(MemberIndex).Name) & " for" &
         Positive'Image(Cost) & " " & To_String(MoneyName) & ".",
         TradeMessage);
      GiveOrders(PlayerShip, MemberIndex, Rest, 0, False);
      GainedExp :=
        GetRandom(10, 60) +
        PlayerShip.Crew(MemberIndex).Attributes
          (Skills_List(SkillIndex).Attribute)
          (1);
      if GainedExp > 100 then
         GainedExp := 100;
      end if;
      GainExp(GainedExp, SkillIndex, MemberIndex);
      UpdateCargo
        (Ship => PlayerShip, CargoIndex => MoneyIndex2, Amount => (0 - Cost));
      UpdateBaseCargo(ProtoMoneyIndex, Cost);
      TraderIndex := FindMember(Talk);
      if TraderIndex > 0 then
         GainExp(5, TalkingSkill, TraderIndex);
      end if;
      GainRep(BaseIndex, 5);
      UpdateGame(60);
   end TrainSkill;

end Bases.Trade;
