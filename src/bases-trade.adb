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

with Messages; use Messages;
with Ships.Cargo; use Ships.Cargo;
with Ships.Crew; use Ships.Crew;
with Crafts; use Crafts;
with Trades; use Trades;
with Utils; use Utils;
with Bases.Cargo; use Bases.Cargo;
with Config; use Config;
with BasesTypes; use BasesTypes;
with Maps; use Maps;

package body Bases.Trade is

   -- ****if* BTrade/BTrade.Check_Money
   -- FUNCTION
   -- Check if player have enough money
   -- PARAMETERS
   -- Price   - Miniumum amount of money which player must have
   -- Message - Additional message to return when player don't have enough
   --           money
   -- RESULT
   -- Cargo index of money from the player ship
   -- SOURCE
   function Check_Money
     (Price: Positive; Message: String := "") return Positive is
      -- ****
      Money_Index_2: constant Natural :=
        Find_Item(Inventory => Player_Ship.Cargo, Proto_Index => Money_Index);
   begin
      if Money_Index_2 = 0 then
         if Message /= "" then
            raise Trade_No_Money with Message;
         else
            raise Trade_No_Money;
         end if;
      end if;
      if Inventory_Container.Element
          (Container => Player_Ship.Cargo, Index => Money_Index_2)
          .Amount <
        Price then
         if Message /= "" then
            raise Trade_Not_Enough_Money with Message;
         else
            raise Trade_Not_Enough_Money;
         end if;
      end if;
      return Money_Index_2;
   end Check_Money;

   procedure Hire_Recruit
     (Recruit_Index: Recruit_Container.Extended_Index; Cost: Positive;
      Daily_Payment, Trade_Payment: Natural; Contract_Length: Integer) is
      use Tiny_String;

      Base_Index: constant Bases_Range :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      Money_Index_2: Inventory_Container.Extended_Index;
      Price: Natural;
      Recruit: constant Recruit_Data :=
        Recruit_Container.Element
          (Container => Sky_Bases(Base_Index).Recruits, Index => Recruit_Index);
      Morale: Skill_Range;
      Inventory: Inventory_Container.Vector (Capacity => 32);
      Trader_Index: constant Crew_Container.Extended_Index := Find_Member(Order => TALK);
   begin
      if Trader_Index = 0 then
         raise Trade_No_Trader;
      end if;
      Price := Cost;
      Count_Price(Price => Price, Trader_Index => Trader_Index);
      Money_Index_2 := Check_Money(Price => Price, Message => To_String(Source => Recruit.Name));
      Add_Recruit_Inventory_Loop :
      for Item of Recruit.Inventory loop
         Inventory_Container.Append
           (Container => Inventory,
            New_Item =>
              (Proto_Index => Item, Amount => 1, Name => Null_Bounded_String,
               Durability => Default_Item_Durability, Price => 0));
      end loop Add_Recruit_Inventory_Loop;
      if Factions_List(Sky_Bases(Base_Index).Owner).Flags.Contains
          (Item => To_Unbounded_String(Source => "nomorale")) then
         Morale := 50;
      else
         Morale :=
           (if 50 + Sky_Bases(Base_Index).Reputation.Level > 100 then 100
            else 50 + Sky_Bases(Base_Index).Reputation.Level);
      end if;
      Player_Ship.Crew.Append
        (New_Item =>
           (Amount_Of_Attributes => Attributes_Amount,
            Amount_Of_Skills => Skills_Amount, Name => Recruit.Name,
            Gender => Recruit.Gender, Health => 100, Tired => 0,
            Skills => Recruit.Skills, Hunger => 0, Thirst => 0, Order => REST,
            Previous_Order => REST, Order_Time => 15, Orders => (others => 0),
            Attributes => Recruit.Attributes, Inventory => Inventory,
            Equipment => Recruit.Equipment,
            Payment => (1 => Daily_Payment, 2 => Trade_Payment),
            Contract_Length => Contract_Length, Morale => (1 => Morale, 2 => 0),
            Loyalty => Morale, Home_Base => Recruit.Home_Base,
            Faction => Recruit.Faction));
      Update_Cargo
        (Ship => Player_Ship, Cargo_Index => Money_Index_2, Amount => -(Price));
      Gain_Exp(1, Talking_Skill, Trader_Index);
      Gain_Rep(Base_Index, 1);
      Add_Message
        ("You hired " & To_String(Recruit.Name) & " for" &
         Positive'Image(Price) & " " & To_String(Money_Name) & ".",
         TRADEMESSAGE);
      Recruit_Container.Delete
        (Container => Sky_Bases(Base_Index).Recruits, Index => Recruit_Index);
      Sky_Bases(Base_Index).Population := Sky_Bases(Base_Index).Population - 1;
      Update_Game(5);
   end Hire_Recruit;

   procedure Buy_Recipe(Recipe_Index: Tiny_String.Bounded_String) is
      use Tiny_String;

      BaseIndex: constant Bases_Range :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      MoneyIndex2: Inventory_Container.Extended_Index;
      Cost: Natural;
      RecipeName: constant String :=
        To_String
          (Objects_Container.Element
             (Container => Items_List,
              Index =>
                Recipes_List
                  (To_Bounded_String
                     (Source => To_String(Source => Recipe_Index)))
                  .Result_Index)
             .Name);
      BaseType: constant Bounded_String := Sky_Bases(BaseIndex).Base_Type;
      TraderIndex: constant Crew_Container.Extended_Index := Find_Member(TALK);
   begin
      if not Bases_Types_List(BaseType).Recipes.Contains
          (To_Unbounded_String
             (Source => To_String(Source => Recipe_Index))) then
         raise Trade_Cant_Buy;
      end if;
      if Known_Recipes.Find_Index(Item => Recipe_Index) /=
        Positive_Container.No_Index then
         raise Trade_Already_Known;
      end if;
      if TraderIndex = 0 then
         raise Trade_No_Trader;
      end if;
      if Get_Price
          (Sky_Bases(BaseIndex).Base_Type,
           Recipes_List
             (To_Bounded_String(Source => To_String(Source => Recipe_Index)))
             .Result_Index) >
        0 then
         Cost :=
           Get_Price
             (Sky_Bases(BaseIndex).Base_Type,
              Recipes_List
                (To_Bounded_String
                   (Source => To_String(Source => Recipe_Index)))
                .Result_Index) *
           Recipes_List
             (To_Bounded_String(Source => To_String(Source => Recipe_Index)))
             .Difficulty *
           10;
      else
         Cost :=
           Recipes_List
             (To_Bounded_String(Source => To_String(Source => Recipe_Index)))
             .Difficulty *
           10;
      end if;
      Cost := Natural(Float(Cost) * Float(New_Game_Settings.Prices_Bonus));
      if Cost = 0 then
         Cost := 1;
      end if;
      Count_Price(Cost, TraderIndex);
      MoneyIndex2 := Check_Money(Cost, RecipeName);
      Update_Cargo
        (Ship => Player_Ship, Cargo_Index => MoneyIndex2, Amount => -(Cost));
      Update_Base_Cargo(Money_Index, Cost);
      Known_Recipes.Append(New_Item => Recipe_Index);
      Add_Message
        ("You bought the recipe for " & RecipeName & " for" &
         Positive'Image(Cost) & " of " & To_String(Money_Name) & ".",
         TRADEMESSAGE);
      Gain_Exp(1, Talking_Skill, TraderIndex);
      Gain_Rep(BaseIndex, 1);
      Update_Game(5);
   end Buy_Recipe;

   procedure Heal_Wounded(Member_Index: Crew_Container.Extended_Index) is
      use Tiny_String;

      BaseIndex: constant Bases_Range :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      MoneyIndex2: Inventory_Container.Extended_Index := 0;
      Cost, Time: Natural := 0;
      TraderIndex: constant Crew_Container.Extended_Index := Find_Member(TALK);
   begin
      Heal_Cost(Cost, Time, Member_Index);
      if Cost = 0 then
         raise Trade_Cant_Heal;
      end if;
      if TraderIndex = 0 then
         raise Trade_No_Trader;
      end if;
      MoneyIndex2 := Check_Money(Cost);
      if Member_Index > 0 then
         Player_Ship.Crew(Member_Index).Health := 100;
         Add_Message
           ("You paid for healing " &
            To_String(Player_Ship.Crew(Member_Index).Name) & " for" &
            Positive'Image(Cost) & " " & To_String(Money_Name) & ".",
            TRADEMESSAGE);
         Give_Orders(Player_Ship, Member_Index, REST, 0, False);
      else
         Give_Rest_Order_Loop :
         for I in Player_Ship.Crew.Iterate loop
            if Player_Ship.Crew(I).Health < 100 then
               Player_Ship.Crew(I).Health := 100;
               Give_Orders
                 (Player_Ship, Crew_Container.To_Index(I), REST, 0, False);
            end if;
         end loop Give_Rest_Order_Loop;
         Add_Message
           ("You paid for healing for all wounded crew members for" &
            Positive'Image(Cost) & " " & To_String(Money_Name) & ".",
            TRADEMESSAGE);
      end if;
      Update_Cargo
        (Ship => Player_Ship, Cargo_Index => MoneyIndex2, Amount => -(Cost));
      Update_Base_Cargo(Money_Index, Cost);
      Gain_Exp(1, Talking_Skill, TraderIndex);
      Gain_Rep(BaseIndex, 1);
      Update_Game(Time);
   end Heal_Wounded;

   procedure Heal_Cost
     (Cost, Time: in out Natural;
      Member_Index: Crew_Container.Extended_Index) is
      use Tiny_String;

      BaseIndex: constant Bases_Range :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
   begin
      if Member_Index > 0 then
         Time := 5 * (100 - Player_Ship.Crew(Member_Index).Health);
         Cost :=
           (5 * (100 - Player_Ship.Crew(Member_Index).Health)) *
           Get_Price
             (To_Bounded_String("0"),
              Find_Proto_Item
                (Item_Type =>
                   Factions_List(Player_Ship.Crew(Member_Index).Faction)
                     .Healing_Tools));
      else
         Count_Heal_Cost_Loop :
         for Member of Player_Ship.Crew loop
            if Member.Health < 100 then
               Time := Time + (5 * (100 - Member.Health));
               Cost :=
                 Cost +
                 ((5 * (100 - Member.Health)) *
                  Objects_Container.Element
                    (Container => Items_List,
                     Index =>
                       Find_Proto_Item
                         (Item_Type =>
                            Factions_List(Member.Faction).Healing_Tools))
                    .Price);
            end if;
         end loop Count_Heal_Cost_Loop;
      end if;
      Cost := Natural(Float(Cost) * Float(New_Game_Settings.Prices_Bonus));
      if Cost = 0 then
         Cost := 1;
      end if;
      Count_Price(Cost, Find_Member(TALK));
      if Time = 0 then
         Time := 1;
      end if;
      if Bases_Types_List(Sky_Bases(BaseIndex).Base_Type).Flags.Contains
          (To_Unbounded_String("temple")) then
         Cost := Cost / 2;
         if Cost = 0 then
            Cost := 1;
         end if;
      end if;
   end Heal_Cost;

   function Train_Cost
     (Member_Index: Crew_Container.Extended_Index;
      Skill_Index: Skills_Container.Extended_Index) return Natural is
      Cost: Natural := Natural(100.0 * New_Game_Settings.Prices_Bonus);
   begin
      Count_Train_Cost_Loop :
      for Skill of Player_Ship.Crew(Member_Index).Skills loop
         if Skill.Index = Skill_Index then
            if Skill.Level = 100 then
               return 0;
            end if;
            Cost :=
              Natural
                (Float((Skill.Level + 1) * 100) *
                 Float(New_Game_Settings.Prices_Bonus));
            if Cost = 0 then
               Cost := 1;
            end if;
            exit Count_Train_Cost_Loop;
         end if;
      end loop Count_Train_Cost_Loop;
      Count_Price(Cost, Find_Member(TALK));
      return Cost;
   end Train_Cost;

   procedure Train_Skill
     (Member_Index: Crew_Container.Extended_Index;
      Skill_Index: Skills_Container.Extended_Index; Amount: Positive;
      Is_Amount: Boolean := True) is
      use Tiny_String;

      Cost: Natural;
      MoneyIndex2: Inventory_Container.Extended_Index;
      GainedExp: Positive;
      BaseIndex: constant Bases_Range :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      TraderIndex: Crew_Container.Extended_Index;
      Sessions, OverallCost: Natural := 0;
      MaxAmount: Integer := Amount;
   begin
      Give_Orders(Player_Ship, Member_Index, REST, 0, False);
      Train_Skill_Loop :
      while MaxAmount > 0 loop
         Cost := Train_Cost(Member_Index, Skill_Index);
         MoneyIndex2 := Find_Item(Player_Ship.Cargo, Money_Index);
         exit Train_Skill_Loop when Cost = 0 or
           Inventory_Container.Element
               (Container => Player_Ship.Cargo, Index => MoneyIndex2)
               .Amount <
             Cost or
           (not Is_Amount and MaxAmount < Cost);
         GainedExp :=
           Get_Random(10, 60) +
           Player_Ship.Crew(Member_Index).Attributes
             (Positive
                (SkillsData_Container.Element(Skills_List, Skill_Index)
                   .Attribute))
             .Level;
         if GainedExp > 100 then
            GainedExp := 100;
         end if;
         Gain_Exp(GainedExp, Skill_Index, Member_Index);
         Update_Cargo
           (Ship => Player_Ship, Cargo_Index => MoneyIndex2,
            Amount => -(Cost));
         Update_Base_Cargo(Money_Index, Cost);
         TraderIndex := Find_Member(TALK);
         if TraderIndex > 0 then
            Gain_Exp(5, Talking_Skill, TraderIndex);
         end if;
         Gain_Rep(BaseIndex, 5);
         Update_Game(60);
         Sessions := Sessions + 1;
         OverallCost := OverallCost + Cost;
         MaxAmount := MaxAmount - (if Is_Amount then 1 else Cost);
      end loop Train_Skill_Loop;
      if Sessions > 0 then
         Add_Message
           ("You purchased" & Positive'Image(Sessions) &
            " training session(s) in " &
            To_String
              (SkillsData_Container.Element(Skills_List, Skill_Index).Name) &
            " for " & To_String(Player_Ship.Crew(Member_Index).Name) & " for" &
            Positive'Image(OverallCost) & " " & To_String(Money_Name) & ".",
            TRADEMESSAGE);
      end if;
   end Train_Skill;

end Bases.Trade;
