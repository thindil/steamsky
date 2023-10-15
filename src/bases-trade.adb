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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Messages; use Messages;
with Ships.Cargo; use Ships.Cargo;
with Ships.Crew; use Ships.Crew;
with Trades; use Trades;
with Utils; use Utils;
with Bases.Cargo; use Bases.Cargo;
with Config; use Config;
with BasesTypes; use BasesTypes;
with Maps; use Maps;
with Factions; use Factions;

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
      use Interfaces.C;

      Base_Index: constant Extended_Base_Range :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      Result: chars_ptr;
      Ada_Result, Exception_Name: Unbounded_String := Null_Unbounded_String;
      Space_Index: Natural := 0;
      function Hire_Ada_Recruit
        (R_Index, C, D_Payment, T_Payment, C_Length: Integer)
         return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "hireAdaRecruit";
   begin
      Set_Ship_In_Nim;
      Get_Base_Cargo(Base_Index => Base_Index);
      Get_Ada_Recruits
        (Recruits => Sky_Bases(Base_Index).Recruits, Base_Index => Base_Index);
      Get_Game_Date;
      Result :=
        Hire_Ada_Recruit
          (R_Index => Recruit_Index, C => Cost, D_Payment => Daily_Payment,
           T_Payment => Trade_Payment, C_Length => Contract_Length);
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
      Set_Ada_Recruits
        (Recruits => Sky_Bases(Base_Index).Recruits, Base_Index => Base_Index);
   end Hire_Recruit;

   procedure Buy_Recipe(Recipe_Index: Tiny_String.Bounded_String) is
      use Interfaces.C;
      use Tiny_String;

      Base_Index: constant Extended_Base_Range :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      Result: chars_ptr;
      Ada_Result, Exception_Name: Unbounded_String := Null_Unbounded_String;
      Space_Index: Natural := 0;
      function Buy_Ada_Recipe(R_Index: chars_ptr) return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "buyAdaRecipe";
   begin
      Set_Ship_In_Nim;
      Get_Base_Cargo(Base_Index => Base_Index);
      Get_Game_Date;
      Result :=
        Buy_Ada_Recipe
          (R_Index => New_String(Str => To_String(Source => Recipe_Index)));
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
           To_Unbounded_String(Source => "CantBuyError") then
            raise Trade_Cant_Buy;
         elsif Exception_Name =
           To_Unbounded_String(Source => "AlreadyKnownError") then
            raise Trade_Already_Known;
         end if;
      end if;
      Get_Ship_From_Nim(Ship => Player_Ship);
      Set_Base_Cargo(Base_Index => Base_Index);
   end Buy_Recipe;

   procedure Heal_Wounded(Member_Index: Crew_Container.Extended_Index) is
      use Tiny_String;

      Base_Index: constant Bases_Range :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      Money_Index_2: Inventory_Container.Extended_Index := 0;
      Cost, Time: Natural := 0;
      Trader_Index: constant Crew_Container.Extended_Index :=
        Find_Member(Order => TALK);
   begin
      Heal_Cost(Cost => Cost, Time => Time, Member_Index => Member_Index);
      if Cost = 0 then
         raise Trade_Cant_Heal;
      end if;
      if Trader_Index = 0 then
         raise Trade_No_Trader;
      end if;
      Money_Index_2 := Check_Money(Price => Cost);
      if Member_Index > 0 then
         Player_Ship.Crew(Member_Index).Health := 100;
         Add_Message
           (Message =>
              "You paid for healing " &
              To_String(Source => Player_Ship.Crew(Member_Index).Name) &
              " for" & Positive'Image(Cost) & " " &
              To_String(Source => Money_Name) & ".",
            M_Type => TRADEMESSAGE);
         Give_Orders
           (Ship => Player_Ship, Member_Index => Member_Index,
            Given_Order => REST, Module_Index => 0, Check_Priorities => False);
      else
         Give_Rest_Order_Loop :
         for I in Player_Ship.Crew.Iterate loop
            if Player_Ship.Crew(I).Health < 100 then
               Player_Ship.Crew(I).Health := 100;
               Give_Orders
                 (Ship => Player_Ship,
                  Member_Index => Crew_Container.To_Index(Position => I),
                  Given_Order => REST, Module_Index => 0,
                  Check_Priorities => False);
            end if;
         end loop Give_Rest_Order_Loop;
         Add_Message
           (Message =>
              "You paid for healing for all wounded crew members for" &
              Positive'Image(Cost) & " " & To_String(Source => Money_Name) &
              ".",
            M_Type => TRADEMESSAGE);
      end if;
      Update_Cargo
        (Ship => Player_Ship, Cargo_Index => Money_Index_2, Amount => -(Cost));
      Update_Base_Cargo(Proto_Index => Money_Index, Amount => Cost);
      Gain_Exp
        (Amount => 1, Skill_Number => Talking_Skill,
         Crew_Index => Trader_Index);
      Gain_Rep(Base_Index => Base_Index, Points => 1);
      Update_Game(Minutes => Time);
   end Heal_Wounded;

   procedure Heal_Cost
     (Cost, Time: in out Natural;
      Member_Index: Crew_Container.Extended_Index) is
      use Tiny_String;

      Base_Index: constant Bases_Range :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
   begin
      if Member_Index > 0 then
         Time := 5 * (100 - Player_Ship.Crew(Member_Index).Health);
         Cost :=
           (5 * (100 - Player_Ship.Crew(Member_Index).Health)) *
           Get_Price
             (Base_Type => To_Bounded_String(Source => "0"),
              Item_Index =>
                Find_Proto_Item
                  (Item_Type =>
                     Get_Faction
                       (Index => Player_Ship.Crew(Member_Index).Faction)
                       .Healing_Tools));
      else
         Count_Heal_Cost_Loop :
         for Member of Player_Ship.Crew loop
            if Member.Health < 100 then
               Time := Time + (5 * (100 - Member.Health));
               Cost :=
                 Cost +
                 ((5 * (100 - Member.Health)) *
                  Get_Proto_Item
                    (Index =>
                       Find_Proto_Item
                         (Item_Type =>
                            Get_Faction(Index => Member.Faction)
                              .Healing_Tools))
                    .Price);
            end if;
         end loop Count_Heal_Cost_Loop;
      end if;
      Cost :=
        Natural(Float(Cost) * Float(Get_Float_Setting(Name => "pricesBonus")));
      if Cost = 0 then
         Cost := 1;
      end if;
      Count_Price(Price => Cost, Trader_Index => Find_Member(Order => TALK));
      if Time = 0 then
         Time := 1;
      end if;
      if Has_Flag
          (Base_Type => Sky_Bases(Base_Index).Base_Type, Flag => "temple") then
         Cost := Cost / 2;
         if Cost = 0 then
            Cost := 1;
         end if;
      end if;
   end Heal_Cost;

   function Train_Cost
     (Member_Index: Crew_Container.Extended_Index;
      Skill_Index: Skills_Container.Extended_Index) return Natural is
      Cost: Natural :=
        Natural(100.0 * Get_Float_Setting(Name => "pricesBonus"));
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
                 Float(Get_Float_Setting(Name => "pricesBonus")));
            if Cost = 0 then
               Cost := 1;
            end if;
            exit Count_Train_Cost_Loop;
         end if;
      end loop Count_Train_Cost_Loop;
      Count_Price(Price => Cost, Trader_Index => Find_Member(Order => TALK));
      return Cost;
   end Train_Cost;

   procedure Train_Skill
     (Member_Index: Crew_Container.Extended_Index;
      Skill_Index: Skills_Container.Extended_Index; Amount: Positive;
      Is_Amount: Boolean := True) is
      use Tiny_String;

      Cost: Natural;
      Money_Index_2: Inventory_Container.Extended_Index;
      Gained_Exp: Positive;
      Base_Index: constant Bases_Range :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      Trader_Index: Crew_Container.Extended_Index;
      Sessions, Overall_Cost: Natural := 0;
      Max_Amount: Integer := Amount;
   begin
      Give_Orders
        (Ship => Player_Ship, Member_Index => Member_Index,
         Given_Order => REST, Module_Index => 0, Check_Priorities => False);
      Train_Skill_Loop :
      while Max_Amount > 0 loop
         Cost :=
           Train_Cost
             (Member_Index => Member_Index, Skill_Index => Skill_Index);
         Money_Index_2 :=
           Find_Item
             (Inventory => Player_Ship.Cargo, Proto_Index => Money_Index);
         exit Train_Skill_Loop when Cost = 0 or
           Inventory_Container.Element
               (Container => Player_Ship.Cargo, Index => Money_Index_2)
               .Amount <
             Cost or
           (not Is_Amount and Max_Amount < Cost);
         Gained_Exp :=
           Get_Random(Min => 10, Max => 60) +
           Player_Ship.Crew(Member_Index).Attributes
             (Positive
                (SkillsData_Container.Element
                   (Container => Skills_List, Index => Skill_Index)
                   .Attribute))
             .Level;
         if Gained_Exp > 100 then
            Gained_Exp := 100;
         end if;
         Gain_Exp
           (Amount => Gained_Exp, Skill_Number => Skill_Index,
            Crew_Index => Member_Index);
         Update_Cargo
           (Ship => Player_Ship, Cargo_Index => Money_Index_2,
            Amount => -(Cost));
         Update_Base_Cargo(Proto_Index => Money_Index, Amount => Cost);
         Trader_Index := Find_Member(Order => TALK);
         if Trader_Index > 0 then
            Gain_Exp
              (Amount => 5, Skill_Number => Talking_Skill,
               Crew_Index => Trader_Index);
         end if;
         Gain_Rep(Base_Index => Base_Index, Points => 5);
         Update_Game(Minutes => 60);
         Sessions := Sessions + 1;
         Overall_Cost := Overall_Cost + Cost;
         Max_Amount := Max_Amount - (if Is_Amount then 1 else Cost);
      end loop Train_Skill_Loop;
      if Sessions > 0 then
         Add_Message
           (Message =>
              "You purchased" & Positive'Image(Sessions) &
              " training session(s) in " &
              To_String
                (Source =>
                   SkillsData_Container.Element
                     (Container => Skills_List, Index => Skill_Index)
                     .Name) &
              " for " &
              To_String(Source => Player_Ship.Crew(Member_Index).Name) &
              " for" & Positive'Image(Overall_Cost) & " " &
              To_String(Source => Money_Name) & ".",
            M_Type => TRADEMESSAGE);
      end if;
   end Train_Skill;

end Bases.Trade;
