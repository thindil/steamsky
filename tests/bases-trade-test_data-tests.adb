--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Bases.Trade.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only

with Crafts; use Crafts;
with Maps; use Maps;
with BasesTypes; use BasesTypes;

--  begin read only
--  end read only
package body Bases.Trade.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only
--  begin read only
   procedure Wrap_Test_HireRecruit_8627ed_6a9998
     (RecruitIndex: Recruit_Container.Extended_Index; Cost: Positive;
      DailyPayment, TradePayment: Natural; ContractLenght: Integer) is
   begin
      GNATtest_Generated.GNATtest_Standard.Bases.Trade.HireRecruit
        (RecruitIndex, Cost, DailyPayment, TradePayment, ContractLenght);
   end Wrap_Test_HireRecruit_8627ed_6a9998;
--  end read only

--  begin read only
   procedure Test_HireRecruit_test_hirerecruit(Gnattest_T: in out Test);
   procedure Test_HireRecruit_8627ed_6a9998(Gnattest_T: in out Test) renames
     Test_HireRecruit_test_hirerecruit;
--  id:2.2/8627ed08fea0f1f9/HireRecruit/1/0/test_hirerecruit/
   procedure Test_HireRecruit_test_hirerecruit(Gnattest_T: in out Test) is
      procedure HireRecruit
        (RecruitIndex: Recruit_Container.Extended_Index; Cost: Positive;
         DailyPayment, TradePayment: Natural; ContractLenght: Integer) renames
        Wrap_Test_HireRecruit_8627ed_6a9998;
--  end read only

      pragma Unreferenced(Gnattest_T);
      Amount: constant Positive := Positive(Player_Ship.Crew.Length);

   begin

      HireRecruit(1, 1, 0, 0, 1);
      Assert
        (Positive(Player_Ship.Crew.Length) = Amount + 1,
         "Failed to hire recruit to player ship crew.");

--  begin read only
   end Test_HireRecruit_test_hirerecruit;
--  end read only

--  begin read only
   procedure Wrap_Test_BuyRecipe_64b3a1_e0c4a8
     (RecipeIndex: Unbounded_String) is
   begin
      begin
         pragma Assert((RecipeIndex /= Null_Unbounded_String));
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(bases-trade.ads:0):Test_BuyRecipe test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Bases.Trade.BuyRecipe(RecipeIndex);
      begin
         pragma Assert(True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(bases-trade.ads:0:):Test_BuyRecipe test commitment violated");
      end;
   end Wrap_Test_BuyRecipe_64b3a1_e0c4a8;
--  end read only

--  begin read only
   procedure Test_BuyRecipe_test_buyrecipe(Gnattest_T: in out Test);
   procedure Test_BuyRecipe_64b3a1_e0c4a8(Gnattest_T: in out Test) renames
     Test_BuyRecipe_test_buyrecipe;
--  id:2.2/64b3a1fdc448171c/BuyRecipe/1/0/test_buyrecipe/
   procedure Test_BuyRecipe_test_buyrecipe(Gnattest_T: in out Test) is
      procedure BuyRecipe(RecipeIndex: Unbounded_String) renames
        Wrap_Test_BuyRecipe_64b3a1_e0c4a8;
--  end read only

      pragma Unreferenced(Gnattest_T);
      Amount: Positive := Positive(Known_Recipes.Length);
      BaseIndex: constant Positive :=
        SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).BaseIndex;
      BaseType: constant Unbounded_String := SkyBases(BaseIndex).BaseType;

   begin

      for Recipe of BasesTypes_List(SkyBases(BaseIndex).BaseType).Recipes loop
         if Known_Recipes.Find_Index(Item => Recipe) =
           Positive_Container.No_Index then
            BuyRecipe(Recipe);
            exit;
         end if;
      end loop;
      Assert
        (Positive(Known_Recipes.Length) = Amount + 1,
         "Failed to buy recipe from base.");

--  begin read only
   end Test_BuyRecipe_test_buyrecipe;
--  end read only

--  begin read only
   procedure Wrap_Test_HealWounded_ec5713_276c05
     (MemberIndex: Crew_Container.Extended_Index) is
   begin
      begin
         pragma Assert((MemberIndex <= Player_Ship.Crew.Last_Index));
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(bases-trade.ads:0):Test_HealWounded test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Bases.Trade.HealWounded
        (MemberIndex);
      begin
         pragma Assert(True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(bases-trade.ads:0:):Test_HealWounded test commitment violated");
      end;
   end Wrap_Test_HealWounded_ec5713_276c05;
--  end read only

--  begin read only
   procedure Test_HealWounded_test_healwounded(Gnattest_T: in out Test);
   procedure Test_HealWounded_ec5713_276c05(Gnattest_T: in out Test) renames
     Test_HealWounded_test_healwounded;
--  id:2.2/ec5713429dea8dfb/HealWounded/1/0/test_healwounded/
   procedure Test_HealWounded_test_healwounded(Gnattest_T: in out Test) is
      procedure HealWounded(MemberIndex: Crew_Container.Extended_Index) renames
        Wrap_Test_HealWounded_ec5713_276c05;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Player_Ship.Cargo(1).Amount := Player_Ship.Cargo(1).Amount + 2_000;
      Player_Ship.Crew(1).Health := 90;
      HealWounded(1);
      Assert
        (Player_Ship.Crew(1).Health = 100,
         "Failed to heal selected crew member.");
      Player_Ship.Crew(1).Health := 90;
      HealWounded(0);
      Assert(Player_Ship.Crew(1).Health = 100, "Failed to heal whole crew.");

--  begin read only
   end Test_HealWounded_test_healwounded;
--  end read only

--  begin read only
   procedure Wrap_Test_HealCost_772065_168081
     (Cost, Time: in out Natural;
      MemberIndex: Crew_Container.Extended_Index) is
   begin
      begin
         pragma Assert(MemberIndex <= Player_Ship.Crew.Last_Index);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(bases-trade.ads:0):Test_HealCost test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Bases.Trade.HealCost
        (Cost, Time, MemberIndex);
      begin
         pragma Assert(Cost > 0 and Time > 0);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(bases-trade.ads:0:):Test_HealCost test commitment violated");
      end;
   end Wrap_Test_HealCost_772065_168081;
--  end read only

--  begin read only
   procedure Test_HealCost_test_healcost(Gnattest_T: in out Test);
   procedure Test_HealCost_772065_168081(Gnattest_T: in out Test) renames
     Test_HealCost_test_healcost;
--  id:2.2/77206542a3e2c8c9/HealCost/1/0/test_healcost/
   procedure Test_HealCost_test_healcost(Gnattest_T: in out Test) is
      procedure HealCost
        (Cost, Time: in out Natural;
         MemberIndex: Crew_Container.Extended_Index) renames
        Wrap_Test_HealCost_772065_168081;
--  end read only

      pragma Unreferenced(Gnattest_T);
      Cost, Time: Natural := 0;

   begin

      Player_Ship.Crew(1).Health := Player_Ship.Crew(1).Health - 10;
      HealCost(Cost, Time, 1);
      Assert(Cost > 0, "Failed to count player crew member heal costs.");
      Assert(Time > 0, "Failed to count player crew member heal time.");

--  begin read only
   end Test_HealCost_test_healcost;
--  end read only

--  begin read only
   function Wrap_Test_TrainCost_976ed7_9e5841
     (MemberIndex: Crew_Container.Extended_Index;
      SkillIndex: Skills_Container.Extended_Index) return Natural is
   begin
      begin
         pragma Assert
           (MemberIndex in
              Player_Ship.Crew.First_Index .. Player_Ship.Crew.Last_Index and
            SkillIndex in Skills_List.First_Index .. Skills_List.Last_Index);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(bases-trade.ads:0):Test_TrainCost test requirement violated");
      end;
      declare
         Test_TrainCost_976ed7_9e5841_Result: constant Natural :=
           GNATtest_Generated.GNATtest_Standard.Bases.Trade.TrainCost
             (MemberIndex, SkillIndex);
      begin
         begin
            pragma Assert(True);
            null;
         exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "ens_sloc(bases-trade.ads:0:):Test_TrainCost test commitment violated");
         end;
         return Test_TrainCost_976ed7_9e5841_Result;
      end;
   end Wrap_Test_TrainCost_976ed7_9e5841;
--  end read only

--  begin read only
   procedure Test_TrainCost_test_traincost(Gnattest_T: in out Test);
   procedure Test_TrainCost_976ed7_9e5841(Gnattest_T: in out Test) renames
     Test_TrainCost_test_traincost;
--  id:2.2/976ed7988ae3d183/TrainCost/1/0/test_traincost/
   procedure Test_TrainCost_test_traincost(Gnattest_T: in out Test) is
      function TrainCost
        (MemberIndex: Crew_Container.Extended_Index;
         SkillIndex: Skills_Container.Extended_Index) return Natural renames
        Wrap_Test_TrainCost_976ed7_9e5841;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Assert
        (TrainCost(1, 1) > 0,
         "Failed to count player crew member training cost.");

--  begin read only
   end Test_TrainCost_test_traincost;
--  end read only

--  begin read only
   procedure Wrap_Test_TrainSkill_97d670_bf5e1b
     (MemberIndex: Crew_Container.Extended_Index;
      SkillIndex: Skills_Container.Extended_Index; Amount: Positive;
      Is_Amount: Boolean := True) is
   begin
      begin
         pragma Assert
           (MemberIndex in
              Player_Ship.Crew.First_Index .. Player_Ship.Crew.Last_Index and
            SkillIndex in Skills_List.First_Index .. Skills_List.Last_Index);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(bases-trade.ads:0):Test_TrainSkill test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Bases.Trade.TrainSkill
        (MemberIndex, SkillIndex, Amount, Is_Amount);
      begin
         pragma Assert(True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(bases-trade.ads:0:):Test_TrainSkill test commitment violated");
      end;
   end Wrap_Test_TrainSkill_97d670_bf5e1b;
--  end read only

--  begin read only
   procedure Test_TrainSkill_test_trainskill(Gnattest_T: in out Test);
   procedure Test_TrainSkill_97d670_bf5e1b(Gnattest_T: in out Test) renames
     Test_TrainSkill_test_trainskill;
--  id:2.2/97d67059a26fe921/TrainSkill/1/0/test_trainskill/
   procedure Test_TrainSkill_test_trainskill(Gnattest_T: in out Test) is
      procedure TrainSkill
        (MemberIndex: Crew_Container.Extended_Index;
         SkillIndex: Skills_Container.Extended_Index; Amount: Positive;
         Is_Amount: Boolean := True) renames
        Wrap_Test_TrainSkill_97d670_bf5e1b;
--  end read only

      pragma Unreferenced(Gnattest_T);
      SkillsAmount: constant Positive :=
        Positive(Player_Ship.Crew(1).Skills.Length);

   begin

      TrainSkill(1, 1, 1);
      Assert
        (Positive(Player_Ship.Crew(1).Skills.Length) > SkillsAmount,
         "Failed to train new skill.");

--  begin read only
   end Test_TrainSkill_test_trainskill;
--  end read only

--  begin read only
--  id:2.2/02/
--
--  This section can be used to add elaboration code for the global state.
--
begin
--  end read only
   null;
--  begin read only
--  end read only
end Bases.Trade.Test_Data.Tests;
