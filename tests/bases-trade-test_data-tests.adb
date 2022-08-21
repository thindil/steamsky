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
   procedure Wrap_Test_Hire_Recruit_6d7d9a_6a9998
     (Recruit_Index: Recruit_Container.Extended_Index; Cost: Positive;
      Daily_Payment, Trade_Payment: Natural; Contract_Length: Integer) is
   begin
      GNATtest_Generated.GNATtest_Standard.Bases.Trade.Hire_Recruit
        (Recruit_Index, Cost, Daily_Payment, Trade_Payment, Contract_Length);
   end Wrap_Test_Hire_Recruit_6d7d9a_6a9998;
--  end read only

--  begin read only
   procedure Test_Hire_Recruit_test_hirerecruit(Gnattest_T: in out Test);
   procedure Test_Hire_Recruit_6d7d9a_6a9998(Gnattest_T: in out Test) renames
     Test_Hire_Recruit_test_hirerecruit;
--  id:2.2/6d7d9a7e5b85a04b/Hire_Recruit/1/0/test_hirerecruit/
   procedure Test_Hire_Recruit_test_hirerecruit(Gnattest_T: in out Test) is
      procedure Hire_Recruit
        (Recruit_Index: Recruit_Container.Extended_Index; Cost: Positive;
         Daily_Payment, Trade_Payment: Natural;
         Contract_Length: Integer) renames
        Wrap_Test_Hire_Recruit_6d7d9a_6a9998;
--  end read only

      pragma Unreferenced(Gnattest_T);
      Amount: constant Positive := Positive(Player_Ship.Crew.Length);

   begin

      Hire_Recruit(1, 1, 0, 0, 1);
      Assert
        (Positive(Player_Ship.Crew.Length) = Amount + 1,
         "Failed to hire recruit to player ship crew.");

--  begin read only
   end Test_Hire_Recruit_test_hirerecruit;
--  end read only

--  begin read only
   procedure Wrap_Test_Buy_Recipe_f9dd3c_e2270a
     (Recipe_Index: Tiny_String.Bounded_String) is
   begin
      begin
         pragma Assert(Tiny_String.Length(Source => Recipe_Index) > 0);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(bases-trade.ads:0):Test_BuyRecipe test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Bases.Trade.Buy_Recipe
        (Recipe_Index);
      begin
         pragma Assert(True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(bases-trade.ads:0:):Test_BuyRecipe test commitment violated");
      end;
   end Wrap_Test_Buy_Recipe_f9dd3c_e2270a;
--  end read only

--  begin read only
   procedure Test_Buy_Recipe_test_buyrecipe(Gnattest_T: in out Test);
   procedure Test_Buy_Recipe_f9dd3c_e2270a(Gnattest_T: in out Test) renames
     Test_Buy_Recipe_test_buyrecipe;
--  id:2.2/f9dd3cb0a4209524/Buy_Recipe/1/0/test_buyrecipe/
   procedure Test_Buy_Recipe_test_buyrecipe(Gnattest_T: in out Test) is
      procedure Buy_Recipe(Recipe_Index: Tiny_String.Bounded_String) renames
        Wrap_Test_Buy_Recipe_f9dd3c_e2270a;
--  end read only

      pragma Unreferenced(Gnattest_T);
      use Tiny_String;

      Amount: constant Positive := Positive(Known_Recipes.Length);
      BaseIndex: constant Positive :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;

   begin

      for Recipe of Bases_Types_List(Sky_Bases(BaseIndex).Base_Type)
        .Recipes loop
         if Known_Recipes.Find_Index
             (Item => To_Bounded_String(To_String(Recipe))) =
           Positive_Container.No_Index then
            Buy_Recipe(To_Bounded_String(To_String(Recipe)));
            exit;
         end if;
      end loop;
      Assert
        (Positive(Known_Recipes.Length) = Amount + 1,
         "Failed to buy recipe from base.");

--  begin read only
   end Test_Buy_Recipe_test_buyrecipe;
--  end read only

--  begin read only
   procedure Wrap_Test_Heal_Wounded_aad41c_6975e9
     (Member_Index: Crew_Container.Extended_Index) is
   begin
      begin
         pragma Assert((Member_Index <= Player_Ship.Crew.Last_Index));
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(bases-trade.ads:0):Test_HealWounded test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Bases.Trade.Heal_Wounded
        (Member_Index);
      begin
         pragma Assert(True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(bases-trade.ads:0:):Test_HealWounded test commitment violated");
      end;
   end Wrap_Test_Heal_Wounded_aad41c_6975e9;
--  end read only

--  begin read only
   procedure Test_Heal_Wounded_test_healwounded(Gnattest_T: in out Test);
   procedure Test_Heal_Wounded_aad41c_6975e9(Gnattest_T: in out Test) renames
     Test_Heal_Wounded_test_healwounded;
--  id:2.2/aad41cf2934a2ea0/Heal_Wounded/1/0/test_healwounded/
   procedure Test_Heal_Wounded_test_healwounded(Gnattest_T: in out Test) is
      procedure Heal_Wounded
        (Member_Index: Crew_Container.Extended_Index) renames
        Wrap_Test_Heal_Wounded_aad41c_6975e9;
--  end read only

      pragma Unreferenced(Gnattest_T);
      Money: Inventory_Data :=
        Inventory_Container.Element
          (Container => Player_Ship.Cargo, Index => 1);

   begin

      Money.Amount := Money.Amount + 2_000;
      Inventory_Container.Replace_Element
        (Container => Player_Ship.Cargo, Index => 1, New_Item => Money);
      Player_Ship.Crew(1).Health := 90;
      Heal_Wounded(1);
      Assert
        (Player_Ship.Crew(1).Health = 100,
         "Failed to heal selected crew member.");
      Player_Ship.Crew(1).Health := 90;
      Heal_Wounded(0);
      Assert(Player_Ship.Crew(1).Health = 100, "Failed to heal whole crew.");

--  begin read only
   end Test_Heal_Wounded_test_healwounded;
--  end read only

--  begin read only
   procedure Wrap_Test_Heal_Cost_115ffd_52d375
     (Cost, Time: in out Natural;
      Member_Index: Crew_Container.Extended_Index) is
   begin
      begin
         pragma Assert(Member_Index <= Player_Ship.Crew.Last_Index);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(bases-trade.ads:0):Test_HealCost test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Bases.Trade.Heal_Cost
        (Cost, Time, Member_Index);
      begin
         pragma Assert(Cost > 0 and Time > 0);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(bases-trade.ads:0:):Test_HealCost test commitment violated");
      end;
   end Wrap_Test_Heal_Cost_115ffd_52d375;
--  end read only

--  begin read only
   procedure Test_Heal_Cost_test_healcost(Gnattest_T: in out Test);
   procedure Test_Heal_Cost_115ffd_52d375(Gnattest_T: in out Test) renames
     Test_Heal_Cost_test_healcost;
--  id:2.2/115ffdcbb5f66733/Heal_Cost/1/0/test_healcost/
   procedure Test_Heal_Cost_test_healcost(Gnattest_T: in out Test) is
      procedure Heal_Cost
        (Cost, Time: in out Natural;
         Member_Index: Crew_Container.Extended_Index) renames
        Wrap_Test_Heal_Cost_115ffd_52d375;
--  end read only

      pragma Unreferenced(Gnattest_T);
      Cost, Time: Natural := 0;

   begin

      Player_Ship.Crew(1).Health := Player_Ship.Crew(1).Health - 10;
      Heal_Cost(Cost, Time, 1);
      Assert(Cost > 0, "Failed to count player crew member heal costs.");
      Assert(Time > 0, "Failed to count player crew member heal time.");

--  begin read only
   end Test_Heal_Cost_test_healcost;
--  end read only

--  begin read only
   function Wrap_Test_Train_Cost_7284bc_f49c70
     (Member_Index: Crew_Container.Extended_Index;
      Skill_Index: Skills_Container.Extended_Index) return Natural is
   begin
      begin
         pragma Assert
           (Member_Index in
              Player_Ship.Crew.First_Index .. Player_Ship.Crew.Last_Index and
            Skill_Index in 1 .. Skills_Amount);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(bases-trade.ads:0):Test_TrainCost test requirement violated");
      end;
      declare
         Test_Train_Cost_7284bc_f49c70_Result: constant Natural :=
           GNATtest_Generated.GNATtest_Standard.Bases.Trade.Train_Cost
             (Member_Index, Skill_Index);
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
         return Test_Train_Cost_7284bc_f49c70_Result;
      end;
   end Wrap_Test_Train_Cost_7284bc_f49c70;
--  end read only

--  begin read only
   procedure Test_Train_Cost_test_traincost(Gnattest_T: in out Test);
   procedure Test_Train_Cost_7284bc_f49c70(Gnattest_T: in out Test) renames
     Test_Train_Cost_test_traincost;
--  id:2.2/7284bc8dcfc21903/Train_Cost/1/0/test_traincost/
   procedure Test_Train_Cost_test_traincost(Gnattest_T: in out Test) is
      function Train_Cost
        (Member_Index: Crew_Container.Extended_Index;
         Skill_Index: Skills_Container.Extended_Index) return Natural renames
        Wrap_Test_Train_Cost_7284bc_f49c70;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Assert
        (Train_Cost(1, 1) > 0,
         "Failed to count player crew member training cost.");

--  begin read only
   end Test_Train_Cost_test_traincost;
--  end read only

--  begin read only
   procedure Wrap_Test_Train_Skill_30157e_5fedf8
     (Member_Index: Crew_Container.Extended_Index;
      Skill_Index: Skills_Container.Extended_Index; Amount: Positive;
      Is_Amount: Boolean := True) is
   begin
      begin
         pragma Assert
           (Member_Index in
              Player_Ship.Crew.First_Index .. Player_Ship.Crew.Last_Index and
            Skill_Index in 1 .. Skills_Amount);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(bases-trade.ads:0):Test_TrainSkill test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Bases.Trade.Train_Skill
        (Member_Index, Skill_Index, Amount, Is_Amount);
      begin
         pragma Assert(True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(bases-trade.ads:0:):Test_TrainSkill test commitment violated");
      end;
   end Wrap_Test_Train_Skill_30157e_5fedf8;
--  end read only

--  begin read only
   procedure Test_Train_Skill_test_trainskill(Gnattest_T: in out Test);
   procedure Test_Train_Skill_30157e_5fedf8(Gnattest_T: in out Test) renames
     Test_Train_Skill_test_trainskill;
--  id:2.2/30157e0a731739f4/Train_Skill/1/0/test_trainskill/
   procedure Test_Train_Skill_test_trainskill(Gnattest_T: in out Test) is
      procedure Train_Skill
        (Member_Index: Crew_Container.Extended_Index;
         Skill_Index: Skills_Container.Extended_Index; Amount: Positive;
         Is_Amount: Boolean := True) renames
        Wrap_Test_Train_Skill_30157e_5fedf8;
--  end read only

      pragma Unreferenced(Gnattest_T);
      SkillsAmount: constant Positive :=
        Positive
          (Skills_Container.Length(Container => Player_Ship.Crew(1).Skills));

   begin

      Train_Skill(1, 1, 1);
      Assert
        (Positive
           (Skills_Container.Length(Container => Player_Ship.Crew(1).Skills)) >
         SkillsAmount,
         "Failed to train new skill.");

--  begin read only
   end Test_Train_Skill_test_trainskill;
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
