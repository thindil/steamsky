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
