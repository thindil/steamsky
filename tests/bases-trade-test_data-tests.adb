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
