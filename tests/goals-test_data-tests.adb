--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Goals.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only

--  begin read only
--  end read only
package body Goals.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only
--  begin read only
   function Wrap_Test_GoalText_c541ba_267fe9 (Index: Goals_Container.Extended_Index)  return String
   is
   begin
      begin
         pragma Assert
           (Index <= Goals_List.Last_Index);
         null;
      exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "req_sloc(goals.ads:0):Test_GoalText test requirement violated");
      end;
      declare
         Test_GoalText_c541ba_267fe9_Result : constant String := GNATtest_Generated.GNATtest_Standard.Goals.GoalText (Index);
      begin
         begin
            pragma Assert
              (True);
            null;
         exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "ens_sloc(goals.ads:0:):Test_GoalText test commitment violated");
         end;
         return Test_GoalText_c541ba_267fe9_Result;
      end;
   end Wrap_Test_GoalText_c541ba_267fe9;
--  end read only

--  begin read only
   procedure Test_GoalText_test_goaltext (Gnattest_T : in out Test);
   procedure Test_GoalText_c541ba_267fe9 (Gnattest_T : in out Test) renames Test_GoalText_test_goaltext;
--  id:2.2/c541bacce47cf44b/GoalText/1/0/test_goaltext/
   procedure Test_GoalText_test_goaltext (Gnattest_T : in out Test) is
      function GoalText (Index: Goals_Container.Extended_Index) return String renames Wrap_Test_GoalText_c541ba_267fe9;
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      Assert(GoalText(1) = "Gain max reputation in 1 base", "Failed to get selected goal text.");

--  begin read only
   end Test_GoalText_test_goaltext;
--  end read only

--  begin read only
   procedure Wrap_Test_ClearCurrentGoal_cb9255_08f1aa
   is
   begin
      GNATtest_Generated.GNATtest_Standard.Goals.ClearCurrentGoal;
   end Wrap_Test_ClearCurrentGoal_cb9255_08f1aa;
--  end read only

--  begin read only
   procedure Test_ClearCurrentGoal_test_clearcurrentgoal (Gnattest_T : in out Test);
   procedure Test_ClearCurrentGoal_cb9255_08f1aa (Gnattest_T : in out Test) renames Test_ClearCurrentGoal_test_clearcurrentgoal;
--  id:2.2/cb92551f0de5a16e/ClearCurrentGoal/1/0/test_clearcurrentgoal/
   procedure Test_ClearCurrentGoal_test_clearcurrentgoal (Gnattest_T : in out Test) is
   procedure ClearCurrentGoal renames Wrap_Test_ClearCurrentGoal_cb9255_08f1aa;
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      ClearCurrentGoal;
      Assert(CurrentGoal.Index = Null_Unbounded_String, "Failed to reset current goal.");
      CurrentGoal := Goals_List(1);


--  begin read only
   end Test_ClearCurrentGoal_test_clearcurrentgoal;
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
end Goals.Test_Data.Tests;
