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
   function Wrap_Test_Goal_Text_2ff075_4fd16a
     (Index: Goals_Container.Extended_Index) return String is
   begin
      begin
         pragma Assert(Index <= Goals_List.Last_Index);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(goals.ads:0):Test_GoalText test requirement violated");
      end;
      declare
         Test_Goal_Text_2ff075_4fd16a_Result: constant String :=
           GNATtest_Generated.GNATtest_Standard.Goals.Goal_Text(Index);
      begin
         begin
            pragma Assert(Test_Goal_Text_2ff075_4fd16a_Result'Length > 0);
            null;
         exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "ens_sloc(goals.ads:0:):Test_GoalText test commitment violated");
         end;
         return Test_Goal_Text_2ff075_4fd16a_Result;
      end;
   end Wrap_Test_Goal_Text_2ff075_4fd16a;
--  end read only

--  begin read only
   procedure Test_Goal_Text_test_goaltext(Gnattest_T: in out Test);
   procedure Test_Goal_Text_2ff075_4fd16a(Gnattest_T: in out Test) renames
     Test_Goal_Text_test_goaltext;
--  id:2.2/2ff075b3d86b9d3f/Goal_Text/1/0/test_goaltext/
   procedure Test_Goal_Text_test_goaltext(Gnattest_T: in out Test) is
      function Goal_Text
        (Index: Goals_Container.Extended_Index) return String renames
        Wrap_Test_Goal_Text_2ff075_4fd16a;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Assert
        (Goal_Text(1) = "Gain max reputation in 1 base",
         "Failed to get selected goal text.");

--  begin read only
   end Test_Goal_Text_test_goaltext;
--  end read only

--  begin read only
   procedure Wrap_Test_Clear_Current_Goal_a99ae8_246a19 is
   begin
      begin
         pragma Assert(True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(goals.ads:0):Test_ClearCurrentGoal test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Goals.Clear_Current_Goal;
      begin
         pragma Assert(Current_Goal.Index = Null_Unbounded_String);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(goals.ads:0:):Test_ClearCurrentGoal test commitment violated");
      end;
   end Wrap_Test_Clear_Current_Goal_a99ae8_246a19;
--  end read only

--  begin read only
   procedure Test_Clear_Current_Goal_test_clearcurrentgoal
     (Gnattest_T: in out Test);
   procedure Test_Clear_Current_Goal_a99ae8_246a19
     (Gnattest_T: in out Test) renames
     Test_Clear_Current_Goal_test_clearcurrentgoal;
--  id:2.2/a99ae824ee9d66fc/Clear_Current_Goal/1/0/test_clearcurrentgoal/
   procedure Test_Clear_Current_Goal_test_clearcurrentgoal
     (Gnattest_T: in out Test) is
      procedure Clear_Current_Goal renames
        Wrap_Test_Clear_Current_Goal_a99ae8_246a19;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Clear_Current_Goal;
      Assert
        (Current_Goal.Index = Null_Unbounded_String,
         "Failed to reset current goal.");
      Current_Goal := Goals_List(2);

--  begin read only
   end Test_Clear_Current_Goal_test_clearcurrentgoal;
--  end read only

--  begin read only
   procedure Wrap_Test_Update_Goal_3df352_b1ee4e
     (G_Type: Goal_Types; Target_Index: Unbounded_String;
      Amount: Positive := 1) is
   begin
      begin
         pragma Assert(True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(goals.ads:0):Test_UpdateGoal test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Goals.Update_Goal
        (G_Type, Target_Index, Amount);
      begin
         pragma Assert(True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(goals.ads:0:):Test_UpdateGoal test commitment violated");
      end;
   end Wrap_Test_Update_Goal_3df352_b1ee4e;
--  end read only

--  begin read only
   procedure Test_Update_Goal_test_updategoal(Gnattest_T: in out Test);
   procedure Test_Update_Goal_3df352_b1ee4e(Gnattest_T: in out Test) renames
     Test_Update_Goal_test_updategoal;
--  id:2.2/3df352363c76aee0/Update_Goal/1/0/test_updategoal/
   procedure Test_Update_Goal_test_updategoal(Gnattest_T: in out Test) is
      procedure Update_Goal
        (G_Type: Goal_Types; Target_Index: Unbounded_String;
         Amount: Positive := 1) renames
        Wrap_Test_Update_Goal_3df352_b1ee4e;
--  end read only

      pragma Unreferenced(Gnattest_T);
      Amount: Natural := Current_Goal.Amount;

   begin

      Update_Goal(DESTROY, To_Unbounded_String("PIRATES"), 1);
      Assert(Current_Goal.Amount = (Amount - 1), "Failed to update goal.");
      Amount := Current_Goal.Amount;
      Update_Goal(REPUTATION, To_Unbounded_String("PIRATES"), 1);
      Assert(Current_Goal.Amount = Amount, "Failed to not update goal.");

--  begin read only
   end Test_Update_Goal_test_updategoal;
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
