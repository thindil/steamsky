--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into HallOfFame.Test_Data.

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
package body HallOfFame.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only
--  begin read only
   procedure Wrap_Test_Update_Hall_Of_Fame_45b65a_b0b945
     (Player_Name, Death_Reason: Unbounded_String) is
   begin
      begin
         pragma Assert
           (Player_Name /= Null_Unbounded_String and
            Death_Reason /= Null_Unbounded_String);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(halloffame.ads:0):Test_UpdateHallOfFame test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.HallOfFame.Update_Hall_Of_Fame
        (Player_Name, Death_Reason);
      begin
         pragma Assert(True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(halloffame.ads:0:):Test_UpdateHallOfFame test commitment violated");
      end;
   end Wrap_Test_Update_Hall_Of_Fame_45b65a_b0b945;
--  end read only

--  begin read only
   procedure Test_Update_Hall_Of_Fame_test_updatehalloffame
     (Gnattest_T: in out Test);
   procedure Test_Update_Hall_Of_Fame_45b65a_b0b945
     (Gnattest_T: in out Test) renames
     Test_Update_Hall_Of_Fame_test_updatehalloffame;
--  id:2.2/45b65a22905d6b25/Update_Hall_Of_Fame/1/0/test_updatehalloffame/
   procedure Test_Update_Hall_Of_Fame_test_updatehalloffame
     (Gnattest_T: in out Test) is
      procedure Update_Hall_Of_Fame
        (Player_Name, Death_Reason: Unbounded_String) renames
        Wrap_Test_Update_Hall_Of_Fame_45b65a_b0b945;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Hall_Of_Fame_Array :=
        (others =>
           Empty_Hall_Of_Fame_Entry);
      Update_Hall_Of_Fame
        (To_Unbounded_String("TestPlayer"), To_Unbounded_String("TestDeath"));
      Assert
        (Hall_Of_Fame_Array(1).Name = To_Unbounded_String("TestPlayer"),
         "Failed to update Hall Of Fame.");

--  begin read only
   end Test_Update_Hall_Of_Fame_test_updatehalloffame;
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
end HallOfFame.Test_Data.Tests;
