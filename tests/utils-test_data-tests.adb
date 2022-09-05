--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Utils.Test_Data.

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
package body Utils.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only
--  begin read only
   function Wrap_Test_Days_Difference_3eb9cd_fd50f2
     (Date_To_Compare: Date_Record) return Integer is
   begin
      declare
         Test_Days_Difference_3eb9cd_fd50f2_Result: constant Integer :=
           GNATtest_Generated.GNATtest_Standard.Utils.Days_Difference
             (Date_To_Compare);
      begin
         return Test_Days_Difference_3eb9cd_fd50f2_Result;
      end;
   end Wrap_Test_Days_Difference_3eb9cd_fd50f2;
--  end read only

--  begin read only
   procedure Test_Days_Difference_test_daysdifference(Gnattest_T: in out Test);
   procedure Test_Days_Difference_3eb9cd_fd50f2
     (Gnattest_T: in out Test) renames
     Test_Days_Difference_test_daysdifference;
--  id:2.2/3eb9cd623ef6a20f/Days_Difference/1/0/test_daysdifference/
   procedure Test_Days_Difference_test_daysdifference
     (Gnattest_T: in out Test) is
      function Days_Difference
        (Date_To_Compare: Date_Record) return Integer renames
        Wrap_Test_Days_Difference_3eb9cd_fd50f2;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Game_Date := (1_600, 1, 2, 0, 0);
      Assert
        (Days_Difference((1_600, 1, 1, 0, 0)) = 1,
         "Invalid count of days difference between game dates.");

--  begin read only
   end Test_Days_Difference_test_daysdifference;
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
end Utils.Test_Data.Tests;
