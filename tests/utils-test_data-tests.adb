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
   function Wrap_Test_GetRandom_9cea97_28ba02 (Min, Max: Integer)  return Integer
   is
   begin
      begin
         pragma Assert
           (Min <= Max);
         null;
      exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "req_sloc(utils.ads:0):Test_GetRandom test requirement violated");
      end;
      declare
         Test_GetRandom_9cea97_28ba02_Result : constant Integer := GNATtest_Generated.GNATtest_Standard.Utils.GetRandom (Min, Max);
      begin
         begin
            pragma Assert
              (True);
            null;
         exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "ens_sloc(utils.ads:0:):Test_GetRandom test commitment violated");
         end;
         return Test_GetRandom_9cea97_28ba02_Result;
      end;
   end Wrap_Test_GetRandom_9cea97_28ba02;
--  end read only

--  begin read only
   procedure Test_GetRandom_test_getrandom (Gnattest_T : in out Test);
   procedure Test_GetRandom_9cea97_28ba02 (Gnattest_T : in out Test) renames Test_GetRandom_test_getrandom;
--  id:2.2/9cea97d085bfefbe/GetRandom/1/0/test_getrandom/
   procedure Test_GetRandom_test_getrandom (Gnattest_T : in out Test) is
      function GetRandom (Min, Max: Integer) return Integer renames Wrap_Test_GetRandom_9cea97_28ba02;
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      for I in 1 .. 5 loop
         Assert(GetRandom(1, 5) in 1 .. 5, "Wrong random number returned.");
      end loop;
      Assert(GetRandom(5, 5) = 5, "Wrong random number from 5 returned.");

--  begin read only
   end Test_GetRandom_test_getrandom;
--  end read only

--  begin read only
   function Wrap_Test_DaysDifference_ef24bd_fd50f2 (DateToCompare: Date_Record)  return Natural
   is
   begin
      declare
         Test_DaysDifference_ef24bd_fd50f2_Result : constant Natural := GNATtest_Generated.GNATtest_Standard.Utils.DaysDifference (DateToCompare);
      begin
         return Test_DaysDifference_ef24bd_fd50f2_Result;
      end;
   end Wrap_Test_DaysDifference_ef24bd_fd50f2;
--  end read only

--  begin read only
   procedure Test_DaysDifference_test_daysdifference (Gnattest_T : in out Test);
   procedure Test_DaysDifference_ef24bd_fd50f2 (Gnattest_T : in out Test) renames Test_DaysDifference_test_daysdifference;
--  id:2.2/ef24bd841a916fae/DaysDifference/1/0/test_daysdifference/
   procedure Test_DaysDifference_test_daysdifference (Gnattest_T : in out Test) is
      function DaysDifference (DateToCompare: Date_Record) return Natural renames Wrap_Test_DaysDifference_ef24bd_fd50f2;
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      GameDate := (1600, 1, 2, 0, 0);
      Assert(DaysDifference((1600, 1, 1, 0, 0)) = 1, "Invalid count of days difference between game dates.");

--  begin read only
   end Test_DaysDifference_test_daysdifference;
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
