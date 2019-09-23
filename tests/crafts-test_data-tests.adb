--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Crafts.Test_Data.

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
package body Crafts.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only
--  begin read only
   procedure Wrap_Test_Manufacturing_dd583a_cf804c (Minutes: Positive) 
   is
   begin
      GNATtest_Generated.GNATtest_Standard.Crafts.Manufacturing (Minutes);
   end Wrap_Test_Manufacturing_dd583a_cf804c;
--  end read only

--  begin read only
   procedure Test_Manufacturing_test_manufacturing (Gnattest_T : in out Test);
   procedure Test_Manufacturing_dd583a_cf804c (Gnattest_T : in out Test) renames Test_Manufacturing_test_manufacturing;
--  id:2.2/dd583af67efcd5dc/Manufacturing/1/0/test_manufacturing/
   procedure Test_Manufacturing_test_manufacturing (Gnattest_T : in out Test) is
   procedure Manufacturing (Minutes: Positive) renames Wrap_Test_Manufacturing_dd583a_cf804c;
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      Manufacturing(15);
      Assert(True, "This test can only crash.");

--  begin read only
   end Test_Manufacturing_test_manufacturing;
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
end Crafts.Test_Data.Tests;
