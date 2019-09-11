--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Bases.Test_Data.

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
package body Bases.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only
--  begin read only
   procedure Wrap_Test_GainRep_6338e6_901e58 (BaseIndex: BasesRange; Points: Integer) 
   is
   begin
      GNATtest_Generated.GNATtest_Standard.Bases.GainRep (BaseIndex, Points);
   end Wrap_Test_GainRep_6338e6_901e58;
--  end read only

--  begin read only
   procedure Test_GainRep_test_gainrep (Gnattest_T : in out Test);
   procedure Test_GainRep_6338e6_901e58 (Gnattest_T : in out Test) renames Test_GainRep_test_gainrep;
--  id:2.2/6338e6483a422dde/GainRep/1/0/test_gainrep/
   procedure Test_GainRep_test_gainrep (Gnattest_T : in out Test) is
   procedure GainRep (BaseIndex: BasesRange; Points: Integer) renames Wrap_Test_GainRep_6338e6_901e58;
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      SkyBases(1).Reputation := (1, 1);
      GainRep(1, 1);
      Assert(SkyBases(1).Reputation(2) = 2, "Failed to gain reputation in base.");
      GainRep(1, -1);
      Assert(SkyBases(1).Reputation(2) = 1, "Failed to lose reputation in base.");

--  begin read only
   end Test_GainRep_test_gainrep;
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
end Bases.Test_Data.Tests;
