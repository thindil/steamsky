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

with Maps; use Maps;
with Events; use Events;

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
   procedure Wrap_Test_Ask_For_Bases_023c5f_f3f6c6 is
   begin
      GNATtest_Generated.GNATtest_Standard.Bases.Ask_For_Bases;
   end Wrap_Test_Ask_For_Bases_023c5f_f3f6c6;
--  end read only

--  begin read only
   procedure Test_Ask_For_Bases_test_askforbases(Gnattest_T: in out Test);
   procedure Test_Ask_For_Bases_023c5f_f3f6c6(Gnattest_T: in out Test) renames
     Test_Ask_For_Bases_test_askforbases;
--  id:2.2/023c5f5732e0c1b0/Ask_For_Bases/1/0/test_askforbases/
   procedure Test_Ask_For_Bases_test_askforbases(Gnattest_T: in out Test) is
      procedure Ask_For_Bases renames Wrap_Test_Ask_For_Bases_023c5f_f3f6c6;
--  end read only

      pragma Unreferenced(Gnattest_T);
      BaseIndex: constant Positive :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;

   begin

      Sky_Bases(BaseIndex).Asked_For_Bases := False;
      Ask_For_Bases;
      Assert(True, "This test can only crash.");

--  begin read only
   end Test_Ask_For_Bases_test_askforbases;
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
