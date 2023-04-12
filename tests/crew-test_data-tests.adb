--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Crew.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only

with Config; use Config;
with Ships; use Ships;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

--  begin read only
--  end read only
package body Crew.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only
--  begin read only
   procedure Wrap_Test_Wait_For_Rest_2f3023_b046aa is
   begin
      GNATtest_Generated.GNATtest_Standard.Crew.Wait_For_Rest;
   end Wrap_Test_Wait_For_Rest_2f3023_b046aa;
--  end read only

--  begin read only
   procedure Test_Wait_For_Rest_test_waitforrest(Gnattest_T: in out Test);
   procedure Test_Wait_For_Rest_2f3023_b046aa(Gnattest_T: in out Test) renames
     Test_Wait_For_Rest_test_waitforrest;
--  id:2.2/2f30237da88c91e8/Wait_For_Rest/1/0/test_waitforrest/
   procedure Test_Wait_For_Rest_test_waitforrest(Gnattest_T: in out Test) is
      procedure Wait_For_Rest renames Wrap_Test_Wait_For_Rest_2f3023_b046aa;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Wait_For_Rest;
      Assert(True, "This test can only crash.");

--  begin read only
   end Test_Wait_For_Rest_test_waitforrest;
--  end read only

--  begin read only
   --  procedure Test_Update_Crew_test_updatecrew (Gnattest_T : in out Test_);
   --  procedure Test_Update_Crew_f9e489_test_updatecrew (Gnattest_T : in out Test_) renames Test_Update_Crew_test_updatecrew;
--  id:2.2/f9e4895418584c1b/Update_Crew/1/1/test_updatecrew/
   --  procedure Test_Update_Crew_test_updatecrew (Gnattest_T : in out Test_) is
--  end read only
--
--        pragma Unreferenced(Gnattest_T);
--        use Tiny_String;
--
--     begin
--
--        Update_Crew(1, 1);
--        Player_Ship.Crew(1).Health := 0;
--        Update_Crew(1, 1);
--        New_Game_Settings.Player_Faction := To_Bounded_String("POLEIS");
--        New_Game_Settings.Player_Career := To_Unbounded_String("general");
--        New_Game_Settings.Starting_Base := To_Bounded_String("1");
--        New_Game;
--        Assert(True, "This tests can only crash.");
--
--  begin read only
   --  end Test_Update_Crew_test_updatecrew;
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
end Crew.Test_Data.Tests;
