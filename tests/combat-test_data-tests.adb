--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Combat.Test_Data.

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
package body Combat.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only
--  begin read only
   procedure Wrap_Test_Combat_Turn_77b950_e12d30 is
   begin
      GNATtest_Generated.GNATtest_Standard.Combat.Combat_Turn;
   end Wrap_Test_Combat_Turn_77b950_e12d30;
--  end read only

--  begin read only
   procedure Test_Combat_Turn_test_combatturn(Gnattest_T: in out Test);
   procedure Test_Combat_Turn_77b950_e12d30(Gnattest_T: in out Test) renames
     Test_Combat_Turn_test_combatturn;
--  id:2.2/77b9506605f815b2/Combat_Turn/1/0/test_combatturn/
   procedure Test_Combat_Turn_test_combatturn(Gnattest_T: in out Test) is
      procedure Combat_Turn renames Wrap_Test_Combat_Turn_77b950_e12d30;
--  end read only

      pragma Unreferenced(Gnattest_T);
      OldX: constant Positive := Player_Ship.Sky_X;
      OldY: constant Positive := Player_Ship.Sky_Y;

   begin

      Player_Ship.Sky_X := 5;
      Player_Ship.Sky_Y := 5;
      Player_Ship.Speed := FULL_SPEED;
      if Start_Combat(2) then
         Combat_Turn;
         Assert(True, "This test can only crash.");
      else
         Combat_Turn;
         Assert(True, "This test can only crash.");
      end if;
      Player_Ship.Speed := DOCKED;
      Player_Ship.Sky_X := OldX;
      Player_Ship.Sky_Y := OldY;

--  begin read only
   end Test_Combat_Turn_test_combatturn;
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
end Combat.Test_Data.Tests;
