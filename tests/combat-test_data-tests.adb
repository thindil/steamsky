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
   function Wrap_Test_Start_Combat_23d1ca_2aacf2
     (Enemy_Index: Unbounded_String; New_Combat: Boolean := True)
      return Boolean is
   begin
      begin
         pragma Assert
           (Proto_Ships_Container.Contains
              (Container => Proto_Ships_List, Key => Enemy_Index));
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(combat.ads:0):Test_StartCombat test requirement violated");
      end;
      declare
         Test_Start_Combat_23d1ca_2aacf2_Result: constant Boolean :=
           GNATtest_Generated.GNATtest_Standard.Combat.Start_Combat
             (Enemy_Index, New_Combat);
      begin
         begin
            pragma Assert(True);
            null;
         exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "ens_sloc(combat.ads:0:):Test_StartCombat test commitment violated");
         end;
         return Test_Start_Combat_23d1ca_2aacf2_Result;
      end;
   end Wrap_Test_Start_Combat_23d1ca_2aacf2;
--  end read only

--  begin read only
   procedure Test_Start_Combat_test_startcombat(Gnattest_T: in out Test);
   procedure Test_Start_Combat_23d1ca_2aacf2(Gnattest_T: in out Test) renames
     Test_Start_Combat_test_startcombat;
--  id:2.2/23d1ca79d0a1bec5/Start_Combat/1/0/test_startcombat/
   procedure Test_Start_Combat_test_startcombat(Gnattest_T: in out Test) is
      function Start_Combat
        (Enemy_Index: Unbounded_String; New_Combat: Boolean := True)
         return Boolean renames
        Wrap_Test_Start_Combat_23d1ca_2aacf2;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      if Start_Combat(To_Unbounded_String("2")) then
         Assert(True, "This test can only crash.");
      else
         Assert(True, "This test can only crash.");
      end if;

--  begin read only
   end Test_Start_Combat_test_startcombat;
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
      if Start_Combat(To_Unbounded_String("2")) then
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
