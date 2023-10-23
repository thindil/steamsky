--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Bases.Ship.Test_Data.

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
package body Bases.Ship.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only
--  begin read only
   procedure Wrap_Test_Upgrade_Ship_5494ec_bcac9d
     (Install: Boolean; Module_Index: Positive) is
   begin
      begin
         pragma Assert
           (Module_Index in
              Player_Ship.Modules.First_Index ..
                    Player_Ship.Modules.Last_Index);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(bases-ship.ads:0):Test_UpdgradeShip test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Bases.Ship.Upgrade_Ship
        (Install, Module_Index);
      begin
         pragma Assert(True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(bases-ship.ads:0:):Test_UpdgradeShip test commitment violated");
      end;
   end Wrap_Test_Upgrade_Ship_5494ec_bcac9d;
--  end read only

--  begin read only
   procedure Test_Upgrade_Ship_test_updgradeship(Gnattest_T: in out Test);
   procedure Test_Upgrade_Ship_5494ec_bcac9d(Gnattest_T: in out Test) renames
     Test_Upgrade_Ship_test_updgradeship;
--  id:2.2/5494ec96ae7f87a3/Upgrade_Ship/1/0/test_updgradeship/
   procedure Test_Upgrade_Ship_test_updgradeship(Gnattest_T: in out Test) is
      procedure Upgrade_Ship(Install: Boolean; Module_Index: Positive) renames
        Wrap_Test_Upgrade_Ship_5494ec_bcac9d;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Upgrade_Ship(False, 10);
      Assert
        (Player_Ship.Modules.Length = 12,
         "Failed to remove module on player ship.");
      Upgrade_Ship(True, 6);
      Assert
        (Player_Ship.Modules.Length = 13,
         "Failed to install module on player ship.");

--  begin read only
   end Test_Upgrade_Ship_test_updgradeship;
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
end Bases.Ship.Test_Data.Tests;
