--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Ships.Upgrade.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only

with Ships.Crew; use Ships.Crew;

--  begin read only
--  end read only
package body Ships.Upgrade.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only
--  begin read only
   procedure Wrap_Test_StartUpgrading_50d3a9_d84296 (ModuleIndex, UpgradeType: Positive) 
   is
   begin
      begin
         pragma Assert
           ((ModuleIndex <= PlayerShip.Modules.Last_Index and UpgradeType < 5));
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(ships-upgrade.ads:0):Test_StartUpgrading test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Ships.Upgrade.StartUpgrading (ModuleIndex, UpgradeType);
      begin
         pragma Assert
           (True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(ships-upgrade.ads:0:):Test_StartUpgrading test commitment violated");
      end;
   end Wrap_Test_StartUpgrading_50d3a9_d84296;
--  end read only

--  begin read only
   procedure Test_StartUpgrading_test_startupgrading (Gnattest_T : in out Test);
   procedure Test_StartUpgrading_50d3a9_d84296 (Gnattest_T : in out Test) renames Test_StartUpgrading_test_startupgrading;
--  id:2.2/50d3a9c354e40966/StartUpgrading/1/0/test_startupgrading/
   procedure Test_StartUpgrading_test_startupgrading (Gnattest_T : in out Test) is
   procedure StartUpgrading (ModuleIndex, UpgradeType: Positive) renames Wrap_Test_StartUpgrading_50d3a9_d84296;
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      StartUpgrading(1, 1);
      Assert(True, "This test can only crash.");

--  begin read only
   end Test_StartUpgrading_test_startupgrading;
--  end read only

--  begin read only
   procedure Wrap_Test_UpgradeShip_4209d2_2b44d6 (Minutes: Positive) 
   is
   begin
      GNATtest_Generated.GNATtest_Standard.Ships.Upgrade.UpgradeShip (Minutes);
   end Wrap_Test_UpgradeShip_4209d2_2b44d6;
--  end read only

--  begin read only
   procedure Test_UpgradeShip_test_upgradeship (Gnattest_T : in out Test);
   procedure Test_UpgradeShip_4209d2_2b44d6 (Gnattest_T : in out Test) renames Test_UpgradeShip_test_upgradeship;
--  id:2.2/4209d24a189d78e6/UpgradeShip/1/0/test_upgradeship/
   procedure Test_UpgradeShip_test_upgradeship (Gnattest_T : in out Test) is
   procedure UpgradeShip (Minutes: Positive) renames Wrap_Test_UpgradeShip_4209d2_2b44d6;
--  end read only

      pragma Unreferenced (Gnattest_T);
      Progress: constant Natural := PlayerShip.Modules(1).UpgradeProgress;

   begin

      GiveOrders(PlayerShip, 4, Upgrading);
      UpgradeShip(15);
      Assert(PlayerShip.Modules(1).UpgradeProgress < Progress, "Failed to upgrade ship.");

--  begin read only
   end Test_UpgradeShip_test_upgradeship;
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
end Ships.Upgrade.Test_Data.Tests;
