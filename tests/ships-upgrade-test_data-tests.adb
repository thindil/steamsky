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

with Config; use Config;
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
   procedure Wrap_Test_Start_Upgrading_a08111_8cb602
     (Module_Index: Modules_Container.Extended_Index;
      Upgrade_Type: Positive) is
   begin
      begin
         pragma Assert
           ((Module_Index in
               Player_Ship.Modules.First_Index ..
                     Player_Ship.Modules.Last_Index and
             Upgrade_Type < 5));
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(ships-upgrade.ads:0):Test_StartUpgrading test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Ships.Upgrade.Start_Upgrading
        (Module_Index, Upgrade_Type);
      begin
         pragma Assert(True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(ships-upgrade.ads:0:):Test_StartUpgrading test commitment violated");
      end;
   end Wrap_Test_Start_Upgrading_a08111_8cb602;
--  end read only

--  begin read only
   procedure Test_Start_Upgrading_test_startupgrading(Gnattest_T: in out Test);
   procedure Test_Start_Upgrading_a08111_8cb602
     (Gnattest_T: in out Test) renames
     Test_Start_Upgrading_test_startupgrading;
--  id:2.2/a08111b285a925c0/Start_Upgrading/1/0/test_startupgrading/
   procedure Test_Start_Upgrading_test_startupgrading
     (Gnattest_T: in out Test) is
      procedure Start_Upgrading
        (Module_Index: Modules_Container.Extended_Index;
         Upgrade_Type: Positive) renames
        Wrap_Test_Start_Upgrading_a08111_8cb602;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Start_Upgrading(1, 1);
      Assert(True, "This test can only crash.");

--  begin read only
   end Test_Start_Upgrading_test_startupgrading;
--  end read only

--  begin read only
   procedure Wrap_Test_Upgrade_Ship_09fde7_2b44d6(Minutes: Positive) is
   begin
      GNATtest_Generated.GNATtest_Standard.Ships.Upgrade.Upgrade_Ship(Minutes);
   end Wrap_Test_Upgrade_Ship_09fde7_2b44d6;
--  end read only

--  begin read only
   procedure Test_Upgrade_Ship_test_upgradeship(Gnattest_T: in out Test);
   procedure Test_Upgrade_Ship_09fde7_2b44d6(Gnattest_T: in out Test) renames
     Test_Upgrade_Ship_test_upgradeship;
--  id:2.2/09fde78968354cd1/Upgrade_Ship/1/0/test_upgradeship/
   procedure Test_Upgrade_Ship_test_upgradeship(Gnattest_T: in out Test) is
      procedure Upgrade_Ship(Minutes: Positive) renames
        Wrap_Test_Upgrade_Ship_09fde7_2b44d6;
--  end read only

      pragma Unreferenced(Gnattest_T);
      Progress: constant Natural := Player_Ship.Modules(1).Upgrade_Progress;
      OldUpgrade: constant Natural := Player_Ship.Upgrade_Module;

   begin

      Player_Ship.Cargo.Swap(5, 12);
      Player_Ship.Cargo(10).Amount := 1;
      Give_Orders(Player_Ship, 4, UPGRADING);
      Upgrade_Ship(15);
      Assert
        (Player_Ship.Modules(1).Upgrade_Progress < Progress,
         "Failed to upgrade ship.");
      Player_Ship.Upgrade_Module := 0;
      Upgrade_Ship(15);
      Assert(True, "This test can only crash");
      Player_Ship.Upgrade_Module := OldUpgrade;
      New_Game_Settings.Player_Faction := To_Unbounded_String("POLEIS");
      New_Game_Settings.Player_Career := To_Unbounded_String("general");
      New_Game_Settings.Starting_Base := To_Unbounded_String("1");
      New_Game;

--  begin read only
   end Test_Upgrade_Ship_test_upgradeship;
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
