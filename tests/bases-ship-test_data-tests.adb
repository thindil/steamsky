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
   procedure Wrap_Test_RepairShip_41c4af_3016a0(ModuleIndex: Integer) is
   begin
      begin
         pragma Assert((ModuleIndex <= Player_Ship.Modules.Last_Index));
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(bases-ship.ads:0):Test_RepairShip test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Bases.Ship.RepairShip(ModuleIndex);
      begin
         pragma Assert(True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(bases-ship.ads:0:):Test_RepairShip test commitment violated");
      end;
   end Wrap_Test_RepairShip_41c4af_3016a0;
--  end read only

--  begin read only
   procedure Test_RepairShip_test_repairship(Gnattest_T: in out Test);
   procedure Test_RepairShip_41c4af_3016a0(Gnattest_T: in out Test) renames
     Test_RepairShip_test_repairship;
--  id:2.2/41c4af333c446830/RepairShip/1/0/test_repairship/
   procedure Test_RepairShip_test_repairship(Gnattest_T: in out Test) is
      procedure RepairShip(ModuleIndex: Integer) renames
        Wrap_Test_RepairShip_41c4af_3016a0;
--  end read only

      pragma Unreferenced(Gnattest_T);
      Durability: constant Positive := Player_Ship.Modules(1).Durability;

   begin

      Player_Ship.Modules(1).Durability :=
        Player_Ship.Modules(1).Durability - 10;
      RepairShip(1);
      Assert
        (Player_Ship.Modules(1).Durability = Durability,
         "Failed to repair selected player ship module in base.");
      Player_Ship.Modules(1).Durability :=
        Player_Ship.Modules(1).Durability - 10;
      RepairShip(0);
      Assert
        (Player_Ship.Modules(1).Durability = Durability,
         "Failed to repair player ship module in base.");

--  begin read only
   end Test_RepairShip_test_repairship;
--  end read only

--  begin read only
   procedure Wrap_Test_UpgradeShip_62a16e_73d66b
     (Install: Boolean; ModuleIndex: Unbounded_String) is
   begin
      begin
         pragma Assert((ModuleIndex /= Null_Unbounded_String));
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(bases-ship.ads:0):Test_UpdgradeShip test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Bases.Ship.UpgradeShip
        (Install, ModuleIndex);
      begin
         pragma Assert(True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(bases-ship.ads:0:):Test_UpdgradeShip test commitment violated");
      end;
   end Wrap_Test_UpgradeShip_62a16e_73d66b;
--  end read only

--  begin read only
   procedure Test_UpgradeShip_test_updgradeship(Gnattest_T: in out Test);
   procedure Test_UpgradeShip_62a16e_73d66b(Gnattest_T: in out Test) renames
     Test_UpgradeShip_test_updgradeship;
--  id:2.2/62a16ebed8881e22/UpgradeShip/1/0/test_updgradeship/
   procedure Test_UpgradeShip_test_updgradeship(Gnattest_T: in out Test) is
      procedure UpgradeShip
        (Install: Boolean; ModuleIndex: Unbounded_String) renames
        Wrap_Test_UpgradeShip_62a16e_73d66b;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      UpgradeShip(False, To_Unbounded_String("10"));
      Assert
        (Player_Ship.Modules.Length = 12,
         "Failed to remove module on player ship.");
      UpgradeShip(True, To_Unbounded_String("6"));
      Assert
        (Player_Ship.Modules.Length = 13,
         "Failed to install module on player ship.");

--  begin read only
   end Test_UpgradeShip_test_updgradeship;
--  end read only

--  begin read only
   procedure Wrap_Test_PayForDock_b46f8e_d92d34 is
   begin
      GNATtest_Generated.GNATtest_Standard.Bases.Ship.PayForDock;
   end Wrap_Test_PayForDock_b46f8e_d92d34;
--  end read only

--  begin read only
   procedure Test_PayForDock_test_payfordock(Gnattest_T: in out Test);
   procedure Test_PayForDock_b46f8e_d92d34(Gnattest_T: in out Test) renames
     Test_PayForDock_test_payfordock;
--  id:2.2/b46f8ee6fa97fa17/PayForDock/1/0/test_payfordock/
   procedure Test_PayForDock_test_payfordock(Gnattest_T: in out Test) is
      procedure PayForDock renames Wrap_Test_PayForDock_b46f8e_d92d34;
--  end read only

      pragma Unreferenced(Gnattest_T);
      Money: constant Positive := Player_Ship.Cargo(1).Amount;

   begin

      PayForDock;
      Assert(Player_Ship.Cargo(1).Amount < Money, "Failed to pay for docks.");

--  begin read only
   end Test_PayForDock_test_payfordock;
--  end read only

--  begin read only
   procedure Wrap_Test_RepairCost_2d9781_862779
     (Cost, Time: in out Natural; ModuleIndex: Integer) is
   begin
      begin
         pragma Assert(ModuleIndex in -2 .. Player_Ship.Modules.Last_Index);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(bases-ship.ads:0):Test_RepairCost test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Bases.Ship.RepairCost
        (Cost, Time, ModuleIndex);
      begin
         pragma Assert(Cost > 0 and Time > 0);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(bases-ship.ads:0:):Test_RepairCost test commitment violated");
      end;
   end Wrap_Test_RepairCost_2d9781_862779;
--  end read only

--  begin read only
   procedure Test_RepairCost_test_repaircost(Gnattest_T: in out Test);
   procedure Test_RepairCost_2d9781_862779(Gnattest_T: in out Test) renames
     Test_RepairCost_test_repaircost;
--  id:2.2/2d9781143dbec48d/RepairCost/1/0/test_repaircost/
   procedure Test_RepairCost_test_repaircost(Gnattest_T: in out Test) is
      procedure RepairCost
        (Cost, Time: in out Natural; ModuleIndex: Integer) renames
        Wrap_Test_RepairCost_2d9781_862779;
--  end read only

      pragma Unreferenced(Gnattest_T);

      Cost, Time, OverallCost, OverallTime: Natural := 0;

   begin

      Player_Ship.Modules(1).Durability :=
        Player_Ship.Modules(1).Durability - 5;
      RepairCost(Cost, Time, 1);
      Assert(Cost > 0, "Failed to count player ship repair costs.");
      Assert(Time > 0, "Failed to count player ship repair time.");
      RepairCost(OverallCost, OverallTime, 0);
      Assert
        (Cost = OverallCost,
         "Failed to count player ship overall repair costs.");
      Assert
        (Time = OverallTime,
         "Failed to count player ship overall repair time.");

--  begin read only
   end Test_RepairCost_test_repaircost;
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
