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
   procedure Wrap_Test_Repair_Ship_a28a55_2d5600(Module_Index: Integer) is
   begin
      begin
         pragma Assert((Module_Index <= Player_Ship.Modules.Last_Index));
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(bases-ship.ads:0):Test_RepairShip test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Bases.Ship.Repair_Ship
        (Module_Index);
      begin
         pragma Assert(True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(bases-ship.ads:0:):Test_RepairShip test commitment violated");
      end;
   end Wrap_Test_Repair_Ship_a28a55_2d5600;
--  end read only

--  begin read only
   procedure Test_Repair_Ship_test_repairship(Gnattest_T: in out Test);
   procedure Test_Repair_Ship_a28a55_2d5600(Gnattest_T: in out Test) renames
     Test_Repair_Ship_test_repairship;
--  id:2.2/a28a558c7a663eb4/Repair_Ship/1/0/test_repairship/
   procedure Test_Repair_Ship_test_repairship(Gnattest_T: in out Test) is
      procedure Repair_Ship(Module_Index: Integer) renames
        Wrap_Test_Repair_Ship_a28a55_2d5600;
--  end read only

      pragma Unreferenced(Gnattest_T);
      Durability: constant Positive := Player_Ship.Modules(1).Durability;

   begin

      Player_Ship.Modules(1).Durability :=
        Player_Ship.Modules(1).Durability - 10;
      Repair_Ship(1);
      Assert
        (Player_Ship.Modules(1).Durability = Durability,
         "Failed to repair selected player ship module in base.");
      Player_Ship.Modules(1).Durability :=
        Player_Ship.Modules(1).Durability - 10;
      Repair_Ship(0);
      Assert
        (Player_Ship.Modules(1).Durability = Durability,
         "Failed to repair player ship module in base.");

--  begin read only
   end Test_Repair_Ship_test_repairship;
--  end read only

--  begin read only
   procedure Wrap_Test_Upgrade_Ship_a374c0_f1dd24
     (Install: Boolean; Module_Index: Tiny_String.Bounded_String) is
   begin
      begin
         pragma Assert(Tiny_String.Length(Source => Module_Index) > 0);
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
   end Wrap_Test_Upgrade_Ship_a374c0_f1dd24;
--  end read only

--  begin read only
   procedure Test_Upgrade_Ship_test_updgradeship(Gnattest_T: in out Test);
   procedure Test_Upgrade_Ship_a374c0_f1dd24(Gnattest_T: in out Test) renames
     Test_Upgrade_Ship_test_updgradeship;
--  id:2.2/a374c0e96c20527f/Upgrade_Ship/1/0/test_updgradeship/
   procedure Test_Upgrade_Ship_test_updgradeship(Gnattest_T: in out Test) is
      procedure Upgrade_Ship
        (Install: Boolean; Module_Index: Tiny_String.Bounded_String) renames
        Wrap_Test_Upgrade_Ship_a374c0_f1dd24;
--  end read only

      pragma Unreferenced(Gnattest_T);
      use Tiny_String;

   begin

      Upgrade_Ship(False, To_Bounded_String("10"));
      Assert
        (Player_Ship.Modules.Length = 12,
         "Failed to remove module on player ship.");
      Upgrade_Ship(True, To_Bounded_String("6"));
      Assert
        (Player_Ship.Modules.Length = 13,
         "Failed to install module on player ship.");

--  begin read only
   end Test_Upgrade_Ship_test_updgradeship;
--  end read only

--  begin read only
   procedure Wrap_Test_Pay_For_Dock_9dddef_d92d34 is
   begin
      GNATtest_Generated.GNATtest_Standard.Bases.Ship.Pay_For_Dock;
   end Wrap_Test_Pay_For_Dock_9dddef_d92d34;
--  end read only

--  begin read only
   procedure Test_Pay_For_Dock_test_payfordock(Gnattest_T: in out Test);
   procedure Test_Pay_For_Dock_9dddef_d92d34(Gnattest_T: in out Test) renames
     Test_Pay_For_Dock_test_payfordock;
--  id:2.2/9dddef712271b3af/Pay_For_Dock/1/0/test_payfordock/
   procedure Test_Pay_For_Dock_test_payfordock(Gnattest_T: in out Test) is
      procedure Pay_For_Dock renames Wrap_Test_Pay_For_Dock_9dddef_d92d34;
--  end read only

      pragma Unreferenced(Gnattest_T);
      Money: constant Positive :=
        Inventory_Container.Element(Container => Player_Ship.Cargo, Index => 1)
          .Amount;

   begin

      Pay_For_Dock;
      Assert
        (Inventory_Container.Element
           (Container => Player_Ship.Cargo, Index => 1)
           .Amount <
         Money,
         "Failed to pay for docks.");

--  begin read only
   end Test_Pay_For_Dock_test_payfordock;
--  end read only

--  begin read only
   procedure Wrap_Test_Repair_Cost_eb3d7e_6cc7b1
     (Cost, Time: in out Natural; Module_Index: Integer) is
   begin
      begin
         pragma Assert(Module_Index in -2 .. Player_Ship.Modules.Last_Index);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(bases-ship.ads:0):Test_RepairCost test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Bases.Ship.Repair_Cost
        (Cost, Time, Module_Index);
      begin
         pragma Assert(Cost > 0 and Time > 0);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(bases-ship.ads:0:):Test_RepairCost test commitment violated");
      end;
   end Wrap_Test_Repair_Cost_eb3d7e_6cc7b1;
--  end read only

--  begin read only
   procedure Test_Repair_Cost_test_repaircost(Gnattest_T: in out Test);
   procedure Test_Repair_Cost_eb3d7e_6cc7b1(Gnattest_T: in out Test) renames
     Test_Repair_Cost_test_repaircost;
--  id:2.2/eb3d7e6578095883/Repair_Cost/1/0/test_repaircost/
   procedure Test_Repair_Cost_test_repaircost(Gnattest_T: in out Test) is
      procedure Repair_Cost
        (Cost, Time: in out Natural; Module_Index: Integer) renames
        Wrap_Test_Repair_Cost_eb3d7e_6cc7b1;
--  end read only

      pragma Unreferenced(Gnattest_T);
      Cost, Time, OverallCost, OverallTime: Natural := 0;

   begin

      Player_Ship.Modules(1).Durability :=
        Player_Ship.Modules(1).Durability - 5;
      Repair_Cost(Cost, Time, 1);
      Assert(Cost > 0, "Failed to count player ship repair costs.");
      Assert(Time > 0, "Failed to count player ship repair time.");
      Repair_Cost(OverallCost, OverallTime, 0);
      Assert
        (Cost = OverallCost,
         "Failed to count player ship overall repair costs.");
      Assert
        (Time = OverallTime,
         "Failed to count player ship overall repair time.");

--  begin read only
   end Test_Repair_Cost_test_repaircost;
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
