--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Ships.Repairs.Test_Data.

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
package body Ships.Repairs.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only
--  begin read only
   procedure Wrap_Test_Repair_Ship_a28a55_831bda(Minutes: Positive) is
   begin
      GNATtest_Generated.GNATtest_Standard.Ships.Repairs.Repair_Ship(Minutes);
   end Wrap_Test_Repair_Ship_a28a55_831bda;
--  end read only

--  begin read only
   procedure Test_Repair_Ship_test_repairship(Gnattest_T: in out Test);
   procedure Test_Repair_Ship_a28a55_831bda(Gnattest_T: in out Test) renames
     Test_Repair_Ship_test_repairship;
--  id:2.2/a28a558c7a663eb4/Repair_Ship/1/0/test_repairship/
   procedure Test_Repair_Ship_test_repairship(Gnattest_T: in out Test) is
      procedure Repair_Ship(Minutes: Positive) renames
        Wrap_Test_Repair_Ship_a28a55_831bda;
--  end read only

      pragma Unreferenced(Gnattest_T);
      Durability: constant Positive := Player_Ship.Modules(1).Durability;

   begin

      Player_Ship.Cargo.Swap(5, 12);
      Player_Ship.Cargo(10).Amount := 1;
      Player_Ship.Modules(1).Durability := Durability - 1;
      Give_Orders(Player_Ship, 4, REPAIR, 0, False);
      Repair_Ship(15);
      Assert
        (Player_Ship.Modules(1).Durability = Durability,
         "Failed to repair ship.");
      New_Game_Settings.Player_Faction := Tiny_String.To_Bounded_String("POLEIS");
      New_Game_Settings.Player_Career := To_Unbounded_String("general");
      New_Game_Settings.Starting_Base := To_Unbounded_String("1");
      New_Game;

--  begin read only
   end Test_Repair_Ship_test_repairship;
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
end Ships.Repairs.Test_Data.Tests;
