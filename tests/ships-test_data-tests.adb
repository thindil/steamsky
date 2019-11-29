--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Ships.Test_Data.

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
package body Ships.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only
--  begin read only
   function Wrap_Test_CreateShip_d36d0c_13521c (ProtoIndex, Name: Unbounded_String; X, Y: Integer; Speed: ShipSpeed; RandomUpgrades: Boolean := True)  return ShipRecord
   is
   begin
      begin
         pragma Assert
           ((ProtoShips_Container.Contains(ProtoShips_List, ProtoIndex)));
         null;
      exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "req_sloc(ships.ads:0):Test_CreateShip test requirement violated");
      end;
      declare
         Test_CreateShip_d36d0c_13521c_Result : constant ShipRecord := GNATtest_Generated.GNATtest_Standard.Ships.CreateShip (ProtoIndex, Name, X, Y, Speed, RandomUpgrades);
      begin
         begin
            pragma Assert
              (True);
            null;
         exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "ens_sloc(ships.ads:0:):Test_CreateShip test commitment violated");
         end;
         return Test_CreateShip_d36d0c_13521c_Result;
      end;
   end Wrap_Test_CreateShip_d36d0c_13521c;
--  end read only

--  begin read only
   procedure Test_CreateShip_test_createship (Gnattest_T : in out Test);
   procedure Test_CreateShip_d36d0c_13521c (Gnattest_T : in out Test) renames Test_CreateShip_test_createship;
--  id:2.2/d36d0cd7f65b1765/CreateShip/1/0/test_createship/
   procedure Test_CreateShip_test_createship (Gnattest_T : in out Test) is
      function CreateShip (ProtoIndex, Name: Unbounded_String; X, Y: Integer; Speed: ShipSpeed; RandomUpgrades: Boolean := True) return ShipRecord renames Wrap_Test_CreateShip_d36d0c_13521c;
--  end read only

      pragma Unreferenced (Gnattest_T);
      TestShip: constant ShipRecord := CreateShip(To_Unbounded_String("2"), Null_Unbounded_String, 5, 5, Full_Speed);

   begin

      Assert(TestShip.Name = To_Unbounded_String("Tiny pirates ship"), "Failed to create a new NPC ship.");

--  begin read only
   end Test_CreateShip_test_createship;
--  end read only

--  begin read only
   function Wrap_Test_CountShipWeight_f01f93_0591fd (Ship: ShipRecord)  return Positive
   is
   begin
      declare
         Test_CountShipWeight_f01f93_0591fd_Result : constant Positive := GNATtest_Generated.GNATtest_Standard.Ships.CountShipWeight (Ship);
      begin
         return Test_CountShipWeight_f01f93_0591fd_Result;
      end;
   end Wrap_Test_CountShipWeight_f01f93_0591fd;
--  end read only

--  begin read only
   procedure Test_CountShipWeight_test_countshipweight (Gnattest_T : in out Test);
   procedure Test_CountShipWeight_f01f93_0591fd (Gnattest_T : in out Test) renames Test_CountShipWeight_test_countshipweight;
--  id:2.2/f01f93836329aac8/CountShipWeight/1/0/test_countshipweight/
   procedure Test_CountShipWeight_test_countshipweight (Gnattest_T : in out Test) is
      function CountShipWeight (Ship: ShipRecord) return Positive renames Wrap_Test_CountShipWeight_f01f93_0591fd;
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      Assert(CountShipWeight(PlayerShip) > 0, "This test can only crash.");

--  begin read only
   end Test_CountShipWeight_test_countshipweight;
--  end read only

--  begin read only
   function Wrap_Test_GenerateShipName_26fd48_7313c0 (Owner: Unbounded_String)  return Unbounded_String
   is
   begin
      begin
         pragma Assert
           (Owner /= Null_Unbounded_String);
         null;
      exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "req_sloc(ships.ads:0):Test_GenerateShipName test requirement violated");
      end;
      declare
         Test_GenerateShipName_26fd48_7313c0_Result : constant Unbounded_String := GNATtest_Generated.GNATtest_Standard.Ships.GenerateShipName (Owner);
      begin
         begin
            pragma Assert
              (True);
            null;
         exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "ens_sloc(ships.ads:0:):Test_GenerateShipName test commitment violated");
         end;
         return Test_GenerateShipName_26fd48_7313c0_Result;
      end;
   end Wrap_Test_GenerateShipName_26fd48_7313c0;
--  end read only

--  begin read only
   procedure Test_GenerateShipName_test_generateshipname (Gnattest_T : in out Test);
   procedure Test_GenerateShipName_26fd48_7313c0 (Gnattest_T : in out Test) renames Test_GenerateShipName_test_generateshipname;
--  id:2.2/26fd4844628732d9/GenerateShipName/1/0/test_generateshipname/
   procedure Test_GenerateShipName_test_generateshipname (Gnattest_T : in out Test) is
      function GenerateShipName (Owner: Unbounded_String) return Unbounded_String renames Wrap_Test_GenerateShipName_26fd48_7313c0;
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      Assert(GenerateShipName(To_Unbounded_String("POLEIS")) /= Null_Unbounded_String, "Failed to generate ship name.");

--  begin read only
   end Test_GenerateShipName_test_generateshipname;
--  end read only

--  begin read only
   function Wrap_Test_CountCombatValue_77d5b0_424a30 return Natural
   is
   begin
      declare
         Test_CountCombatValue_77d5b0_424a30_Result : constant Natural := GNATtest_Generated.GNATtest_Standard.Ships.CountCombatValue;
      begin
         return Test_CountCombatValue_77d5b0_424a30_Result;
      end;
   end Wrap_Test_CountCombatValue_77d5b0_424a30;
--  end read only

--  begin read only
   procedure Test_CountCombatValue_test_countcombatvalue (Gnattest_T : in out Test);
   procedure Test_CountCombatValue_77d5b0_424a30 (Gnattest_T : in out Test) renames Test_CountCombatValue_test_countcombatvalue;
--  id:2.2/77d5b05ac9b3095d/CountCombatValue/1/0/test_countcombatvalue/
   procedure Test_CountCombatValue_test_countcombatvalue (Gnattest_T : in out Test) is
      function CountCombatValue return Natural renames Wrap_Test_CountCombatValue_77d5b0_424a30;
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      Assert(CountCombatValue > -1, "Failed to get player ship combat value.");

--  begin read only
   end Test_CountCombatValue_test_countcombatvalue;
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
end Ships.Test_Data.Tests;
