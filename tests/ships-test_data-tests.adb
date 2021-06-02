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
   function Wrap_Test_CreateShip_1a2f4d_ff8887
     (ProtoIndex, Name: Unbounded_String; X: Map_X_Range; Y: Map_Y_Range;
      Speed: Ship_Speed; RandomUpgrades: Boolean := True) return Ship_Record is
   begin
      begin
         pragma Assert
           ((Proto_Ships_Container.Contains(Proto_Ships_List, ProtoIndex)));
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(ships.ads:0):Test_CreateShip test requirement violated");
      end;
      declare
         Test_CreateShip_1a2f4d_ff8887_Result: constant Ship_Record :=
           GNATtest_Generated.GNATtest_Standard.Ships.CreateShip
             (ProtoIndex, Name, X, Y, Speed, RandomUpgrades);
      begin
         begin
            pragma Assert(True);
            null;
         exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "ens_sloc(ships.ads:0:):Test_CreateShip test commitment violated");
         end;
         return Test_CreateShip_1a2f4d_ff8887_Result;
      end;
   end Wrap_Test_CreateShip_1a2f4d_ff8887;
--  end read only

--  begin read only
   procedure Test_CreateShip_test_createship(Gnattest_T: in out Test);
   procedure Test_CreateShip_1a2f4d_ff8887(Gnattest_T: in out Test) renames
     Test_CreateShip_test_createship;
--  id:2.2/1a2f4d8dd86b513c/CreateShip/1/0/test_createship/
   procedure Test_CreateShip_test_createship(Gnattest_T: in out Test) is
      function CreateShip
        (ProtoIndex, Name: Unbounded_String; X: Map_X_Range; Y: Map_Y_Range;
         Speed: Ship_Speed; RandomUpgrades: Boolean := True)
         return Ship_Record renames
        Wrap_Test_CreateShip_1a2f4d_ff8887;
--  end read only

      pragma Unreferenced(Gnattest_T);
      TestShip: constant Ship_Record :=
        CreateShip
          (To_Unbounded_String("2"), Null_Unbounded_String, 5, 5, FULL_SPEED);

   begin

      Assert
        (TestShip.Name = To_Unbounded_String("Tiny pirates ship"),
         "Failed to create a new NPC ship.");

--  begin read only
   end Test_CreateShip_test_createship;
--  end read only

--  begin read only
   function Wrap_Test_CountShipWeight_dd08c2_0591fd
     (Ship: Ship_Record) return Positive is
   begin
      declare
         Test_CountShipWeight_dd08c2_0591fd_Result: constant Positive :=
           GNATtest_Generated.GNATtest_Standard.Ships.CountShipWeight(Ship);
      begin
         return Test_CountShipWeight_dd08c2_0591fd_Result;
      end;
   end Wrap_Test_CountShipWeight_dd08c2_0591fd;
--  end read only

--  begin read only
   procedure Test_CountShipWeight_test_countshipweight
     (Gnattest_T: in out Test);
   procedure Test_CountShipWeight_dd08c2_0591fd
     (Gnattest_T: in out Test) renames
     Test_CountShipWeight_test_countshipweight;
--  id:2.2/dd08c24ed2279fe4/CountShipWeight/1/0/test_countshipweight/
   procedure Test_CountShipWeight_test_countshipweight
     (Gnattest_T: in out Test) is
      function CountShipWeight(Ship: Ship_Record) return Positive renames
        Wrap_Test_CountShipWeight_dd08c2_0591fd;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Assert(CountShipWeight(PlayerShip) > 0, "This test can only crash.");

--  begin read only
   end Test_CountShipWeight_test_countshipweight;
--  end read only

--  begin read only
   function Wrap_Test_GenerateShipName_26fd48_7313c0
     (Owner: Unbounded_String) return Unbounded_String is
   begin
      begin
         pragma Assert(Owner /= Null_Unbounded_String);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(ships.ads:0):Test_GenerateShipName test requirement violated");
      end;
      declare
         Test_GenerateShipName_26fd48_7313c0_Result: constant Unbounded_String :=
           GNATtest_Generated.GNATtest_Standard.Ships.GenerateShipName(Owner);
      begin
         begin
            pragma Assert(True);
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
   procedure Test_GenerateShipName_test_generateshipname
     (Gnattest_T: in out Test);
   procedure Test_GenerateShipName_26fd48_7313c0
     (Gnattest_T: in out Test) renames
     Test_GenerateShipName_test_generateshipname;
--  id:2.2/26fd4844628732d9/GenerateShipName/1/0/test_generateshipname/
   procedure Test_GenerateShipName_test_generateshipname
     (Gnattest_T: in out Test) is
      function GenerateShipName
        (Owner: Unbounded_String) return Unbounded_String renames
        Wrap_Test_GenerateShipName_26fd48_7313c0;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Assert
        (GenerateShipName(To_Unbounded_String("POLEIS")) /=
         Null_Unbounded_String,
         "Failed to generate ship name.");

--  begin read only
   end Test_GenerateShipName_test_generateshipname;
--  end read only

--  begin read only
   function Wrap_Test_CountCombatValue_77d5b0_424a30 return Natural is
   begin
      declare
         Test_CountCombatValue_77d5b0_424a30_Result: constant Natural :=
           GNATtest_Generated.GNATtest_Standard.Ships.CountCombatValue;
      begin
         return Test_CountCombatValue_77d5b0_424a30_Result;
      end;
   end Wrap_Test_CountCombatValue_77d5b0_424a30;
--  end read only

--  begin read only
   procedure Test_CountCombatValue_test_countcombatvalue
     (Gnattest_T: in out Test);
   procedure Test_CountCombatValue_77d5b0_424a30
     (Gnattest_T: in out Test) renames
     Test_CountCombatValue_test_countcombatvalue;
--  id:2.2/77d5b05ac9b3095d/CountCombatValue/1/0/test_countcombatvalue/
   procedure Test_CountCombatValue_test_countcombatvalue
     (Gnattest_T: in out Test) is
      function CountCombatValue return Natural renames
        Wrap_Test_CountCombatValue_77d5b0_424a30;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Assert(CountCombatValue > -1, "Failed to get player ship combat value.");

--  begin read only
   end Test_CountCombatValue_test_countcombatvalue;
--  end read only

--  begin read only
   function Wrap_Test_GetCabinQuality_33b05d_0027a0
     (Quality: Natural) return String is
   begin
      declare
         Test_GetCabinQuality_33b05d_0027a0_Result: constant String :=
           GNATtest_Generated.GNATtest_Standard.Ships.GetCabinQuality(Quality);
      begin
         return Test_GetCabinQuality_33b05d_0027a0_Result;
      end;
   end Wrap_Test_GetCabinQuality_33b05d_0027a0;
--  end read only

--  begin read only
   procedure Test_GetCabinQuality_test_getcabinquality
     (Gnattest_T: in out Test);
   procedure Test_GetCabinQuality_33b05d_0027a0
     (Gnattest_T: in out Test) renames
     Test_GetCabinQuality_test_getcabinquality;
--  id:2.2/33b05d3651ff8168/GetCabinQuality/1/0/test_getcabinquality/
   procedure Test_GetCabinQuality_test_getcabinquality
     (Gnattest_T: in out Test) is
      function GetCabinQuality(Quality: Natural) return String renames
        Wrap_Test_GetCabinQuality_33b05d_0027a0;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Assert
        (GetCabinQuality(10) = "Empty room",
         "Failed to get quality of cabin.");

--  begin read only
   end Test_GetCabinQuality_test_getcabinquality;
--  end read only

--  begin read only
   procedure Wrap_Test_DamageModule_225e80_8bb198
     (Ship: in out Ship_Record; ModuleIndex: Modules_Container.Extended_Index;
      Damage: Positive; DeathReason: String) is
   begin
      begin
         pragma Assert
           (ModuleIndex in
              Ship.Modules.First_Index .. Ship.Modules.Last_Index);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(ships.ads:0):Test_DamageModule test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Ships.DamageModule
        (Ship, ModuleIndex, Damage, DeathReason);
      begin
         pragma Assert(True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(ships.ads:0:):Test_DamageModule test commitment violated");
      end;
   end Wrap_Test_DamageModule_225e80_8bb198;
--  end read only

--  begin read only
   procedure Test_DamageModule_test_damagemodule(Gnattest_T: in out Test);
   procedure Test_DamageModule_225e80_8bb198(Gnattest_T: in out Test) renames
     Test_DamageModule_test_damagemodule;
--  id:2.2/225e8074e29bed62/DamageModule/1/0/test_damagemodule/
   procedure Test_DamageModule_test_damagemodule(Gnattest_T: in out Test) is
      procedure DamageModule
        (Ship: in out Ship_Record;
         ModuleIndex: Modules_Container.Extended_Index; Damage: Positive;
         DeathReason: String) renames
        Wrap_Test_DamageModule_225e80_8bb198;
--  end read only

      pragma Unreferenced(Gnattest_T);
      OldDurability: constant Positive := PlayerShip.Modules(1).Durability;

   begin

      DamageModule(PlayerShip, 1, 10, "during tests");
      AUnit.Assertions.Assert
        (PlayerShip.Modules(1).Durability + 10 = OldDurability,
         "Failed to damage player ship hull.");
      PlayerShip.Modules(1).Durability := OldDurability;

--  begin read only
   end Test_DamageModule_test_damagemodule;
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
