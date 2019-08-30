--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Bases.Cargo.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only

with Maps; use Maps;

--  begin read only
--  end read only
package body Bases.Cargo.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only
--  begin read only
   procedure Wrap_Test_GenerateCargo_1e6cf6_021eea
   is
   begin
      GNATtest_Generated.GNATtest_Standard.Bases.Cargo.GenerateCargo;
   end Wrap_Test_GenerateCargo_1e6cf6_021eea;
--  end read only

--  begin read only
   procedure Test_GenerateCargo_test_generatecargo (Gnattest_T : in out Test);
   procedure Test_GenerateCargo_1e6cf6_021eea (Gnattest_T : in out Test) renames Test_GenerateCargo_test_generatecargo;
--  id:2.2/1e6cf6e4bcd576d4/GenerateCargo/1/0/test_generatecargo/
   procedure Test_GenerateCargo_test_generatecargo (Gnattest_T : in out Test) is
   procedure GenerateCargo renames Wrap_Test_GenerateCargo_1e6cf6_021eea;
--  end read only

      pragma Unreferenced (Gnattest_T);
      BaseIndex: constant Positive :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;

   begin

      SkyBases(BaseIndex).Cargo.Clear;
      GenerateCargo;
      Assert(SkyBases(BaseIndex).Cargo.Length > 0, "Failed to generate base cargo.");

--  begin read only
   end Test_GenerateCargo_test_generatecargo;
--  end read only

--  begin read only
   procedure Wrap_Test_UpdateBaseCargo_8dbba5_1e1787 (ProtoIndex: Unbounded_String := Null_Unbounded_String; Amount: Integer; Durability: Natural := 100; CargoIndex: Natural := 0) 
   is
   begin
      GNATtest_Generated.GNATtest_Standard.Bases.Cargo.UpdateBaseCargo (ProtoIndex, Amount, Durability, CargoIndex);
   end Wrap_Test_UpdateBaseCargo_8dbba5_1e1787;
--  end read only

--  begin read only
   procedure Test_UpdateBaseCargo_test_updatebasecargo (Gnattest_T : in out Test);
   procedure Test_UpdateBaseCargo_8dbba5_1e1787 (Gnattest_T : in out Test) renames Test_UpdateBaseCargo_test_updatebasecargo;
--  id:2.2/8dbba5e239df57ed/UpdateBaseCargo/1/0/test_updatebasecargo/
   procedure Test_UpdateBaseCargo_test_updatebasecargo (Gnattest_T : in out Test) is
   procedure UpdateBaseCargo (ProtoIndex: Unbounded_String := Null_Unbounded_String; Amount: Integer; Durability: Natural := 100; CargoIndex: Natural := 0) renames Wrap_Test_UpdateBaseCargo_8dbba5_1e1787;
--  end read only

      pragma Unreferenced (Gnattest_T);
      BaseIndex: constant Positive :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      Amount: Positive;
      ProtoIndex: Unbounded_String;

   begin

      SkyBases(BaseIndex).Cargo.Clear;
      GenerateCargo;
      Amount := SkyBases(BaseIndex).Cargo(1).Amount - 1;
      ProtoIndex := SkyBases(BaseIndex).Cargo(1).ProtoIndex;
      UpdateBaseCargo(ProtoIndex, -1);
      Assert(SkyBases(BaseIndex).Cargo(1).Amount = Amount, "Failed to update base cargo with proto index.");
      UpdateBaseCargo(CargoIndex => 1, Amount => -1);
      Assert(SkyBases(BaseIndex).Cargo(1).Amount = Amount - 1, "Failed to update base cargo with cargo index.");

--  begin read only
   end Test_UpdateBaseCargo_test_updatebasecargo;
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
end Bases.Cargo.Test_Data.Tests;
