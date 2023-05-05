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
   procedure Wrap_Test_Update_Base_Cargo_a5e235_1e1787
     (Proto_Index: Natural := 0; Amount: Integer;
      Durability: Items_Durability := Default_Item_Durability;
      Cargo_Index: Inventory_Container.Extended_Index := 0) is
   begin
      GNATtest_Generated.GNATtest_Standard.Bases.Cargo.Update_Base_Cargo
        (Proto_Index, Amount, Durability, Cargo_Index);
   end Wrap_Test_Update_Base_Cargo_a5e235_1e1787;
--  end read only

--  begin read only
   procedure Test_Update_Base_Cargo_test_updatebasecargo
     (Gnattest_T: in out Test);
   procedure Test_Update_Base_Cargo_a5e235_1e1787
     (Gnattest_T: in out Test) renames
     Test_Update_Base_Cargo_test_updatebasecargo;
--  id:2.2/a5e23599823e81c0/Update_Base_Cargo/1/0/test_updatebasecargo/
   procedure Test_Update_Base_Cargo_test_updatebasecargo
     (Gnattest_T: in out Test) is
      procedure Update_Base_Cargo
        (Proto_Index: Natural := 0; Amount: Integer;
         Durability: Items_Durability := Default_Item_Durability;
         Cargo_Index: Inventory_Container.Extended_Index := 0) renames
        Wrap_Test_Update_Base_Cargo_a5e235_1e1787;
--  end read only

      pragma Unreferenced(Gnattest_T);
      BaseIndex: constant Positive :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      Amount: Positive;
      ProtoIndex: Natural;

   begin

      BaseCargo_Container.Clear(Container => Sky_Bases(BaseIndex).Cargo);
      Generate_Cargo;
      Amount :=
        BaseCargo_Container.Element(Sky_Bases(BaseIndex).Cargo, 1).Amount - 1;
      ProtoIndex :=
        BaseCargo_Container.Element(Sky_Bases(BaseIndex).Cargo, 1).Proto_Index;
      Update_Base_Cargo(ProtoIndex, -1);
      Assert
        (BaseCargo_Container.Element(Sky_Bases(BaseIndex).Cargo, 1).Amount =
         Amount,
         "Failed to update base cargo with proto index.");
      Update_Base_Cargo(Cargo_Index => 1, Amount => -1);
      Assert
        (BaseCargo_Container.Element(Sky_Bases(BaseIndex).Cargo, 1).Amount =
         Amount - 1,
         "Failed to update base cargo with cargo index.");

--  begin read only
   end Test_Update_Base_Cargo_test_updatebasecargo;
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
