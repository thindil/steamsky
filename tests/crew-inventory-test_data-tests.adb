--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Crew.Inventory.Test_Data.

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
package body Crew.Inventory.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only
--  begin read only
   procedure Wrap_Test_UpdateInventory_e20eca_ea0095 (MemberIndex: Positive; Amount: Integer; ProtoIndex: Unbounded_String := Null_Unbounded_String; Durability, InventoryIndex, Price: Natural := 0) 
   is
   begin
      begin
         pragma Assert
           ((MemberIndex <= PlayerShip.Crew.Last_Index and InventoryIndex <= PlayerShip.Crew(MemberIndex).Inventory.Last_Index));
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(crew-inventory.ads:0):Test_UpdateInventory test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Crew.Inventory.UpdateInventory (MemberIndex, Amount, ProtoIndex, Durability, InventoryIndex, Price);
      begin
         pragma Assert
           (True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(crew-inventory.ads:0:):Test_UpdateInventory test commitment violated");
      end;
   end Wrap_Test_UpdateInventory_e20eca_ea0095;
--  end read only

--  begin read only
   procedure Test_UpdateInventory_test_updateinventory (Gnattest_T : in out Test);
   procedure Test_UpdateInventory_e20eca_ea0095 (Gnattest_T : in out Test) renames Test_UpdateInventory_test_updateinventory;
--  id:2.2/e20eca86c3534ae1/UpdateInventory/1/0/test_updateinventory/
   procedure Test_UpdateInventory_test_updateinventory (Gnattest_T : in out Test) is
   procedure UpdateInventory (MemberIndex: Positive; Amount: Integer; ProtoIndex: Unbounded_String := Null_Unbounded_String; Durability, InventoryIndex, Price: Natural := 0) renames Wrap_Test_UpdateInventory_e20eca_ea0095;
--  end read only

      pragma Unreferenced (Gnattest_T);
      Amount: constant Positive := Positive(PlayerShip.Crew(1).Inventory.Length);

   begin

      UpdateInventory(1, 1, To_Unbounded_String("1"));
      Assert(Positive(PlayerShip.Crew(1).Inventory.Length) = Amount + 1, "Failed to add item to crew member inventory.");
      UpdateInventory(1, -1, To_Unbounded_String("1"));
      Assert(Positive(PlayerShip.Crew(1).Inventory.Length) = Amount, "Failed to remove item from crew member inventory.");

--  begin read only
   end Test_UpdateInventory_test_updateinventory;
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
end Crew.Inventory.Test_Data.Tests;
