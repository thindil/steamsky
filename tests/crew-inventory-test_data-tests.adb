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
   function Wrap_Test_FreeInventory_df8fe5_59014f (MemberIndex: Positive; Amount: Integer)  return Integer
   is
   begin
      begin
         pragma Assert
           (MemberIndex <= PlayerShip.Crew.Last_Index);
         null;
      exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "req_sloc(crew-inventory.ads:0):Test_FreeInventory test requirement violated");
      end;
      declare
         Test_FreeInventory_df8fe5_59014f_Result : constant Integer := GNATtest_Generated.GNATtest_Standard.Crew.Inventory.FreeInventory (MemberIndex, Amount);
      begin
         begin
            pragma Assert
              (True);
            null;
         exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "ens_sloc(crew-inventory.ads:0:):Test_FreeInventory test commitment violated");
         end;
         return Test_FreeInventory_df8fe5_59014f_Result;
      end;
   end Wrap_Test_FreeInventory_df8fe5_59014f;
--  end read only

--  begin read only
   procedure Test_FreeInventory_test_freeinventory (Gnattest_T : in out Test);
   procedure Test_FreeInventory_df8fe5_59014f (Gnattest_T : in out Test) renames Test_FreeInventory_test_freeinventory;
--  id:2.2/df8fe5d066a1fde9/FreeInventory/1/0/test_freeinventory/
   procedure Test_FreeInventory_test_freeinventory (Gnattest_T : in out Test) is
      function FreeInventory (MemberIndex: Positive; Amount: Integer) return Integer renames Wrap_Test_FreeInventory_df8fe5_59014f;
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      if FreeInventory(1, 0) /= 0 then
         Assert(True, "This test can only crash.");
      end if;

--  begin read only
   end Test_FreeInventory_test_freeinventory;
--  end read only

--  begin read only
   procedure Wrap_Test_TakeOffItem_a8b09e_c7fb64 (MemberIndex, ItemIndex: Positive) 
   is
   begin
      begin
         pragma Assert
           ((MemberIndex <= PlayerShip.Crew.Last_Index and ItemIndex <= PlayerShip.Crew(MemberIndex).Inventory.Last_Index));
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(crew-inventory.ads:0):Test_TakeOffItem test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Crew.Inventory.TakeOffItem (MemberIndex, ItemIndex);
      begin
         pragma Assert
           (True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(crew-inventory.ads:0:):Test_TakeOffItem test commitment violated");
      end;
   end Wrap_Test_TakeOffItem_a8b09e_c7fb64;
--  end read only

--  begin read only
   procedure Test_TakeOffItem_test_takeoffitem (Gnattest_T : in out Test);
   procedure Test_TakeOffItem_a8b09e_c7fb64 (Gnattest_T : in out Test) renames Test_TakeOffItem_test_takeoffitem;
--  id:2.2/a8b09e84477e626f/TakeOffItem/1/0/test_takeoffitem/
   procedure Test_TakeOffItem_test_takeoffitem (Gnattest_T : in out Test) is
   procedure TakeOffItem (MemberIndex, ItemIndex: Positive) renames Wrap_Test_TakeOffItem_a8b09e_c7fb64;
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      TakeOffItem(1, 1);
      Assert(not ItemIsUsed(1, 1), "Failed to take off item from the player character.");

--  begin read only
   end Test_TakeOffItem_test_takeoffitem;
--  end read only

--  begin read only
   function Wrap_Test_ItemIsUsed_9a8ce5_fa2743 (MemberIndex, ItemIndex: Positive)  return Boolean
   is
   begin
      begin
         pragma Assert
           ((MemberIndex <= PlayerShip.Crew.Last_Index and ItemIndex <= PlayerShip.Crew(MemberIndex).Inventory.Last_Index));
         null;
      exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "req_sloc(crew-inventory.ads:0):Test_ItemIsUsed test requirement violated");
      end;
      declare
         Test_ItemIsUsed_9a8ce5_fa2743_Result : constant Boolean := GNATtest_Generated.GNATtest_Standard.Crew.Inventory.ItemIsUsed (MemberIndex, ItemIndex);
      begin
         begin
            pragma Assert
              (True);
            null;
         exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "ens_sloc(crew-inventory.ads:0:):Test_ItemIsUsed test commitment violated");
         end;
         return Test_ItemIsUsed_9a8ce5_fa2743_Result;
      end;
   end Wrap_Test_ItemIsUsed_9a8ce5_fa2743;
--  end read only

--  begin read only
   procedure Test_ItemIsUsed_test_itemisused (Gnattest_T : in out Test);
   procedure Test_ItemIsUsed_9a8ce5_fa2743 (Gnattest_T : in out Test) renames Test_ItemIsUsed_test_itemisused;
--  id:2.2/9a8ce5527fb6a663/ItemIsUsed/1/0/test_itemisused/
   procedure Test_ItemIsUsed_test_itemisused (Gnattest_T : in out Test) is
      function ItemIsUsed (MemberIndex, ItemIndex: Positive) return Boolean renames Wrap_Test_ItemIsUsed_9a8ce5_fa2743;
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      Assert(ItemIsUsed(1, 1) = False, "Failed to detect that item is not used.");
      Assert(ItemIsUsed(1, 2) = True, "Failed to detect that item is used.");

--  begin read only
   end Test_ItemIsUsed_test_itemisused;
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
