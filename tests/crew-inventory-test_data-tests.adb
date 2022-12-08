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
   procedure Wrap_Test_Update_Inventory_98f7ee_774caf
     (Member_Index: Positive; Amount: Integer;
      Proto_Index: Objects_Container.Extended_Index := 0;
      Durability: Items_Durability := 0; Inventory_Index, Price: Natural := 0;
      Ship: in out Ship_Record) is
   begin
      begin
         pragma Assert
           ((Member_Index <= Ship.Crew.Last_Index and
             Inventory_Index <=
               Inventory_Container.Last_Index
                 (Container => Ship.Crew(Member_Index).Inventory)));
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(crew-inventory.ads:0):Test_UpdateInventory test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Crew.Inventory.Update_Inventory
        (Member_Index, Amount, Proto_Index, Durability, Inventory_Index, Price,
         Ship);
      begin
         pragma Assert(True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(crew-inventory.ads:0:):Test_UpdateInventory test commitment violated");
      end;
   end Wrap_Test_Update_Inventory_98f7ee_774caf;
--  end read only

--  begin read only
   procedure Test_Update_Inventory_test_updateinventory
     (Gnattest_T: in out Test);
   procedure Test_Update_Inventory_98f7ee_774caf
     (Gnattest_T: in out Test) renames
     Test_Update_Inventory_test_updateinventory;
--  id:2.2/98f7ee6f48ca7231/Update_Inventory/1/0/test_updateinventory/
   procedure Test_Update_Inventory_test_updateinventory
     (Gnattest_T: in out Test) is
      procedure Update_Inventory
        (Member_Index: Positive; Amount: Integer;
         Proto_Index: Objects_Container.Extended_Index := 0;
         Durability: Items_Durability := 0;
         Inventory_Index, Price: Natural := 0;
         Ship: in out Ship_Record) renames
        Wrap_Test_Update_Inventory_98f7ee_774caf;
--  end read only

      pragma Unreferenced(Gnattest_T);
      Amount: constant Positive :=
        Positive
          (Inventory_Container.Length
             (Container => Player_Ship.Crew(1).Inventory));

   begin

      Update_Inventory(1, 1, 1, Ship => Player_Ship);
      Assert
        (Positive
           (Inventory_Container.Length
              (Container => Player_Ship.Crew(1).Inventory)) =
         Amount + 1,
         "Failed to add item to crew member inventory.");
      Update_Inventory(1, -1, 1, Ship => Player_Ship);
      Assert
        (Positive
           (Inventory_Container.Length
              (Container => Player_Ship.Crew(1).Inventory)) =
         Amount,
         "Failed to remove item from crew member inventory.");
      begin
         Update_Inventory(1, 10_000, 1, Ship => Player_Ship);
         Assert
           (False,
            "Failed to not add too much items to the crew member inventory.");
      exception
         when Crew_No_Space_Error =>
            null;
         when others =>
            Assert
              (False,
               "Exception when trying to add more items than crew member can take.");
      end;

--  begin read only
   end Test_Update_Inventory_test_updateinventory;
--  end read only

--  begin read only
   procedure Wrap_Test_Take_Off_Item_a8430e_5a9c8c
     (Member_Index, Item_Index: Positive) is
   begin
      begin
         pragma Assert
           ((Member_Index <= Player_Ship.Crew.Last_Index and
             Item_Index <=
               Inventory_Container.Last_Index
                 (Container => Player_Ship.Crew(Member_Index).Inventory)));
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(crew-inventory.ads:0):Test_TakeOffItem test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Crew.Inventory.Take_Off_Item
        (Member_Index, Item_Index);
      begin
         pragma Assert(True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(crew-inventory.ads:0:):Test_TakeOffItem test commitment violated");
      end;
   end Wrap_Test_Take_Off_Item_a8430e_5a9c8c;
--  end read only

--  begin read only
   procedure Test_Take_Off_Item_test_takeoffitem(Gnattest_T: in out Test);
   procedure Test_Take_Off_Item_a8430e_5a9c8c(Gnattest_T: in out Test) renames
     Test_Take_Off_Item_test_takeoffitem;
--  id:2.2/a8430e1854a2013f/Take_Off_Item/1/0/test_takeoffitem/
   procedure Test_Take_Off_Item_test_takeoffitem(Gnattest_T: in out Test) is
      procedure Take_Off_Item(Member_Index, Item_Index: Positive) renames
        Wrap_Test_Take_Off_Item_a8430e_5a9c8c;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Take_Off_Item(1, 1);
      Assert
        (not Item_Is_Used(1, 1),
         "Failed to take off item from the player character.");

--  begin read only
   end Test_Take_Off_Item_test_takeoffitem;
--  end read only

--  begin read only
   function Wrap_Test_Item_Is_Used_ab1e32_1896db
     (Member_Index, Item_Index: Positive) return Boolean is
   begin
      begin
         pragma Assert
           ((Member_Index <= Player_Ship.Crew.Last_Index and
             Item_Index <=
               Inventory_Container.Last_Index
                 (Container => Player_Ship.Crew(Member_Index).Inventory)));
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(crew-inventory.ads:0):Test_ItemIsUsed test requirement violated");
      end;
      declare
         Test_Item_Is_Used_ab1e32_1896db_Result: constant Boolean :=
           GNATtest_Generated.GNATtest_Standard.Crew.Inventory.Item_Is_Used
             (Member_Index, Item_Index);
      begin
         begin
            pragma Assert(True);
            null;
         exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "ens_sloc(crew-inventory.ads:0:):Test_ItemIsUsed test commitment violated");
         end;
         return Test_Item_Is_Used_ab1e32_1896db_Result;
      end;
   end Wrap_Test_Item_Is_Used_ab1e32_1896db;
--  end read only

--  begin read only
   procedure Test_Item_Is_Used_test_itemisused(Gnattest_T: in out Test);
   procedure Test_Item_Is_Used_ab1e32_1896db(Gnattest_T: in out Test) renames
     Test_Item_Is_Used_test_itemisused;
--  id:2.2/ab1e320453a535cb/Item_Is_Used/1/0/test_itemisused/
   procedure Test_Item_Is_Used_test_itemisused(Gnattest_T: in out Test) is
      function Item_Is_Used
        (Member_Index, Item_Index: Positive) return Boolean renames
        Wrap_Test_Item_Is_Used_ab1e32_1896db;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Assert
        (Item_Is_Used(1, 1) = False,
         "Failed to detect that item is not used.");
      Assert(Item_Is_Used(1, 2) = True, "Failed to detect that item is used.");

--  begin read only
   end Test_Item_Is_Used_test_itemisused;
--  end read only

--  begin read only
   function Wrap_Test_Find_Tools_2a8899_b022e2
     (Member_Index: Positive; Item_Type: Tiny_String.Bounded_String;
      Order: Crew_Orders; Tool_Quality: Positive := 100) return Natural is
   begin
      begin
         pragma Assert
           ((Member_Index <= Player_Ship.Crew.Last_Index and
             Tiny_String.Length(Source => Item_Type) > 0));
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(crew-inventory.ads:0):Test_FindTools test requirement violated");
      end;
      declare
         Test_Find_Tools_2a8899_b022e2_Result: constant Natural :=
           GNATtest_Generated.GNATtest_Standard.Crew.Inventory.Find_Tools
             (Member_Index, Item_Type, Order, Tool_Quality);
      begin
         begin
            pragma Assert(True);
            null;
         exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "ens_sloc(crew-inventory.ads:0:):Test_FindTools test commitment violated");
         end;
         return Test_Find_Tools_2a8899_b022e2_Result;
      end;
   end Wrap_Test_Find_Tools_2a8899_b022e2;
--  end read only

--  begin read only
   procedure Test_Find_Tools_test_findtools(Gnattest_T: in out Test);
   procedure Test_Find_Tools_2a8899_b022e2(Gnattest_T: in out Test) renames
     Test_Find_Tools_test_findtools;
--  id:2.2/2a8899f31b749150/Find_Tools/1/0/test_findtools/
   procedure Test_Find_Tools_test_findtools(Gnattest_T: in out Test) is
      function Find_Tools
        (Member_Index: Positive; Item_Type: Tiny_String.Bounded_String;
         Order: Crew_Orders; Tool_Quality: Positive := 100)
         return Natural renames
        Wrap_Test_Find_Tools_2a8899_b022e2;
--  end read only

      pragma Unreferenced(Gnattest_T);
      use Tiny_String;

   begin

      Assert
        (Find_Tools(1, To_Bounded_String("Bucket"), Clean) > 0,
         "Failed to find tools for cleaning.");
      Assert
        (Find_Tools(1, To_Bounded_String("sdfsdfds"), Talk) = 0,
         "Failed to not find non-existing tools.");

--  begin read only
   end Test_Find_Tools_test_findtools;
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
