--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Ships.Cargo.Test_Data.

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
package body Ships.Cargo.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only
--  begin read only
   function Wrap_Test_Get_Item_Amount_a81592_e03f36
     (Item_Type: Tiny_String.Bounded_String) return Natural is
   begin
      begin
         pragma Assert(Tiny_String.Length(Source => Item_Type) > 0);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(ships-cargo.ads:0):Test_GetItemAmount test requirement violated");
      end;
      declare
         Test_Get_Item_Amount_a81592_e03f36_Result: constant Natural :=
           GNATtest_Generated.GNATtest_Standard.Ships.Cargo.Get_Item_Amount
             (Item_Type);
      begin
         begin
            pragma Assert(True);
            null;
         exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "ens_sloc(ships-cargo.ads:0:):Test_GetItemAmount test commitment violated");
         end;
         return Test_Get_Item_Amount_a81592_e03f36_Result;
      end;
   end Wrap_Test_Get_Item_Amount_a81592_e03f36;
--  end read only

--  begin read only
   procedure Test_Get_Item_Amount_test_getitemamount(Gnattest_T: in out Test);
   procedure Test_Get_Item_Amount_a81592_e03f36
     (Gnattest_T: in out Test) renames
     Test_Get_Item_Amount_test_getitemamount;
--  id:2.2/a815924becf7138f/Get_Item_Amount/1/0/test_getitemamount/
   procedure Test_Get_Item_Amount_test_getitemamount
     (Gnattest_T: in out Test) is
      function Get_Item_Amount
        (Item_Type: Tiny_String.Bounded_String) return Natural renames
        Wrap_Test_Get_Item_Amount_a81592_e03f36;
--  end read only

      pragma Unreferenced(Gnattest_T);
      use Tiny_String;

      Money: Inventory_Data :=
        Inventory_Container.Element
          (Container => Player_Ship.Cargo, Index => 1);

   begin

      Money.Amount := 2_000;
      Inventory_Container.Replace_Element
        (Container => Player_Ship.Cargo, Index => 1, New_Item => Money);
      Assert
        (Get_Item_Amount(To_Bounded_String("Fuel")) = 2_000,
         "Failed to get proper amount of item.");

--  begin read only
   end Test_Get_Item_Amount_test_getitemamount;
--  end read only

--  begin read only
   function Wrap_Test_Get_Items_Amount_da377d_75c143
     (I_Type: String) return Natural is
   begin
      begin
         pragma Assert(I_Type in "Drinks" | "Food");
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(ships-cargo.ads:0):Test_GetItemsAmount test requirement violated");
      end;
      declare
         Test_Get_Items_Amount_da377d_75c143_Result: constant Natural :=
           GNATtest_Generated.GNATtest_Standard.Ships.Cargo.Get_Items_Amount
             (I_Type);
      begin
         begin
            pragma Assert(True);
            null;
         exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "ens_sloc(ships-cargo.ads:0:):Test_GetItemsAmount test commitment violated");
         end;
         return Test_Get_Items_Amount_da377d_75c143_Result;
      end;
   end Wrap_Test_Get_Items_Amount_da377d_75c143;
--  end read only

--  begin read only
   procedure Test_Get_Items_Amount_test_getitemsamount
     (Gnattest_T: in out Test);
   procedure Test_Get_Items_Amount_da377d_75c143
     (Gnattest_T: in out Test) renames
     Test_Get_Items_Amount_test_getitemsamount;
--  id:2.2/da377d3cb87d421d/Get_Items_Amount/1/0/test_getitemsamount/
   procedure Test_Get_Items_Amount_test_getitemsamount
     (Gnattest_T: in out Test) is
      function Get_Items_Amount(I_Type: String) return Natural renames
        Wrap_Test_Get_Items_Amount_da377d_75c143;
--  end read only

      pragma Unreferenced(Gnattest_T);
      Drinks: Inventory_Data :=
        Inventory_Container.Element
          (Container => Player_Ship.Cargo, Index => 3);

   begin

      for Member of Player_Ship.Crew loop
         Member.Faction := Tiny_String.To_Bounded_String("POLEIS");
      end loop;
      Drinks.Amount := 200;
      Inventory_Container.Replace_Element
        (Container => Player_Ship.Cargo, Index => 3, New_Item => Drinks);
      Assert
        (Get_Items_Amount("Drinks") = 200, "Failed to get amount of drinks.");

--  begin read only
   end Test_Get_Items_Amount_test_getitemsamount;
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
end Ships.Cargo.Test_Data.Tests;
