--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into BasesTypes.Test_Data.

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
package body BasesTypes.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only
--  begin read only
   function Wrap_Test_Is_Buyable_e30787_0acf10 (BaseType, ItemIndex: Unbounded_String; CheckFlag: Boolean := True)  return Boolean
   is
   begin
      begin
         pragma Assert
           (BasesTypes_List.Contains(BaseType) and Items_List.Contains(ItemIndex));
         null;
      exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "req_sloc(basestypes.ads:0):Test_Is_Buyable test requirement violated");
      end;
      declare
         Test_Is_Buyable_e30787_0acf10_Result : constant Boolean := GNATtest_Generated.GNATtest_Standard.BasesTypes.Is_Buyable (BaseType, ItemIndex, CheckFlag);
      begin
         begin
            pragma Assert
              (True);
            null;
         exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "ens_sloc(basestypes.ads:0:):Test_Is_Buyable test commitment violated");
         end;
         return Test_Is_Buyable_e30787_0acf10_Result;
      end;
   end Wrap_Test_Is_Buyable_e30787_0acf10;
--  end read only

--  begin read only
   procedure Test_Is_Buyable_test_is_buyable (Gnattest_T : in out Test);
   procedure Test_Is_Buyable_e30787_0acf10 (Gnattest_T : in out Test) renames Test_Is_Buyable_test_is_buyable;
--  id:2.2/e30787f515c5282e/Is_Buyable/1/0/test_is_buyable/
   procedure Test_Is_Buyable_test_is_buyable (Gnattest_T : in out Test) is
      function Is_Buyable (BaseType, ItemIndex: Unbounded_String; CheckFlag: Boolean := True) return Boolean renames Wrap_Test_Is_Buyable_e30787_0acf10;
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      Assert(Is_Buyable(To_Unbounded_String("0"), To_Unbounded_String("1")) = False, "Failed to check if item is not buyable in base.");
      Assert(Is_Buyable(To_Unbounded_String("1"), To_Unbounded_String("2")), "Failed to check if item is buyable in base.");

--  begin read only
   end Test_Is_Buyable_test_is_buyable;
--  end read only

--  begin read only
   function Wrap_Test_Get_Price_58bb07_522dbd (BaseType, ItemIndex: Unbounded_String)  return Natural
   is
   begin
      begin
         pragma Assert
           (BasesTypes_List.Contains(BaseType) and Items_List.Contains(ItemIndex));
         null;
      exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "req_sloc(basestypes.ads:0):Test_Get_Price test requirement violated");
      end;
      declare
         Test_Get_Price_58bb07_522dbd_Result : constant Natural := GNATtest_Generated.GNATtest_Standard.BasesTypes.Get_Price (BaseType, ItemIndex);
      begin
         begin
            pragma Assert
              (True);
            null;
         exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "ens_sloc(basestypes.ads:0:):Test_Get_Price test commitment violated");
         end;
         return Test_Get_Price_58bb07_522dbd_Result;
      end;
   end Wrap_Test_Get_Price_58bb07_522dbd;
--  end read only

--  begin read only
   procedure Test_Get_Price_test_get_price (Gnattest_T : in out Test);
   procedure Test_Get_Price_58bb07_522dbd (Gnattest_T : in out Test) renames Test_Get_Price_test_get_price;
--  id:2.2/58bb076ead9f93c1/Get_Price/1/0/test_get_price/
   procedure Test_Get_Price_test_get_price (Gnattest_T : in out Test) is
      function Get_Price (BaseType, ItemIndex: Unbounded_String) return Natural renames Wrap_Test_Get_Price_58bb07_522dbd;
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      Assert(Get_Price(To_Unbounded_String("0"), To_Unbounded_String("1")) = 0, "Failed to get price of not buyable item.");
      Assert(Get_Price(To_Unbounded_String("1"), To_Unbounded_String("2")) > 0, "Failed to get price of buyable item.");

--  begin read only
   end Test_Get_Price_test_get_price;
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
end BasesTypes.Test_Data.Tests;
