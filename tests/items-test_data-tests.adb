--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Items.Test_Data.

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
package body Items.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only
--  begin read only
   function Wrap_Test_FindProtoItem_f36791_112bba (ItemType: Unbounded_String)  return Unbounded_String
   is
   begin
      begin
         pragma Assert
           ((ItemType /= Null_Unbounded_String));
         null;
      exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "req_sloc(items.ads:0):Test_FindProtoItem test requirement violated");
      end;
      declare
         Test_FindProtoItem_f36791_112bba_Result : constant Unbounded_String := GNATtest_Generated.GNATtest_Standard.Items.FindProtoItem (ItemType);
      begin
         begin
            pragma Assert
              (True);
            null;
         exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "ens_sloc(items.ads:0:):Test_FindProtoItem test commitment violated");
         end;
         return Test_FindProtoItem_f36791_112bba_Result;
      end;
   end Wrap_Test_FindProtoItem_f36791_112bba;
--  end read only

--  begin read only
   procedure Test_FindProtoItem_test_findprotoitem (Gnattest_T : in out Test);
   procedure Test_FindProtoItem_f36791_112bba (Gnattest_T : in out Test) renames Test_FindProtoItem_test_findprotoitem;
--  id:2.2/f36791a587ee5451/FindProtoItem/1/0/test_findprotoitem/
   procedure Test_FindProtoItem_test_findprotoitem (Gnattest_T : in out Test) is
      function FindProtoItem (ItemType: Unbounded_String) return Unbounded_String renames Wrap_Test_FindProtoItem_f36791_112bba;
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      Assert(FindProtoItem(To_Unbounded_String("Iron")) /= Null_Unbounded_String, "Can't find existing item.");
      Assert(FindProtoItem(To_Unbounded_String("sdfsfsdfdsfsd")) = Null_Unbounded_String, "Non existing item should return null string.");

--  begin read only
   end Test_FindProtoItem_test_findprotoitem;
--  end read only

--  begin read only
   function Wrap_Test_GetItemDamage_dedcfa_d584da (ItemDurability: Natural; ToLower: Boolean := False)  return String
   is
   begin
      declare
         Test_GetItemDamage_dedcfa_d584da_Result : constant String := GNATtest_Generated.GNATtest_Standard.Items.GetItemDamage (ItemDurability, ToLower);
      begin
         return Test_GetItemDamage_dedcfa_d584da_Result;
      end;
   end Wrap_Test_GetItemDamage_dedcfa_d584da;
--  end read only

--  begin read only
   procedure Test_GetItemDamage_test_getitemdamage (Gnattest_T : in out Test);
   procedure Test_GetItemDamage_dedcfa_d584da (Gnattest_T : in out Test) renames Test_GetItemDamage_test_getitemdamage;
--  id:2.2/dedcfaf3e24b7100/GetItemDamage/1/0/test_getitemdamage/
   procedure Test_GetItemDamage_test_getitemdamage (Gnattest_T : in out Test) is
      function GetItemDamage (ItemDurability: Natural; ToLower: Boolean := False) return String renames Wrap_Test_GetItemDamage_dedcfa_d584da;
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      Assert(GetItemDamage(60) = "Damaged", "Returned wrong description for item durability.");
      Assert(GetItemDamage(60, True) = "damaged", "Not lowered description for item durability.");

--  begin read only
   end Test_GetItemDamage_test_getitemdamage;
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
end Items.Test_Data.Tests;
