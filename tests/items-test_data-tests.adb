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

with Config; use Config;
with Ships; use Ships;

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
   function Wrap_Test_Find_Proto_Item_dbaac8_3e4d1a
     (Item_Type: Unbounded_String) return Tiny_String.Bounded_String is
   begin
      begin
         pragma Assert((Item_Type /= Null_Unbounded_String));
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(items.ads:0):Test_FindProtoItem test requirement violated");
      end;
      declare
         Test_Find_Proto_Item_dbaac8_3e4d1a_Result: constant Tiny_String
           .Bounded_String :=
           GNATtest_Generated.GNATtest_Standard.Items.Find_Proto_Item
             (Item_Type);
      begin
         begin
            pragma Assert(True);
            null;
         exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "ens_sloc(items.ads:0:):Test_FindProtoItem test commitment violated");
         end;
         return Test_Find_Proto_Item_dbaac8_3e4d1a_Result;
      end;
   end Wrap_Test_Find_Proto_Item_dbaac8_3e4d1a;
--  end read only

--  begin read only
   procedure Test_Find_Proto_Item_test_findprotoitem(Gnattest_T: in out Test);
   procedure Test_Find_Proto_Item_dbaac8_3e4d1a
     (Gnattest_T: in out Test) renames
     Test_Find_Proto_Item_test_findprotoitem;
--  id:2.2/dbaac83ab19c68a1/Find_Proto_Item/1/0/test_findprotoitem/
   procedure Test_Find_Proto_Item_test_findprotoitem
     (Gnattest_T: in out Test) is
      function Find_Proto_Item
        (Item_Type: Unbounded_String) return Tiny_String.Bounded_String renames
        Wrap_Test_Find_Proto_Item_dbaac8_3e4d1a;
--  end read only

      pragma Unreferenced(Gnattest_T);
      use Tiny_String;

   begin

      Assert
        (Find_Proto_Item(To_Unbounded_String("Iron")) /= Null_Bounded_String,
         "Can't find existing item.");
      Assert
        (Find_Proto_Item(To_Unbounded_String("sdfsfsdfdsfsd")) =
         Null_Bounded_String,
         "Non existing item should return null string.");

--  begin read only
   end Test_Find_Proto_Item_test_findprotoitem;
--  end read only

--  begin read only
   function Wrap_Test_GetItemDamage_dedcfa_216935
     (ItemDurability: Items_Durability; ToLower: Boolean := False)
      return String is
   begin
      begin
         pragma Assert(True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(items.ads:0):Test_GetItemDamage test requirement violated");
      end;
      declare
         Test_GetItemDamage_dedcfa_216935_Result: constant String :=
           GNATtest_Generated.GNATtest_Standard.Items.GetItemDamage
             (ItemDurability, ToLower);
      begin
         begin
            pragma Assert(Test_GetItemDamage_dedcfa_216935_Result'Length > 0);
            null;
         exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "ens_sloc(items.ads:0:):Test_GetItemDamage test commitment violated");
         end;
         return Test_GetItemDamage_dedcfa_216935_Result;
      end;
   end Wrap_Test_GetItemDamage_dedcfa_216935;
--  end read only

--  begin read only
   procedure Test_GetItemDamage_test_getitemdamage(Gnattest_T: in out Test);
   procedure Test_GetItemDamage_dedcfa_216935(Gnattest_T: in out Test) renames
     Test_GetItemDamage_test_getitemdamage;
--  id:2.2/dedcfaf3e24b7100/GetItemDamage/1/0/test_getitemdamage/
   procedure Test_GetItemDamage_test_getitemdamage(Gnattest_T: in out Test) is
      function GetItemDamage
        (ItemDurability: Items_Durability; ToLower: Boolean := False)
         return String renames
        Wrap_Test_GetItemDamage_dedcfa_216935;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Assert
        (GetItemDamage(60) = "Damaged",
         "Returned wrong description for item durability.");
      Assert
        (GetItemDamage(60, True) = "damaged",
         "Not lowered description for item durability.");

--  begin read only
   end Test_GetItemDamage_test_getitemdamage;
--  end read only

--  begin read only
   function Wrap_Test_GetItemName_abedef_611409
     (Item: Inventory_Data; DamageInfo, ToLower: Boolean := True)
      return String is
   begin
      begin
         pragma Assert(True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(items.ads:0):Test_GetItemName test requirement violated");
      end;
      declare
         Test_GetItemName_abedef_611409_Result: constant String :=
           GNATtest_Generated.GNATtest_Standard.Items.GetItemName
             (Item, DamageInfo, ToLower);
      begin
         begin
            pragma Assert(Test_GetItemName_abedef_611409_Result'Length > 0);
            null;
         exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "ens_sloc(items.ads:0:):Test_GetItemName test commitment violated");
         end;
         return Test_GetItemName_abedef_611409_Result;
      end;
   end Wrap_Test_GetItemName_abedef_611409;
--  end read only

--  begin read only
   procedure Test_GetItemName_test_getitemname(Gnattest_T: in out Test);
   procedure Test_GetItemName_abedef_611409(Gnattest_T: in out Test) renames
     Test_GetItemName_test_getitemname;
--  id:2.2/abedef61267c2b82/GetItemName/1/0/test_getitemname/
   procedure Test_GetItemName_test_getitemname(Gnattest_T: in out Test) is
      function GetItemName
        (Item: Inventory_Data; DamageInfo, ToLower: Boolean := True)
         return String renames
        Wrap_Test_GetItemName_abedef_611409;
--  end read only

      pragma Unreferenced(Gnattest_T);
      use Tiny_String;
      Item: Inventory_Data :=
        (Proto_Index => To_Bounded_String("2"), Amount => 1,
         Name => Null_Unbounded_String, Durability => 80, Price => 0);

   begin

      Assert
        (GetItemName(Item) = "Basic Ration (slightly used)",
         "Invalid item name with lowered damage info.");
      Assert(GetItemName(Item, False) = "Basic Ration", "Invalid item name.");
      Assert
        (GetItemName(Item, True, False) = "Basic Ration (Slightly used)",
         "Invalid item name with damage info.");
      Item.Name := To_Unbounded_String("New name");
      Assert
        (GetItemName(Item, False) = "New name",
         "Invalid item name with local name.");

--  begin read only
   end Test_GetItemName_test_getitemname;
--  end read only

--  begin read only
   procedure Wrap_Test_DamageItem_f848d1_f75741
     (Inventory: in out Inventory_Container.Vector; ItemIndex: Positive;
      SkillLevel, MemberIndex: Natural := 0) is
   begin
      begin
         pragma Assert((ItemIndex <= Inventory.Last_Index));
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(items.ads:0):Test_DamageItem test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Items.DamageItem
        (Inventory, ItemIndex, SkillLevel, MemberIndex);
      begin
         pragma Assert(True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(items.ads:0:):Test_DamageItem test commitment violated");
      end;
   end Wrap_Test_DamageItem_f848d1_f75741;
--  end read only

--  begin read only
   procedure Test_DamageItem_test_damageitem(Gnattest_T: in out Test);
   procedure Test_DamageItem_f848d1_f75741(Gnattest_T: in out Test) renames
     Test_DamageItem_test_damageitem;
--  id:2.2/f848d19e08f0418b/DamageItem/1/0/test_damageitem/
   procedure Test_DamageItem_test_damageitem(Gnattest_T: in out Test) is
      procedure DamageItem
        (Inventory: in out Inventory_Container.Vector; ItemIndex: Positive;
         SkillLevel, MemberIndex: Natural := 0) renames
        Wrap_Test_DamageItem_f848d1_f75741;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      for I in 1 .. 100 loop
         DamageItem(Player_Ship.Crew(1).Inventory, 1);
      end loop;
      Assert(True, "This test can only crash.");

--  begin read only
   end Test_DamageItem_test_damageitem;
--  end read only

--  begin read only
   function Wrap_Test_FindItem_88b1dc_73ae4d
     (Inventory: Inventory_Container.Vector;
      ProtoIndex: Tiny_String.Bounded_String :=
        Tiny_String.Null_Bounded_String;
      ItemType: Unbounded_String := Null_Unbounded_String;
      Durability: Items_Durability := Items_Durability'Last;
      Quality: Positive := 100) return Natural is
   begin
      begin
         pragma Assert(True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(items.ads:0):Test_FindItem test requirement violated");
      end;
      declare
         Test_FindItem_88b1dc_73ae4d_Result: constant Natural :=
           GNATtest_Generated.GNATtest_Standard.Items.FindItem
             (Inventory, ProtoIndex, ItemType, Durability, Quality);
      begin
         begin
            pragma Assert
              (Test_FindItem_88b1dc_73ae4d_Result <= Inventory.Last_Index);
            null;
         exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "ens_sloc(items.ads:0:):Test_FindItem test commitment violated");
         end;
         return Test_FindItem_88b1dc_73ae4d_Result;
      end;
   end Wrap_Test_FindItem_88b1dc_73ae4d;
--  end read only

--  begin read only
   procedure Test_FindItem_test_finditem(Gnattest_T: in out Test);
   procedure Test_FindItem_88b1dc_73ae4d(Gnattest_T: in out Test) renames
     Test_FindItem_test_finditem;
--  id:2.2/88b1dc8fe843cf39/FindItem/1/0/test_finditem/
   procedure Test_FindItem_test_finditem(Gnattest_T: in out Test) is
      function FindItem
        (Inventory: Inventory_Container.Vector;
         ProtoIndex: Tiny_String.Bounded_String :=
           Tiny_String.Null_Bounded_String;
         ItemType: Unbounded_String := Null_Unbounded_String;
         Durability: Items_Durability := Items_Durability'Last;
         Quality: Positive := 100) return Natural renames
        Wrap_Test_FindItem_88b1dc_73ae4d;
--  end read only

      pragma Unreferenced(Gnattest_T);
      use Tiny_String;

   begin

      Assert
        (FindItem(Player_Ship.Crew(1).Inventory, To_Bounded_String("67")) = 2,
         "Can't find item with ProtoIndex.");
      Assert
        (FindItem
           (Inventory => Player_Ship.Crew(1).Inventory,
            ItemType => To_Unbounded_String("Weapon")) =
         1,
         "Can't find item wiht ItemType.");
      Assert
        (FindItem
           (Player_Ship.Crew(1).Inventory, To_Bounded_String("tsdfsdf")) =
         0,
         "Item with not existing ProtoIndex found.");
      Assert
        (FindItem
           (Inventory => Player_Ship.Crew(1).Inventory,
            ItemType => To_Unbounded_String("sdfsdfds")) =
         0,
         "Item with non existing ItemType found.");

--  begin read only
   end Test_FindItem_test_finditem;
--  end read only

--  begin read only
   function Wrap_Test_GetItemChanceToDamage_71801d_14d19a
     (ItemData: Natural) return String is
   begin
      begin
         pragma Assert(True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(items.ads:0):Test_GetItemChanceToDamage test requirement violated");
      end;
      declare
         Test_GetItemChanceToDamage_71801d_14d19a_Result: constant String :=
           GNATtest_Generated.GNATtest_Standard.Items.GetItemChanceToDamage
             (ItemData);
      begin
         begin
            pragma Assert
              (Test_GetItemChanceToDamage_71801d_14d19a_Result'Length > 0);
            null;
         exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "ens_sloc(items.ads:0:):Test_GetItemChanceToDamage test commitment violated");
         end;
         return Test_GetItemChanceToDamage_71801d_14d19a_Result;
      end;
   end Wrap_Test_GetItemChanceToDamage_71801d_14d19a;
--  end read only

--  begin read only
   procedure Test_GetItemChanceToDamage_test_getitemchancetodamage
     (Gnattest_T: in out Test);
   procedure Test_GetItemChanceToDamage_71801d_14d19a
     (Gnattest_T: in out Test) renames
     Test_GetItemChanceToDamage_test_getitemchancetodamage;
--  id:2.2/71801da93fac4ec5/GetItemChanceToDamage/1/0/test_getitemchancetodamage/
   procedure Test_GetItemChanceToDamage_test_getitemchancetodamage
     (Gnattest_T: in out Test) is
      function GetItemChanceToDamage(ItemData: Natural) return String renames
        Wrap_Test_GetItemChanceToDamage_71801d_14d19a;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Game_Settings.Show_Numbers := False;
      Assert
        (GetItemChanceToDamage(3) = "Small", "Wrong value returned for 3.");
      Assert
        (GetItemChanceToDamage(30) = "Very high",
         "Wrong value returned for 30.");
      Game_Settings.Show_Numbers := True;
      Assert
        (GetItemChanceToDamage(3) = " 3%",
         "Wrong value returned for 3 (numeric).");
      Assert
        (GetItemChanceToDamage(30) = " 30%",
         "Wrong value returned for 30 (numeric).");

--  begin read only
   end Test_GetItemChanceToDamage_test_getitemchancetodamage;
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
