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
   function Wrap_Test_Get_Item_Damage_5414f2_efab16
     (Item_Durability: Items_Durability; To_Lower: Boolean := False)
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
         Test_Get_Item_Damage_5414f2_efab16_Result: constant String :=
           GNATtest_Generated.GNATtest_Standard.Items.Get_Item_Damage
             (Item_Durability, To_Lower);
      begin
         begin
            pragma Assert
              (Test_Get_Item_Damage_5414f2_efab16_Result'Length > 0);
            null;
         exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "ens_sloc(items.ads:0:):Test_GetItemDamage test commitment violated");
         end;
         return Test_Get_Item_Damage_5414f2_efab16_Result;
      end;
   end Wrap_Test_Get_Item_Damage_5414f2_efab16;
--  end read only

--  begin read only
   procedure Test_Get_Item_Damage_test_getitemdamage(Gnattest_T: in out Test);
   procedure Test_Get_Item_Damage_5414f2_efab16
     (Gnattest_T: in out Test) renames
     Test_Get_Item_Damage_test_getitemdamage;
--  id:2.2/5414f21fe1a6d3bf/Get_Item_Damage/1/0/test_getitemdamage/
   procedure Test_Get_Item_Damage_test_getitemdamage
     (Gnattest_T: in out Test) is
      function Get_Item_Damage
        (Item_Durability: Items_Durability; To_Lower: Boolean := False)
         return String renames
        Wrap_Test_Get_Item_Damage_5414f2_efab16;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Assert
        (Get_Item_Damage(60) = "Damaged",
         "Returned wrong description for item durability.");
      Assert
        (Get_Item_Damage(60, True) = "damaged",
         "Not lowered description for item durability.");

--  begin read only
   end Test_Get_Item_Damage_test_getitemdamage;
--  end read only

--  begin read only
   function Wrap_Test_Get_Item_Name_1f8ef2_6e935f
     (Item: Inventory_Data; Damage_Info, To_Lower: Boolean := True)
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
         Test_Get_Item_Name_1f8ef2_6e935f_Result: constant String :=
           GNATtest_Generated.GNATtest_Standard.Items.Get_Item_Name
             (Item, Damage_Info, To_Lower);
      begin
         begin
            pragma Assert(Test_Get_Item_Name_1f8ef2_6e935f_Result'Length > 0);
            null;
         exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "ens_sloc(items.ads:0:):Test_GetItemName test commitment violated");
         end;
         return Test_Get_Item_Name_1f8ef2_6e935f_Result;
      end;
   end Wrap_Test_Get_Item_Name_1f8ef2_6e935f;
--  end read only

--  begin read only
   procedure Test_Get_Item_Name_test_getitemname(Gnattest_T: in out Test);
   procedure Test_Get_Item_Name_1f8ef2_6e935f(Gnattest_T: in out Test) renames
     Test_Get_Item_Name_test_getitemname;
--  id:2.2/1f8ef2f02c8d91c7/Get_Item_Name/1/0/test_getitemname/
   procedure Test_Get_Item_Name_test_getitemname(Gnattest_T: in out Test) is
      function Get_Item_Name
        (Item: Inventory_Data; Damage_Info, To_Lower: Boolean := True)
         return String renames
        Wrap_Test_Get_Item_Name_1f8ef2_6e935f;
--  end read only

      pragma Unreferenced(Gnattest_T);
      use Tiny_String;
      Item: Inventory_Data :=
        (Proto_Index => To_Bounded_String("2"), Amount => 1,
         Name => Null_Bounded_String, Durability => 80, Price => 0);

   begin

      Assert
        (Get_Item_Name(Item) = "Basic Ration (slightly used)",
         "Invalid item name with lowered damage info.");
      Assert
        (Get_Item_Name(Item, False) = "Basic Ration", "Invalid item name.");
      Assert
        (Get_Item_Name(Item, True, False) = "Basic Ration (Slightly used)",
         "Invalid item name with damage info.");
      Item.Name := To_Bounded_String("New name");
      Assert
        (Get_Item_Name(Item, False) = "New name",
         "Invalid item name with local name.");

--  begin read only
   end Test_Get_Item_Name_test_getitemname;
--  end read only

--  begin read only
   procedure Wrap_Test_Damage_Item_397a3e_95e715
     (Inventory: in out Inventory_Container.Vector; Item_Index: Positive;
      Skill_Level, Member_Index: Natural := 0;
      Ship: in out Ships.Ship_Record) is
   begin
      begin
         pragma Assert((Item_Index <= Inventory.Last_Index));
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(items.ads:0):Test_DamageItem test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Items.Damage_Item
        (Inventory, Item_Index, Skill_Level, Member_Index, Ship);
      begin
         pragma Assert(True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(items.ads:0:):Test_DamageItem test commitment violated");
      end;
   end Wrap_Test_Damage_Item_397a3e_95e715;
--  end read only

--  begin read only
   procedure Test_Damage_Item_test_damageitem(Gnattest_T: in out Test);
   procedure Test_Damage_Item_397a3e_95e715(Gnattest_T: in out Test) renames
     Test_Damage_Item_test_damageitem;
--  id:2.2/397a3ea71ff6505b/Damage_Item/1/0/test_damageitem/
   procedure Test_Damage_Item_test_damageitem(Gnattest_T: in out Test) is
      procedure Damage_Item
        (Inventory: in out Inventory_Container.Vector; Item_Index: Positive;
         Skill_Level, Member_Index: Natural := 0;
         Ship: in out Ships.Ship_Record) renames
        Wrap_Test_Damage_Item_397a3e_95e715;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      for I in 1 .. 100 loop
         Damage_Item(Player_Ship.Crew(1).Inventory, 1, Ship => Player_Ship);
      end loop;
      Assert(True, "This test can only crash.");

--  begin read only
   end Test_Damage_Item_test_damageitem;
--  end read only

--  begin read only
   function Wrap_Test_Find_Item_18d79b_2d33f4
     (Inventory: Inventory_Container.Vector;
      Proto_Index: Tiny_String.Bounded_String :=
        Tiny_String.Null_Bounded_String;
      Item_Type: Unbounded_String := Null_Unbounded_String;
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
         Test_Find_Item_18d79b_2d33f4_Result: constant Natural :=
           GNATtest_Generated.GNATtest_Standard.Items.Find_Item
             (Inventory, Proto_Index, Item_Type, Durability, Quality);
      begin
         begin
            pragma Assert
              (Test_Find_Item_18d79b_2d33f4_Result <= Inventory.Last_Index);
            null;
         exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "ens_sloc(items.ads:0:):Test_FindItem test commitment violated");
         end;
         return Test_Find_Item_18d79b_2d33f4_Result;
      end;
   end Wrap_Test_Find_Item_18d79b_2d33f4;
--  end read only

--  begin read only
   procedure Test_Find_Item_test_finditem(Gnattest_T: in out Test);
   procedure Test_Find_Item_18d79b_2d33f4(Gnattest_T: in out Test) renames
     Test_Find_Item_test_finditem;
--  id:2.2/18d79b268175855a/Find_Item/1/0/test_finditem/
   procedure Test_Find_Item_test_finditem(Gnattest_T: in out Test) is
      function Find_Item
        (Inventory: Inventory_Container.Vector;
         Proto_Index: Tiny_String.Bounded_String :=
           Tiny_String.Null_Bounded_String;
         Item_Type: Unbounded_String := Null_Unbounded_String;
         Durability: Items_Durability := Items_Durability'Last;
         Quality: Positive := 100) return Natural renames
        Wrap_Test_Find_Item_18d79b_2d33f4;
--  end read only

      pragma Unreferenced(Gnattest_T);
      use Tiny_String;

   begin

      Assert
        (Find_Item(Player_Ship.Crew(1).Inventory, To_Bounded_String("67")) = 2,
         "Can't find item with ProtoIndex.");
      Assert
        (Find_Item
           (Inventory => Player_Ship.Crew(1).Inventory,
            Item_Type => To_Unbounded_String("Weapon")) =
         1,
         "Can't find item wiht ItemType.");
      Assert
        (Find_Item
           (Player_Ship.Crew(1).Inventory, To_Bounded_String("tsdfsdf")) =
         0,
         "Item with not existing ProtoIndex found.");
      Assert
        (Find_Item
           (Inventory => Player_Ship.Crew(1).Inventory,
            Item_Type => To_Unbounded_String("sdfsdfds")) =
         0,
         "Item with non existing ItemType found.");

--  begin read only
   end Test_Find_Item_test_finditem;
--  end read only

--  begin read only
   function Wrap_Test_Get_Item_Chance_To_Damage_32e0c6_2bbc61
     (Item_Data: Natural) return String is
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
         Test_Get_Item_Chance_To_Damage_32e0c6_2bbc61_Result: constant String :=
           GNATtest_Generated.GNATtest_Standard.Items.Get_Item_Chance_To_Damage
             (Item_Data);
      begin
         begin
            pragma Assert
              (Test_Get_Item_Chance_To_Damage_32e0c6_2bbc61_Result'Length > 0);
            null;
         exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "ens_sloc(items.ads:0:):Test_GetItemChanceToDamage test commitment violated");
         end;
         return Test_Get_Item_Chance_To_Damage_32e0c6_2bbc61_Result;
      end;
   end Wrap_Test_Get_Item_Chance_To_Damage_32e0c6_2bbc61;
--  end read only

--  begin read only
   procedure Test_Get_Item_Chance_To_Damage_test_getitemchancetodamage
     (Gnattest_T: in out Test);
   procedure Test_Get_Item_Chance_To_Damage_32e0c6_2bbc61
     (Gnattest_T: in out Test) renames
     Test_Get_Item_Chance_To_Damage_test_getitemchancetodamage;
--  id:2.2/32e0c65d47fa071c/Get_Item_Chance_To_Damage/1/0/test_getitemchancetodamage/
   procedure Test_Get_Item_Chance_To_Damage_test_getitemchancetodamage
     (Gnattest_T: in out Test) is
      function Get_Item_Chance_To_Damage
        (Item_Data: Natural) return String renames
        Wrap_Test_Get_Item_Chance_To_Damage_32e0c6_2bbc61;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Game_Settings.Show_Numbers := False;
      Assert
        (Get_Item_Chance_To_Damage(3) = "Small",
         "Wrong value returned for 3.");
      Assert
        (Get_Item_Chance_To_Damage(30) = "Very high",
         "Wrong value returned for 30.");
      Game_Settings.Show_Numbers := True;
      Assert
        (Get_Item_Chance_To_Damage(3) = " 3%",
         "Wrong value returned for 3 (numeric).");
      Assert
        (Get_Item_Chance_To_Damage(30) = " 30%",
         "Wrong value returned for 30 (numeric).");

--  begin read only
   end Test_Get_Item_Chance_To_Damage_test_getitemchancetodamage;
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
