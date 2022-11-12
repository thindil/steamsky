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
   procedure Wrap_Test_Damage_Item_397a3e_4cae84
     (Inventory: in out Inventory_Container.Vector; Item_Index: Positive;
      Skill_Level, Member_Index: Natural := 0;
      Ship: in out Ships.Ship_Record) is
   begin
      begin
         pragma Assert
           ((Item_Index <=
             Inventory_Container.Last_Index(Container => Inventory)));
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
   end Wrap_Test_Damage_Item_397a3e_4cae84;
--  end read only

--  begin read only
   procedure Test_Damage_Item_test_damageitem(Gnattest_T: in out Test);
   procedure Test_Damage_Item_397a3e_4cae84(Gnattest_T: in out Test) renames
     Test_Damage_Item_test_damageitem;
--  id:2.2/397a3ea71ff6505b/Damage_Item/1/0/test_damageitem/
   procedure Test_Damage_Item_test_damageitem(Gnattest_T: in out Test) is
      procedure Damage_Item
        (Inventory: in out Inventory_Container.Vector; Item_Index: Positive;
         Skill_Level, Member_Index: Natural := 0;
         Ship: in out Ships.Ship_Record) renames
        Wrap_Test_Damage_Item_397a3e_4cae84;
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
   function Wrap_Test_Find_Item_800e7b_9d4eb9
     (Inventory: Inventory_Container.Vector;
      Proto_Index: Objects_Container.Extended_Index := 0;
      Item_Type: Tiny_String.Bounded_String := Tiny_String.Null_Bounded_String;
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
         Test_Find_Item_800e7b_9d4eb9_Result: constant Natural :=
           GNATtest_Generated.GNATtest_Standard.Items.Find_Item
             (Inventory, Proto_Index, Item_Type, Durability, Quality);
      begin
         begin
            pragma Assert
              (Test_Find_Item_800e7b_9d4eb9_Result <=
               Inventory_Container.Last_Index(Container => Inventory));
            null;
         exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "ens_sloc(items.ads:0:):Test_FindItem test commitment violated");
         end;
         return Test_Find_Item_800e7b_9d4eb9_Result;
      end;
   end Wrap_Test_Find_Item_800e7b_9d4eb9;
--  end read only

--  begin read only
   procedure Test_Find_Item_test_finditem(Gnattest_T: in out Test);
   procedure Test_Find_Item_800e7b_9d4eb9(Gnattest_T: in out Test) renames
     Test_Find_Item_test_finditem;
--  id:2.2/800e7b9569f17779/Find_Item/1/0/test_finditem/
   procedure Test_Find_Item_test_finditem(Gnattest_T: in out Test) is
      function Find_Item
        (Inventory: Inventory_Container.Vector;
         Proto_Index: Objects_Container.Extended_Index := 0;
         Item_Type: Tiny_String.Bounded_String :=
           Tiny_String.Null_Bounded_String;
         Durability: Items_Durability := Items_Durability'Last;
         Quality: Positive := 100) return Natural renames
        Wrap_Test_Find_Item_800e7b_9d4eb9;
--  end read only

      pragma Unreferenced(Gnattest_T);
      use Tiny_String;

   begin

      Assert
        (Find_Item(Player_Ship.Crew(1).Inventory, 67) = 2,
         "Can't find item with ProtoIndex.");
      Assert
        (Find_Item
           (Inventory => Player_Ship.Crew(1).Inventory,
            Item_Type => To_Bounded_String("Weapon")) =
         1,
         "Can't find item wiht ItemType.");
      Assert
        (Find_Item(Player_Ship.Crew(1).Inventory, 500) = 0,
         "Item with not existing ProtoIndex found.");
      Assert
        (Find_Item
           (Inventory => Player_Ship.Crew(1).Inventory,
            Item_Type => To_Bounded_String("sdfsdfds")) =
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
