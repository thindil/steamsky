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
