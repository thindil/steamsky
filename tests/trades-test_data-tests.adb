--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Trades.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only

with Ada.Containers; use Ada.Containers;
with Maps; use Maps;

--  begin read only
--  end read only
package body Trades.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only
--  begin read only
   procedure Wrap_Test_Buy_Items_eb7d42_e10e97
     (Base_Item_Index: BaseCargo_Container.Extended_Index; Amount: String) is
   begin
      begin
         pragma Assert(Amount'Length > 0);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(trades.ads:0):Test_BuyItems test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Trades.Buy_Items
        (Base_Item_Index, Amount);
      begin
         pragma Assert(True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(trades.ads:0:):Test_BuyItems test commitment violated");
      end;
   end Wrap_Test_Buy_Items_eb7d42_e10e97;
--  end read only

--  begin read only
   procedure Test_Buy_Items_test_buyitems(Gnattest_T: in out Test);
   procedure Test_Buy_Items_eb7d42_e10e97(Gnattest_T: in out Test) renames
     Test_Buy_Items_test_buyitems;
--  id:2.2/eb7d428261c76e34/Buy_Items/1/0/test_buyitems/
   procedure Test_Buy_Items_test_buyitems(Gnattest_T: in out Test) is
      procedure Buy_Items
        (Base_Item_Index: BaseCargo_Container.Extended_Index;
         Amount: String) renames
        Wrap_Test_Buy_Items_eb7d42_e10e97;
--  end read only

      pragma Unreferenced(Gnattest_T);
      BaseIndex: constant Natural :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      OldAmount: constant Natural :=
        BaseCargo_Container.Element(Sky_Bases(BaseIndex).Cargo, 2).Amount;
      Item: Base_Cargo;

   begin

      Item := BaseCargo_Container.Element(Sky_Bases(BaseIndex).Cargo, 2);
      if OldAmount = 0 then
         Item.Amount := 2;
      end if;
      BaseCargo_Container.Replace_Element(Sky_Bases(BaseIndex).Cargo, 2, Item);
      Buy_Items(2, "1");
      Item.Amount := OldAmount;
      BaseCargo_Container.Replace_Element(Sky_Bases(BaseIndex).Cargo, 2, Item);
      Assert(True, "This test can only crash.");

--  begin read only
   end Test_Buy_Items_test_buyitems;
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
end Trades.Test_Data.Tests;
