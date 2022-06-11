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
   procedure Wrap_Test_Sell_Items_51dfd2_132f39
     (Item_Index: Inventory_Container.Extended_Index; Amount: String) is
   begin
      begin
         pragma Assert
           (Item_Index in
              Inventory_Container.First_Index
                    (Container => Player_Ship.Cargo) ..
                    Inventory_Container.Last_Index
                      (Container => Player_Ship.Cargo) and
            Amount'Length > 0);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(trades.ads:0):Test_SellItems test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Trades.Sell_Items
        (Item_Index, Amount);
      begin
         pragma Assert(True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(trades.ads:0:):Test_SellItems test commitment violated");
      end;
   end Wrap_Test_Sell_Items_51dfd2_132f39;
--  end read only

--  begin read only
   procedure Test_Sell_Items_test_sellitems(Gnattest_T: in out Test);
   procedure Test_Sell_Items_51dfd2_132f39(Gnattest_T: in out Test) renames
     Test_Sell_Items_test_sellitems;
--  id:2.2/51dfd29e319e1473/Sell_Items/1/0/test_sellitems/
   procedure Test_Sell_Items_test_sellitems(Gnattest_T: in out Test) is
      procedure Sell_Items
        (Item_Index: Inventory_Container.Extended_Index;
         Amount: String) renames
        Wrap_Test_Sell_Items_51dfd2_132f39;
--  end read only

      pragma Unreferenced(Gnattest_T);
      Item: constant Inventory_Data :=
        Inventory_Container.Element
          (Container => Player_Ship.Cargo, Index => 2);

   begin

      Sell_Items(2, "1");
      Inventory_Container.Replace_Element
        (Container => Player_Ship.Cargo, Index => 2, New_Item => Item);
      Player_Ship.Crew(2).Payment(2) := 1;
      Player_Ship.Crew(3).Payment(2) := 4;
      Player_Ship.Crew(4).Payment(2) := 1;
      Sell_Items(2, "1");
      Inventory_Container.Replace_Element
        (Container => Player_Ship.Cargo, Index => 2, New_Item => Item);
      Player_Ship.Crew(2).Payment(2) := 0;
      Player_Ship.Crew(3).Payment(2) := 0;
      Player_Ship.Crew(4).Payment(2) := 0;
      Assert(True, "This tests can only crash.");

--  begin read only
   end Test_Sell_Items_test_sellitems;
--  end read only

--  begin read only
   procedure Wrap_Test_Generate_Trader_Cargo_49c5d5_cfbd61
     (Proto_Index: Proto_Ships_Container.Extended_Index) is
   begin
      begin
         pragma Assert(Proto_Index <= Proto_Ships_List.Last_Index);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(trades.ads:0):Test_GenerateTraderCargo test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Trades.Generate_Trader_Cargo
        (Proto_Index);
      begin
         pragma Assert(True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(trades.ads:0:):Test_GenerateTraderCargo test commitment violated");
      end;
   end Wrap_Test_Generate_Trader_Cargo_49c5d5_cfbd61;
--  end read only

--  begin read only
   procedure Test_Generate_Trader_Cargo_test_generatetradercargo
     (Gnattest_T: in out Test);
   procedure Test_Generate_Trader_Cargo_49c5d5_cfbd61
     (Gnattest_T: in out Test) renames
     Test_Generate_Trader_Cargo_test_generatetradercargo;
--  id:2.2/49c5d50bda1af19f/Generate_Trader_Cargo/1/0/test_generatetradercargo/
   procedure Test_Generate_Trader_Cargo_test_generatetradercargo
     (Gnattest_T: in out Test) is
      procedure Generate_Trader_Cargo
        (Proto_Index: Proto_Ships_Container.Extended_Index) renames
        Wrap_Test_Generate_Trader_Cargo_49c5d5_cfbd61;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Generate_Trader_Cargo(96);
      Assert
        (BaseCargo_Container.Length(Trader_Cargo) > 0,
         "Failed to generate cargo for trade.");

--  begin read only
   end Test_Generate_Trader_Cargo_test_generatetradercargo;
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
