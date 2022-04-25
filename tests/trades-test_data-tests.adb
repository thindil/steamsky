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
   procedure Wrap_Test_BuyItems_295d66_e10e97
     (BaseItemIndex: BaseCargo_Container.Extended_Index; Amount: String) is
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
      GNATtest_Generated.GNATtest_Standard.Trades.BuyItems
        (BaseItemIndex, Amount);
      begin
         pragma Assert(True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(trades.ads:0:):Test_BuyItems test commitment violated");
      end;
   end Wrap_Test_BuyItems_295d66_e10e97;
--  end read only

--  begin read only
   procedure Test_BuyItems_test_buyitems(Gnattest_T: in out Test);
   procedure Test_BuyItems_295d66_e10e97(Gnattest_T: in out Test) renames
     Test_BuyItems_test_buyitems;
--  id:2.2/295d66b8c191f255/BuyItems/1/0/test_buyitems/
   procedure Test_BuyItems_test_buyitems(Gnattest_T: in out Test) is
      procedure BuyItems
        (BaseItemIndex: BaseCargo_Container.Extended_Index;
         Amount: String) renames
        Wrap_Test_BuyItems_295d66_e10e97;
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
      BuyItems(2, "1");
      Item.Amount := OldAmount;
      BaseCargo_Container.Replace_Element(Sky_Bases(BaseIndex).Cargo, 2, Item);
      Assert(True, "This test can only crash.");

--  begin read only
   end Test_BuyItems_test_buyitems;
--  end read only

--  begin read only
   procedure Wrap_Test_SellItems_079195_1775af
     (ItemIndex: Inventory_Container.Extended_Index; Amount: String) is
   begin
      begin
         pragma Assert
           (ItemIndex in
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
      GNATtest_Generated.GNATtest_Standard.Trades.SellItems(ItemIndex, Amount);
      begin
         pragma Assert(True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(trades.ads:0:):Test_SellItems test commitment violated");
      end;
   end Wrap_Test_SellItems_079195_1775af;
--  end read only

--  begin read only
   procedure Test_SellItems_test_sellitems(Gnattest_T: in out Test);
   procedure Test_SellItems_079195_1775af(Gnattest_T: in out Test) renames
     Test_SellItems_test_sellitems;
--  id:2.2/0791958f8fd18173/SellItems/1/0/test_sellitems/
   procedure Test_SellItems_test_sellitems(Gnattest_T: in out Test) is
      procedure SellItems
        (ItemIndex: Inventory_Container.Extended_Index; Amount: String) renames
        Wrap_Test_SellItems_079195_1775af;
--  end read only

      pragma Unreferenced(Gnattest_T);
      Item: constant Inventory_Data :=
        Inventory_Container.Element
          (Container => Player_Ship.Cargo, Index => 2);

   begin

      SellItems(2, "1");
      Inventory_Container.Replace_Element
        (Container => Player_Ship.Cargo, Index => 2, New_Item => Item);
      Player_Ship.Crew(2).Payment(2) := 1;
      Player_Ship.Crew(3).Payment(2) := 4;
      Player_Ship.Crew(4).Payment(2) := 1;
      SellItems(2, "1");
      Inventory_Container.Replace_Element
        (Container => Player_Ship.Cargo, Index => 2, New_Item => Item);
      Player_Ship.Crew(2).Payment(2) := 0;
      Player_Ship.Crew(3).Payment(2) := 0;
      Player_Ship.Crew(4).Payment(2) := 0;
      Assert(True, "This tests can only crash.");

--  begin read only
   end Test_SellItems_test_sellitems;
--  end read only

--  begin read only
   procedure Wrap_Test_GenerateTraderCargo_04d2f8_e2d9dd
     (ProtoIndex: Tiny_String.Bounded_String) is
   begin
      begin
         pragma Assert
           (Proto_Ships_Container.Contains(Proto_Ships_List, ProtoIndex));
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(trades.ads:0):Test_GenerateTraderCargo test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Trades.GenerateTraderCargo
        (ProtoIndex);
      begin
         pragma Assert(True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(trades.ads:0:):Test_GenerateTraderCargo test commitment violated");
      end;
   end Wrap_Test_GenerateTraderCargo_04d2f8_e2d9dd;
--  end read only

--  begin read only
   procedure Test_GenerateTraderCargo_test_generatetradercargo
     (Gnattest_T: in out Test);
   procedure Test_GenerateTraderCargo_04d2f8_e2d9dd
     (Gnattest_T: in out Test) renames
     Test_GenerateTraderCargo_test_generatetradercargo;
--  id:2.2/04d2f8daff961a2d/GenerateTraderCargo/1/0/test_generatetradercargo/
   procedure Test_GenerateTraderCargo_test_generatetradercargo
     (Gnattest_T: in out Test) is
      procedure GenerateTraderCargo
        (ProtoIndex: Tiny_String.Bounded_String) renames
        Wrap_Test_GenerateTraderCargo_04d2f8_e2d9dd;
--  end read only

      pragma Unreferenced(Gnattest_T);
      use Tiny_String;

   begin

      GenerateTraderCargo(To_Bounded_String("96"));
      Assert
        (BaseCargo_Container.Length(TraderCargo) > 0,
         "Failed to generate cargo for trade.");

--  begin read only
   end Test_GenerateTraderCargo_test_generatetradercargo;
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
