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

with Bases; use Bases;
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
   procedure Wrap_Test_BuyItems_295d66_69cd52 (BaseItemIndex: Positive; Amount: String) 
   is
   begin
      GNATtest_Generated.GNATtest_Standard.Trades.BuyItems (BaseItemIndex, Amount);
   end Wrap_Test_BuyItems_295d66_69cd52;
--  end read only

--  begin read only
   procedure Test_BuyItems_test_buyitems (Gnattest_T : in out Test);
   procedure Test_BuyItems_295d66_69cd52 (Gnattest_T : in out Test) renames Test_BuyItems_test_buyitems;
--  id:2.2/295d66b8c191f255/BuyItems/1/0/test_buyitems/
   procedure Test_BuyItems_test_buyitems (Gnattest_T : in out Test) is
   procedure BuyItems (BaseItemIndex: Positive; Amount: String) renames Wrap_Test_BuyItems_295d66_69cd52;
--  end read only

      pragma Unreferenced (Gnattest_T);
      BaseIndex: constant Natural :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      OldAmount: Natural := SkyBases(BaseIndex).Cargo(2).Amount;

   begin

      if OldAmount = 0 then
         SkyBases(BaseIndex).Cargo(2).Amount := 2;
      end if;
      BuyItems(2, "1");
      SkyBases(BaseIndex).Cargo(2).Amount := OldAmount;
      Assert(True, "This test can only crash.");

--  begin read only
   end Test_BuyItems_test_buyitems;
--  end read only

--  begin read only
   procedure Wrap_Test_SellItems_079195_da8da3 (ItemIndex: Positive; Amount: String) 
   is
   begin
      begin
         pragma Assert
           (ItemIndex <= PlayerShip.Cargo.Last_Index);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(trades.ads:0):Test_SellItems test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Trades.SellItems (ItemIndex, Amount);
      begin
         pragma Assert
           (True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(trades.ads:0:):Test_SellItems test commitment violated");
      end;
   end Wrap_Test_SellItems_079195_da8da3;
--  end read only

--  begin read only
   procedure Test_SellItems_test_sellitems (Gnattest_T : in out Test);
   procedure Test_SellItems_079195_da8da3 (Gnattest_T : in out Test) renames Test_SellItems_test_sellitems;
--  id:2.2/0791958f8fd18173/SellItems/1/0/test_sellitems/
   procedure Test_SellItems_test_sellitems (Gnattest_T : in out Test) is
   procedure SellItems (ItemIndex: Positive; Amount: String) renames Wrap_Test_SellItems_079195_da8da3;
--  end read only

      pragma Unreferenced (Gnattest_T);
      OldAmount: Positive := PlayerShip.Cargo(2).Amount;

   begin

      SellItems(2, "1");
      PlayerShip.Cargo(2).Amount := OldAmount;
      Assert(True, "This test can only crash.");

--  begin read only
   end Test_SellItems_test_sellitems;
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
