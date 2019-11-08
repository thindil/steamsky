--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Ships.Cargo.Test_Data.

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
package body Ships.Cargo.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only
--  begin read only
   procedure Wrap_Test_UpdateCargo_87d3a7_53988c (Ship: in out ShipRecord; ProtoIndex: Unbounded_String := Null_Unbounded_String; Amount: Integer; Durability: Natural := 100; CargoIndex, Price: Natural := 0) 
   is
   begin
      begin
         pragma Assert
           (CargoIndex <= Ship.Cargo.Last_Index);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(ships-cargo.ads:0):Test_UpdateCargo test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Ships.Cargo.UpdateCargo (Ship, ProtoIndex, Amount, Durability, CargoIndex, Price);
      begin
         pragma Assert
           (True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(ships-cargo.ads:0:):Test_UpdateCargo test commitment violated");
      end;
   end Wrap_Test_UpdateCargo_87d3a7_53988c;
--  end read only

--  begin read only
   procedure Test_UpdateCargo_test_updatecargo (Gnattest_T : in out Test);
   procedure Test_UpdateCargo_87d3a7_53988c (Gnattest_T : in out Test) renames Test_UpdateCargo_test_updatecargo;
--  id:2.2/87d3a721378c9b6a/UpdateCargo/1/0/test_updatecargo/
   procedure Test_UpdateCargo_test_updatecargo (Gnattest_T : in out Test) is
   procedure UpdateCargo (Ship: in out ShipRecord; ProtoIndex: Unbounded_String := Null_Unbounded_String; Amount: Integer; Durability: Natural := 100; CargoIndex, Price: Natural := 0) renames Wrap_Test_UpdateCargo_87d3a7_53988c;
--  end read only

      pragma Unreferenced (Gnattest_T);
      Amount: constant Natural := PlayerShip.Cargo(1).Amount;

   begin

      UpdateCargo(PlayerShip, To_Unbounded_String("1"), -1);
      Assert(Amount = PlayerShip.Cargo(1).Amount + 1, "Failed to remove some items from player ship cargo.");
      UpdateCargo(PlayerShip, To_Unbounded_String("1"), 1);
      Assert(Amount = PlayerShip.Cargo(1).Amount, "Failed to add some items to player ship cargo.");

--  begin read only
   end Test_UpdateCargo_test_updatecargo;
--  end read only

--  begin read only
   function Wrap_Test_FreeCargo_2845af_4f2f60 (Amount: Integer; Ship: ShipRecord := PlayerShip)  return Integer
   is
   begin
      declare
         Test_FreeCargo_2845af_4f2f60_Result : constant Integer := GNATtest_Generated.GNATtest_Standard.Ships.Cargo.FreeCargo (Amount, Ship);
      begin
         return Test_FreeCargo_2845af_4f2f60_Result;
      end;
   end Wrap_Test_FreeCargo_2845af_4f2f60;
--  end read only

--  begin read only
   procedure Test_FreeCargo_test_freecargo (Gnattest_T : in out Test);
   procedure Test_FreeCargo_2845af_4f2f60 (Gnattest_T : in out Test) renames Test_FreeCargo_test_freecargo;
--  id:2.2/2845af0c133e2533/FreeCargo/1/0/test_freecargo/
   procedure Test_FreeCargo_test_freecargo (Gnattest_T : in out Test) is
      function FreeCargo (Amount: Integer; Ship: ShipRecord := PlayerShip) return Integer renames Wrap_Test_FreeCargo_2845af_4f2f60;
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      Assert(FreeCargo(1) > FreeCargo(0), "Failed to get proper amount of free cargo in player ship.");

--  begin read only
   end Test_FreeCargo_test_freecargo;
--  end read only

--  begin read only
   function Wrap_Test_GetItemAmount_57499f_15cacd (ItemType: Unbounded_String)  return Natural
   is
   begin
      begin
         pragma Assert
           (ItemType /= Null_Unbounded_String);
         null;
      exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "req_sloc(ships-cargo.ads:0):Test_GetItemAmount test requirement violated");
      end;
      declare
         Test_GetItemAmount_57499f_15cacd_Result : constant Natural := GNATtest_Generated.GNATtest_Standard.Ships.Cargo.GetItemAmount (ItemType);
      begin
         begin
            pragma Assert
              (True);
            null;
         exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "ens_sloc(ships-cargo.ads:0:):Test_GetItemAmount test commitment violated");
         end;
         return Test_GetItemAmount_57499f_15cacd_Result;
      end;
   end Wrap_Test_GetItemAmount_57499f_15cacd;
--  end read only

--  begin read only
   procedure Test_GetItemAmount_test_getitemamount (Gnattest_T : in out Test);
   procedure Test_GetItemAmount_57499f_15cacd (Gnattest_T : in out Test) renames Test_GetItemAmount_test_getitemamount;
--  id:2.2/57499f0478a1da15/GetItemAmount/1/0/test_getitemamount/
   procedure Test_GetItemAmount_test_getitemamount (Gnattest_T : in out Test) is
      function GetItemAmount (ItemType: Unbounded_String) return Natural renames Wrap_Test_GetItemAmount_57499f_15cacd;
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      PlayerShip.Cargo(1).Amount := 2000;
      Assert(GetItemAmount(To_Unbounded_String("Fuel")) = 2000, "Failed to get proper amount of item.");

--  begin read only
   end Test_GetItemAmount_test_getitemamount;
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
end Ships.Cargo.Test_Data.Tests;
