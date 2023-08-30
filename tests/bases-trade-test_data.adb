--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Config; use Config;

package body Bases.Trade.Test_Data is

   procedure Set_Up(Gnattest_T: in out Test) is
      pragma Unreferenced(Gnattest_T);
      use Tiny_String;
   begin
      New_Game;
   end Set_Up;

   procedure Tear_Down(Gnattest_T: in out Test) is
      pragma Unreferenced(Gnattest_T);
   begin
      null;
   end Tear_Down;
end Bases.Trade.Test_Data;
