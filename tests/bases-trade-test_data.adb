--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

with Ada.Text_IO; use Ada.Text_IO;
with Config; use Config;
with Game; use Game;

package body Bases.Trade.Test_Data is

   procedure Set_Up(Gnattest_T: in out Test) is
      pragma Unreferenced(Gnattest_T);
   begin
      NewGameSettings.Player_Faction := To_Unbounded_String("POLEIS");
      NewGameSettings.Player_Career := To_Unbounded_String("general");
      NewGameSettings.Starting_Base := To_Unbounded_String("1");
      New_Game;
   end Set_Up;

   procedure Tear_Down(Gnattest_T: in out Test) is
      pragma Unreferenced(Gnattest_T);
   begin
      null;
   end Tear_Down;
end Bases.Trade.Test_Data;
