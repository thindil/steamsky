--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

with Ada.Text_IO; use Ada.Text_IO;
with Config; use Config;
with Game; use Game;

package body Goals.Test_Data is

   procedure Set_Up (Gnattest_T : in out Test) is
      pragma Unreferenced (Gnattest_T);
   begin
      if DataDirectory = To_Unbounded_String("../../bin/data/") then
         return;
      end if;
      DataDirectory := To_Unbounded_String("../../bin/data/");
      SaveDirectory := To_Unbounded_String("../../bin/data/saves/");
      DocDirectory := To_Unbounded_String("../../bin/doc/");
      ModsDirectory := To_Unbounded_String("../../bin/data/mods/");
      ThemesDirectory := To_Unbounded_String("../../bin/data/themes/");
      LoadConfig;
      Ada.Text_IO.Put_Line(LoadGameData);
      NewGame;
   end Set_Up;

   procedure Tear_Down (Gnattest_T : in out Test) is
      pragma Unreferenced (Gnattest_T);
   begin
      null;
   end Tear_Down;end Goals.Test_Data;
