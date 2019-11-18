--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Ships.Movement.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only

with Ada.Text_IO; use Ada.Text_IO;
with Crafts; use Crafts;
with Ships.Crew; use Ships.Crew;

--  begin read only
--  end read only
package body Ships.Movement.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only
--  begin read only
   function Wrap_Test_MoveShip_143def_3bb6cb (X, Y: Integer; Message: in out Unbounded_String)  return Natural
   is
   begin
      declare
         Test_MoveShip_143def_3bb6cb_Result : constant Natural := GNATtest_Generated.GNATtest_Standard.Ships.Movement.MoveShip (X, Y, Message);
      begin
         return Test_MoveShip_143def_3bb6cb_Result;
      end;
   end Wrap_Test_MoveShip_143def_3bb6cb;
--  end read only

--  begin read only
   procedure Test_MoveShip_test_moveship (Gnattest_T : in out Test);
   procedure Test_MoveShip_143def_3bb6cb (Gnattest_T : in out Test) renames Test_MoveShip_test_moveship;
--  id:2.2/143def44414090ef/MoveShip/1/0/test_moveship/
   procedure Test_MoveShip_test_moveship (Gnattest_T : in out Test) is
      function MoveShip (X, Y: Integer; Message: in out Unbounded_String) return Natural renames Wrap_Test_MoveShip_143def_3bb6cb;
--  end read only

      pragma Unreferenced (Gnattest_T);
      OldX: constant Natural := PlayerShip.SkyX;
      OldY: constant Natural := PlayerShip.SkyY;
      Message: Unbounded_String;
      NewX, NewY: Natural := 0;

   begin

      PlayerShip.Speed := FULL_SPEED;
      if PlayerShip.SkyX + 1 <= 1024 then
         NewX := 1;
      end if;
      if PlayerShip.SkyY + 1 <= 1024 then
         NewY := 1;
      end if;
      if MoveShip(NewX, NewY, Message) = 0 then
         Ada.Text_IO.Put_Line(To_String(Message));
      end if;
      Assert(PlayerShip.SkyX - NewX = OldX, "Failed to move player ship in X axis");
      Assert(PlayerShip.SkyY - NewY = OldY, "Failed to move player ship in Y axis");
      PlayerShip.SkyX := OldX;
      PlayerShip.SkyY := OldY;
      PlayerShip.Speed := DOCKED;

--  begin read only
   end Test_MoveShip_test_moveship;
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
end Ships.Movement.Test_Data.Tests;
