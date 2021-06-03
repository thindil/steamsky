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
   function Wrap_Test_MoveShip_143def_3bb6cb
     (X, Y: Integer; Message: in out Unbounded_String) return Natural is
   begin
      declare
         Test_MoveShip_143def_3bb6cb_Result: constant Natural :=
           GNATtest_Generated.GNATtest_Standard.Ships.Movement.MoveShip
             (X, Y, Message);
      begin
         return Test_MoveShip_143def_3bb6cb_Result;
      end;
   end Wrap_Test_MoveShip_143def_3bb6cb;
--  end read only

--  begin read only
   procedure Test_MoveShip_test_moveship(Gnattest_T: in out Test);
   procedure Test_MoveShip_143def_3bb6cb(Gnattest_T: in out Test) renames
     Test_MoveShip_test_moveship;
--  id:2.2/143def44414090ef/MoveShip/1/0/test_moveship/
   procedure Test_MoveShip_test_moveship(Gnattest_T: in out Test) is
      function MoveShip
        (X, Y: Integer; Message: in out Unbounded_String)
         return Natural renames
        Wrap_Test_MoveShip_143def_3bb6cb;
--  end read only

      pragma Unreferenced(Gnattest_T);
      OldX: constant Natural := Player_Ship.Sky_X;
      OldY: constant Natural := Player_Ship.Sky_Y;
      Message: Unbounded_String;
      NewX, NewY: Natural := 0;

   begin

      Player_Ship.Speed := FULL_SPEED;
      if Player_Ship.Sky_X + 1 <= 1_024 then
         NewX := 1;
      end if;
      if Player_Ship.Sky_Y + 1 <= 1_024 then
         NewY := 1;
      end if;
      if MoveShip(NewX, NewY, Message) = 0 then
         Ada.Text_IO.Put_Line(To_String(Message));
      end if;
      Assert
        (Player_Ship.Sky_X - NewX = OldX,
         "Failed to move player ship in X axis");
      Assert
        (Player_Ship.Sky_Y - NewY = OldY,
         "Failed to move player ship in Y axis");
      Player_Ship.Sky_X := OldX;
      Player_Ship.Sky_Y := OldY;
      Player_Ship.Speed := DOCKED;

--  begin read only
   end Test_MoveShip_test_moveship;
--  end read only

--  begin read only
   function Wrap_Test_DockShip_bfbe82_875e5b
     (Docking: Boolean; Escape: Boolean := False) return String is
   begin
      declare
         Test_DockShip_bfbe82_875e5b_Result: constant String :=
           GNATtest_Generated.GNATtest_Standard.Ships.Movement.DockShip
             (Docking, Escape);
      begin
         return Test_DockShip_bfbe82_875e5b_Result;
      end;
   end Wrap_Test_DockShip_bfbe82_875e5b;
--  end read only

--  begin read only
   procedure Test_DockShip_test_dockship(Gnattest_T: in out Test);
   procedure Test_DockShip_bfbe82_875e5b(Gnattest_T: in out Test) renames
     Test_DockShip_test_dockship;
--  id:2.2/bfbe82e1179e6b20/DockShip/1/0/test_dockship/
   procedure Test_DockShip_test_dockship(Gnattest_T: in out Test) is
      function DockShip
        (Docking: Boolean; Escape: Boolean := False) return String renames
        Wrap_Test_DockShip_bfbe82_875e5b;
--  end read only

      pragma Unreferenced(Gnattest_T);
      Message: Unbounded_String;

   begin

      Message := To_Unbounded_String(DockShip(False));
      Assert
        (Message = Null_Unbounded_String, "Failed to undock from the base.");
      Message := To_Unbounded_String(DockShip(True));
      Assert(Message = Null_Unbounded_String, "Failed to dock to the base.");

--  begin read only
   end Test_DockShip_test_dockship;
--  end read only

--  begin read only
   function Wrap_Test_ChangeShipSpeed_a103ef_17b968
     (SpeedValue: Ship_Speed) return String is
   begin
      declare
         Test_ChangeShipSpeed_a103ef_17b968_Result: constant String :=
           GNATtest_Generated.GNATtest_Standard.Ships.Movement.ChangeShipSpeed
             (SpeedValue);
      begin
         return Test_ChangeShipSpeed_a103ef_17b968_Result;
      end;
   end Wrap_Test_ChangeShipSpeed_a103ef_17b968;
--  end read only

--  begin read only
   procedure Test_ChangeShipSpeed_test_changeshipspeed
     (Gnattest_T: in out Test);
   procedure Test_ChangeShipSpeed_a103ef_17b968
     (Gnattest_T: in out Test) renames
     Test_ChangeShipSpeed_test_changeshipspeed;
--  id:2.2/a103efdf9c3f9d91/ChangeShipSpeed/1/0/test_changeshipspeed/
   procedure Test_ChangeShipSpeed_test_changeshipspeed
     (Gnattest_T: in out Test) is
      function ChangeShipSpeed(SpeedValue: Ship_Speed) return String renames
        Wrap_Test_ChangeShipSpeed_a103ef_17b968;
--  end read only

      pragma Unreferenced(Gnattest_T);
      Message: Unbounded_String;

   begin

      Player_Ship.Crew(2).Order := Pilot;
      Player_Ship.Crew(3).Order := Engineer;
      Message := To_Unbounded_String(ChangeShipSpeed(FULL_SPEED));
      if Message /= Null_Unbounded_String then
         Ada.Text_IO.Put_Line(To_String(Message));
         Assert(False, "Failed to change speed of docked ship.");
      end if;
      Player_Ship.Crew(2).Order := Pilot;
      Player_Ship.Crew(3).Order := Engineer;
      Message := To_Unbounded_String(DockShip(False));
      if Message /= Null_Unbounded_String then
         Ada.Text_IO.Put_Line(To_String(Message));
         Assert(False, "Failed to dock ship again.");
      end if;
      Player_Ship.Crew(2).Order := Pilot;
      Player_Ship.Crew(3).Order := Engineer;
      Message := To_Unbounded_String(ChangeShipSpeed(FULL_STOP));
      if Message /= Null_Unbounded_String then
         Ada.Text_IO.Put_Line(To_String(Message));
         Assert(False, "Failed to change speed of ship.");
      end if;
      Player_Ship.Crew(2).Order := Pilot;
      Player_Ship.Crew(3).Order := Engineer;
      Message := To_Unbounded_String(DockShip(True));
      if Message /= Null_Unbounded_String then
         Ada.Text_IO.Put_Line(To_String(Message));
         Assert(False, "Failed to dock ship again second time.");
      end if;

--  begin read only
   end Test_ChangeShipSpeed_test_changeshipspeed;
--  end read only

--  begin read only
   function Wrap_Test_RealSpeed_da7fcb_f7fd56
     (Ship: Ship_Record; InfoOnly: Boolean := False) return Natural is
   begin
      declare
         Test_RealSpeed_da7fcb_f7fd56_Result: constant Natural :=
           GNATtest_Generated.GNATtest_Standard.Ships.Movement.RealSpeed
             (Ship, InfoOnly);
      begin
         return Test_RealSpeed_da7fcb_f7fd56_Result;
      end;
   end Wrap_Test_RealSpeed_da7fcb_f7fd56;
--  end read only

--  begin read only
   procedure Test_RealSpeed_test_realspeed(Gnattest_T: in out Test);
   procedure Test_RealSpeed_da7fcb_f7fd56(Gnattest_T: in out Test) renames
     Test_RealSpeed_test_realspeed;
--  id:2.2/da7fcba60b6babad/RealSpeed/1/0/test_realspeed/
   procedure Test_RealSpeed_test_realspeed(Gnattest_T: in out Test) is
      function RealSpeed
        (Ship: Ship_Record; InfoOnly: Boolean := False) return Natural renames
        Wrap_Test_RealSpeed_da7fcb_f7fd56;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Assert
        (RealSpeed(Player_Ship) = 0, "Failed to get speed of docked ship.");
      Player_Ship.Speed := FULL_SPEED;
      Assert
        (RealSpeed(Player_Ship) /= 0,
         "Failed to get speed of ship with full speed.");
      Player_Ship.Speed := DOCKED;
      Assert
        (RealSpeed(Player_Ship, True) /= 0,
         "Failed to get potential speed of docked ship.");

--  begin read only
   end Test_RealSpeed_test_realspeed;
--  end read only

--  begin read only
   function Wrap_Test_CountFuelNeeded_db602d_18e85d return Integer is
   begin
      declare
         Test_CountFuelNeeded_db602d_18e85d_Result: constant Integer :=
           GNATtest_Generated.GNATtest_Standard.Ships.Movement.CountFuelNeeded;
      begin
         return Test_CountFuelNeeded_db602d_18e85d_Result;
      end;
   end Wrap_Test_CountFuelNeeded_db602d_18e85d;
--  end read only

--  begin read only
   procedure Test_CountFuelNeeded_test_countfuelneeded
     (Gnattest_T: in out Test);
   procedure Test_CountFuelNeeded_db602d_18e85d
     (Gnattest_T: in out Test) renames
     Test_CountFuelNeeded_test_countfuelneeded;
--  id:2.2/db602d4cda90f238/CountFuelNeeded/1/0/test_countfuelneeded/
   procedure Test_CountFuelNeeded_test_countfuelneeded
     (Gnattest_T: in out Test) is
      function CountFuelNeeded return Integer renames
        Wrap_Test_CountFuelNeeded_db602d_18e85d;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Assert(CountFuelNeeded < 1, "Failed to count needed fuel to travel.");

--  begin read only
   end Test_CountFuelNeeded_test_countfuelneeded;
--  end read only

--  begin read only
   procedure Wrap_Test_WaitInPlace_a6040e_d787da(Minutes: Positive) is
   begin
      GNATtest_Generated.GNATtest_Standard.Ships.Movement.WaitInPlace(Minutes);
   end Wrap_Test_WaitInPlace_a6040e_d787da;
--  end read only

--  begin read only
   procedure Test_WaitInPlace_test_waitinplace(Gnattest_T: in out Test);
   procedure Test_WaitInPlace_a6040e_d787da(Gnattest_T: in out Test) renames
     Test_WaitInPlace_test_waitinplace;
--  id:2.2/a6040ed3f85f9963/WaitInPlace/1/0/test_waitinplace/
   procedure Test_WaitInPlace_test_waitinplace(Gnattest_T: in out Test) is
      procedure WaitInPlace(Minutes: Positive) renames
        Wrap_Test_WaitInPlace_a6040e_d787da;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      WaitInPlace(1);
      Assert(True, "This test can only crash.");

--  begin read only
   end Test_WaitInPlace_test_waitinplace;
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
