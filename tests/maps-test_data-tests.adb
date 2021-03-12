--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Maps.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only

with Ships; use Ships;

--  begin read only
--  end read only
package body Maps.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only
--  begin read only
   function Wrap_Test_CountDistance_ecd188_2a2146
     (DestinationX: Map_X_Range; DestinationY: Map_Y_Range) return Natural is
   begin
      declare
         Test_CountDistance_ecd188_2a2146_Result: constant Natural :=
           GNATtest_Generated.GNATtest_Standard.Maps.CountDistance
             (DestinationX, DestinationY);
      begin
         return Test_CountDistance_ecd188_2a2146_Result;
      end;
   end Wrap_Test_CountDistance_ecd188_2a2146;
--  end read only

--  begin read only
   procedure Test_CountDistance_test_countdistance(Gnattest_T: in out Test);
   procedure Test_CountDistance_ecd188_2a2146(Gnattest_T: in out Test) renames
     Test_CountDistance_test_countdistance;
--  id:2.2/ecd188bba777e9d6/CountDistance/1/0/test_countdistance/
   procedure Test_CountDistance_test_countdistance(Gnattest_T: in out Test) is
      function CountDistance
        (DestinationX: Map_X_Range; DestinationY: Map_Y_Range)
         return Natural renames
        Wrap_Test_CountDistance_ecd188_2a2146;
--  end read only

      pragma Unreferenced(Gnattest_T);
      X: Positive := PlayerShip.SkyX + 1;
      Y: Positive := PlayerShip.SkyY + 1;

   begin

      if X > Map_X_Range'Last then
         X := PlayerShip.SkyX - 1;
      end if;
      if Y > Map_Y_Range'Last then
         Y := PlayerShip.SkyY - 1;
      end if;
      Assert
        (CountDistance(X, Y) = 1,
         "Failed to count distance between two points on map.");

--  begin read only
   end Test_CountDistance_test_countdistance;
--  end read only

--  begin read only
   procedure Wrap_Test_NormalizeCoord_6338a5_63c4fc
     (Coord: in out Integer; IsXAxis: Boolean := True) is
   begin
      GNATtest_Generated.GNATtest_Standard.Maps.NormalizeCoord(Coord, IsXAxis);
   end Wrap_Test_NormalizeCoord_6338a5_63c4fc;
--  end read only

--  begin read only
   procedure Test_NormalizeCoord_test_normalizecoord(Gnattest_T: in out Test);
   procedure Test_NormalizeCoord_6338a5_63c4fc(Gnattest_T: in out Test) renames
     Test_NormalizeCoord_test_normalizecoord;
--  id:2.2/6338a59b69707203/NormalizeCoord/1/0/test_normalizecoord/
   procedure Test_NormalizeCoord_test_normalizecoord
     (Gnattest_T: in out Test) is
      procedure NormalizeCoord
        (Coord: in out Integer; IsXAxis: Boolean := True) renames
        Wrap_Test_NormalizeCoord_6338a5_63c4fc;
--  end read only

      pragma Unreferenced(Gnattest_T);
      Coord: Integer := 0;

   begin

      NormalizeCoord(Coord);
      Assert(Coord = 1, "Failed to normalize map coordinate.");
      NormalizeCoord(Coord);
      Assert(Coord = 1, "Failed to not normalize map coordinate.");

--  begin read only
   end Test_NormalizeCoord_test_normalizecoord;
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
end Maps.Test_Data.Tests;
