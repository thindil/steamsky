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
   function Wrap_Test_CountDistance_603480_547c35 (DestinationX, DestinationY: Positive)  return Natural
   is
   begin
      begin
         pragma Assert
           (DestinationX < 1025 and DestinationY < 1025);
         null;
      exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "req_sloc(maps.ads:0):Test_CountDistance test requirement violated");
      end;
      declare
         Test_CountDistance_603480_547c35_Result : constant Natural := GNATtest_Generated.GNATtest_Standard.Maps.CountDistance (DestinationX, DestinationY);
      begin
         begin
            pragma Assert
              (True);
            null;
         exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "ens_sloc(maps.ads:0:):Test_CountDistance test commitment violated");
         end;
         return Test_CountDistance_603480_547c35_Result;
      end;
   end Wrap_Test_CountDistance_603480_547c35;
--  end read only

--  begin read only
   procedure Test_CountDistance_test_countdistance (Gnattest_T : in out Test);
   procedure Test_CountDistance_603480_547c35 (Gnattest_T : in out Test) renames Test_CountDistance_test_countdistance;
--  id:2.2/6034801a19743abe/CountDistance/1/0/test_countdistance/
   procedure Test_CountDistance_test_countdistance (Gnattest_T : in out Test) is
      function CountDistance (DestinationX, DestinationY: Positive) return Natural renames Wrap_Test_CountDistance_603480_547c35;
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      Assert(CountDistance(PlayerShip.SkyX + 1, PlayerShip.SkyY) = 1, "Failed to count distance between two points on map.");

--  begin read only
   end Test_CountDistance_test_countdistance;
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
