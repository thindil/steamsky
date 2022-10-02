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
   function Wrap_Test_Count_Distance_328204_2a2146
     (Destination_X: Map_X_Range; Destination_Y: Map_Y_Range) return Natural is
   begin
      declare
         Test_Count_Distance_328204_2a2146_Result: constant Natural :=
           GNATtest_Generated.GNATtest_Standard.Maps.Count_Distance
             (Destination_X, Destination_Y);
      begin
         return Test_Count_Distance_328204_2a2146_Result;
      end;
   end Wrap_Test_Count_Distance_328204_2a2146;
--  end read only

--  begin read only
   procedure Test_Count_Distance_test_countdistance(Gnattest_T: in out Test);
   procedure Test_Count_Distance_328204_2a2146(Gnattest_T: in out Test) renames
     Test_Count_Distance_test_countdistance;
--  id:2.2/328204a2351ca022/Count_Distance/1/0/test_countdistance/
   procedure Test_Count_Distance_test_countdistance(Gnattest_T: in out Test) is
      function Count_Distance
        (Destination_X: Map_X_Range; Destination_Y: Map_Y_Range)
         return Natural renames
        Wrap_Test_Count_Distance_328204_2a2146;
--  end read only

      pragma Unreferenced(Gnattest_T);
      X: Positive := Player_Ship.Sky_X + 1;
      Y: Positive := Player_Ship.Sky_Y + 1;

   begin

      if X > Map_X_Range'Last then
         X := Player_Ship.Sky_X - 1;
      end if;
      if Y > Map_Y_Range'Last then
         Y := Player_Ship.Sky_Y - 1;
      end if;
      Assert
        (Count_Distance(X, Y) = 1,
         "Failed to count distance between two points on map.");

--  begin read only
   end Test_Count_Distance_test_countdistance;
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
