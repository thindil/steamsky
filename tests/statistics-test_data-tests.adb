--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Statistics.Test_Data.

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
package body Statistics.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only
--  begin read only
   procedure Wrap_Test_Update_Destroyed_Ships_151582_3aed49
     (Ship_Name: Tiny_String.Bounded_String) is
   begin
      begin
         pragma Assert(Ship_Name /= Tiny_String.Null_Bounded_String);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(statistics.ads:0):Test_UpdateDestroyedShips test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Statistics.Update_Destroyed_Ships
        (Ship_Name);
      begin
         pragma Assert(True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(statistics.ads:0:):Test_UpdateDestroyedShips test commitment violated");
      end;
   end Wrap_Test_Update_Destroyed_Ships_151582_3aed49;
--  end read only

--  begin read only
   procedure Test_Update_Destroyed_Ships_test_updatedestroyedships
     (Gnattest_T: in out Test);
   procedure Test_Update_Destroyed_Ships_151582_3aed49
     (Gnattest_T: in out Test) renames
     Test_Update_Destroyed_Ships_test_updatedestroyedships;
--  id:2.2/151582b07ae3ffa1/Update_Destroyed_Ships/1/0/test_updatedestroyedships/
   procedure Test_Update_Destroyed_Ships_test_updatedestroyedships
     (Gnattest_T: in out Test) is
      procedure Update_Destroyed_Ships
        (Ship_Name: Tiny_String.Bounded_String) renames
        Wrap_Test_Update_Destroyed_Ships_151582_3aed49;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Update_Destroyed_Ships(To_Bounded_String("Tiny pirates ship"));
      Assert
        (Game_Stats.Destroyed_Ships.Length = 1,
         "Failed to add ship to destroyed ships list.");
      Update_Destroyed_Ships(To_Bounded_String("Sfdsfdsf"));
      Assert
        (Game_Stats.Destroyed_Ships.Length = 1,
         "Failed to not add non existing ship to destroyed ships list.");

--  begin read only
   end Test_Update_Destroyed_Ships_test_updatedestroyedships;
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
end Statistics.Test_Data.Tests;
