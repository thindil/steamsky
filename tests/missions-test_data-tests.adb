--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Missions.Test_Data.

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
package body Missions.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only
--  begin read only
   procedure Wrap_Test_DeleteMission_4bf0c5_8b646f (MissionIndex: Positive; Failed: Boolean := True) 
   is
   begin
      begin
         pragma Assert
           (MissionIndex <= AcceptedMissions.Last_Index);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(missions.ads:0):Test_DeleteMission test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Missions.DeleteMission (MissionIndex, Failed);
      begin
         pragma Assert
           (True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(missions.ads:0:):Test_DeleteMission test commitment violated");
      end;
   end Wrap_Test_DeleteMission_4bf0c5_8b646f;
--  end read only

--  begin read only
   procedure Test_DeleteMission_test_deletemission (Gnattest_T : in out Test);
   procedure Test_DeleteMission_4bf0c5_8b646f (Gnattest_T : in out Test) renames Test_DeleteMission_test_deletemission;
--  id:2.2/4bf0c536f42cefa3/DeleteMission/1/0/test_deletemission/
   procedure Test_DeleteMission_test_deletemission (Gnattest_T : in out Test) is
   procedure DeleteMission (MissionIndex: Positive; Failed: Boolean := True) renames Wrap_Test_DeleteMission_4bf0c5_8b646f;
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AcceptedMissions.Append((MType => Explore, Time => 1, TargetX => 1, TargetY => 1,
                  Reward => 1, StartBase => 1, Finished => True,
                  Multiplier => 0.0, Target => 0));
      DeleteMission(1, False);
      Assert(AcceptedMissions.Length = 0, "Failed delete mission with 0 money reward.");

--  begin read only
   end Test_DeleteMission_test_deletemission;
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
end Missions.Test_Data.Tests;
