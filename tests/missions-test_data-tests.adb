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

with Ships; use Ships;
with Maps; use Maps;
with Bases; use Bases;

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
   procedure Wrap_Test_GenerateMissions_2a8787_14c74a
   is
   begin
      GNATtest_Generated.GNATtest_Standard.Missions.GenerateMissions;
   end Wrap_Test_GenerateMissions_2a8787_14c74a;
--  end read only

--  begin read only
   procedure Test_GenerateMissions_test_generatemissions (Gnattest_T : in out Test);
   procedure Test_GenerateMissions_2a8787_14c74a (Gnattest_T : in out Test) renames Test_GenerateMissions_test_generatemissions;
--  id:2.2/2a8787b975b577a4/GenerateMissions/1/0/test_generatemissions/
   procedure Test_GenerateMissions_test_generatemissions (Gnattest_T : in out Test) is
   procedure GenerateMissions renames Wrap_Test_GenerateMissions_2a8787_14c74a;
--  end read only

      pragma Unreferenced (Gnattest_T);
      BaseIndex: constant Natural :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;

   begin

      for I in 1 .. 1000 loop
         SkyBases(BaseIndex).MissionsDate := (others => 0);
         GenerateMissions;
      end loop;
      Assert(True, "This test can only crash.");

--  begin read only
   end Test_GenerateMissions_test_generatemissions;
--  end read only

--  begin read only
   procedure Wrap_Test_AcceptMission_979505_57ce38 (MissionIndex: Positive) 
   is
   begin
      begin
         pragma Assert
           (True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(missions.ads:0):Test_AcceptMission test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Missions.AcceptMission (MissionIndex);
      begin
         pragma Assert
           (True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(missions.ads:0:):Test_AcceptMission test commitment violated");
      end;
   end Wrap_Test_AcceptMission_979505_57ce38;
--  end read only

--  begin read only
   procedure Test_AcceptMission_test_acceptmission (Gnattest_T : in out Test);
   procedure Test_AcceptMission_979505_57ce38 (Gnattest_T : in out Test) renames Test_AcceptMission_test_acceptmission;
--  id:2.2/9795058c0b298911/AcceptMission/1/0/test_acceptmission/
   procedure Test_AcceptMission_test_acceptmission (Gnattest_T : in out Test) is
   procedure AcceptMission (MissionIndex: Positive) renames Wrap_Test_AcceptMission_979505_57ce38;
--  end read only

      pragma Unreferenced (Gnattest_T);

      BaseIndex: constant Positive :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      MissionIndex: Positive;
   begin

      for I in SkyBases(BaseIndex).Missions.Iterate loop
         if SkyBases(BaseIndex).Missions(I).MType = Explore or SkyBases(BaseIndex).Missions(I).MType = Patrol or SkyBases(BaseIndex).Missions(I).MType = Destroy then
            MissionIndex := Mission_Container.To_Index(I);
            exit;
         end if;
      end loop;
      AcceptMission(MissionIndex);
      Assert(AcceptedMissions.Length = 1, "Accepting mission failed.");

--  begin read only
   end Test_AcceptMission_test_acceptmission;
--  end read only

--  begin read only
   procedure Wrap_Test_UpdateMissions_b5358e_60a195 (Minutes: Positive) 
   is
   begin
      GNATtest_Generated.GNATtest_Standard.Missions.UpdateMissions (Minutes);
   end Wrap_Test_UpdateMissions_b5358e_60a195;
--  end read only

--  begin read only
   procedure Test_UpdateMissions_test_updatemissions (Gnattest_T : in out Test);
   procedure Test_UpdateMissions_b5358e_60a195 (Gnattest_T : in out Test) renames Test_UpdateMissions_test_updatemissions;
--  id:2.2/b5358ee94cb1cec0/UpdateMissions/1/0/test_updatemissions/
   procedure Test_UpdateMissions_test_updatemissions (Gnattest_T : in out Test) is
   procedure UpdateMissions (Minutes: Positive) renames Wrap_Test_UpdateMissions_b5358e_60a195;
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AcceptedMissions.Clear;
      AcceptedMissions.Append((MType => Explore, Time => 10, TargetX => 1, TargetY => 1,
                  Reward => 1, StartBase => 1, Finished => True,
                  Multiplier => 0.0, Target => 0));
      UpdateMissions(8);
      Assert(AcceptedMissions(1).Time = 2, "Missions wrongly updated.");
      UpdateMissions(2);
      Assert(AcceptedMissions.Length = 0, "Missions not removed after update");

--  begin read only
   end Test_UpdateMissions_test_updatemissions;
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

      AcceptedMissions.Clear;
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
