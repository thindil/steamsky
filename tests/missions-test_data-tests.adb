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
   procedure Wrap_Test_GenerateMissions_2a8787_14c74a is
   begin
      GNATtest_Generated.GNATtest_Standard.Missions.GenerateMissions;
   end Wrap_Test_GenerateMissions_2a8787_14c74a;
--  end read only

--  begin read only
   procedure Test_GenerateMissions_test_generatemissions
     (Gnattest_T: in out Test);
   procedure Test_GenerateMissions_2a8787_14c74a
     (Gnattest_T: in out Test) renames
     Test_GenerateMissions_test_generatemissions;
--  id:2.2/2a8787b975b577a4/GenerateMissions/1/0/test_generatemissions/
   procedure Test_GenerateMissions_test_generatemissions
     (Gnattest_T: in out Test) is
      procedure GenerateMissions renames
        Wrap_Test_GenerateMissions_2a8787_14c74a;
--  end read only

      pragma Unreferenced(Gnattest_T);
      BaseIndex: constant Natural :=
        SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).BaseIndex;

   begin

      SkyBases(BaseIndex).MissionsDate := (others => 0);
      GenerateMissions;
      Assert(True, "This test can only crash.");

--  begin read only
   end Test_GenerateMissions_test_generatemissions;
--  end read only

--  begin read only
   procedure Wrap_Test_AcceptMission_979505_57ce38(MissionIndex: Positive) is
   begin
      begin
         pragma Assert(True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(missions.ads:0):Test_AcceptMission test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Missions.AcceptMission
        (MissionIndex);
      begin
         pragma Assert(True);
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
   procedure Test_AcceptMission_test_acceptmission(Gnattest_T: in out Test);
   procedure Test_AcceptMission_979505_57ce38(Gnattest_T: in out Test) renames
     Test_AcceptMission_test_acceptmission;
--  id:2.2/9795058c0b298911/AcceptMission/1/0/test_acceptmission/
   procedure Test_AcceptMission_test_acceptmission(Gnattest_T: in out Test) is
      procedure AcceptMission(MissionIndex: Positive) renames
        Wrap_Test_AcceptMission_979505_57ce38;
--  end read only

      pragma Unreferenced(Gnattest_T);

      BaseIndex: constant Positive :=
        SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).BaseIndex;
      MissionIndex: Positive :=
        Positive(SkyBases(BaseIndex).Missions.Length + 1);
   begin

      while MissionIndex > Natural(SkyBases(BaseIndex).Missions.Length) loop
         for I in SkyBases(BaseIndex).Missions.Iterate loop
            if SkyBases(BaseIndex).Missions(I).MType = Explore or
              SkyBases(BaseIndex).Missions(I).MType = Patrol or
              SkyBases(BaseIndex).Missions(I).MType = Destroy then
               MissionIndex := Mission_Container.To_Index(I);
               exit;
            end if;
         end loop;
         if MissionIndex > Natural(SkyBases(BaseIndex).Missions.Length) then
            SkyBases(BaseIndex).MissionsDate := (others => 0);
            GenerateMissions;
            MissionIndex := Positive(SkyBases(BaseIndex).Missions.Length + 1);
         end if;
      end loop;
      AcceptMission(MissionIndex);
      Assert(AcceptedMissions.Length = 1, "Accepting mission failed.");

--  begin read only
   end Test_AcceptMission_test_acceptmission;
--  end read only

--  begin read only
   procedure Wrap_Test_UpdateMissions_b5358e_60a195(Minutes: Positive) is
   begin
      GNATtest_Generated.GNATtest_Standard.Missions.UpdateMissions(Minutes);
   end Wrap_Test_UpdateMissions_b5358e_60a195;
--  end read only

--  begin read only
   procedure Test_UpdateMissions_test_updatemissions(Gnattest_T: in out Test);
   procedure Test_UpdateMissions_b5358e_60a195(Gnattest_T: in out Test) renames
     Test_UpdateMissions_test_updatemissions;
--  id:2.2/b5358ee94cb1cec0/UpdateMissions/1/0/test_updatemissions/
   procedure Test_UpdateMissions_test_updatemissions
     (Gnattest_T: in out Test) is
      procedure UpdateMissions(Minutes: Positive) renames
        Wrap_Test_UpdateMissions_b5358e_60a195;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      AcceptedMissions.Clear;
      AcceptedMissions.Append
        ((MType => Explore, Time => 10, TargetX => 1, TargetY => 1,
          Reward => 1, StartBase => 1, Finished => True, Multiplier => 0.0,
          Target => 0));
      UpdateMissions(8);
      Assert(AcceptedMissions(1).Time = 2, "Missions wrongly updated.");
      UpdateMissions(2);
      Assert(AcceptedMissions.Length = 0, "Missions not removed after update");

--  begin read only
   end Test_UpdateMissions_test_updatemissions;
--  end read only

--  begin read only
   procedure Wrap_Test_FinishMission_c82383_b2ab56(MissionIndex: Positive) is
   begin
      begin
         pragma Assert(MissionIndex <= AcceptedMissions.Last_Index);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(missions.ads:0):Test_FinishMission test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Missions.FinishMission
        (MissionIndex);
      begin
         pragma Assert(True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(missions.ads:0:):Test_FinishMission test commitment violated");
      end;
   end Wrap_Test_FinishMission_c82383_b2ab56;
--  end read only

--  begin read only
   procedure Test_FinishMission_test_finishmission(Gnattest_T: in out Test);
   procedure Test_FinishMission_c82383_b2ab56(Gnattest_T: in out Test) renames
     Test_FinishMission_test_finishmission;
--  id:2.2/c823837fea6a8759/FinishMission/1/0/test_finishmission/
   procedure Test_FinishMission_test_finishmission(Gnattest_T: in out Test) is
      procedure FinishMission(MissionIndex: Positive) renames
        Wrap_Test_FinishMission_c82383_b2ab56;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      AcceptedMissions.Clear;
      AcceptedMissions.Append
        ((MType => Explore, Time => 10, TargetX => 1, TargetY => 1,
          Reward => 1, StartBase => 1, Finished => True, Multiplier => 0.0,
          Target => 0));
      FinishMission(1);
      Assert(AcceptedMissions.Length = 0, "Mission not finished correctly.");

--  begin read only
   end Test_FinishMission_test_finishmission;
--  end read only

--  begin read only
   procedure Wrap_Test_DeleteMission_4bf0c5_8b646f
     (MissionIndex: Positive; Failed: Boolean := True) is
   begin
      begin
         pragma Assert(MissionIndex <= AcceptedMissions.Last_Index);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(missions.ads:0):Test_DeleteMission test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Missions.DeleteMission
        (MissionIndex, Failed);
      begin
         pragma Assert(True);
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
   procedure Test_DeleteMission_test_deletemission(Gnattest_T: in out Test);
   procedure Test_DeleteMission_4bf0c5_8b646f(Gnattest_T: in out Test) renames
     Test_DeleteMission_test_deletemission;
--  id:2.2/4bf0c536f42cefa3/DeleteMission/1/0/test_deletemission/
   procedure Test_DeleteMission_test_deletemission(Gnattest_T: in out Test) is
      procedure DeleteMission
        (MissionIndex: Positive; Failed: Boolean := True) renames
        Wrap_Test_DeleteMission_4bf0c5_8b646f;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      AcceptedMissions.Clear;
      AcceptedMissions.Append
        ((MType => Explore, Time => 1, TargetX => 1, TargetY => 1, Reward => 1,
          StartBase => 1, Finished => True, Multiplier => 0.0, Target => 0));
      DeleteMission(1, False);
      Assert
        (AcceptedMissions.Length = 0,
         "Failed delete mission with 0 money reward.");

--  begin read only
   end Test_DeleteMission_test_deletemission;
--  end read only

--  begin read only
   procedure Wrap_Test_UpdateMission_06efd0_8b6bc6(MissionIndex: Positive) is
   begin
      begin
         pragma Assert(MissionIndex <= AcceptedMissions.Last_Index);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(missions.ads:0):Test_UpdateMission test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Missions.UpdateMission
        (MissionIndex);
      begin
         pragma Assert(True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(missions.ads:0:):Test_UpdateMission test commitment violated");
      end;
   end Wrap_Test_UpdateMission_06efd0_8b6bc6;
--  end read only

--  begin read only
   procedure Test_UpdateMission_test_updatemission(Gnattest_T: in out Test);
   procedure Test_UpdateMission_06efd0_8b6bc6(Gnattest_T: in out Test) renames
     Test_UpdateMission_test_updatemission;
--  id:2.2/06efd0aaaa7e1e74/UpdateMission/1/0/test_updatemission/
   procedure Test_UpdateMission_test_updatemission(Gnattest_T: in out Test) is
      procedure UpdateMission(MissionIndex: Positive) renames
        Wrap_Test_UpdateMission_06efd0_8b6bc6;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      AcceptedMissions.Clear;
      AcceptedMissions.Append
        ((MType => Explore, Time => 1, TargetX => 1, TargetY => 1, Reward => 1,
          StartBase => 1, Finished => True, Multiplier => 0.0, Target => 0));
      UpdateMission(1);
      Assert(AcceptedMissions(1).Finished, "Failed to update mission.");

--  begin read only
   end Test_UpdateMission_test_updatemission;
--  end read only

--  begin read only
   function Wrap_Test_AutoFinishMissions_ca7126_527254 return String is
   begin
      declare
         Test_AutoFinishMissions_ca7126_527254_Result: constant String :=
           GNATtest_Generated.GNATtest_Standard.Missions.AutoFinishMissions;
      begin
         return Test_AutoFinishMissions_ca7126_527254_Result;
      end;
   end Wrap_Test_AutoFinishMissions_ca7126_527254;
--  end read only

--  begin read only
   procedure Test_AutoFinishMissions_test_autofinishmissions
     (Gnattest_T: in out Test);
   procedure Test_AutoFinishMissions_ca7126_527254
     (Gnattest_T: in out Test) renames
     Test_AutoFinishMissions_test_autofinishmissions;
--  id:2.2/ca7126890331fcb0/AutoFinishMissions/1/0/test_autofinishmissions/
   procedure Test_AutoFinishMissions_test_autofinishmissions
     (Gnattest_T: in out Test) is
      function AutoFinishMissions return String renames
        Wrap_Test_AutoFinishMissions_ca7126_527254;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      AcceptedMissions.Clear;
      AcceptedMissions.Append
        ((MType => Explore, Time => 1, TargetX => 1, TargetY => 1, Reward => 1,
          StartBase => 1, Finished => True, Multiplier => 0.0, Target => 0));
      Assert(AutoFinishMissions'Length = 0, "Can't autoupdate missions.");

--  begin read only
   end Test_AutoFinishMissions_test_autofinishmissions;
--  end read only

--  begin read only
   function Wrap_Test_Get_Mission_Type_0b70ab_fb18a6
     (MType: Missions_Types) return String is
   begin
      begin
         pragma Assert(True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(missions.ads:0):Test_Get_Mission_Type test requirement violated");
      end;
      declare
         Test_Get_Mission_Type_0b70ab_fb18a6_Result: constant String :=
           GNATtest_Generated.GNATtest_Standard.Missions.Get_Mission_Type
             (MType);
      begin
         begin
            pragma Assert
              (Test_Get_Mission_Type_0b70ab_fb18a6_Result'Length > 0);
            null;
         exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "ens_sloc(missions.ads:0:):Test_Get_Mission_Type test commitment violated");
         end;
         return Test_Get_Mission_Type_0b70ab_fb18a6_Result;
      end;
   end Wrap_Test_Get_Mission_Type_0b70ab_fb18a6;
--  end read only

--  begin read only
   procedure Test_Get_Mission_Type_test_get_mission_type
     (Gnattest_T: in out Test);
   procedure Test_Get_Mission_Type_0b70ab_fb18a6
     (Gnattest_T: in out Test) renames
     Test_Get_Mission_Type_test_get_mission_type;
--  id:2.2/0b70abad8e94f349/Get_Mission_Type/1/0/test_get_mission_type/
   procedure Test_Get_Mission_Type_test_get_mission_type
     (Gnattest_T: in out Test) is
      function Get_Mission_Type(MType: Missions_Types) return String renames
        Wrap_Test_Get_Mission_Type_0b70ab_fb18a6;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Assert
        (Get_Mission_Type(Patrol) = "Patrol area",
         "Failed to get the name of the selected mission type.");

--  begin read only
   end Test_Get_Mission_Type_test_get_mission_type;
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
