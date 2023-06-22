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
   procedure Wrap_Test_Accept_Mission_1d4ba5_57ce38(Mission_Index: Positive) is
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
      GNATtest_Generated.GNATtest_Standard.Missions.Accept_Mission
        (Mission_Index);
      begin
         pragma Assert(True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(missions.ads:0:):Test_AcceptMission test commitment violated");
      end;
   end Wrap_Test_Accept_Mission_1d4ba5_57ce38;
--  end read only

--  begin read only
   procedure Test_Accept_Mission_test_acceptmission(Gnattest_T: in out Test);
   procedure Test_Accept_Mission_1d4ba5_57ce38(Gnattest_T: in out Test) renames
     Test_Accept_Mission_test_acceptmission;
--  id:2.2/1d4ba5d3a7e6c9e7/Accept_Mission/1/0/test_acceptmission/
   procedure Test_Accept_Mission_test_acceptmission(Gnattest_T: in out Test) is
      procedure Accept_Mission(Mission_Index: Positive) renames
        Wrap_Test_Accept_Mission_1d4ba5_57ce38;
--  end read only

      pragma Unreferenced(Gnattest_T);
      BaseIndex: constant Positive :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      MissionIndex: Positive :=
        Positive(Sky_Bases(BaseIndex).Missions.Length + 1);

   begin

      while MissionIndex > Natural(Sky_Bases(BaseIndex).Missions.Length) loop
         for I in Sky_Bases(BaseIndex).Missions.Iterate loop
            if Sky_Bases(BaseIndex).Missions(I).M_Type = EXPLORE or
              Sky_Bases(BaseIndex).Missions(I).M_Type = PATROL or
              Sky_Bases(BaseIndex).Missions(I).M_Type = DESTROY then
               MissionIndex := Mission_Container.To_Index(I);
               exit;
            end if;
         end loop;
         if MissionIndex > Natural(Sky_Bases(BaseIndex).Missions.Length) then
            Sky_Bases(BaseIndex).Missions_Date := (others => 0);
            Generate_Missions;
            MissionIndex := Positive(Sky_Bases(BaseIndex).Missions.Length + 1);
         end if;
      end loop;
      Accept_Mission(MissionIndex);
      Assert(Accepted_Missions.Length = 1, "Accepting mission failed.");

--  begin read only
   end Test_Accept_Mission_test_acceptmission;
--  end read only

--  begin read only
   procedure Wrap_Test_Update_Mission_edbb9b_cfdd9a(Mission_Index: Positive) is
   begin
      begin
         pragma Assert(Mission_Index <= Accepted_Missions.Last_Index);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(missions.ads:0):Test_UpdateMission test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Missions.Update_Mission
        (Mission_Index);
      begin
         pragma Assert(True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(missions.ads:0:):Test_UpdateMission test commitment violated");
      end;
   end Wrap_Test_Update_Mission_edbb9b_cfdd9a;
--  end read only

--  begin read only
   procedure Test_Update_Mission_test_updatemission(Gnattest_T: in out Test);
   procedure Test_Update_Mission_edbb9b_cfdd9a(Gnattest_T: in out Test) renames
     Test_Update_Mission_test_updatemission;
--  id:2.2/edbb9b4c3b273460/Update_Mission/1/0/test_updatemission/
   procedure Test_Update_Mission_test_updatemission(Gnattest_T: in out Test) is
      procedure Update_Mission(Mission_Index: Positive) renames
        Wrap_Test_Update_Mission_edbb9b_cfdd9a;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Accepted_Missions.Clear;
      Accepted_Missions.Append
        ((M_Type => EXPLORE, Time => 1, Target_X => 1, Target_Y => 1,
          Reward => 1, Start_Base => 1, Finished => True, Multiplier => 0.0,
          Target => 0));
      Update_Mission(1);
      Assert(Accepted_Missions(1).Finished, "Failed to update mission.");

--  begin read only
   end Test_Update_Mission_test_updatemission;
--  end read only

--  begin read only
   function Wrap_Test_Auto_Finish_Missions_3ccc57_527254 return String is
   begin
      declare
         Test_Auto_Finish_Missions_3ccc57_527254_Result: constant String :=
           GNATtest_Generated.GNATtest_Standard.Missions.Auto_Finish_Missions;
      begin
         return Test_Auto_Finish_Missions_3ccc57_527254_Result;
      end;
   end Wrap_Test_Auto_Finish_Missions_3ccc57_527254;
--  end read only

--  begin read only
   procedure Test_Auto_Finish_Missions_test_autofinishmissions
     (Gnattest_T: in out Test);
   procedure Test_Auto_Finish_Missions_3ccc57_527254
     (Gnattest_T: in out Test) renames
     Test_Auto_Finish_Missions_test_autofinishmissions;
--  id:2.2/3ccc575cde3b6889/Auto_Finish_Missions/1/0/test_autofinishmissions/
   procedure Test_Auto_Finish_Missions_test_autofinishmissions
     (Gnattest_T: in out Test) is
      function Auto_Finish_Missions return String renames
        Wrap_Test_Auto_Finish_Missions_3ccc57_527254;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Accepted_Missions.Clear;
      Accepted_Missions.Append
        ((M_Type => EXPLORE, Time => 1, Target_X => 1, Target_Y => 1,
          Reward => 1, Start_Base => 1, Finished => True, Multiplier => 0.0,
          Target => 0));
      Assert(Auto_Finish_Missions'Length = 0, "Can't autoupdate missions.");

--  begin read only
   end Test_Auto_Finish_Missions_test_autofinishmissions;
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
