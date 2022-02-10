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
   procedure Wrap_Test_Generate_Missions_b0e31e_14c74a is
   begin
      GNATtest_Generated.GNATtest_Standard.Missions.Generate_Missions;
   end Wrap_Test_Generate_Missions_b0e31e_14c74a;
--  end read only

--  begin read only
   procedure Test_Generate_Missions_test_generatemissions
     (Gnattest_T: in out Test);
   procedure Test_Generate_Missions_b0e31e_14c74a
     (Gnattest_T: in out Test) renames
     Test_Generate_Missions_test_generatemissions;
--  id:2.2/b0e31efb412e501e/Generate_Missions/1/0/test_generatemissions/
   procedure Test_Generate_Missions_test_generatemissions
     (Gnattest_T: in out Test) is
      procedure Generate_Missions renames
        Wrap_Test_Generate_Missions_b0e31e_14c74a;
--  end read only

      pragma Unreferenced(Gnattest_T);
      BaseIndex: constant Natural :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;

   begin

      Sky_Bases(BaseIndex).Missions_Date := (others => 0);
      Generate_Missions;
      Assert(True, "This test can only crash.");

--  begin read only
   end Test_Generate_Missions_test_generatemissions;
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
   procedure Wrap_Test_Update_Missions_dd0e76_60a195(Minutes: Positive) is
   begin
      GNATtest_Generated.GNATtest_Standard.Missions.Update_Missions(Minutes);
   end Wrap_Test_Update_Missions_dd0e76_60a195;
--  end read only

--  begin read only
   procedure Test_Update_Missions_test_updatemissions(Gnattest_T: in out Test);
   procedure Test_Update_Missions_dd0e76_60a195
     (Gnattest_T: in out Test) renames
     Test_Update_Missions_test_updatemissions;
--  id:2.2/dd0e763fefc4ba42/Update_Missions/1/0/test_updatemissions/
   procedure Test_Update_Missions_test_updatemissions
     (Gnattest_T: in out Test) is
      procedure Update_Missions(Minutes: Positive) renames
        Wrap_Test_Update_Missions_dd0e76_60a195;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Accepted_Missions.Clear;
      Accepted_Missions.Append
        ((M_Type => EXPLORE, Time => 10, Target_X => 1, Target_Y => 1,
          Reward => 1, Start_Base => 1, Finished => True, Multiplier => 0.0,
          Target => 0));
      Update_Missions(8);
      Assert(Accepted_Missions(1).Time = 2, "Missions wrongly updated.");
      Update_Missions(2);
      Assert
        (Accepted_Missions.Length = 0, "Missions not removed after update");

--  begin read only
   end Test_Update_Missions_test_updatemissions;
--  end read only

--  begin read only
   procedure Wrap_Test_Finish_Mission_03361e_685cb8(Mission_Index: Positive) is
   begin
      begin
         pragma Assert(Mission_Index <= Accepted_Missions.Last_Index);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(missions.ads:0):Test_FinishMission test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Missions.Finish_Mission
        (Mission_Index);
      begin
         pragma Assert(True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(missions.ads:0:):Test_FinishMission test commitment violated");
      end;
   end Wrap_Test_Finish_Mission_03361e_685cb8;
--  end read only

--  begin read only
   procedure Test_Finish_Mission_test_finishmission(Gnattest_T: in out Test);
   procedure Test_Finish_Mission_03361e_685cb8(Gnattest_T: in out Test) renames
     Test_Finish_Mission_test_finishmission;
--  id:2.2/03361e25a000350a/Finish_Mission/1/0/test_finishmission/
   procedure Test_Finish_Mission_test_finishmission(Gnattest_T: in out Test) is
      procedure Finish_Mission(Mission_Index: Positive) renames
        Wrap_Test_Finish_Mission_03361e_685cb8;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Accepted_Missions.Clear;
      Accepted_Missions.Append
        ((M_Type => EXPLORE, Time => 10, Target_X => 1, Target_Y => 1,
          Reward => 1, Start_Base => 1, Finished => True, Multiplier => 0.0,
          Target => 0));
      Finish_Mission(1);
      Assert
        (Accepted_Missions.Length = 0,
         "Explore mission not finished correctly.");
      Player_Ship.Crew.Append
        ((Amount_Of_Attributes => Attributes_Amount,
          Amount_Of_Skills => Skills_Amount,
          Name => To_Unbounded_String("OTKAM-740"),
          Faction => Tiny_String.To_Bounded_String("DRONES"),
          Contract_Length => 100, others => <>));
      Accepted_Missions.Append
        ((M_Type => PASSENGER, Time => 100, Target_X => 1, Target_Y => 1,
          Reward => 1, Start_Base => 1, Finished => False, Multiplier => 0.0,
          Data => 5));
      Finish_Mission(1);
      Assert
        (Accepted_Missions.Length = 0,
         "Passenger drone mission not finished correctly.");

--  begin read only
   end Test_Finish_Mission_test_finishmission;
--  end read only

--  begin read only
   procedure Wrap_Test_Delete_Mission_29ff6f_7d785b
     (Mission_Index: Positive; Failed: Boolean := True) is
   begin
      begin
         pragma Assert(Mission_Index <= Accepted_Missions.Last_Index);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(missions.ads:0):Test_DeleteMission test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Missions.Delete_Mission
        (Mission_Index, Failed);
      begin
         pragma Assert(True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(missions.ads:0:):Test_DeleteMission test commitment violated");
      end;
   end Wrap_Test_Delete_Mission_29ff6f_7d785b;
--  end read only

--  begin read only
   procedure Test_Delete_Mission_test_deletemission(Gnattest_T: in out Test);
   procedure Test_Delete_Mission_29ff6f_7d785b(Gnattest_T: in out Test) renames
     Test_Delete_Mission_test_deletemission;
--  id:2.2/29ff6f09eab055f5/Delete_Mission/1/0/test_deletemission/
   procedure Test_Delete_Mission_test_deletemission(Gnattest_T: in out Test) is
      procedure Delete_Mission
        (Mission_Index: Positive; Failed: Boolean := True) renames
        Wrap_Test_Delete_Mission_29ff6f_7d785b;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Accepted_Missions.Clear;
      Accepted_Missions.Append
        ((M_Type => EXPLORE, Time => 1, Target_X => 1, Target_Y => 1,
          Reward => 1, Start_Base => 1, Finished => True, Multiplier => 0.0,
          Target => 0));
      Delete_Mission(1, False);
      Assert
        (Accepted_Missions.Length = 0,
         "Failed delete mission with 0 money reward.");

--  begin read only
   end Test_Delete_Mission_test_deletemission;
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
   function Wrap_Test_Get_Mission_Type_0b70ab_fb18a6
     (M_Type: Missions_Types) return String is
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
             (M_Type);
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
      function Get_Mission_Type(M_Type: Missions_Types) return String renames
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
