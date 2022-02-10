--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Stories.Test_Data.

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
package body Stories.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only
--  begin read only
   procedure Wrap_Test_Start_Story_542c86_eea028
     (Faction_Name: Tiny_String.Bounded_String;
      Condition: Start_Condition_Type) is
   begin
      begin
         pragma Assert(True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(stories.ads:0):Test_StartStory test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Stories.Start_Story
        (Faction_Name, Condition);
      begin
         pragma Assert(True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(stories.ads:0:):Test_StartStory test commitment violated");
      end;
   end Wrap_Test_Start_Story_542c86_eea028;
--  end read only

--  begin read only
   procedure Test_Start_Story_test_startstory(Gnattest_T: in out Test);
   procedure Test_Start_Story_542c86_eea028(Gnattest_T: in out Test) renames
     Test_Start_Story_test_startstory;
--  id:2.2/542c867b03c55aa2/Start_Story/1/0/test_startstory/
   procedure Test_Start_Story_test_startstory(Gnattest_T: in out Test) is
      procedure Start_Story
        (Faction_Name: Tiny_String.Bounded_String;
         Condition: Start_Condition_Type) renames
        Wrap_Test_Start_Story_542c86_eea028;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      loop
         Start_Story(Tiny_String.To_Bounded_String("Undead"), DROPITEM);
         exit when Current_Story.Index /= Null_Unbounded_String;
      end loop;
      Assert(True, "This test can only crash or hang.");

--  begin read only
   end Test_Start_Story_test_startstory;
--  end read only

--  begin read only
   procedure Wrap_Test_Clear_Current_Story_40d661_0d61b8 is
   begin
      begin
         pragma Assert(True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(stories.ads:0):Test_ClearCurrentStory test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Stories.Clear_Current_Story;
      begin
         pragma Assert(Current_Story.Index = Null_Unbounded_String);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(stories.ads:0:):Test_ClearCurrentStory test commitment violated");
      end;
   end Wrap_Test_Clear_Current_Story_40d661_0d61b8;
--  end read only

--  begin read only
   procedure Test_Clear_Current_Story_test_clearcurrentstory
     (Gnattest_T: in out Test);
   procedure Test_Clear_Current_Story_40d661_0d61b8
     (Gnattest_T: in out Test) renames
     Test_Clear_Current_Story_test_clearcurrentstory;
--  id:2.2/40d66181022dcc4f/Clear_Current_Story/1/0/test_clearcurrentstory/
   procedure Test_Clear_Current_Story_test_clearcurrentstory
     (Gnattest_T: in out Test) is
      procedure Clear_Current_Story renames
        Wrap_Test_Clear_Current_Story_40d661_0d61b8;
--  end read only

      pragma Unreferenced(Gnattest_T);
      OldStory: constant Current_Story_Data := Current_Story;

   begin

      Clear_Current_Story;
      Assert
        (Current_Story.Index = Null_Unbounded_String,
         "Failed to clear current story.");
      Current_Story := OldStory;

--  begin read only
   end Test_Clear_Current_Story_test_clearcurrentstory;
--  end read only

--  begin read only
   function Wrap_Test_Progress_Story_323e5c_14aed6
     (Next_Step: Boolean := False) return Boolean is
   begin
      declare
         Test_Progress_Story_323e5c_14aed6_Result: constant Boolean :=
           GNATtest_Generated.GNATtest_Standard.Stories.Progress_Story
             (Next_Step);
      begin
         return Test_Progress_Story_323e5c_14aed6_Result;
      end;
   end Wrap_Test_Progress_Story_323e5c_14aed6;
--  end read only

--  begin read only
   procedure Test_Progress_Story_test_progressstory(Gnattest_T: in out Test);
   procedure Test_Progress_Story_323e5c_14aed6(Gnattest_T: in out Test) renames
     Test_Progress_Story_test_progressstory;
--  id:2.2/323e5ccfda3d5599/Progress_Story/1/0/test_progressstory/
   procedure Test_Progress_Story_test_progressstory(Gnattest_T: in out Test) is
      function Progress_Story
        (Next_Step: Boolean := False) return Boolean renames
        Wrap_Test_Progress_Story_323e5c_14aed6;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      if Progress_Story then
         null;
      end if;
      Assert(True, "This test can only crash.");

--  begin read only
   end Test_Progress_Story_test_progressstory;
--  end read only

--  begin read only
   function Wrap_Test_Get_Current_Story_Text_893370_8f71b5
      return Unbounded_String is
   begin
      declare
         Test_Get_Current_Story_Text_893370_8f71b5_Result: constant Unbounded_String :=
           GNATtest_Generated.GNATtest_Standard.Stories.Get_Current_Story_Text;
      begin
         return Test_Get_Current_Story_Text_893370_8f71b5_Result;
      end;
   end Wrap_Test_Get_Current_Story_Text_893370_8f71b5;
--  end read only

--  begin read only
   procedure Test_Get_Current_Story_Text_tets_getcurrentstorytext
     (Gnattest_T: in out Test);
   procedure Test_Get_Current_Story_Text_893370_8f71b5
     (Gnattest_T: in out Test) renames
     Test_Get_Current_Story_Text_tets_getcurrentstorytext;
--  id:2.2/89337066f7e37ccc/Get_Current_Story_Text/1/0/tets_getcurrentstorytext/
   procedure Test_Get_Current_Story_Text_tets_getcurrentstorytext
     (Gnattest_T: in out Test) is
      function Get_Current_Story_Text return Unbounded_String renames
        Wrap_Test_Get_Current_Story_Text_893370_8f71b5;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Assert
        (Get_Current_Story_Text /= Null_Unbounded_String,
         "Failed to get text of current story step.");

--  begin read only
   end Test_Get_Current_Story_Text_tets_getcurrentstorytext;
--  end read only

--  begin read only
   function Wrap_Test_Get_Step_Data_551f39_456123
     (Finish_Data: StepData_Container.Vector; Name: String)
      return Unbounded_String is
   begin
      begin
         pragma Assert(Name'Length > 0);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(stories.ads:0):Test_GetStepData test requirement violated");
      end;
      declare
         Test_Get_Step_Data_551f39_456123_Result: constant Unbounded_String :=
           GNATtest_Generated.GNATtest_Standard.Stories.Get_Step_Data
             (Finish_Data, Name);
      begin
         begin
            pragma Assert(True);
            null;
         exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "ens_sloc(stories.ads:0:):Test_GetStepData test commitment violated");
         end;
         return Test_Get_Step_Data_551f39_456123_Result;
      end;
   end Wrap_Test_Get_Step_Data_551f39_456123;
--  end read only

--  begin read only
   procedure Test_Get_Step_Data_test_getstepdata(Gnattest_T: in out Test);
   procedure Test_Get_Step_Data_551f39_456123(Gnattest_T: in out Test) renames
     Test_Get_Step_Data_test_getstepdata;
--  id:2.2/551f39a9b78daf7d/Get_Step_Data/1/0/test_getstepdata/
   procedure Test_Get_Step_Data_test_getstepdata(Gnattest_T: in out Test) is
      function Get_Step_Data
        (Finish_Data: StepData_Container.Vector; Name: String)
         return Unbounded_String renames
        Wrap_Test_Get_Step_Data_551f39_456123;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Assert
        (Get_Step_Data
           (Stories_List(To_Unbounded_String("1")).Steps(1).Finish_Data,
            "condition") =
         To_Unbounded_String("Rhetoric"),
         "Failed to get finish data of selected step.");
      Assert
        (Get_Step_Data
           (Stories_List(To_Unbounded_String("1")).Steps(1).Finish_Data,
            "sdfdsf") =
         Null_Unbounded_String,
         "Failed to not get non existing finish data of selected step.");

--  begin read only
   end Test_Get_Step_Data_test_getstepdata;
--  end read only

--  begin read only
   procedure Wrap_Test_Get_Story_Location_5f0f68_b0f396
     (Story_X: out Map_X_Range; Story_Y: out Map_Y_Range) is
   begin
      GNATtest_Generated.GNATtest_Standard.Stories.Get_Story_Location
        (Story_X, Story_Y);
   end Wrap_Test_Get_Story_Location_5f0f68_b0f396;
--  end read only

--  begin read only
   procedure Test_Get_Story_Location_test_getstorylocation
     (Gnattest_T: in out Test);
   procedure Test_Get_Story_Location_5f0f68_b0f396
     (Gnattest_T: in out Test) renames
     Test_Get_Story_Location_test_getstorylocation;
--  id:2.2/5f0f6843b5d21e88/Get_Story_Location/1/0/test_getstorylocation/
   procedure Test_Get_Story_Location_test_getstorylocation
     (Gnattest_T: in out Test) is
      procedure Get_Story_Location
        (Story_X: out Map_X_Range; Story_Y: out Map_Y_Range) renames
        Wrap_Test_Get_Story_Location_5f0f68_b0f396;
--  end read only

      pragma Unreferenced(Gnattest_T);
      X, Y: Positive := 1;

   begin

      Get_Story_Location(X, Y);
      Assert(True, "This test can only crash.");

--  begin read only
   end Test_Get_Story_Location_test_getstorylocation;
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
end Stories.Test_Data.Tests;
