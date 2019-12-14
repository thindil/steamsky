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
   procedure Wrap_Test_StartStory_edaf80_b2037e (FactionName: Unbounded_String; Condition: StartConditionType) 
   is
   begin
      begin
         pragma Assert
           (FactionName /= Null_Unbounded_String);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(stories.ads:0):Test_StartStory test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Stories.StartStory (FactionName, Condition);
      begin
         pragma Assert
           (True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(stories.ads:0:):Test_StartStory test commitment violated");
      end;
   end Wrap_Test_StartStory_edaf80_b2037e;
--  end read only

--  begin read only
   procedure Test_StartStory_test_startstory (Gnattest_T : in out Test);
   procedure Test_StartStory_edaf80_b2037e (Gnattest_T : in out Test) renames Test_StartStory_test_startstory;
--  id:2.2/edaf80b58d7d34e7/StartStory/1/0/test_startstory/
   procedure Test_StartStory_test_startstory (Gnattest_T : in out Test) is
   procedure StartStory (FactionName: Unbounded_String; Condition: StartConditionType) renames Wrap_Test_StartStory_edaf80_b2037e;
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      loop
         StartStory(To_Unbounded_String("Undead"), DROPITEM);
         exit when CurrentStory.Index /= Null_Unbounded_String;
      end loop;
      Assert(True, "This test can only crash or hang.");

--  begin read only
   end Test_StartStory_test_startstory;
--  end read only

--  begin read only
   procedure Wrap_Test_ClearCurrentStory_0648d1_ff8276
   is
   begin
      GNATtest_Generated.GNATtest_Standard.Stories.ClearCurrentStory;
   end Wrap_Test_ClearCurrentStory_0648d1_ff8276;
--  end read only

--  begin read only
   procedure Test_ClearCurrentStory_test_clearcurrentstory (Gnattest_T : in out Test);
   procedure Test_ClearCurrentStory_0648d1_ff8276 (Gnattest_T : in out Test) renames Test_ClearCurrentStory_test_clearcurrentstory;
--  id:2.2/0648d16dba1bb959/ClearCurrentStory/1/0/test_clearcurrentstory/
   procedure Test_ClearCurrentStory_test_clearcurrentstory (Gnattest_T : in out Test) is
   procedure ClearCurrentStory renames Wrap_Test_ClearCurrentStory_0648d1_ff8276;
--  end read only

      pragma Unreferenced (Gnattest_T);
      OldStory: constant CurrentStory_Data := CurrentStory;

   begin

      ClearCurrentStory;
      Assert(CurrentStory.Index = Null_Unbounded_String, "Failed to clear current story.");
      CurrentStory := OldStory;

--  begin read only
   end Test_ClearCurrentStory_test_clearcurrentstory;
--  end read only

--  begin read only
   function Wrap_Test_ProgressStory_80c408_14aed6 (NextStep: Boolean := False)  return Boolean
   is
   begin
      declare
         Test_ProgressStory_80c408_14aed6_Result : constant Boolean := GNATtest_Generated.GNATtest_Standard.Stories.ProgressStory (NextStep);
      begin
         return Test_ProgressStory_80c408_14aed6_Result;
      end;
   end Wrap_Test_ProgressStory_80c408_14aed6;
--  end read only

--  begin read only
   procedure Test_ProgressStory_test_progressstory (Gnattest_T : in out Test);
   procedure Test_ProgressStory_80c408_14aed6 (Gnattest_T : in out Test) renames Test_ProgressStory_test_progressstory;
--  id:2.2/80c4088c0068e59a/ProgressStory/1/0/test_progressstory/
   procedure Test_ProgressStory_test_progressstory (Gnattest_T : in out Test) is
      function ProgressStory (NextStep: Boolean := False) return Boolean renames Wrap_Test_ProgressStory_80c408_14aed6;
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      if ProgressStory then
         null;
      end if;
      Assert(True, "This test can only crash.");

--  begin read only
   end Test_ProgressStory_test_progressstory;
--  end read only

--  begin read only
   function Wrap_Test_GetCurrentStoryText_b9136f_8f71b5 return Unbounded_String
   is
   begin
      declare
         Test_GetCurrentStoryText_b9136f_8f71b5_Result : constant Unbounded_String := GNATtest_Generated.GNATtest_Standard.Stories.GetCurrentStoryText;
      begin
         return Test_GetCurrentStoryText_b9136f_8f71b5_Result;
      end;
   end Wrap_Test_GetCurrentStoryText_b9136f_8f71b5;
--  end read only

--  begin read only
   procedure Test_GetCurrentStoryText_tets_getcurrentstorytext (Gnattest_T : in out Test);
   procedure Test_GetCurrentStoryText_b9136f_8f71b5 (Gnattest_T : in out Test) renames Test_GetCurrentStoryText_tets_getcurrentstorytext;
--  id:2.2/b9136fdf6bb9efe6/GetCurrentStoryText/1/0/tets_getcurrentstorytext/
   procedure Test_GetCurrentStoryText_tets_getcurrentstorytext (Gnattest_T : in out Test) is
      function GetCurrentStoryText return Unbounded_String renames Wrap_Test_GetCurrentStoryText_b9136f_8f71b5;
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      Assert(GetCurrentStoryText /= Null_Unbounded_String, "Failed to get text of current story step.");

--  begin read only
   end Test_GetCurrentStoryText_tets_getcurrentstorytext;
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
