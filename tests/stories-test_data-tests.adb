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
