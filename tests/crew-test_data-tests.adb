--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Crew.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only

with Config; use Config;
with Ships; use Ships;

--  begin read only
--  end read only
package body Crew.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only
--  begin read only
   procedure Wrap_Test_Gain_Exp_2f784d_7ccf15
     (Amount: Natural; Skill_Number: Skills_Amount_Range;
      Crew_Index: Positive) is
   begin
      begin
         pragma Assert(Skill_Number <= Skills_Amount);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(crew.ads:0):Test_GainExp test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Crew.Gain_Exp
        (Amount, Skill_Number, Crew_Index);
      begin
         pragma Assert(True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(crew.ads:0:):Test_GainExp test commitment violated");
      end;
   end Wrap_Test_Gain_Exp_2f784d_7ccf15;
--  end read only

--  begin read only
   procedure Test_Gain_Exp_test_gainexp(Gnattest_T: in out Test);
   procedure Test_Gain_Exp_2f784d_7ccf15(Gnattest_T: in out Test) renames
     Test_Gain_Exp_test_gainexp;
--  id:2.2/2f784d3d0239060c/Gain_Exp/1/0/test_gainexp/
   procedure Test_Gain_Exp_test_gainexp(Gnattest_T: in out Test) is
      procedure Gain_Exp
        (Amount: Natural; Skill_Number: Skills_Amount_Range;
         Crew_Index: Positive) renames
        Wrap_Test_Gain_Exp_2f784d_7ccf15;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Gain_Exp(1, 4, 1);
      Assert
        (Skills_Container.Element
           (Container => Player_Ship.Crew(1).Skills, Index => 1)
           .Experience =
         8,
         "Failed to gain experience in skill.");

--  begin read only
   end Test_Gain_Exp_test_gainexp;
--  end read only

--  begin read only
   function Wrap_Test_Find_Cabin_6a58e0_006804
     (Member_Index: Positive) return Natural is
   begin
      declare
         Test_Find_Cabin_6a58e0_006804_Result: constant Natural :=
           GNATtest_Generated.GNATtest_Standard.Crew.Find_Cabin(Member_Index);
      begin
         return Test_Find_Cabin_6a58e0_006804_Result;
      end;
   end Wrap_Test_Find_Cabin_6a58e0_006804;
--  end read only

--  begin read only
   procedure Test_Find_Cabin_test_findcabin(Gnattest_T: in out Test);
   procedure Test_Find_Cabin_6a58e0_006804(Gnattest_T: in out Test) renames
     Test_Find_Cabin_test_findcabin;
--  id:2.2/6a58e0936b2f4107/Find_Cabin/1/0/test_findcabin/
   procedure Test_Find_Cabin_test_findcabin(Gnattest_T: in out Test) is
      function Find_Cabin(Member_Index: Positive) return Natural renames
        Wrap_Test_Find_Cabin_6a58e0_006804;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Assert
        (Find_Cabin(1) > 0, "Failed to find cabin for existing crew member.");
      Assert
        (Find_Cabin(100) = 0,
         "Failed to not find cabin for non existing crew member.");

--  begin read only
   end Test_Find_Cabin_test_findcabin;
--  end read only

--  begin read only
   procedure Wrap_Test_Update_Crew_f9e489_011eae
     (Minutes: Positive; Tired_Points: Natural; In_Combat: Boolean := False) is
   begin
      GNATtest_Generated.GNATtest_Standard.Crew.Update_Crew
        (Minutes, Tired_Points, In_Combat);
   end Wrap_Test_Update_Crew_f9e489_011eae;
--  end read only

--  begin read only
   procedure Test_Update_Crew_test_updatecrew(Gnattest_T: in out Test);
   procedure Test_Update_Crew_f9e489_011eae(Gnattest_T: in out Test) renames
     Test_Update_Crew_test_updatecrew;
--  id:2.2/f9e4895418584c1b/Update_Crew/1/0/test_updatecrew/
   procedure Test_Update_Crew_test_updatecrew(Gnattest_T: in out Test) is
      procedure Update_Crew
        (Minutes: Positive; Tired_Points: Natural;
         In_Combat: Boolean := False) renames
        Wrap_Test_Update_Crew_f9e489_011eae;
--  end read only

      pragma Unreferenced(Gnattest_T);
      use Tiny_String;

   begin

      Update_Crew(1, 1);
      Player_Ship.Crew(1).Health := 0;
      Update_Crew(1, 1);
      New_Game_Settings.Player_Faction := To_Bounded_String("POLEIS");
      New_Game_Settings.Player_Career := To_Unbounded_String("general");
      New_Game_Settings.Starting_Base := To_Bounded_String("1");
      New_Game;
      Assert(True, "This tests can only crash.");

--  begin read only
   end Test_Update_Crew_test_updatecrew;
--  end read only

--  begin read only
   procedure Wrap_Test_Wait_For_Rest_2f3023_b046aa is
   begin
      GNATtest_Generated.GNATtest_Standard.Crew.Wait_For_Rest;
   end Wrap_Test_Wait_For_Rest_2f3023_b046aa;
--  end read only

--  begin read only
   procedure Test_Wait_For_Rest_test_waitforrest(Gnattest_T: in out Test);
   procedure Test_Wait_For_Rest_2f3023_b046aa(Gnattest_T: in out Test) renames
     Test_Wait_For_Rest_test_waitforrest;
--  id:2.2/2f30237da88c91e8/Wait_For_Rest/1/0/test_waitforrest/
   procedure Test_Wait_For_Rest_test_waitforrest(Gnattest_T: in out Test) is
      procedure Wait_For_Rest renames Wrap_Test_Wait_For_Rest_2f3023_b046aa;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Wait_For_Rest;
      Assert(True, "This test can only crash.");

--  begin read only
   end Test_Wait_For_Rest_test_waitforrest;
--  end read only

--  begin read only
   function Wrap_Test_Get_Skill_Level_Name_ac7e2a_35c4c0
     (Skill_Level: Skill_Range) return String is
   begin
      begin
         pragma Assert(True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(crew.ads:0):Test_GetSkillLevelName test requirement violated");
      end;
      declare
         Test_Get_Skill_Level_Name_ac7e2a_35c4c0_Result: constant String :=
           GNATtest_Generated.GNATtest_Standard.Crew.Get_Skill_Level_Name
             (Skill_Level);
      begin
         begin
            pragma Assert(True);
            null;
         exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "ens_sloc(crew.ads:0:):Test_GetSkillLevelName test commitment violated");
         end;
         return Test_Get_Skill_Level_Name_ac7e2a_35c4c0_Result;
      end;
   end Wrap_Test_Get_Skill_Level_Name_ac7e2a_35c4c0;
--  end read only

--  begin read only
   procedure Test_Get_Skill_Level_Name_test_getskilllevelname
     (Gnattest_T: in out Test);
   procedure Test_Get_Skill_Level_Name_ac7e2a_35c4c0
     (Gnattest_T: in out Test) renames
     Test_Get_Skill_Level_Name_test_getskilllevelname;
--  id:2.2/ac7e2a886c376900/Get_Skill_Level_Name/1/0/test_getskilllevelname/
   procedure Test_Get_Skill_Level_Name_test_getskilllevelname
     (Gnattest_T: in out Test) is
      function Get_Skill_Level_Name
        (Skill_Level: Skill_Range) return String renames
        Wrap_Test_Get_Skill_Level_Name_ac7e2a_35c4c0;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Game_Settings.Show_Numbers := False;
      Assert
        (Get_Skill_Level_Name(9) = "Beginner",
         "Failed to get skill level name for level 9");
      Assert
        (Get_Skill_Level_Name(54) = "Respected",
         "Failed to get skill level name for level 54");
      Assert
        (Get_Skill_Level_Name(92) = "Legendary",
         "Failed to get skill level name for level 92");
      Game_Settings.Show_Numbers := True;
      Assert
        (Get_Skill_Level_Name(9) = " 9",
         "Failed to get skill level name for level 9 (numeric)");
      Assert
        (Get_Skill_Level_Name(54) = " 54",
         "Failed to get skill level name for level 54 (numeric)");
      Assert
        (Get_Skill_Level_Name(92) = " 92",
         "Failed to get skill level name for level 92 (numeric)");

--  begin read only
   end Test_Get_Skill_Level_Name_test_getskilllevelname;
--  end read only

--  begin read only
   function Wrap_Test_Get_Attribute_Level_Name_e6c169_3af3bf
     (Attribute_Level: Positive) return String is
   begin
      begin
         pragma Assert(Attribute_Level <= 50);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(crew.ads:0):Test_GetAttributeLevelName test requirement violated");
      end;
      declare
         Test_Get_Attribute_Level_Name_e6c169_3af3bf_Result: constant String :=
           GNATtest_Generated.GNATtest_Standard.Crew.Get_Attribute_Level_Name
             (Attribute_Level);
      begin
         begin
            pragma Assert(True);
            null;
         exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "ens_sloc(crew.ads:0:):Test_GetAttributeLevelName test commitment violated");
         end;
         return Test_Get_Attribute_Level_Name_e6c169_3af3bf_Result;
      end;
   end Wrap_Test_Get_Attribute_Level_Name_e6c169_3af3bf;
--  end read only

--  begin read only
   procedure Test_Get_Attribute_Level_Name_test_getattributelevelname
     (Gnattest_T: in out Test);
   procedure Test_Get_Attribute_Level_Name_e6c169_3af3bf
     (Gnattest_T: in out Test) renames
     Test_Get_Attribute_Level_Name_test_getattributelevelname;
--  id:2.2/e6c169a9e17af1de/Get_Attribute_Level_Name/1/0/test_getattributelevelname/
   procedure Test_Get_Attribute_Level_Name_test_getattributelevelname
     (Gnattest_T: in out Test) is
      function Get_Attribute_Level_Name
        (Attribute_Level: Positive) return String renames
        Wrap_Test_Get_Attribute_Level_Name_e6c169_3af3bf;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Game_Settings.Show_Numbers := False;
      Assert
        (Get_Attribute_Level_Name(3) = "Very low",
         "Failed to get attribute level name for level 3");
      Assert
        (Get_Attribute_Level_Name(12) = "Below average",
         "Failed to get attribute level name for level 12");
      Assert
        (Get_Attribute_Level_Name(48) = "Very high",
         "Failed to get attribute level name for level 48");
      Game_Settings.Show_Numbers := True;
      Assert
        (Get_Attribute_Level_Name(3) = " 3",
         "Failed to get attribute level name for level 3 (numeric)");
      Assert
        (Get_Attribute_Level_Name(12) = " 12",
         "Failed to get attribute level name for level 12 (numeric)");
      Assert
        (Get_Attribute_Level_Name(48) = " 48",
         "Failed to get attribute level name for level 48 (numeric)");

--  begin read only
   end Test_Get_Attribute_Level_Name_test_getattributelevelname;
--  end read only

--  begin read only
   procedure Wrap_Test_Daily_Payment_f0944a_0bfd06 is
   begin
      GNATtest_Generated.GNATtest_Standard.Crew.Daily_Payment;
   end Wrap_Test_Daily_Payment_f0944a_0bfd06;
--  end read only

--  begin read only
   procedure Test_Daily_Payment_test_dailypayment(Gnattest_T: in out Test);
   procedure Test_Daily_Payment_f0944a_0bfd06(Gnattest_T: in out Test) renames
     Test_Daily_Payment_test_dailypayment;
--  id:2.2/f0944a11fc920b7c/Daily_Payment/1/0/test_dailypayment/
   procedure Test_Daily_Payment_test_dailypayment(Gnattest_T: in out Test) is
      procedure Daily_Payment renames Wrap_Test_Daily_Payment_f0944a_0bfd06;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Daily_Payment;
      Assert(True, "This test can only crash.");

--  begin read only
   end Test_Daily_Payment_test_dailypayment;
--  end read only

--  begin read only
   function Wrap_Test_Get_Training_Tool_Quality_6d83ce_af65c0
     (Member_Index, Skill_Index: Positive) return Positive is
   begin
      begin
         pragma Assert(Skill_Index <= Natural(Skills_Amount));
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(crew.ads:0):Test_GetTrainingToolQuality test requirement violated");
      end;
      declare
         Test_Get_Training_Tool_Quality_6d83ce_af65c0_Result: constant Positive :=
           GNATtest_Generated.GNATtest_Standard.Crew.Get_Training_Tool_Quality
             (Member_Index, Skill_Index);
      begin
         begin
            pragma Assert(True);
            null;
         exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "ens_sloc(crew.ads:0:):Test_GetTrainingToolQuality test commitment violated");
         end;
         return Test_Get_Training_Tool_Quality_6d83ce_af65c0_Result;
      end;
   end Wrap_Test_Get_Training_Tool_Quality_6d83ce_af65c0;
--  end read only

--  begin read only
   procedure Test_Get_Training_Tool_Quality_test_gettrainingtoolquality
     (Gnattest_T: in out Test);
   procedure Test_Get_Training_Tool_Quality_6d83ce_af65c0
     (Gnattest_T: in out Test) renames
     Test_Get_Training_Tool_Quality_test_gettrainingtoolquality;
--  id:2.2/6d83ce0903ec5b7d/Get_Training_Tool_Quality/1/0/test_gettrainingtoolquality/
   procedure Test_Get_Training_Tool_Quality_test_gettrainingtoolquality
     (Gnattest_T: in out Test) is
      function Get_Training_Tool_Quality
        (Member_Index, Skill_Index: Positive) return Positive renames
        Wrap_Test_Get_Training_Tool_Quality_6d83ce_af65c0;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Get_Training_Tool_Quality(1, 1) = 100,
         "Failed to get minimun quality of training tool.");

--  begin read only
   end Test_Get_Training_Tool_Quality_test_gettrainingtoolquality;
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
end Crew.Test_Data.Tests;
