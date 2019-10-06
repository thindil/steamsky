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
   procedure Wrap_Test_GainExp_685058_9e4143 (Amount: Natural; SkillNumber, CrewIndex: Positive) 
   is
   begin
      begin
         pragma Assert
           (SkillNumber <= Skills_List.Last_Index);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(crew.ads:0):Test_GainExp test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Crew.GainExp (Amount, SkillNumber, CrewIndex);
      begin
         pragma Assert
           (True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(crew.ads:0:):Test_GainExp test commitment violated");
      end;
   end Wrap_Test_GainExp_685058_9e4143;
--  end read only

--  begin read only
   procedure Test_GainExp_test_gainexp (Gnattest_T : in out Test);
   procedure Test_GainExp_685058_9e4143 (Gnattest_T : in out Test) renames Test_GainExp_test_gainexp;
--  id:2.2/685058e06b47ff9b/GainExp/1/0/test_gainexp/
   procedure Test_GainExp_test_gainexp (Gnattest_T : in out Test) is
   procedure GainExp (Amount: Natural; SkillNumber, CrewIndex: Positive) renames Wrap_Test_GainExp_685058_9e4143;
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      GainExp(1, 4, 1);
      Assert(PlayerShip.Crew(1).Skills(1)(3) = 8, "Failed to gain experience in skill.");

--  begin read only
   end Test_GainExp_test_gainexp;
--  end read only

--  begin read only
   function Wrap_Test_GenerateMemberName_b4591b_2ce78d (Gender: Character; FactionIndex: Unbounded_String)  return Unbounded_String
   is
   begin
      begin
         pragma Assert
           (((Gender = 'M' or Gender = 'F') and FactionIndex /= Null_Unbounded_String));
         null;
      exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "req_sloc(crew.ads:0):Test_GenerateMemberName test requirement violated");
      end;
      declare
         Test_GenerateMemberName_b4591b_2ce78d_Result : constant Unbounded_String := GNATtest_Generated.GNATtest_Standard.Crew.GenerateMemberName (Gender, FactionIndex);
      begin
         begin
            pragma Assert
              (True);
            null;
         exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "ens_sloc(crew.ads:0:):Test_GenerateMemberName test commitment violated");
         end;
         return Test_GenerateMemberName_b4591b_2ce78d_Result;
      end;
   end Wrap_Test_GenerateMemberName_b4591b_2ce78d;
--  end read only

--  begin read only
   procedure Test_GenerateMemberName_test_generatemembername (Gnattest_T : in out Test);
   procedure Test_GenerateMemberName_b4591b_2ce78d (Gnattest_T : in out Test) renames Test_GenerateMemberName_test_generatemembername;
--  id:2.2/b4591b69c6a992ff/GenerateMemberName/1/0/test_generatemembername/
   procedure Test_GenerateMemberName_test_generatemembername (Gnattest_T : in out Test) is
      function GenerateMemberName (Gender: Character; FactionIndex: Unbounded_String) return Unbounded_String renames Wrap_Test_GenerateMemberName_b4591b_2ce78d;
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      Assert(GenerateMemberName('M', To_Unbounded_String("POLEIS")) /= Null_Unbounded_String, "Failed to generate male name for poleis faction.");

--  begin read only
   end Test_GenerateMemberName_test_generatemembername;
--  end read only

--  begin read only
   function Wrap_Test_FindCabin_c60907_006804 (MemberIndex: Positive)  return Natural
   is
   begin
      declare
         Test_FindCabin_c60907_006804_Result : constant Natural := GNATtest_Generated.GNATtest_Standard.Crew.FindCabin (MemberIndex);
      begin
         return Test_FindCabin_c60907_006804_Result;
      end;
   end Wrap_Test_FindCabin_c60907_006804;
--  end read only

--  begin read only
   procedure Test_FindCabin_test_findcabin (Gnattest_T : in out Test);
   procedure Test_FindCabin_c60907_006804 (Gnattest_T : in out Test) renames Test_FindCabin_test_findcabin;
--  id:2.2/c60907de3ec73748/FindCabin/1/0/test_findcabin/
   procedure Test_FindCabin_test_findcabin (Gnattest_T : in out Test) is
      function FindCabin (MemberIndex: Positive) return Natural renames Wrap_Test_FindCabin_c60907_006804;
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      Assert(FindCabin(1) > 0, "Failed to find cabin for existing crew member.");
      Assert(FindCabin(100) = 0, "Failed to not find cabin for non existing crew member.");

--  begin read only
   end Test_FindCabin_test_findcabin;
--  end read only

--  begin read only
   procedure Wrap_Test_UpdateCrew_123b55_011eae (Minutes: Positive; TiredPoints: Natural; InCombat: Boolean := False) 
   is
   begin
      GNATtest_Generated.GNATtest_Standard.Crew.UpdateCrew (Minutes, TiredPoints, InCombat);
   end Wrap_Test_UpdateCrew_123b55_011eae;
--  end read only

--  begin read only
   procedure Test_UpdateCrew_test_updatecrew (Gnattest_T : in out Test);
   procedure Test_UpdateCrew_123b55_011eae (Gnattest_T : in out Test) renames Test_UpdateCrew_test_updatecrew;
--  id:2.2/123b55a332c8ae22/UpdateCrew/1/0/test_updatecrew/
   procedure Test_UpdateCrew_test_updatecrew (Gnattest_T : in out Test) is
   procedure UpdateCrew (Minutes: Positive; TiredPoints: Natural; InCombat: Boolean := False) renames Wrap_Test_UpdateCrew_123b55_011eae;
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      UpdateCrew(1, 1);
      Assert(True, "This test can only crash.");

--  begin read only
   end Test_UpdateCrew_test_updatecrew;
--  end read only

--  begin read only
   procedure Wrap_Test_WaitForRest_237f93_b046aa
   is
   begin
      GNATtest_Generated.GNATtest_Standard.Crew.WaitForRest;
   end Wrap_Test_WaitForRest_237f93_b046aa;
--  end read only

--  begin read only
   procedure Test_WaitForRest_test_waitforrest (Gnattest_T : in out Test);
   procedure Test_WaitForRest_237f93_b046aa (Gnattest_T : in out Test) renames Test_WaitForRest_test_waitforrest;
--  id:2.2/237f93172c11704d/WaitForRest/1/0/test_waitforrest/
   procedure Test_WaitForRest_test_waitforrest (Gnattest_T : in out Test) is
   procedure WaitForRest renames Wrap_Test_WaitForRest_237f93_b046aa;
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      WaitForRest;
      Assert(True, "This test can only crash.");

--  begin read only
   end Test_WaitForRest_test_waitforrest;
--  end read only

--  begin read only
   function Wrap_Test_GetSkillLevelName_b5615e_88354f (SkillLevel: Positive)  return String
   is
   begin
      begin
         pragma Assert
           ((SkillLevel <= 100));
         null;
      exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "req_sloc(crew.ads:0):Test_GetSkillLevelName test requirement violated");
      end;
      declare
         Test_GetSkillLevelName_b5615e_88354f_Result : constant String := GNATtest_Generated.GNATtest_Standard.Crew.GetSkillLevelName (SkillLevel);
      begin
         begin
            pragma Assert
              (True);
            null;
         exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "ens_sloc(crew.ads:0:):Test_GetSkillLevelName test commitment violated");
         end;
         return Test_GetSkillLevelName_b5615e_88354f_Result;
      end;
   end Wrap_Test_GetSkillLevelName_b5615e_88354f;
--  end read only

--  begin read only
   procedure Test_GetSkillLevelName_test_getskilllevelname (Gnattest_T : in out Test);
   procedure Test_GetSkillLevelName_b5615e_88354f (Gnattest_T : in out Test) renames Test_GetSkillLevelName_test_getskilllevelname;
--  id:2.2/b5615ec8a22d7d74/GetSkillLevelName/1/0/test_getskilllevelname/
   procedure Test_GetSkillLevelName_test_getskilllevelname (Gnattest_T : in out Test) is
      function GetSkillLevelName (SkillLevel: Positive) return String renames Wrap_Test_GetSkillLevelName_b5615e_88354f;
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      Assert(GetSkillLevelName(9) = "Beginner", "Failed to get skill level name for level 9.");
      Assert(GetSkillLevelName(54) = "Respected", "Failed to get skill level name for level 54");
      Assert(GetSkillLevelName(92) = "Legendary", "Failed to get skill level name for level 92");

--  begin read only
   end Test_GetSkillLevelName_test_getskilllevelname;
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
