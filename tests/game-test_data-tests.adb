--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Game.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only

with Messages; use Messages;
with Config; use Config;

--  begin read only
--  end read only
package body Game.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only
--  begin read only
   procedure Wrap_Test_Update_Game_25a566_5306b6
     (Minutes: Positive; In_Combat: Boolean := False) is
   begin
      GNATtest_Generated.GNATtest_Standard.Game.Update_Game
        (Minutes, In_Combat);
   end Wrap_Test_Update_Game_25a566_5306b6;
--  end read only

--  begin read only
   procedure Test_Update_Game_test_updategame(Gnattest_T: in out Test);
   procedure Test_Update_Game_25a566_5306b6(Gnattest_T: in out Test) renames
     Test_Update_Game_test_updategame;
--  id:2.2/25a566d308fb13f6/Update_Game/1/0/test_updategame/
   procedure Test_Update_Game_test_updategame(Gnattest_T: in out Test) is
      procedure Update_Game
        (Minutes: Positive; In_Combat: Boolean := False) renames
        Wrap_Test_Update_Game_25a566_5306b6;
--  end read only

      pragma Unreferenced(Gnattest_T);
      Minutes: constant Natural := Game_Date.Minutes;

   begin

      Update_Game(1);
      Assert(Minutes + 1 = Game_Date.Minutes, "Failed to update game time.");

--  begin read only
   end Test_Update_Game_test_updategame;
--  end read only

--  begin read only
   procedure Wrap_Test_End_Game_29871f_745ef4(Save: Boolean) is
   begin
      GNATtest_Generated.GNATtest_Standard.Game.End_Game(Save);
   end Wrap_Test_End_Game_29871f_745ef4;
--  end read only

--  begin read only
   procedure Test_End_Game_test_endgame(Gnattest_T: in out Test);
   procedure Test_End_Game_29871f_745ef4(Gnattest_T: in out Test) renames
     Test_End_Game_test_endgame;
--  id:2.2/29871f76a299de21/End_Game/1/0/test_endgame/
   procedure Test_End_Game_test_endgame(Gnattest_T: in out Test) is
      procedure End_Game(Save: Boolean) renames
        Wrap_Test_End_Game_29871f_745ef4;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      End_Game(False);
      Assert(Messages_List.Length = 0, "Failed to clear old game data.");
      New_Game_Settings.Player_Faction := To_Unbounded_String("POLEIS");
      New_Game_Settings.Player_Career := To_Unbounded_String("general");
      New_Game_Settings.Starting_Base := To_Unbounded_String("1");
      New_Game;

--  begin read only
   end Test_End_Game_test_endgame;
--  end read only

--  begin read only
   function Wrap_Test_Find_Skill_Index_2a5948_bcc9a6
     (Skill_Name: Unbounded_String) return Natural is
   begin
      begin
         pragma Assert(Skill_Name /= Null_Unbounded_String);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(game.ads:0):Test_FindSkillIndex test requirement violated");
      end;
      declare
         Test_Find_Skill_Index_2a5948_bcc9a6_Result: constant Natural :=
           GNATtest_Generated.GNATtest_Standard.Game.Find_Skill_Index
             (Skill_Name);
      begin
         begin
            pragma Assert(True);
            null;
         exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "ens_sloc(game.ads:0:):Test_FindSkillIndex test commitment violated");
         end;
         return Test_Find_Skill_Index_2a5948_bcc9a6_Result;
      end;
   end Wrap_Test_Find_Skill_Index_2a5948_bcc9a6;
--  end read only

--  begin read only
   procedure Test_Find_Skill_Index_test_findskillindex
     (Gnattest_T: in out Test);
   procedure Test_Find_Skill_Index_2a5948_bcc9a6
     (Gnattest_T: in out Test) renames
     Test_Find_Skill_Index_test_findskillindex;
--  id:2.2/2a5948be5170f7b8/Find_Skill_Index/1/0/test_findskillindex/
   procedure Test_Find_Skill_Index_test_findskillindex
     (Gnattest_T: in out Test) is
      function Find_Skill_Index
        (Skill_Name: Unbounded_String) return Natural renames
        Wrap_Test_Find_Skill_Index_2a5948_bcc9a6;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Assert
        (Find_Skill_Index(To_Unbounded_String("Piloting")) = 1,
         "Failed to find existing skill.");
      Assert
        (Find_Skill_Index(To_Unbounded_String("sdljfskfhsf")) = 0,
         "Failed to not find non exisiting skill.");

--  begin read only
   end Test_Find_Skill_Index_test_findskillindex;
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
end Game.Test_Data.Tests;
