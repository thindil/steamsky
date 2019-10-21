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
   procedure Wrap_Test_UpdateGame_93bf31_5306b6 (Minutes: Positive; InCombat: Boolean := False) 
   is
   begin
      GNATtest_Generated.GNATtest_Standard.Game.UpdateGame (Minutes, InCombat);
   end Wrap_Test_UpdateGame_93bf31_5306b6;
--  end read only

--  begin read only
   procedure Test_UpdateGame_test_updategame (Gnattest_T : in out Test);
   procedure Test_UpdateGame_93bf31_5306b6 (Gnattest_T : in out Test) renames Test_UpdateGame_test_updategame;
--  id:2.2/93bf3174ce6811a5/UpdateGame/1/0/test_updategame/
   procedure Test_UpdateGame_test_updategame (Gnattest_T : in out Test) is
   procedure UpdateGame (Minutes: Positive; InCombat: Boolean := False) renames Wrap_Test_UpdateGame_93bf31_5306b6;
--  end read only

      pragma Unreferenced (Gnattest_T);
      Minutes: constant Natural := GameDate.Minutes;

   begin

      UpdateGame(1);
      Assert(Minutes + 1 = GameDate.Minutes, "Failed to update game time.");

--  begin read only
   end Test_UpdateGame_test_updategame;
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
