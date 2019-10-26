--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into HallOfFame.Test_Data.

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
package body HallOfFame.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only
--  begin read only
   procedure Wrap_Test_UpdateHallOfFame_117948_539c16 (PlayerName, DeathReason: Unbounded_String) 
   is
   begin
      begin
         pragma Assert
           ((PlayerName /= Null_Unbounded_String and DeathReason /= Null_Unbounded_String));
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(halloffame.ads:0):Test_UpdateHallOfFame test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.HallOfFame.UpdateHallOfFame (PlayerName, DeathReason);
      begin
         pragma Assert
           (True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(halloffame.ads:0:):Test_UpdateHallOfFame test commitment violated");
      end;
   end Wrap_Test_UpdateHallOfFame_117948_539c16;
--  end read only

--  begin read only
   procedure Test_UpdateHallOfFame_test_updatehalloffame (Gnattest_T : in out Test);
   procedure Test_UpdateHallOfFame_117948_539c16 (Gnattest_T : in out Test) renames Test_UpdateHallOfFame_test_updatehalloffame;
--  id:2.2/1179489e293ca621/UpdateHallOfFame/1/0/test_updatehalloffame/
   procedure Test_UpdateHallOfFame_test_updatehalloffame (Gnattest_T : in out Test) is
   procedure UpdateHallOfFame (PlayerName, DeathReason: Unbounded_String) renames Wrap_Test_UpdateHallOfFame_117948_539c16;
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

   HallOfFame_Array :=
     (others =>
        (Name => Null_Unbounded_String, Points => 0,
         DeathReason => Null_Unbounded_String));
   UpdateHallOfFame(To_Unbounded_String("TestPlayer"), To_Unbounded_String("TestDeath"));
   Assert(HallOfFame_Array(1).Name = To_Unbounded_String("TestPlayer"), "Failed to update Hall Of Fame.");

--  begin read only
   end Test_UpdateHallOfFame_test_updatehalloffame;
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
end HallOfFame.Test_Data.Tests;
