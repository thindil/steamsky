--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Ships.Crew.Test_Data.

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
package body Ships.Crew.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only
--  begin read only
   function Wrap_Test_GetSkillLevel_f7e690_019f8b (Member: Member_Data; SkillIndex: Positive)  return Natural
   is
   begin
      begin
         pragma Assert
           (SkillIndex <= Skills_List.Last_Index);
         null;
      exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "req_sloc(ships-crew.ads:0):Test_GetSkillLevel test requirement violated");
      end;
      declare
         Test_GetSkillLevel_f7e690_019f8b_Result : constant Natural := GNATtest_Generated.GNATtest_Standard.Ships.Crew.GetSkillLevel (Member, SkillIndex);
      begin
         begin
            pragma Assert
              (True);
            null;
         exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "ens_sloc(ships-crew.ads:0:):Test_GetSkillLevel test commitment violated");
         end;
         return Test_GetSkillLevel_f7e690_019f8b_Result;
      end;
   end Wrap_Test_GetSkillLevel_f7e690_019f8b;
--  end read only

--  begin read only
   procedure Test_GetSkillLevel_test_getskilllevel (Gnattest_T : in out Test);
   procedure Test_GetSkillLevel_f7e690_019f8b (Gnattest_T : in out Test) renames Test_GetSkillLevel_test_getskilllevel;
--  id:2.2/f7e690bba6071759/GetSkillLevel/1/0/test_getskilllevel/
   procedure Test_GetSkillLevel_test_getskilllevel (Gnattest_T : in out Test) is
      function GetSkillLevel (Member: Member_Data; SkillIndex: Positive) return Natural renames Wrap_Test_GetSkillLevel_f7e690_019f8b;
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      Assert(GetSkillLevel(PlayerShip.Crew(1), 1) = 0, "Failed to get real level of not owned skill.");
      Assert(GetSkillLevel(PlayerShip.Crew(1), 4) = 9, "Failed to get real level of skill.");

--  begin read only
   end Test_GetSkillLevel_test_getskilllevel;
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
end Ships.Crew.Test_Data.Tests;
