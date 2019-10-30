--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Messages.Test_Data.

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
package body Messages.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only
--  begin read only
   function Wrap_Test_FormatedTime_5bb1ad_45f0f1 (Time: Date_Record := GameDate)  return String
   is
   begin
      declare
         Test_FormatedTime_5bb1ad_45f0f1_Result : constant String := GNATtest_Generated.GNATtest_Standard.Messages.FormatedTime (Time);
      begin
         return Test_FormatedTime_5bb1ad_45f0f1_Result;
      end;
   end Wrap_Test_FormatedTime_5bb1ad_45f0f1;
--  end read only

--  begin read only
   procedure Test_FormatedTime_test_formattedtime (Gnattest_T : in out Test);
   procedure Test_FormatedTime_5bb1ad_45f0f1 (Gnattest_T : in out Test) renames Test_FormatedTime_test_formattedtime;
--  id:2.2/5bb1ad5dbd52690f/FormatedTime/1/0/test_formattedtime/
   procedure Test_FormatedTime_test_formattedtime (Gnattest_T : in out Test) is
      function FormatedTime (Time: Date_Record := GameDate) return String renames Wrap_Test_FormatedTime_5bb1ad_45f0f1;
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      Assert(FormatedTime /= "", "Failed to get formated game time.");

--  begin read only
   end Test_FormatedTime_test_formattedtime;
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
end Messages.Test_Data.Tests;
