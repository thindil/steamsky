--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Events.Test_Data.

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
package body Events.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only
--  begin read only
   function Wrap_Test_CheckForEvent_1c4562_e01b25 return Boolean
   is
   begin
      declare
         Test_CheckForEvent_1c4562_e01b25_Result : constant Boolean := GNATtest_Generated.GNATtest_Standard.Events.CheckForEvent;
      begin
         return Test_CheckForEvent_1c4562_e01b25_Result;
      end;
   end Wrap_Test_CheckForEvent_1c4562_e01b25;
--  end read only

--  begin read only
   procedure Test_CheckForEvent_test_checkforevent (Gnattest_T : in out Test);
   procedure Test_CheckForEvent_1c4562_e01b25 (Gnattest_T : in out Test) renames Test_CheckForEvent_test_checkforevent;
--  id:2.2/1c45624e0a8cde64/CheckForEvent/1/0/test_checkforevent/
   procedure Test_CheckForEvent_test_checkforevent (Gnattest_T : in out Test) is
      function CheckForEvent return Boolean renames Wrap_Test_CheckForEvent_1c4562_e01b25;
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      if CheckForEvent then
         null;
      end if;
      Assert(True, "This test can only crash");

--  begin read only
   end Test_CheckForEvent_test_checkforevent;
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
end Events.Test_Data.Tests;
