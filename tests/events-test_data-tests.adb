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

with Bases; use Bases;

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
   procedure Wrap_Test_UpdateEvents_96e988_646fe5 (Minutes: Positive) 
   is
   begin
      GNATtest_Generated.GNATtest_Standard.Events.UpdateEvents (Minutes);
   end Wrap_Test_UpdateEvents_96e988_646fe5;
--  end read only

--  begin read only
   procedure Test_UpdateEvents_test_updateevents (Gnattest_T : in out Test);
   procedure Test_UpdateEvents_96e988_646fe5 (Gnattest_T : in out Test) renames Test_UpdateEvents_test_updateevents;
--  id:2.2/96e988ace71f5fcf/UpdateEvents/1/0/test_updateevents/
   procedure Test_UpdateEvents_test_updateevents (Gnattest_T : in out Test) is
   procedure UpdateEvents (Minutes: Positive) renames Wrap_Test_UpdateEvents_96e988_646fe5;
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      UpdateEvents(1);
      Assert(True, "This test can only crash.");

--  begin read only
   end Test_UpdateEvents_test_updateevents;
--  end read only

--  begin read only
   procedure Wrap_Test_DeleteEvent_0ca9ce_33228f (EventIndex: Positive) 
   is
   begin
      begin
         pragma Assert
           (EventIndex <= Events_List.Last_Index);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(events.ads:0):Test_DeleteEvent test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Events.DeleteEvent (EventIndex);
      begin
         pragma Assert
           (True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(events.ads:0:):Test_DeleteEvent test commitment violated");
      end;
   end Wrap_Test_DeleteEvent_0ca9ce_33228f;
--  end read only

--  begin read only
   procedure Test_DeleteEvent_test_deleteevent (Gnattest_T : in out Test);
   procedure Test_DeleteEvent_0ca9ce_33228f (Gnattest_T : in out Test) renames Test_DeleteEvent_test_deleteevent;
--  id:2.2/0ca9ce05c1aa70d1/DeleteEvent/1/0/test_deleteevent/
   procedure Test_DeleteEvent_test_deleteevent (Gnattest_T : in out Test) is
   procedure DeleteEvent (EventIndex: Positive) renames Wrap_Test_DeleteEvent_0ca9ce_33228f;
--  end read only

      pragma Unreferenced (Gnattest_T);
      Amount: constant Natural := Natural(Events_List.Length);

   begin

      DeleteEvent(1);
      Assert(Natural(Events_List.Length) < Amount, "Failed to delete event.");

--  begin read only
   end Test_DeleteEvent_test_deleteevent;
--  end read only

--  begin read only
   procedure Wrap_Test_GenerateTraders_8d2b65_5d00a3
   is
   begin
      GNATtest_Generated.GNATtest_Standard.Events.GenerateTraders;
   end Wrap_Test_GenerateTraders_8d2b65_5d00a3;
--  end read only

--  begin read only
   procedure Test_GenerateTraders_test_generatetraders (Gnattest_T : in out Test);
   procedure Test_GenerateTraders_8d2b65_5d00a3 (Gnattest_T : in out Test) renames Test_GenerateTraders_test_generatetraders;
--  id:2.2/8d2b65740d8f0270/GenerateTraders/1/0/test_generatetraders/
   procedure Test_GenerateTraders_test_generatetraders (Gnattest_T : in out Test) is
   procedure GenerateTraders renames Wrap_Test_GenerateTraders_8d2b65_5d00a3;
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      GenerateTraders;
      Assert(True, "This test can only crash.");

--  begin read only
   end Test_GenerateTraders_test_generatetraders;
--  end read only

--  begin read only
   procedure Wrap_Test_RecoverBase_904011_a032fd (BaseIndex: BasesRange) 
   is
   begin
      GNATtest_Generated.GNATtest_Standard.Events.RecoverBase (BaseIndex);
   end Wrap_Test_RecoverBase_904011_a032fd;
--  end read only

--  begin read only
   procedure Test_RecoverBase_test_recoverbase (Gnattest_T : in out Test);
   procedure Test_RecoverBase_904011_a032fd (Gnattest_T : in out Test) renames Test_RecoverBase_test_recoverbase;
--  id:2.2/904011d165b5f6d4/RecoverBase/1/0/test_recoverbase/
   procedure Test_RecoverBase_test_recoverbase (Gnattest_T : in out Test) is
   procedure RecoverBase (BaseIndex: BasesRange) renames Wrap_Test_RecoverBase_904011_a032fd;
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      for I in SkyBases'Range loop
         if SkyBases(I).Population = 0 then
            RecoverBase(I);
            exit;
         end if;
      end loop;
      Assert(True, "This test can only crash.");

--  begin read only
   end Test_RecoverBase_test_recoverbase;
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
