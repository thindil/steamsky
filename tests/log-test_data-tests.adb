--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Log.Test_Data.

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
package body Log.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only
--  begin read only
   procedure Wrap_Test_Log_Message_6a7537_caf43b
     (Message: String;
      Message_Type: Debug_Types;
      New_Line, Time_Stamp: Boolean := True) is
   begin
      begin
         pragma Assert(Message'Length > 0);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(log.ads:0):Test_LogMessage test requirement violated");
      end;
      Gnattest_Generated.GNATtest_Standard.Log.Log_Message
        (Message,
         Message_Type,
         New_Line,
         Time_Stamp);
      begin
         pragma Assert(True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(log.ads:0:):Test_LogMessage test commitment violated");
      end;
   end Wrap_Test_Log_Message_6a7537_caf43b;
--  end read only

--  begin read only
   procedure Test_Log_Message_test_logmessage(Gnattest_T: in out Test);
   procedure Test_Log_Message_6a7537_caf43b
     (Gnattest_T: in out Test) renames
     Test_Log_Message_test_logmessage;
--  id:2.2/6a7537630b1363a5/Log_Message/1/0/test_logmessage/
   procedure Test_Log_Message_test_logmessage(Gnattest_T: in out Test) is
      procedure Log_Message
        (Message: String;
         Message_Type: Debug_Types;
         New_Line, Time_Stamp: Boolean := True) renames
        Wrap_Test_Log_Message_6a7537_caf43b;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Debug_Mode := EVERYTHING;
      Start_Logging;
      Log_Message("Test message", EVERYTHING);
      End_Logging;
      Assert(True, "This test can only crash.");

--  begin read only
   end Test_Log_Message_test_logmessage;
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
end Log.Test_Data.Tests;
