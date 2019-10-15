--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Factions.Test_Data.

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
package body Factions.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only
--  begin read only
   function Wrap_Test_GetReputation_f138bb_fd71d1 (SourceFaction, TargetFaction: Unbounded_String)  return Integer
   is
   begin
      begin
         pragma Assert
           ((Factions_Container.Contains(Factions_List, SourceFaction) and Factions_Container.Contains(Factions_List, TargetFaction)));
         null;
      exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "req_sloc(factions.ads:0):Test_GetReputation test requirement violated");
      end;
      declare
         Test_GetReputation_f138bb_fd71d1_Result : constant Integer := GNATtest_Generated.GNATtest_Standard.Factions.GetReputation (SourceFaction, TargetFaction);
      begin
         begin
            pragma Assert
              (True);
            null;
         exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "ens_sloc(factions.ads:0:):Test_GetReputation test commitment violated");
         end;
         return Test_GetReputation_f138bb_fd71d1_Result;
      end;
   end Wrap_Test_GetReputation_f138bb_fd71d1;
--  end read only

--  begin read only
   procedure Test_GetReputation_test_getreputation (Gnattest_T : in out Test);
   procedure Test_GetReputation_f138bb_fd71d1 (Gnattest_T : in out Test) renames Test_GetReputation_test_getreputation;
--  id:2.2/f138bbb5c8b2b971/GetReputation/1/0/test_getreputation/
   procedure Test_GetReputation_test_getreputation (Gnattest_T : in out Test) is
      function GetReputation (SourceFaction, TargetFaction: Unbounded_String) return Integer renames Wrap_Test_GetReputation_f138bb_fd71d1;
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      Assert(GetReputation(To_Unbounded_String("POLEIS"), To_Unbounded_String("POLEIS")) = 0, "Failed to get reputation for Poleis to Poleis.");
      Assert(GetReputation(To_Unbounded_String("POLEIS"), To_Unbounded_String("PIRATES")) = -10, "Failed to get reputation for Poleis to Pirates.");

--  begin read only
   end Test_GetReputation_test_getreputation;
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
end Factions.Test_Data.Tests;
