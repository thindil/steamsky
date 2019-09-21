--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Combat.Test_Data.

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
package body Combat.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only
--  begin read only
   function Wrap_Test_StartCombat_523cd4_8ba7bd (EnemyIndex: Unbounded_String; NewCombat: Boolean := True)  return Boolean
   is
   begin
      begin
         pragma Assert
           (ProtoShips_Container.Contains(ProtoShips_List, EnemyIndex));
         null;
      exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "req_sloc(combat.ads:0):Test_StartCombat test requirement violated");
      end;
      declare
         Test_StartCombat_523cd4_8ba7bd_Result : constant Boolean := GNATtest_Generated.GNATtest_Standard.Combat.StartCombat (EnemyIndex, NewCombat);
      begin
         begin
            pragma Assert
              (True);
            null;
         exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "ens_sloc(combat.ads:0:):Test_StartCombat test commitment violated");
         end;
         return Test_StartCombat_523cd4_8ba7bd_Result;
      end;
   end Wrap_Test_StartCombat_523cd4_8ba7bd;
--  end read only

--  begin read only
   procedure Test_StartCombat_test_startcombat (Gnattest_T : in out Test);
   procedure Test_StartCombat_523cd4_8ba7bd (Gnattest_T : in out Test) renames Test_StartCombat_test_startcombat;
--  id:2.2/523cd4ef15c88057/StartCombat/1/0/test_startcombat/
   procedure Test_StartCombat_test_startcombat (Gnattest_T : in out Test) is
      function StartCombat (EnemyIndex: Unbounded_String; NewCombat: Boolean := True) return Boolean renames Wrap_Test_StartCombat_523cd4_8ba7bd;
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      if StartCombat(To_Unbounded_String("2")) then
         Assert(True, "This test can only crash.");
      else
         Assert(True, "This test can only crash.");
      end if;

--  begin read only
   end Test_StartCombat_test_startcombat;
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
end Combat.Test_Data.Tests;
