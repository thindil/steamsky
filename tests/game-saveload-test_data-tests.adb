--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Game.SaveLoad.Test_Data.

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
package body Game.SaveLoad.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only
--  begin read only
   procedure Wrap_Test_GenerateSaveName_ddf358_e85ed1 (RenameSave: Boolean := False) 
   is
   begin
      GNATtest_Generated.GNATtest_Standard.Game.SaveLoad.GenerateSaveName (RenameSave);
   end Wrap_Test_GenerateSaveName_ddf358_e85ed1;
--  end read only

--  begin read only
   procedure Test_GenerateSaveName_test_generatesavename (Gnattest_T : in out Test);
   procedure Test_GenerateSaveName_ddf358_e85ed1 (Gnattest_T : in out Test) renames Test_GenerateSaveName_test_generatesavename;
--  id:2.2/ddf358647b83b861/GenerateSaveName/1/0/test_generatesavename/
   procedure Test_GenerateSaveName_test_generatesavename (Gnattest_T : in out Test) is
   procedure GenerateSaveName (RenameSave: Boolean := False) renames Wrap_Test_GenerateSaveName_ddf358_e85ed1;
--  end read only

      pragma Unreferenced (Gnattest_T);
      OldSaveName: Unbounded_String := SaveName;

   begin

      GenerateSaveName;
      Assert(OldSaveName /= SaveName, "Failed to generate new save name.");

--  begin read only
   end Test_GenerateSaveName_test_generatesavename;
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
end Game.SaveLoad.Test_Data.Tests;
