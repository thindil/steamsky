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
   procedure Wrap_Test_GenerateSaveName_ddf358_d26994
     (RenameSave: Boolean := False) is
      Gnattest_1_SaveName: constant Unbounded_String := SaveName;
   begin
      begin
         pragma Assert(True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(game-saveload.ads:0):Test_GenerateSaveName test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Game.SaveLoad.GenerateSaveName
        (RenameSave);
      begin
         pragma Assert(SaveName /= Gnattest_1_SaveName);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(game-saveload.ads:0:):Test_GenerateSaveName test commitment violated");
      end;
   end Wrap_Test_GenerateSaveName_ddf358_d26994;
--  end read only

--  begin read only
   procedure Test_GenerateSaveName_test_generatesavename
     (Gnattest_T: in out Test);
   procedure Test_GenerateSaveName_ddf358_d26994
     (Gnattest_T: in out Test) renames
     Test_GenerateSaveName_test_generatesavename;
--  id:2.2/ddf358647b83b861/GenerateSaveName/1/0/test_generatesavename/
   procedure Test_GenerateSaveName_test_generatesavename
     (Gnattest_T: in out Test) is
      procedure GenerateSaveName(RenameSave: Boolean := False) renames
        Wrap_Test_GenerateSaveName_ddf358_d26994;
--  end read only

      pragma Unreferenced(Gnattest_T);
      OldSaveName: Unbounded_String := SaveName;

   begin

      GenerateSaveName;
      Assert
        (OldSaveName /= SaveName,
         "Failed to generate new save name (Oldsavename = '" &
         To_String(OldSaveName) & "', SaveName = '" & To_String(SaveName) &
         "').");

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
