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
   procedure Wrap_Test_Generate_Save_Name_3f9630_a0a8d0
     (Rename_Save: Boolean := False) is
      Gnattest_1_Save_Name: constant Unbounded_String := Save_Name;
   begin
      begin
         pragma Assert(True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(game-saveload.ads:0):Test_GenerateSave_Name test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Game.SaveLoad.Generate_Save_Name
        (Rename_Save);
      begin
         pragma Assert(Save_Name /= Gnattest_1_Save_Name);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(game-saveload.ads:0:):Test_GenerateSave_Name test commitment violated");
      end;
   end Wrap_Test_Generate_Save_Name_3f9630_a0a8d0;
--  end read only

--  begin read only
   procedure Test_Generate_Save_Name_test_generatesave_name
     (Gnattest_T: in out Test);
   procedure Test_Generate_Save_Name_3f9630_a0a8d0
     (Gnattest_T: in out Test) renames
     Test_Generate_Save_Name_test_generatesave_name;
--  id:2.2/3f9630bf78865443/Generate_Save_Name/1/0/test_generatesave_name/
   procedure Test_Generate_Save_Name_test_generatesave_name
     (Gnattest_T: in out Test) is
      procedure Generate_Save_Name(Rename_Save: Boolean := False) renames
        Wrap_Test_Generate_Save_Name_3f9630_a0a8d0;
--  end read only

      pragma Unreferenced(Gnattest_T);
      Old_Save_Name: constant Unbounded_String := Save_Name;

   begin

      Generate_Save_Name;
      Assert
        (Old_Save_Name /= Save_Name,
         "Failed to generate new save name (Old_Save_Name = '" &
         To_String(Old_Save_Name) & "', Save_Name = '" & To_String(Save_Name) &
         "').");

--  begin read only
   end Test_Generate_Save_Name_test_generatesave_name;
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
