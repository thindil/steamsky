--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into ShipModules.Test_Data.

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
package body ShipModules.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only
--  begin read only
   function Wrap_Test_GetModuleType_51fe9c_8ea853
     (ModuleIndex: Unbounded_String) return String is
   begin
      begin
         pragma Assert(Length(ModuleIndex) > 0);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(shipmodules.ads:0):Test_GetModuleType test requirement violated");
      end;
      declare
         Test_GetModuleType_51fe9c_8ea853_Result: constant String :=
           GNATtest_Generated.GNATtest_Standard.ShipModules.GetModuleType
             (ModuleIndex);
      begin
         begin
            pragma Assert(True);
            null;
         exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "ens_sloc(shipmodules.ads:0:):Test_GetModuleType test commitment violated");
         end;
         return Test_GetModuleType_51fe9c_8ea853_Result;
      end;
   end Wrap_Test_GetModuleType_51fe9c_8ea853;
--  end read only

--  begin read only
   procedure Test_GetModuleType_test_getmoduletype(Gnattest_T: in out Test);
   procedure Test_GetModuleType_51fe9c_8ea853(Gnattest_T: in out Test) renames
     Test_GetModuleType_test_getmoduletype;
--  id:2.2/51fe9c61c193b590/GetModuleType/1/0/test_getmoduletype/
   procedure Test_GetModuleType_test_getmoduletype(Gnattest_T: in out Test) is
      function GetModuleType
        (ModuleIndex: Unbounded_String) return String renames
        Wrap_Test_GetModuleType_51fe9c_8ea853;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Assert
        (GetModuleType(To_Unbounded_String("1")) = "Hull",
         "Failed to get type of selected module.");
      Assert
        (GetModuleType(To_Unbounded_String("6")) = "Alchemy lab",
         "Failed to get type of module with underscore.");

--  begin read only
   end Test_GetModuleType_test_getmoduletype;
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
end ShipModules.Test_Data.Tests;
