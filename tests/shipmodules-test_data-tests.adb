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
   function Wrap_Test_Get_Module_Type_8ab8da_d7b87a
     (Module_Index: Unbounded_String) return String is
   begin
      begin
         pragma Assert(Length(Source => Module_Index) > 0);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(shipmodules.ads:0):Test_GetModuleType test requirement violated");
      end;
      declare
         Test_Get_Module_Type_8ab8da_d7b87a_Result: constant String :=
           GNATtest_Generated.GNATtest_Standard.ShipModules.Get_Module_Type
             (Module_Index);
      begin
         begin
            pragma Assert
              (Test_Get_Module_Type_8ab8da_d7b87a_Result'Length > 0);
            null;
         exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "ens_sloc(shipmodules.ads:0:):Test_GetModuleType test commitment violated");
         end;
         return Test_Get_Module_Type_8ab8da_d7b87a_Result;
      end;
   end Wrap_Test_Get_Module_Type_8ab8da_d7b87a;
--  end read only

--  begin read only
   procedure Test_Get_Module_Type_test_getmoduletype(Gnattest_T: in out Test);
   procedure Test_Get_Module_Type_8ab8da_d7b87a
     (Gnattest_T: in out Test) renames
     Test_Get_Module_Type_test_getmoduletype;
--  id:2.2/8ab8da4884194349/Get_Module_Type/1/0/test_getmoduletype/
   procedure Test_Get_Module_Type_test_getmoduletype
     (Gnattest_T: in out Test) is
      function Get_Module_Type
        (Module_Index: Unbounded_String) return String renames
        Wrap_Test_Get_Module_Type_8ab8da_d7b87a;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Assert
        (Get_Module_Type(To_Unbounded_String("1")) = "Hull",
         "Failed to get type of selected module.");
      Assert
        (Get_Module_Type(To_Unbounded_String("6")) = "Alchemy lab",
         "Failed to get type of module with underscore.");

--  begin read only
   end Test_Get_Module_Type_test_getmoduletype;
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
