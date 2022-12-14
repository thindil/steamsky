--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Crew.Inventory.Test_Data.

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
package body Crew.Inventory.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only
--  begin read only
   function Wrap_Test_Find_Tools_2a8899_b022e2
     (Member_Index: Positive; Item_Type: Tiny_String.Bounded_String;
      Order: Crew_Orders; Tool_Quality: Positive := 100) return Natural is
   begin
      begin
         pragma Assert
           ((Member_Index <= Player_Ship.Crew.Last_Index and
             Tiny_String.Length(Source => Item_Type) > 0));
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(crew-inventory.ads:0):Test_FindTools test requirement violated");
      end;
      declare
         Test_Find_Tools_2a8899_b022e2_Result: constant Natural :=
           GNATtest_Generated.GNATtest_Standard.Crew.Inventory.Find_Tools
             (Member_Index, Item_Type, Order, Tool_Quality);
      begin
         begin
            pragma Assert(True);
            null;
         exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "ens_sloc(crew-inventory.ads:0:):Test_FindTools test commitment violated");
         end;
         return Test_Find_Tools_2a8899_b022e2_Result;
      end;
   end Wrap_Test_Find_Tools_2a8899_b022e2;
--  end read only

--  begin read only
   procedure Test_Find_Tools_test_findtools(Gnattest_T: in out Test);
   procedure Test_Find_Tools_2a8899_b022e2(Gnattest_T: in out Test) renames
     Test_Find_Tools_test_findtools;
--  id:2.2/2a8899f31b749150/Find_Tools/1/0/test_findtools/
   procedure Test_Find_Tools_test_findtools(Gnattest_T: in out Test) is
      function Find_Tools
        (Member_Index: Positive; Item_Type: Tiny_String.Bounded_String;
         Order: Crew_Orders; Tool_Quality: Positive := 100)
         return Natural renames
        Wrap_Test_Find_Tools_2a8899_b022e2;
--  end read only

      pragma Unreferenced(Gnattest_T);
      use Tiny_String;

   begin

      Assert
        (Find_Tools(1, To_Bounded_String("Bucket"), Clean) > 0,
         "Failed to find tools for cleaning.");
      Assert
        (Find_Tools(1, To_Bounded_String("sdfsdfds"), Talk) = 0,
         "Failed to not find non-existing tools.");

--  begin read only
   end Test_Find_Tools_test_findtools;
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
end Crew.Inventory.Test_Data.Tests;
