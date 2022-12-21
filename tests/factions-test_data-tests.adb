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
   function Wrap_Test_Get_Random_Faction_162df6_103989
      return Tiny_String.Bounded_String is
   begin
      declare
         Test_Get_Random_Faction_162df6_103989_Result: constant Tiny_String
           .Bounded_String :=
           GNATtest_Generated.GNATtest_Standard.Factions.Get_Random_Faction;
      begin
         return Test_Get_Random_Faction_162df6_103989_Result;
      end;
   end Wrap_Test_Get_Random_Faction_162df6_103989;
--  end read only

--  begin read only
   procedure Test_Get_Random_Faction_test_getrandomfaction
     (Gnattest_T: in out Test);
   procedure Test_Get_Random_Faction_162df6_103989
     (Gnattest_T: in out Test) renames
     Test_Get_Random_Faction_test_getrandomfaction;
--  id:2.2/162df647e9284c49/Get_Random_Faction/1/0/test_getrandomfaction/
   procedure Test_Get_Random_Faction_test_getrandomfaction
     (Gnattest_T: in out Test) is
      function Get_Random_Faction return Tiny_String.Bounded_String renames
        Wrap_Test_Get_Random_Faction_162df6_103989;
--  end read only

      pragma Unreferenced(Gnattest_T);
      use Tiny_String;
      FactionName: Bounded_String := Null_Bounded_String;

   begin

      FactionName := Get_Random_Faction;
      Assert
        (FactionName /= Null_Bounded_String,
         "Failed to get random faction name. Empty name.");
      Assert
        (Factions_List.Contains(FactionName),
         "Failed to get random faction name. Got not existing name.");

--  begin read only
   end Test_Get_Random_Faction_test_getrandomfaction;
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
