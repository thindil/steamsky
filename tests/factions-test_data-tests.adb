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
   function Wrap_Test_Get_Reputation_da24fe_e0b5d2
     (Source_Faction, Target_Faction: Unbounded_String) return Integer is
   begin
      begin
         pragma Assert
           ((Factions_List.Contains(Key => Source_Faction) and
             Factions_List.Contains(Key => Target_Faction)));
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(factions.ads:0):Test_GetReputation test requirement violated");
      end;
      declare
         Test_Get_Reputation_da24fe_e0b5d2_Result: constant Integer :=
           GNATtest_Generated.GNATtest_Standard.Factions.Get_Reputation
             (Source_Faction, Target_Faction);
      begin
         begin
            pragma Assert(True);
            null;
         exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "ens_sloc(factions.ads:0:):Test_GetReputation test commitment violated");
         end;
         return Test_Get_Reputation_da24fe_e0b5d2_Result;
      end;
   end Wrap_Test_Get_Reputation_da24fe_e0b5d2;
--  end read only

--  begin read only
   procedure Test_Get_Reputation_test_getreputation(Gnattest_T: in out Test);
   procedure Test_Get_Reputation_da24fe_e0b5d2(Gnattest_T: in out Test) renames
     Test_Get_Reputation_test_getreputation;
--  id:2.2/da24fe8e0fd6fb84/Get_Reputation/1/0/test_getreputation/
   procedure Test_Get_Reputation_test_getreputation(Gnattest_T: in out Test) is
      function Get_Reputation
        (Source_Faction, Target_Faction: Unbounded_String)
         return Integer renames
        Wrap_Test_Get_Reputation_da24fe_e0b5d2;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Assert
        (Get_Reputation
           (To_Unbounded_String("POLEIS"), To_Unbounded_String("POLEIS")) =
         0,
         "Failed to get reputation for Poleis to Poleis.");
      Assert
        (Get_Reputation
           (To_Unbounded_String("POLEIS"), To_Unbounded_String("PIRATES")) =
         -10,
         "Failed to get reputation for Poleis to Pirates.");

--  begin read only
   end Test_Get_Reputation_test_getreputation;
--  end read only

--  begin read only
   function Wrap_Test_Is_Friendly_7378ce_37dbd7
     (Source_Faction, Target_Faction: Unbounded_String) return Boolean is
   begin
      begin
         pragma Assert
           ((Factions_List.Contains(Source_Faction) and
             Factions_List.Contains(Target_Faction)));
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(factions.ads:0):Test_IsFriendly test requirement violated");
      end;
      declare
         Test_Is_Friendly_7378ce_37dbd7_Result: constant Boolean :=
           GNATtest_Generated.GNATtest_Standard.Factions.Is_Friendly
             (Source_Faction, Target_Faction);
      begin
         begin
            pragma Assert(True);
            null;
         exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "ens_sloc(factions.ads:0:):Test_IsFriendly test commitment violated");
         end;
         return Test_Is_Friendly_7378ce_37dbd7_Result;
      end;
   end Wrap_Test_Is_Friendly_7378ce_37dbd7;
--  end read only

--  begin read only
   procedure Test_Is_Friendly_test_isfriendly(Gnattest_T: in out Test);
   procedure Test_Is_Friendly_7378ce_37dbd7(Gnattest_T: in out Test) renames
     Test_Is_Friendly_test_isfriendly;
--  id:2.2/7378cef796f8bb48/Is_Friendly/1/0/test_isfriendly/
   procedure Test_Is_Friendly_test_isfriendly(Gnattest_T: in out Test) is
      function Is_Friendly
        (Source_Faction, Target_Faction: Unbounded_String)
         return Boolean renames
        Wrap_Test_Is_Friendly_7378ce_37dbd7;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Assert
        (Is_Friendly
           (To_Unbounded_String("POLEIS"),
            To_Unbounded_String("INDEPENDENT")) =
         True,
         "Failed to check two friendly factions.");
      Assert
        (Is_Friendly
           (To_Unbounded_String("POLEIS"), To_Unbounded_String("PIRATES")) =
         False,
         "Failed to check two unfriendly factions.");

--  begin read only
   end Test_Is_Friendly_test_isfriendly;
--  end read only

--  begin read only
   function Wrap_Test_Get_Random_Faction_477b1c_103989
      return Unbounded_String is
   begin
      declare
         Test_Get_Random_Faction_477b1c_103989_Result: constant Unbounded_String :=
           GNATtest_Generated.GNATtest_Standard.Factions.Get_Random_Faction;
      begin
         return Test_Get_Random_Faction_477b1c_103989_Result;
      end;
   end Wrap_Test_Get_Random_Faction_477b1c_103989;
--  end read only

--  begin read only
   procedure Test_Get_Random_Faction_test_getrandomfaction
     (Gnattest_T: in out Test);
   procedure Test_Get_Random_Faction_477b1c_103989
     (Gnattest_T: in out Test) renames
     Test_Get_Random_Faction_test_getrandomfaction;
--  id:2.2/477b1cd65ed16393/Get_Random_Faction/1/0/test_getrandomfaction/
   procedure Test_Get_Random_Faction_test_getrandomfaction
     (Gnattest_T: in out Test) is
      function Get_Random_Faction return Unbounded_String renames
        Wrap_Test_Get_Random_Faction_477b1c_103989;
--  end read only

      pragma Unreferenced(Gnattest_T);
      FactionName: Unbounded_String := Null_Unbounded_String;

   begin

      FactionName := Get_Random_Faction;
      Assert
        (FactionName /= Null_Unbounded_String,
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
