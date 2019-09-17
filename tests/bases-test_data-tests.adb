--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Bases.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only

with Maps; use Maps;
with Events; use Events;

--  begin read only
--  end read only
package body Bases.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only
--  begin read only
   procedure Wrap_Test_GainRep_6338e6_901e58 (BaseIndex: BasesRange; Points: Integer) 
   is
   begin
      GNATtest_Generated.GNATtest_Standard.Bases.GainRep (BaseIndex, Points);
   end Wrap_Test_GainRep_6338e6_901e58;
--  end read only

--  begin read only
   procedure Test_GainRep_test_gainrep (Gnattest_T : in out Test);
   procedure Test_GainRep_6338e6_901e58 (Gnattest_T : in out Test) renames Test_GainRep_test_gainrep;
--  id:2.2/6338e6483a422dde/GainRep/1/0/test_gainrep/
   procedure Test_GainRep_test_gainrep (Gnattest_T : in out Test) is
   procedure GainRep (BaseIndex: BasesRange; Points: Integer) renames Wrap_Test_GainRep_6338e6_901e58;
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      SkyBases(1).Reputation := (1, 1);
      GainRep(1, 1);
      Assert(SkyBases(1).Reputation(2) = 2, "Failed to gain reputation in base.");
      GainRep(1, -1);
      Assert(SkyBases(1).Reputation(2) = 1, "Failed to lose reputation in base.");

--  begin read only
   end Test_GainRep_test_gainrep;
--  end read only

--  begin read only
   procedure Wrap_Test_CountPrice_173272_bef05e (Price: in out Natural; TraderIndex: Crew_Container.Extended_Index; Reduce: Boolean := True) 
   is
   begin
      begin
         pragma Assert
           (TraderIndex <= PlayerShip.Crew.Last_Index);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(bases.ads:0):Test_CountPrice test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Bases.CountPrice (Price, TraderIndex, Reduce);
      begin
         pragma Assert
           (True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(bases.ads:0:):Test_CountPrice test commitment violated");
      end;
   end Wrap_Test_CountPrice_173272_bef05e;
--  end read only

--  begin read only
   procedure Test_CountPrice_test_countprice (Gnattest_T : in out Test);
   procedure Test_CountPrice_173272_bef05e (Gnattest_T : in out Test) renames Test_CountPrice_test_countprice;
--  id:2.2/17327298eafedc9a/CountPrice/1/0/test_countprice/
   procedure Test_CountPrice_test_countprice (Gnattest_T : in out Test) is
   procedure CountPrice (Price: in out Natural; TraderIndex: Crew_Container.Extended_Index; Reduce: Boolean := True) renames Wrap_Test_CountPrice_173272_bef05e;
--  end read only

      pragma Unreferenced (Gnattest_T);
      Price: Positive := 100;

   begin

      CountPrice(Price, 1, False);
      Assert(Price > 100, "Failed to raise price in base.");
      Price := 100;
      CountPrice(Price, 1);
      Assert(Price < 100, "Failed to reduce price in base.");

--  begin read only
   end Test_CountPrice_test_countprice;
--  end read only

--  begin read only
   function Wrap_Test_GenerateBaseName_e09aa7_c4cd74 (FactionIndex: Unbounded_String)  return Unbounded_String
   is
   begin
      begin
         pragma Assert
           (Factions_Container.Contains(Factions_List, FactionIndex));
         null;
      exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "req_sloc(bases.ads:0):Test_GenerateBaseName test requirement violated");
      end;
      declare
         Test_GenerateBaseName_e09aa7_c4cd74_Result : constant Unbounded_String := GNATtest_Generated.GNATtest_Standard.Bases.GenerateBaseName (FactionIndex);
      begin
         begin
            pragma Assert
              (True);
            null;
         exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "ens_sloc(bases.ads:0:):Test_GenerateBaseName test commitment violated");
         end;
         return Test_GenerateBaseName_e09aa7_c4cd74_Result;
      end;
   end Wrap_Test_GenerateBaseName_e09aa7_c4cd74;
--  end read only

--  begin read only
   procedure Test_GenerateBaseName_test_generatebasename (Gnattest_T : in out Test);
   procedure Test_GenerateBaseName_e09aa7_c4cd74 (Gnattest_T : in out Test) renames Test_GenerateBaseName_test_generatebasename;
--  id:2.2/e09aa72173a8bcc3/GenerateBaseName/1/0/test_generatebasename/
   procedure Test_GenerateBaseName_test_generatebasename (Gnattest_T : in out Test) is
      function GenerateBaseName (FactionIndex: Unbounded_String) return Unbounded_String renames Wrap_Test_GenerateBaseName_e09aa7_c4cd74;
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      Assert(GenerateBaseName(To_Unbounded_String("POLEIS")) /= Null_Unbounded_String, "Failed to generate new base name.");

--  begin read only
   end Test_GenerateBaseName_test_generatebasename;
--  end read only

--  begin read only
   procedure Wrap_Test_GenerateRecruits_71442c_06ea09
   is
   begin
      GNATtest_Generated.GNATtest_Standard.Bases.GenerateRecruits;
   end Wrap_Test_GenerateRecruits_71442c_06ea09;
--  end read only

--  begin read only
   procedure Test_GenerateRecruits_test_generaterecruits (Gnattest_T : in out Test);
   procedure Test_GenerateRecruits_71442c_06ea09 (Gnattest_T : in out Test) renames Test_GenerateRecruits_test_generaterecruits;
--  id:2.2/71442c2bd1e072c1/GenerateRecruits/1/0/test_generaterecruits/
   procedure Test_GenerateRecruits_test_generaterecruits (Gnattest_T : in out Test) is
   procedure GenerateRecruits renames Wrap_Test_GenerateRecruits_71442c_06ea09;
--  end read only

      pragma Unreferenced (Gnattest_T);
      BaseIndex: constant Positive :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;

   begin

      SkyBases(BaseIndex).Recruits.Clear;
      SkyBases(BaseIndex).RecruitDate := (others => 0);
      GenerateRecruits;
      Assert(SkyBases(BaseIndex).Recruits.Length > 0, "Failed to generate recruits.");

--  begin read only
   end Test_GenerateRecruits_test_generaterecruits;
--  end read only

--  begin read only
   procedure Wrap_Test_AskForBases_73243b_f3f6c6
   is
   begin
      GNATtest_Generated.GNATtest_Standard.Bases.AskForBases;
   end Wrap_Test_AskForBases_73243b_f3f6c6;
--  end read only

--  begin read only
   procedure Test_AskForBases_test_askforbases (Gnattest_T : in out Test);
   procedure Test_AskForBases_73243b_f3f6c6 (Gnattest_T : in out Test) renames Test_AskForBases_test_askforbases;
--  id:2.2/73243b5c6c15a56d/AskForBases/1/0/test_askforbases/
   procedure Test_AskForBases_test_askforbases (Gnattest_T : in out Test) is
   procedure AskForBases renames Wrap_Test_AskForBases_73243b_f3f6c6;
--  end read only

      pragma Unreferenced (Gnattest_T);
      BaseIndex: constant Positive :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;

   begin

      SkyBases(BaseIndex).AskedForBases := False;
      AskForBases;
      Assert(True, "This test can only crash.");

--  begin read only
   end Test_AskForBases_test_askforbases;
--  end read only

--  begin read only
   procedure Wrap_Test_AskForEvents_2dde2f_3e359b
   is
   begin
      GNATtest_Generated.GNATtest_Standard.Bases.AskForEvents;
   end Wrap_Test_AskForEvents_2dde2f_3e359b;
--  end read only

--  begin read only
   procedure Test_AskForEvents_test_askforevents (Gnattest_T : in out Test);
   procedure Test_AskForEvents_2dde2f_3e359b (Gnattest_T : in out Test) renames Test_AskForEvents_test_askforevents;
--  id:2.2/2dde2f14a34f8154/AskForEvents/1/0/test_askforevents/
   procedure Test_AskForEvents_test_askforevents (Gnattest_T : in out Test) is
   procedure AskForEvents renames Wrap_Test_AskForEvents_2dde2f_3e359b;
--  end read only

      pragma Unreferenced (Gnattest_T);
      BaseIndex: constant Positive :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      Amount: constant Natural := Natural(Events_List.Length);

   begin

      SkyBases(BaseIndex).AskedForEvents := (others => 0);
      AskForEvents;
      Assert(Natural(Events_List.Length) > Amount, "Failed to ask for events in base.");

--  begin read only
   end Test_AskForEvents_test_askforevents;
--  end read only

--  begin read only
   procedure Wrap_Test_UpdatePopulation_b3200e_10dec8
   is
   begin
      GNATtest_Generated.GNATtest_Standard.Bases.UpdatePopulation;
   end Wrap_Test_UpdatePopulation_b3200e_10dec8;
--  end read only

--  begin read only
   procedure Test_UpdatePopulation_test_updatepopulation (Gnattest_T : in out Test);
   procedure Test_UpdatePopulation_b3200e_10dec8 (Gnattest_T : in out Test) renames Test_UpdatePopulation_test_updatepopulation;
--  id:2.2/b3200e8f4431ca20/UpdatePopulation/1/0/test_updatepopulation/
   procedure Test_UpdatePopulation_test_updatepopulation (Gnattest_T : in out Test) is
   procedure UpdatePopulation renames Wrap_Test_UpdatePopulation_b3200e_10dec8;
--  end read only

      pragma Unreferenced (Gnattest_T);
      BaseIndex: constant Positive :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;

   begin

      SkyBases(BaseIndex).RecruitDate := (others => 0);
      UpdatePopulation;
      Assert(True, "This test can only crash.");

--  begin read only
   end Test_UpdatePopulation_test_updatepopulation;
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
end Bases.Test_Data.Tests;
