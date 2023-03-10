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
   procedure Wrap_Test_Generate_Recruits_05fabc_06ea09 is
   begin
      GNATtest_Generated.GNATtest_Standard.Bases.Generate_Recruits;
   end Wrap_Test_Generate_Recruits_05fabc_06ea09;
--  end read only

--  begin read only
   procedure Test_Generate_Recruits_test_generaterecruits
     (Gnattest_T: in out Test);
   procedure Test_Generate_Recruits_05fabc_06ea09
     (Gnattest_T: in out Test) renames
     Test_Generate_Recruits_test_generaterecruits;
--  id:2.2/05fabc33d45842e6/Generate_Recruits/1/0/test_generaterecruits/
   procedure Test_Generate_Recruits_test_generaterecruits
     (Gnattest_T: in out Test) is
      procedure Generate_Recruits renames
        Wrap_Test_Generate_Recruits_05fabc_06ea09;
--  end read only

      pragma Unreferenced(Gnattest_T);
      BaseIndex: constant Positive :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      OldReputation: constant Integer := Sky_Bases(BaseIndex).Reputation.Level;

   begin

      Recruit_Container.Clear(Container => Sky_Bases(BaseIndex).Recruits);
      Sky_Bases(BaseIndex).Recruit_Date := (others => 0);
      Sky_Bases(BaseIndex).Reputation.Level := 1;
      Generate_Recruits;
      Assert
        (Recruit_Container.Length(Container => Sky_Bases(BaseIndex).Recruits) >
         0,
         "Failed to generate recruits for bases with positive reputation.");
      Recruit_Container.Clear(Container => Sky_Bases(BaseIndex).Recruits);
      Sky_Bases(BaseIndex).Recruit_Date := (others => 0);
      Sky_Bases(BaseIndex).Reputation.Level := -50;
      Generate_Recruits;
      Assert
        (Recruit_Container.Length(Container => Sky_Bases(BaseIndex).Recruits) >
         0,
         "Failed to generate recruits for bases with negative reputation.");
      Recruit_Container.Clear(Container => Sky_Bases(BaseIndex).Recruits);
      Sky_Bases(BaseIndex).Recruit_Date := (others => 0);
      Sky_Bases(BaseIndex).Reputation.Level := 0;
      Generate_Recruits;
      Assert
        (Recruit_Container.Length(Container => Sky_Bases(BaseIndex).Recruits) >
         0,
         "Failed to generate recruits for bases with no reputation.");
      Sky_Bases(BaseIndex).Reputation.Level := OldReputation;

--  begin read only
   end Test_Generate_Recruits_test_generaterecruits;
--  end read only

--  begin read only
   procedure Wrap_Test_Ask_For_Bases_023c5f_f3f6c6 is
   begin
      GNATtest_Generated.GNATtest_Standard.Bases.Ask_For_Bases;
   end Wrap_Test_Ask_For_Bases_023c5f_f3f6c6;
--  end read only

--  begin read only
   procedure Test_Ask_For_Bases_test_askforbases(Gnattest_T: in out Test);
   procedure Test_Ask_For_Bases_023c5f_f3f6c6(Gnattest_T: in out Test) renames
     Test_Ask_For_Bases_test_askforbases;
--  id:2.2/023c5f5732e0c1b0/Ask_For_Bases/1/0/test_askforbases/
   procedure Test_Ask_For_Bases_test_askforbases(Gnattest_T: in out Test) is
      procedure Ask_For_Bases renames Wrap_Test_Ask_For_Bases_023c5f_f3f6c6;
--  end read only

      pragma Unreferenced(Gnattest_T);
      BaseIndex: constant Positive :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;

   begin

      Sky_Bases(BaseIndex).Asked_For_Bases := False;
      Ask_For_Bases;
      Assert(True, "This test can only crash.");

--  begin read only
   end Test_Ask_For_Bases_test_askforbases;
--  end read only

--  begin read only
   procedure Wrap_Test_Ask_For_Events_4f17f6_3e359b is
   begin
      GNATtest_Generated.GNATtest_Standard.Bases.Ask_For_Events;
   end Wrap_Test_Ask_For_Events_4f17f6_3e359b;
--  end read only

--  begin read only
   procedure Test_Ask_For_Events_test_askforevents(Gnattest_T: in out Test);
   procedure Test_Ask_For_Events_4f17f6_3e359b(Gnattest_T: in out Test) renames
     Test_Ask_For_Events_test_askforevents;
--  id:2.2/4f17f6e7a1281fa6/Ask_For_Events/1/0/test_askforevents/
   procedure Test_Ask_For_Events_test_askforevents(Gnattest_T: in out Test) is
      procedure Ask_For_Events renames Wrap_Test_Ask_For_Events_4f17f6_3e359b;
--  end read only

      pragma Unreferenced(Gnattest_T);
      BaseIndex: constant Positive :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      Amount: constant Natural := Natural(Events_List.Length);

   begin

      Sky_Bases(BaseIndex).Asked_For_Events := (others => 0);
      Ask_For_Events;
      Assert
        (Natural(Events_List.Length) > Amount,
         "Failed to ask for events in base.");

--  begin read only
   end Test_Ask_For_Events_test_askforevents;
--  end read only

--  begin read only
   procedure Wrap_Test_Update_Population_31557d_10dec8 is
   begin
      GNATtest_Generated.GNATtest_Standard.Bases.Update_Population;
   end Wrap_Test_Update_Population_31557d_10dec8;
--  end read only

--  begin read only
   procedure Test_Update_Population_test_updatepopulation
     (Gnattest_T: in out Test);
   procedure Test_Update_Population_31557d_10dec8
     (Gnattest_T: in out Test) renames
     Test_Update_Population_test_updatepopulation;
--  id:2.2/31557dac2b0606af/Update_Population/1/0/test_updatepopulation/
   procedure Test_Update_Population_test_updatepopulation
     (Gnattest_T: in out Test) is
      procedure Update_Population renames
        Wrap_Test_Update_Population_31557d_10dec8;
--  end read only

      pragma Unreferenced(Gnattest_T);
      BaseIndex: constant Positive :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;

   begin

      Sky_Bases(BaseIndex).Recruit_Date := (others => 0);
      Update_Population;
      Assert(True, "This test can only crash.");

--  begin read only
   end Test_Update_Population_test_updatepopulation;
--  end read only

--  begin read only
   procedure Wrap_Test_Update_Prices_56b29f_f6cd8d is
   begin
      GNATtest_Generated.GNATtest_Standard.Bases.Update_Prices;
   end Wrap_Test_Update_Prices_56b29f_f6cd8d;
--  end read only

--  begin read only
   procedure Test_Update_Prices_test_updateprices(Gnattest_T: in out Test);
   procedure Test_Update_Prices_56b29f_f6cd8d(Gnattest_T: in out Test) renames
     Test_Update_Prices_test_updateprices;
--  id:2.2/56b29fc1748d90da/Update_Prices/1/0/test_updateprices/
   procedure Test_Update_Prices_test_updateprices(Gnattest_T: in out Test) is
      procedure Update_Prices renames Wrap_Test_Update_Prices_56b29f_f6cd8d;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Update_Prices;
      Assert(True, "This test can only crash.");

--  begin read only
   end Test_Update_Prices_test_updateprices;
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
