--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Statistics.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only

with Ships; use Ships;

--  begin read only
--  end read only
package body Statistics.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only
--  begin read only
   procedure Wrap_Test_Update_Destroyed_Ships_151582_3aed49
     (Ship_Name: Tiny_String.Bounded_String) is
   begin
      begin
         pragma Assert(Ship_Name /= Tiny_String.Null_Bounded_String);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(statistics.ads:0):Test_UpdateDestroyedShips test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Statistics.Update_Destroyed_Ships
        (Ship_Name);
      begin
         pragma Assert(True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(statistics.ads:0:):Test_UpdateDestroyedShips test commitment violated");
      end;
   end Wrap_Test_Update_Destroyed_Ships_151582_3aed49;
--  end read only

--  begin read only
   procedure Test_Update_Destroyed_Ships_test_updatedestroyedships
     (Gnattest_T: in out Test);
   procedure Test_Update_Destroyed_Ships_151582_3aed49
     (Gnattest_T: in out Test) renames
     Test_Update_Destroyed_Ships_test_updatedestroyedships;
--  id:2.2/151582b07ae3ffa1/Update_Destroyed_Ships/1/0/test_updatedestroyedships/
   procedure Test_Update_Destroyed_Ships_test_updatedestroyedships
     (Gnattest_T: in out Test) is
      procedure Update_Destroyed_Ships
        (Ship_Name: Tiny_String.Bounded_String) renames
        Wrap_Test_Update_Destroyed_Ships_151582_3aed49;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Update_Destroyed_Ships(To_Bounded_String("Tiny pirates ship"));
      Assert
        (Game_Stats.Destroyed_Ships.Length = 1,
         "Failed to add ship to destroyed ships list.");
      Update_Destroyed_Ships(To_Bounded_String("Sfdsfdsf"));
      Assert
        (Game_Stats.Destroyed_Ships.Length = 1,
         "Failed to not add non existing ship to destroyed ships list.");

--  begin read only
   end Test_Update_Destroyed_Ships_test_updatedestroyedships;
--  end read only

--  begin read only
   procedure Wrap_Test_Clear_Game_Stats_b75ef6_2f902c is
   begin
      begin
         pragma Assert(True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(statistics.ads:0):Test_ClearGameStats test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Statistics.Clear_Game_Stats;
      begin
         pragma Assert(Game_Stats.Points = 0);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(statistics.ads:0:):Test_ClearGameStats test commitment violated");
      end;
   end Wrap_Test_Clear_Game_Stats_b75ef6_2f902c;
--  end read only

--  begin read only
   procedure Test_Clear_Game_Stats_test_cleargamestats
     (Gnattest_T: in out Test);
   procedure Test_Clear_Game_Stats_b75ef6_2f902c
     (Gnattest_T: in out Test) renames
     Test_Clear_Game_Stats_test_cleargamestats;
--  id:2.2/b75ef638f8dc802f/Clear_Game_Stats/1/0/test_cleargamestats/
   procedure Test_Clear_Game_Stats_test_cleargamestats
     (Gnattest_T: in out Test) is
      procedure Clear_Game_Stats renames
        Wrap_Test_Clear_Game_Stats_b75ef6_2f902c;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Clear_Game_Stats;
      Assert
        (Game_Stats.Destroyed_Ships.Length = 0,
         "Failed to clear game statistics.");

--  begin read only
   end Test_Clear_Game_Stats_test_cleargamestats;
--  end read only

--  begin read only
   procedure Wrap_Test_Update_Finished_Goals_77cad5_51796d
     (Index: Unbounded_String) is
   begin
      begin
         pragma Assert(Index /= Null_Unbounded_String);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(statistics.ads:0):Test_UpdateFinishedGoals test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Statistics.Update_Finished_Goals
        (Index);
      begin
         pragma Assert(True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(statistics.ads:0:):Test_UpdateFinishedGoals test commitment violated");
      end;
   end Wrap_Test_Update_Finished_Goals_77cad5_51796d;
--  end read only

--  begin read only
   procedure Test_Update_Finished_Goals_test_updatefinishedgoals
     (Gnattest_T: in out Test);
   procedure Test_Update_Finished_Goals_77cad5_51796d
     (Gnattest_T: in out Test) renames
     Test_Update_Finished_Goals_test_updatefinishedgoals;
--  id:2.2/77cad5e4fc2b052e/Update_Finished_Goals/1/0/test_updatefinishedgoals/
   procedure Test_Update_Finished_Goals_test_updatefinishedgoals
     (Gnattest_T: in out Test) is
      procedure Update_Finished_Goals(Index: Unbounded_String) renames
        Wrap_Test_Update_Finished_Goals_77cad5_51796d;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Update_Finished_Goals(To_Unbounded_String("1"));
      Assert
        (Game_Stats.Finished_Goals.Length = 1,
         "Failed to add goal to finished goals list.");
      Update_Finished_Goals(To_Unbounded_String("Sfdsfdsf"));
      Assert
        (Game_Stats.Finished_Goals.Length = 1,
         "Failed to not add non existing goal to finished goals list.");

--  begin read only
   end Test_Update_Finished_Goals_test_updatefinishedgoals;
--  end read only

--  begin read only
   procedure Wrap_Test_Update_Finished_Missions_dc1df6_dab6ba
     (M_Type: Unbounded_String) is
   begin
      begin
         pragma Assert(M_Type /= Null_Unbounded_String);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(statistics.ads:0):Test_UpdateFinishedMissions test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Statistics.Update_Finished_Missions
        (M_Type);
      begin
         pragma Assert(True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(statistics.ads:0:):Test_UpdateFinishedMissions test commitment violated");
      end;
   end Wrap_Test_Update_Finished_Missions_dc1df6_dab6ba;
--  end read only

--  begin read only
   procedure Test_Update_Finished_Missions_test_updatefinishedmissions
     (Gnattest_T: in out Test);
   procedure Test_Update_Finished_Missions_dc1df6_dab6ba
     (Gnattest_T: in out Test) renames
     Test_Update_Finished_Missions_test_updatefinishedmissions;
--  id:2.2/dc1df6a630d0cf7a/Update_Finished_Missions/1/0/test_updatefinishedmissions/
   procedure Test_Update_Finished_Missions_test_updatefinishedmissions
     (Gnattest_T: in out Test) is
      procedure Update_Finished_Missions(M_Type: Unbounded_String) renames
        Wrap_Test_Update_Finished_Missions_dc1df6_dab6ba;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Update_Finished_Missions(To_Unbounded_String("DESTROY"));
      Assert
        (Game_Stats.Finished_Missions.Length = 1,
         "Failed to add mission to finished missions list.");
      Update_Finished_Missions(To_Unbounded_String("Sfdsfdsf"));
      Assert
        (Game_Stats.Finished_Goals.Length = 1,
         "Failed to not add non existing mission to finished missions list.");

--  begin read only
   end Test_Update_Finished_Missions_test_updatefinishedmissions;
--  end read only

--  begin read only
   procedure Wrap_Test_Update_Crafting_Orders_26bcdc_6660d1
     (Index: Tiny_String.Bounded_String) is
   begin
      begin
         pragma Assert(Tiny_String.Length(Source => Index) > 0);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(statistics.ads:0):Test_UpdateCraftingOrders test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Statistics.Update_Crafting_Orders
        (Index);
      begin
         pragma Assert(True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(statistics.ads:0:):Test_UpdateCraftingOrders test commitment violated");
      end;
   end Wrap_Test_Update_Crafting_Orders_26bcdc_6660d1;
--  end read only

--  begin read only
   procedure Test_Update_Crafting_Orders_test_updatecraftingorders
     (Gnattest_T: in out Test);
   procedure Test_Update_Crafting_Orders_26bcdc_6660d1
     (Gnattest_T: in out Test) renames
     Test_Update_Crafting_Orders_test_updatecraftingorders;
--  id:2.2/26bcdc4d93afefce/Update_Crafting_Orders/1/0/test_updatecraftingorders/
   procedure Test_Update_Crafting_Orders_test_updatecraftingorders
     (Gnattest_T: in out Test) is
      procedure Update_Crafting_Orders
        (Index: Tiny_String.Bounded_String) renames
        Wrap_Test_Update_Crafting_Orders_26bcdc_6660d1;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Update_Crafting_Orders(Tiny_String.To_Bounded_String("1"));
      Assert
        (Game_Stats.Crafting_Orders.Length = 1,
         "Failed to add finished crafting order to game statistics.");

--  begin read only
   end Test_Update_Crafting_Orders_test_updatecraftingorders;
--  end read only

--  begin read only
   procedure Wrap_Test_Update_Killed_Mobs_d347f4_3672b4
     (Mob: Member_Data; Fraction_Name: Unbounded_String) is
   begin
      begin
         pragma Assert(Fraction_Name /= Null_Unbounded_String);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(statistics.ads:0):Test_UpdateKilledMobs test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Statistics.Update_Killed_Mobs
        (Mob, Fraction_Name);
      begin
         pragma Assert(True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(statistics.ads:0:):Test_UpdateKilledMobs test commitment violated");
      end;
   end Wrap_Test_Update_Killed_Mobs_d347f4_3672b4;
--  end read only

--  begin read only
   procedure Test_Update_Killed_Mobs_test_updatekilledmobs
     (Gnattest_T: in out Test);
   procedure Test_Update_Killed_Mobs_d347f4_3672b4
     (Gnattest_T: in out Test) renames
     Test_Update_Killed_Mobs_test_updatekilledmobs;
--  id:2.2/d347f4cb9002fbb0/Update_Killed_Mobs/1/0/test_updatekilledmobs/
   procedure Test_Update_Killed_Mobs_test_updatekilledmobs
     (Gnattest_T: in out Test) is
      procedure Update_Killed_Mobs
        (Mob: Member_Data; Fraction_Name: Unbounded_String) renames
        Wrap_Test_Update_Killed_Mobs_d347f4_3672b4;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Update_Killed_Mobs(Player_Ship.Crew(2), To_Unbounded_String("POLEIS"));
      Assert
        (Game_Stats.Killed_Mobs.Length = 1,
         "Failed to add killed mob to game statistics.");

--  begin read only
   end Test_Update_Killed_Mobs_test_updatekilledmobs;
--  end read only

--  begin read only
   function Wrap_Test_Get_Game_Points_9ad2e4_4eed1d return Natural is
   begin
      declare
         Test_Get_Game_Points_9ad2e4_4eed1d_Result: constant Natural :=
           GNATtest_Generated.GNATtest_Standard.Statistics.Get_Game_Points;
      begin
         return Test_Get_Game_Points_9ad2e4_4eed1d_Result;
      end;
   end Wrap_Test_Get_Game_Points_9ad2e4_4eed1d;
--  end read only

--  begin read only
   procedure Test_Get_Game_Points_test_getgamepoints(Gnattest_T: in out Test);
   procedure Test_Get_Game_Points_9ad2e4_4eed1d
     (Gnattest_T: in out Test) renames
     Test_Get_Game_Points_test_getgamepoints;
--  id:2.2/9ad2e4143cf06854/Get_Game_Points/1/0/test_getgamepoints/
   procedure Test_Get_Game_Points_test_getgamepoints
     (Gnattest_T: in out Test) is
      function Get_Game_Points return Natural renames
        Wrap_Test_Get_Game_Points_9ad2e4_4eed1d;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      if Get_Game_Points = 0 then
         Assert(True, "This test can only crash.");
         return;
      end if;
      Assert(True, "This test can only crash.");

--  begin read only
   end Test_Get_Game_Points_test_getgamepoints;
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
end Statistics.Test_Data.Tests;
