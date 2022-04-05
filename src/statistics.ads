--    Copyright 2017-2022 Bartek thindil Jasicki
--
--    This file is part of Steam Sky.
--
--    Steam Sky is free software: you can redistribute it and/or modify
--    it under the terms of the GNU General Public License as published by
--    the Free Software Foundation, either version 3 of the License, or
--    (at your option) any later version.
--
--    Steam Sky is distributed in the hope that it will be useful,
--    but WITHOUT ANY WARRANTY; without even the implied warranty of
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--    GNU General Public License for more details.
--
--    You should have received a copy of the GNU General Public License
--    along with Steam Sky.  If not, see <http://www.gnu.org/licenses/>.

with Ada.Containers.Vectors; use Ada.Containers;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Crew; use Crew;
with Game; use Game;
use Game.Tiny_String;

-- ****h* Statistics/Statistics
-- FUNCTION
-- Provides code for manipulate the game statistic
-- SOURCE
package Statistics is
-- ****

   -- ****s* Statistics/Statistics.Statistics_Data
   -- FUNCTION
   -- Data for finished goals, destroyed ships and killed mobs
   -- PARAMETERS
   -- Index  - Index of goal or ship name or name of fraction of killed mobs
   -- Amount - Amount of finished goals or ships or mobs of that type
   -- SOURCE
   type Statistics_Data is record
      Index: Unbounded_String;
      Amount: Positive := 1;
   end record;
   -- ****

   -- ****t* Statistics/Statistics.Statistics_Container
   -- FUNCTION
   -- Used to store game statistics data
   -- SOURCE
   package Statistics_Container is new Vectors
     (Index_Type => Positive, Element_Type => Statistics_Data);
   -- ****

   -- ****s* Statistics/Statistics.Game_Stats_Data
   -- FUNCTION
   -- Data for game statistics
   -- PARAMETERS
   -- Destroyed_Ships   - Data for all destroyed ships by player
   -- Bases_Visited     - Amount of visited bases
   -- Map_Visited       - Amount of visited map fields
   -- Distance_Traveled - Amount of map fields travelled
   -- Crafting_Orders   - Data for finished crafting orders
   -- Accepted_Missions - Amount of accepted missions
   -- Finished_Missions - Data for all finished missions
   -- Finished_Goals    - Data for all finished goals
   -- Killed_Mobs       - Data for all mobs killed by player
   -- Points            - Amount of gained points
   -- SOURCE
   type Game_Stats_Data is record
      Destroyed_Ships: Statistics_Container.Vector;
      Bases_Visited: Bases_Range;
      Map_Visited: Positive := 1;
      Distance_Traveled: Natural := 0;
      Crafting_Orders: Statistics_Container.Vector;
      Accepted_Missions: Natural := 0;
      Finished_Missions: Statistics_Container.Vector;
      Finished_Goals: Statistics_Container.Vector;
      Killed_Mobs: Statistics_Container.Vector;
      Points: Natural := 0;
   end record;
   -- ****

   -- ****v* Statistics/Statistics.Game_Stats
   -- FUNCTION
   -- Game statistics
   -- SOURCE
   Game_Stats: Game_Stats_Data;
   -- ****

   -- ****f* Statistics/Statistics.Update_Destroyed_Ships
   -- FUNCTION
   -- Add new destroyed ship to list
   -- PARAMETERS
   -- Ship_Name - Name of the ship to add to destroyed list
   -- SOURCE
   procedure Update_Destroyed_Ships(Ship_Name: Tiny_String.Bounded_String) with
      Pre => Ship_Name /= Tiny_String.Null_Bounded_String,
      Test_Case => (Name => "Test_UpdateDestroyedShips", Mode => Nominal);
      -- ****

      -- ****f* Statistics/Statistics.Clear_Game_Stats
      -- FUNCTION
      -- Clear game statistics
      -- SOURCE
   procedure Clear_Game_Stats with
      Post => Game_Stats.Points = 0,
      Test_Case => (Name => "Test_ClearGameStats", Mode => Nominal);
      -- ****

      -- ****f* Statistics/Statistics.Update_Finished_Goals
      -- FUNCTION
      -- Add new finished goal to list
      -- PARAMETERS
      -- Index - Index of goal to update
      -- SOURCE
   procedure Update_Finished_Goals(Index: Unbounded_String) with
      Pre => Index /= Null_Unbounded_String,
      Test_Case => (Name => "Test_UpdateFinishedGoals", Mode => Nominal);
      -- ****

      -- ****f* Statistics/Statistics.Update_Finished_Missions
      -- FUNCTION
      -- Add new finished mission to list
      -- PARAMETERS
      -- M_Type - Type of mission to update
      -- SOURCE
   procedure Update_Finished_Missions(M_Type: Unbounded_String) with
      Pre => M_Type /= Null_Unbounded_String,
      Test_Case => (Name => "Test_UpdateFinishedMissions", Mode => Nominal);
      -- ****

      -- ****f* Statistics/Statistics.Update_Crafting_Orders
      -- FUNCTION
      -- Add new finished crafting order to list
      -- PARAMETERS
      -- Index - Index of crafting order to update
      -- SOURCE
   procedure Update_Crafting_Orders(Index: Unbounded_String) with
      Pre => Index /= Null_Unbounded_String,
      Test_Case => (Name => "Test_UpdateCraftingOrders", Mode => Nominal);
      -- ****

      -- ****f* Statistics/Statistics.Update_Killed_Mobs
      -- FUNCTION
      -- Add new killed mob to list
      -- PARAMETERS
      -- Mob          - Killed mobile data
      -- Faction_Name - Faction name to which killed mobile belongs
      -- SOURCE
   procedure Update_Killed_Mobs
     (Mob: Member_Data; Fraction_Name: Unbounded_String) with
      Pre => Fraction_Name /= Null_Unbounded_String,
      Test_Case => (Name => "Test_UpdateKilledMobs", Mode => Nominal);
      -- ****

      -- ****f* Statistics/Statistics.Get_Game_Points
      -- FUNCTION
      -- Get amount of gained points multiplied by difficulty bonus
      -- RESULT
      -- Amount of gained points by player in this game
      -- SOURCE
   function Get_Game_Points return Natural with
      Test_Case => (Name => "Test_GetGamePoints", Mode => Robustness);
      -- ****

end Statistics;
