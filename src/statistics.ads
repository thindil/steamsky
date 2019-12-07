--    Copyright 2017-2019 Bartek thindil Jasicki
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

-- ****h* Steamsky/Statistics
-- FUNCTION
-- Provides code for manipulate the game statistic
-- SOURCE
package Statistics is
-- ****

   -- ****t* Statistics/Statistics_Data
   -- FUNCTION
   -- Data for finished goals, destroyed ships and killed mobs
   -- PARAMETERS
   -- Index  - Index of goal or ship name or name of fraction of killed mobs
   -- Amount - Amount of finished goals or ships or mobs of that type
   -- SOURCE
   type Statistics_Data is record
      Index: Unbounded_String;
      Amount: Positive;
   end record;
   -- ****

   -- ****t* Statistics/Statistics_Container
   -- FUNCTION
   -- Used to store game statistics data
   -- SOURCE
   package Statistics_Container is new Vectors(Positive, Statistics_Data);
   -- ****

   -- ****t* Statistics/GameStats_Data
   -- FUNCTION
   -- Data for game statistics
   -- PARAMETERS
   -- DestroyedShips   - Data for all destroyed ships by player
   -- BasesVisited     - Amount of visited bases
   -- MapVisited       - Amount of visited map fields
   -- DistanceTraveled - Amount of map fields travelled
   -- CraftingOrders   - Data for finished crafting orders
   -- AcceptedMissions - Amount of accepted missions
   -- FinishedMissions - Data for all finished missions
   -- FinishedGoals    - Data for all finished goals
   -- KilledMobs       - Data for all mobs killed by player
   -- Points           - Amount of gained points
   -- SOURCE
   type GameStats_Data is record
      DestroyedShips: Statistics_Container.Vector;
      BasesVisited: Positive;
      MapVisited: Positive;
      DistanceTraveled: Natural;
      CraftingOrders: Statistics_Container.Vector;
      AcceptedMissions: Natural;
      FinishedMissions: Statistics_Container.Vector;
      FinishedGoals: Statistics_Container.Vector;
      KilledMobs: Statistics_Container.Vector;
      Points: Natural;
   end record;
   -- ****

   -- ****v* Statistics/GameStats
   -- FUNCTION
   -- Game statistics
   -- SOURCE
   GameStats: GameStats_Data;
   -- ****

   -- ****f* Statistics/UpdateDestroyedShips
   -- FUNCTION
   -- Add new destroyed ship to list
   -- PARAMETERS
   -- ShipName - Name of the ship to add to destroyed list
   -- SOURCE
   procedure UpdateDestroyedShips(ShipName: Unbounded_String) with
      Pre => ShipName /= Null_Unbounded_String,
      Test_Case => ("Test_UpdateDestroyedShips", Nominal);
      -- ****

      -- ****f* Statistics/ClearGameStats
      -- FUNCTION
      -- Clear game statistics
      -- SOURCE
   procedure ClearGameStats with
      Test_Case => ("Test_ClearGameStats", Robustness);
      -- ****

      -- ****f* Statistics/UpdateFinishedGoals
      -- FUNCTION
      -- Add new finished goal to list
      -- PARAMETERS
      -- Index - Index of goal to update
      -- SOURCE
   procedure UpdateFinishedGoals(Index: Unbounded_String) with
      Pre => Index /= Null_Unbounded_String,
      Test_Case => ("Test_UpdateFinishedGoals", Nominal);
      -- ****

      -- ****f* Statistics/UpdateFinishedMissions
      -- FUNCTION
      -- Add new finished mission to list
      -- PARAMETERS
      -- MType - Type of mission to update
      -- SOURCE
   procedure UpdateFinishedMissions(MType: Unbounded_String) with
      Pre => MType /= Null_Unbounded_String,
      Test_Case => ("Test_UpdateFinishedMissions", Nominal);
      -- ****

      -- ****f* Statistics/UpdateCraftingOrders
      -- FUNCTION
      -- Add new finished crafting order to list
      -- PARAMETERS
      -- Index - Index of crafting order to update
      -- SOURCE
   procedure UpdateCraftingOrders(Index: Unbounded_String) with
      Pre => Index /= Null_Unbounded_String,
      Test_Case => ("Test_UpdateCraftingOrders", Nominal);
      -- ****

      -- ****f* Statistics/UpdateKilledMobs
      -- FUNCTION
      -- Add new killed mob to list
      -- PARAMETERS
      -- Mob         - Killed mobile data
      -- FactionName - Faction name to which killed mobile belongs
      -- SOURCE
   procedure UpdateKilledMobs
     (Mob: Member_Data; FractionName: Unbounded_String) with
      Pre => FractionName /= Null_Unbounded_String,
      Test_Case => ("Test_UpdateKilledMobs", Nominal);
      -- ****

      -- ****f* Statistics/GetGamePoints
      -- FUNCTION
      -- Get amount of gained points multiplied by difficulty bonus
      -- RESULT
      -- Amount of gained points by player in this game
      -- SOURCE
   function GetGamePoints return Natural;
   -- ****

end Statistics;
