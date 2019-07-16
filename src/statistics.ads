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

package Statistics is

   -- Data for finished goals, destroyed ships and killed mobs
   type Statistics_Data is record
      Index: Unbounded_String; -- Index of goal or ship name or name of fraction of killed mobs
      Amount: Positive; -- Amount of finished goals or ships or mobs of that type
   end record;
   package Statistics_Container is new Vectors(Positive, Statistics_Data);
   -- Data for game statistics
   type GameStats_Data is record
      DestroyedShips: Statistics_Container
        .Vector; -- Data for all destroyed ships by player
      BasesVisited: Positive; -- Amount of visited bases
      MapVisited: Positive; -- Amount of visited map fields
      DistanceTraveled: Natural; -- Amount of map fields travelled
      CraftingOrders: Statistics_Container
        .Vector; -- Data for finished crafting orders
      AcceptedMissions: Natural; -- Amount of accepted missions
      FinishedMissions: Statistics_Container
        .Vector; -- Data for all finished missions
      FinishedGoals: Statistics_Container
        .Vector; -- Data for all finished goals
      KilledMobs: Statistics_Container
        .Vector; -- Data for all mobs killed by player
      Points: Natural; -- Amount of gained points
   end record;
   -- Game statistics
   GameStats: GameStats_Data;

   -- Add new destroyed ship to list
   procedure UpdateDestroyedShips(ShipName: Unbounded_String) with
      Pre => ShipName /= Null_Unbounded_String;
      -- Clear game statistics
   procedure ClearGameStats;
   -- Add new finished goal to list
   procedure UpdateFinishedGoals(Index: Unbounded_String) with
      Pre => Index /= Null_Unbounded_String;
      -- Add new finished mission to list
   procedure UpdateFinishedMissions(MType: Unbounded_String) with
      Pre => MType /= Null_Unbounded_String;
      -- Add new finished crafting order to list
   procedure UpdateCraftingOrders(Index: Unbounded_String) with
      Pre => Index /= Null_Unbounded_String;
      -- Add new killed mob to list
   procedure UpdateKilledMobs
     (Mob: Member_Data; FractionName: Unbounded_String) with
      Pre => FractionName /= Null_Unbounded_String;
      -- Get amount of gained points multiplied by difficulty bonus
   function GetGamePoints return Natural;

end Statistics;
