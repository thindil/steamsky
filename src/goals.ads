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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors; use Ada.Containers;
with DOM.Readers; use DOM.Readers;

package Goals is

-- ****t* Goals/GoalTypes
-- FUNCTION
-- Types of goals
-- SOURCE
   type GoalTypes is
     (RANDOM, REPUTATION, DESTROY, DISCOVER, VISIT, CRAFT, MISSION, KILL);
-- ****
-- ****t* Goals/Goal_Data
-- FUNCTION
-- Data structure for each goal
-- SOURCE
   type Goal_Data is record
      Index: Unbounded_String; -- Index of goal
      GType: GoalTypes; -- Type of goal
      Amount: Natural; -- Amount of targets needed for finish goal
      TargetIndex: Unbounded_String; -- Index of target needed for finish goal. If empty, mean all targets selected type (bases, ships, etc)
      Multiplier: Positive; -- Multiplier for points awarded for finish this goal.
   end record;
-- ****

-- ****t* Goals/Goals_Container
-- SOURCE
   package Goals_Container is new Vectors(Positive, Goal_Data);
-- ****

-- ****v* Goals/Goals_List
-- FUNCTION
-- List of player goals available in game
-- SOURCE
   Goals_List: Goals_Container.Vector;
-- ****
-- ****v* Goals/CurrentGoal
-- FUNCTION
-- Player current goal
-- SOURCE
   CurrentGoal: Goal_Data;
-- ****

-- ****f* Goals/LoadGoals
-- FUNCTION
-- Load player goals from files
-- SOURCE
   procedure LoadGoals(Reader: Tree_Reader);
-- ****
-- ****f* Goals/GoalText
-- FUNCTION
-- Return info about selected goal or current goal if Index = 0
-- SOURCE
   function GoalText(Index: Goals_Container.Extended_Index) return String with
      Pre => Index <= Goals_List.Last_Index;
-- ****
-- ****f* Goals/ClearCurrentGoal;
-- FUNCTION
-- Reset current goal
-- SOURCE
   procedure ClearCurrentGoal;
-- ****
-- ****f* Goals/UpdateGoal
-- FUNCTION
-- Update current goal
-- SOURCE
   procedure UpdateGoal
     (GType: GoalTypes; TargetIndex: Unbounded_String; Amount: Positive := 1);
-- ****
end Goals;
