--    Copyright 2017-2021 Bartek thindil Jasicki
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

-- ****h* Goals/Goals
-- FUNCTION
-- Provide code for manipulate goals
-- SOURCE
package Goals is
-- ****

   -- ****t* Goals/Goals.GoalTypes
   -- FUNCTION
   -- Types of goals
   -- SOURCE
   type GoalTypes is
     (RANDOM, REPUTATION, DESTROY, DISCOVER, VISIT, CRAFT, MISSION, KILL) with
      Default_Value => RANDOM;
   -- ****

   -- ****s* Goals/Goals.Goal_Data
   -- FUNCTION
   -- Data structure for each goal
   -- PARAMETERS
   -- Index       - Index of goal
   -- GType       - Type of goal
   -- Amount      - Amount of targets needed for finish goal
   -- TargetIndex - Index of target needed for finish goal. If empty, mean all targets selected type (bases, ships, etc)
   -- Multiplier  - Multiplier for points awarded for finish this goal
   -- SOURCE
   type Goal_Data is record
      Index: Unbounded_String;
      GType: GoalTypes;
      Amount: Natural;
      TargetIndex: Unbounded_String;
      Multiplier: Positive;
   end record;
   -- ****

   -- ****t* Goals/Goals.Goals_Container
   -- FUNCTION
   -- Used to store goals data
   -- SOURCE
   package Goals_Container is new Vectors(Positive, Goal_Data);
   -- ****

   -- ****v* Goals/Goals.Goals_List
   -- FUNCTION
   -- List of player goals available in game
   -- SOURCE
   Goals_List: Goals_Container.Vector;
   -- ****

   -- ****v* Goals/Goals.CurrentGoal
   -- FUNCTION
   -- Player current goal
   -- SOURCE
   CurrentGoal: Goal_Data;
   -- ****

   -- ****f* Goals/Goals.LoadGoals
   -- FUNCTION
   -- Load player goals from files
   -- FUNCTION
   -- Reader - XML Reader from which goals data will be read
   -- SOURCE
   procedure LoadGoals(Reader: Tree_Reader);
   -- ****

   -- ****f* Goals/Goals.GoalText
   -- FUNCTION
   -- Get info about selected goal
   -- PARAMETERS
   -- Index - Index of goal from which we take info. If 0 then get info for
   --         current goal
   -- RESULT
   -- Info about selected goal
   -- SOURCE
   function GoalText(Index: Goals_Container.Extended_Index) return String with
      Pre => Index <= Goals_List.Last_Index,
      Post => GoalText'Result'Length > 0,
      Test_Case => (Name => "Test_GoalText", Mode => Nominal);
      -- ****

      -- ****f* Goals/Goals.ClearCurrentGoal
      -- FUNCTION
      -- Reset current goal
      -- SOURCE
   procedure ClearCurrentGoal with
      Post => CurrentGoal.Index = Null_Unbounded_String,
      Test_Case => (Name => "Test_ClearCurrentGoal", Mode => Nominal);
      -- ****

      -- ****f* Goals/Goals.UpdateGoal
      -- FUNCTION
      -- Update current goal
      -- PARAMETERS
      -- GType       - Type of goal to check
      -- TargetIndex - Index of target to check
      -- Amount      - Amount for goal to modify if both checks are valid
      -- SOURCE
   procedure UpdateGoal
     (GType: GoalTypes; TargetIndex: Unbounded_String;
      Amount: Positive := 1) with
      Test_Case => (Name => "Test_UpdateGoal", Mode => Robustness);
      -- ****

end Goals;
