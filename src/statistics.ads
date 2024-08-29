--    Copyright 2017-2024 Bartek thindil Jasicki
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

   --## rule off REDUCEABLE_SCOPE
   -- ****d* Statistics/Statistics.Empty_Statistics_Data
   -- FUNCTION
   -- Empty statistic data
   -- SOURCE
   Empty_Statistics_Data: constant Statistics_Data := (others => <>);
   -- ****
   --## rule on REDUCEABLE_SCOPE

   -- ****t* Statistics/Statistics.Statistics_Container
   -- FUNCTION
   -- Used to store game statistics data
   -- SOURCE
   package Statistics_Container is new Vectors
     (Index_Type => Positive, Element_Type => Statistics_Data);
   -- ****

   -- ****f* Statistics/Statistics.Clear_Game_Stats
   -- FUNCTION
   -- Clear game statistics
   -- SOURCE
   procedure Clear_Game_Stats with
      Post => Get_Game_Points = 0;
      -- ****

      -- ****f* Statistics/Statistics.Get_Game_Points
      -- FUNCTION
      -- Get amount of gained points multiplied by difficulty bonus
      -- RESULT
      -- Amount of gained points by player in this game
      -- SOURCE
   function Get_Game_Points return Natural;
      -- ****

end Statistics;
