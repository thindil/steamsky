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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

-- ****h* HallOfFame/HallOfFame
-- FUNCTION
-- Provide code to manipulate hall of fame data
-- SOURCE
package HallOfFame is
-- ****

   -- ****s* HallOfFame/HallOfFame.Hall_Of_Fame_Data
   -- FUNCTION
   -- Data structure for hall of fame
   -- PARAMETERS
   -- Name        - Name of player
   -- Points      - Amount of points earned
   -- Death_Reason - What caused player death
   -- SOURCE
   type Hall_Of_Fame_Data is record
      Name: Unbounded_String;
      Points: Natural := 0;
      Death_Reason: Unbounded_String;
   end record;
   -- ****

   --## rule off REDUCEABLE_SCOPE
   -- ****d* HallOfFame/HallOfFame.Empty_Hall_Of_Fame_Entry
   -- FUNCTION
   -- Empty entry for Hall of Fame
   -- SOURCE
   Empty_Hall_Of_Fame_Entry: constant Hall_Of_Fame_Data :=
     (Name => Null_Unbounded_String, Points => 0,
      Death_Reason => Null_Unbounded_String);
   -- ****
   --## rule on REDUCEABLE_SCOPE

   -- ****t* HallOfFame/HallOfFame.Hall_Of_Fame_List
   -- FUNCTION
   -- List of all entries in the hall of fame
   -- SOURCE
   type Hall_Of_Fame_List is array(1 .. 10) of Hall_Of_Fame_Data;
   -- ****

   --## rule off REDUCEABLE_SCOPE
   -- ****d* HallOfFame/HallOfFameEmpty_Hall_Of_Fame
   -- FUNCTION
   -- Empty hall of fame list
   -- SOURCE
   Empty_Hall_Of_Fame: constant Hall_Of_Fame_List :=
     (others => Empty_Hall_Of_Fame_Entry);
   -- ****
   --## rule on REDUCEABLE_SCOPE

   -- ****f* HallOfFame/HallOfFame.Load_Hall_Of_Fame
   -- FUNCTION
   -- Read hall of fame data from file
   -- SOURCE
   procedure Load_Hall_Of_Fame;
   -- ****

end HallOfFame;
