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

-- ****h* Steamsky/HallOfFame
-- FUNCTION
-- Provide code to manipulate hall of fame data
-- SOURCE
package HallOfFame is
-- ****

   -- ****t* HallOfFame/HallOfFame_Data
   -- FUNCTION
   -- Data structure for hall of fame
   -- PARAMETERS
   -- Name        - Name of player
   -- Points      - Amount of points earned
   -- DeathReason - What caused player death
   -- SOURCE
   type HallOfFame_Data is record
      Name: Unbounded_String;
      Points: Natural;
      DeathReason: Unbounded_String;
   end record;
   -- ****
   -- ****v* HallOfFame/HallOfFame_Array
   -- FUNCTION
   -- Store all hall of fame entries
   -- SOURCE
   HallOfFame_Array: array(1 .. 10) of HallOfFame_Data :=
     (others =>
        (Name => Null_Unbounded_String, Points => 0,
         DeathReason => Null_Unbounded_String));
   -- ****

   -- ****f* HallOfFame/LoadHallOfFame;
   -- FUNCTION
   -- Read hall of fame data from file
   -- SOURCE
   procedure LoadHallOfFame;
   -- ****
   -- ****f* HallOfFame/UpdateHallOfFame
   -- FUNCTION
   -- Check did new entry should enter hall of fame
   -- PARAMETERS
   -- PlayerName  - Name of player's character to add to the hall of fame
   -- DeathReason - Reason of death of selected character
   -- SOURCE
   procedure UpdateHallOfFame(PlayerName, DeathReason: Unbounded_String) with
      Pre =>
      (PlayerName /= Null_Unbounded_String and
       DeathReason /= Null_Unbounded_String);
      -- ****

end HallOfFame;
