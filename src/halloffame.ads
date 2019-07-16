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

package HallOfFame is

   -- Data structure for hall of fame
   type HallOfFame_Data is record
      Name: Unbounded_String; -- Name of player
      Points: Natural; -- Amount of points earned
      DeathReason: Unbounded_String; -- What caused player death
   end record;
   -- Store all hall of fame entries
   HallOfFame_Array: array(1 .. 10) of HallOfFame_Data :=
     (others =>
        (Name => Null_Unbounded_String, Points => 0,
         DeathReason => Null_Unbounded_String));

   -- Read hall of fame data from file
   procedure LoadHallOfFame;
   -- Check did new entry should enter hall of fame
   procedure UpdateHallOfFame(PlayerName, DeathReason: Unbounded_String) with
      Pre =>
      (PlayerName /= Null_Unbounded_String and
       DeathReason /= Null_Unbounded_String);
end HallOfFame;
