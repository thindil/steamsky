--    Copyright 2017-2018 Bartek thindil Jasicki
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

with Ada.Numerics.Discrete_Random; use Ada.Numerics;

package body Utils is

   function GetRandom(Min, Max: Integer) return Integer is
      subtype Rand_Range is Integer range Min .. Max;
      package Rand_Roll is new Discrete_Random(Rand_Range);
      Generator: Rand_Roll.Generator;
   begin
      Rand_Roll.Reset(Generator);
      return Rand_Roll.Random(Generator);
   end GetRandom;

   function DaysDifference(DateToCompare: Date_Record) return Natural is
   begin
      return (GameDate.Day + (30 * GameDate.Month) + (GameDate.Year * 360)) -
        (DateToCompare.Day + (30 * DateToCompare.Month) +
         (DateToCompare.Year * 360));
   end DaysDifference;

end Utils;
