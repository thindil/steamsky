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

with Game; use Game;

-- ****h* Utils/Utils
-- FUNCTION
-- Provided various uncategorized code
-- SOURCE
package Utils with
   SPARK_Mode
is
-- ****

   -- ****f* Utils/Utils.GetRandom
   -- FUNCTION
   -- Return random number from Min to Max range. This one can't be formaly
   -- verified as is depends on random number generator.
   -- PARAMETERS
   -- Min - Starting value from which generate random number
   -- Max - End value from which generate random number
   -- RESULT
   -- Random number between Min and Max
   -- SOURCE
   function Get_Random(Min, Max: Integer) return Integer with
      Global => null,
      Pre => Min <= Max,
      Post => Get_Random'Result in Min .. Max,
      Test_Case => (Name => "Test_GetRandom", Mode => Nominal);
      -- ****

      --## rule off SIMPLIFIABLE_EXPRESSIONS
      -- ****f* Utils/Utils.DaysDifference
      -- FUNCTION
      -- Count days difference between selected date and current game date
      -- PARAMETERS
      -- DateToCompare - In game date to compare with current game date
      -- RESULT
      -- Amount of days difference between DateToCompare and current game date
      -- SOURCE
   function Days_Difference(Date_To_Compare: Date_Record) return Integer is
     ((Game_Date.Day + (30 * Game_Date.Month) + (Game_Date.Year * 360)) -
      (Date_To_Compare.Day + (30 * Date_To_Compare.Month) +
       (Date_To_Compare.Year * 360))) with
      Global => Game_Date,
      Test_Case => (Name => "Test_DaysDifference", Mode => Robustness);
      -- ****
      --## rule on SIMPLIFIABLE_EXPRESSIONS

      -- ****f* Utils/Utils.GenerateRoboticName
      -- FUNCTION
      -- Generate robotic type name for bases, mobs, ships, etc
      -- RESULT
      -- Random robotic name
      -- SOURCE
   function Generate_Robotic_Name return Game.Tiny_String.Bounded_String with
      Global => null,
      Post => Game.Tiny_String.Length(Source => Generate_Robotic_Name'Result) > 0,
      Test_Case => (Name => "Test_GenerateRoboticName", Mode => Nominal);
   -- ****

end Utils;
