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

with Ada.Numerics.Discrete_Random; use Ada.Numerics;

package body Utils with
   SPARK_Mode
is

   function Get_Random(Min, Max: Integer) return Integer with
      SPARK_Mode => Off
   is
      subtype Rand_Range is Integer range Min .. Max;
      package Rand_Roll is new Discrete_Random(Result_Subtype => Rand_Range);
      Generator: Rand_Roll.Generator;
   begin
      Rand_Roll.Reset(Gen => Generator);
      return Rand_Roll.Random(Gen => Generator);
   end Get_Random;

   function Generate_Robotic_Name return Unbounded_String is
      Letters_Amount: constant Positive := Get_Random(Min => 2, Max => 5);
      Numbers_Amount: constant Positive := Get_Random(Min => 2, Max => 4);
      subtype Letters is Character range 'A' .. 'Z';
      subtype Numbers is Character range '0' .. '9';
      New_Name: Unbounded_String :=
        To_Unbounded_String
          (Source =>
             "" &
             Letters'Val
               (Get_Random
                  (Min => Letters'Pos(Letters'First),
                   Max => Letters'Pos(Letters'Last))));
   begin
      First_Name_Part_Loop :
      for I in 2 .. Letters_Amount loop
         pragma Loop_Invariant(Length(Source => New_Name) = I - 1);
         Append
           (Source => New_Name,
            New_Item =>
              Letters'Val
                (Get_Random
                   (Min => Letters'Pos(Letters'First),
                    Max => Letters'Pos(Letters'Last))));
      end loop First_Name_Part_Loop;
      Append(Source => New_Name, New_Item => '-');
      Second_Name_Part_Loop :
      for I in 1 .. Numbers_Amount loop
         pragma Loop_Invariant
           (Length(Source => New_Name) =
            Length(Source => New_Name'Loop_Entry) + (I - 1));
         Append
           (Source => New_Name,
            New_Item =>
              Numbers'Val
                (Get_Random
                   (Min => Numbers'Pos(Numbers'First),
                    Max => Numbers'Pos(Numbers'Last))));
      end loop Second_Name_Part_Loop;
      return New_Name;
   end Generate_Robotic_Name;

end Utils;
