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
      package Rand_Roll is new Discrete_Random(Rand_Range);
      Generator: Rand_Roll.Generator;
   begin
      Rand_Roll.Reset(Generator);
      return Rand_Roll.Random(Generator);
   end Get_Random;

   function Generate_Robotic_Name return Unbounded_String is
      Letters_Amount: constant Positive := Get_Random(2, 5);
      Numbers_Amount: constant Positive := Get_Random(2, 4);
      subtype Letters is Character range 'A' .. 'Z';
      subtype Numbers is Character range '0' .. '9';
      New_Name: Unbounded_String :=
        To_Unbounded_String
          ("" &
           Letters'Val
             (Get_Random
                (Letters'Pos(Letters'First), Letters'Pos(Letters'Last))));
   begin
      First_Name_Part_Loop :
      for I in 2 .. Letters_Amount loop
         pragma Loop_Invariant(Length(New_Name) = I - 1);
         Append
           (New_Name,
            Letters'Val
              (Get_Random
                 (Letters'Pos(Letters'First), Letters'Pos(Letters'Last))));
      end loop First_Name_Part_Loop;
      Append(New_Name, '-');
      Second_Name_Part_Loop :
      for I in 1 .. Numbers_Amount loop
         pragma Loop_Invariant
           (Length(New_Name) = Length(New_Name'Loop_Entry) + (I - 1));
         Append
           (New_Name,
            Numbers'Val
              (Get_Random
                 (Numbers'Pos(Numbers'First), Numbers'Pos(Numbers'Last))));
      end loop Second_Name_Part_Loop;
      return New_Name;
   end Generate_Robotic_Name;

end Utils;
