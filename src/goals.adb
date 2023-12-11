--    Copyright 2017-2023 Bartek thindil Jasicki
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

with Interfaces.C.Strings; use Interfaces.C.Strings;

package body Goals is

   --## rule off TYPE_INITIAL_VALUES
   type Nim_Goal_Data is record
      Index: chars_ptr;
      G_Type: Integer;
      Amount: Natural;
      Target_Index: chars_ptr;
      Multiplier: Positive;
   end record;
   --## rule on TYPE_INITIAL_VALUES

   function Goal_Text(Index: Natural) return String is
      function Goal_Ada_Text(I: Natural) return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "goalAdaText";
   begin
      return Value(Item => Goal_Ada_Text(I => Index));
   end Goal_Text;

   procedure Clear_Current_Goal is
      procedure Clear_Ada_Current_Goal with
         Import => True,
         Convention => C,
         External_Name => "clearAdaCurrentGoal";
   begin
      Clear_Ada_Current_Goal;
   end Clear_Current_Goal;

   function Get_Goal(Index: Positive) return Goal_Data is
      use Interfaces.C;

      --## rule off IMPROPER_INITIALIZATION
      Nim_Goal: Nim_Goal_Data;
      --## rule on IMPROPER_INITIALIZATION
      procedure Get_Ada_Goal(I: Natural; Goal: out Nim_Goal_Data) with
         Import => True,
         Convention => C,
         External_Name => "getAdaGoal";
   begin
      Get_Ada_Goal(I => Index, Goal => Nim_Goal);
      if Strlen(Item => Nim_Goal.Index) = 0 then
         return Empty_Goal;
      end if;
      return
        (Index => To_Unbounded_String(Source => Value(Item => Nim_Goal.Index)),
         G_Type => Goal_Types'Val(Nim_Goal.G_Type), Amount => Nim_Goal.Amount,
         Target_Index =>
           To_Unbounded_String(Source => Value(Item => Nim_Goal.Target_Index)),
         Multiplier => Nim_Goal.Multiplier);
   end Get_Goal;

end Goals;
