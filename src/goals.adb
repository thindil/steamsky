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

with Interfaces.C.Strings; use Interfaces.C.Strings;

package body Goals is

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

end Goals;
