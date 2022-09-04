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

with Interfaces.C.Strings;

package body Utils is

   function Generate_Robotic_Name return Game.Tiny_String.Bounded_String is
      use Interfaces.C.Strings;
      use Tiny_String;

      function Robotic_Name return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "generateRoboticName";
   begin
      return To_Bounded_String(Source => Value(Item => Robotic_Name));
   end Generate_Robotic_Name;

end Utils;
