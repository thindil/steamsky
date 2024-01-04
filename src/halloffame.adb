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
with Game;

package body HallOfFame is

   procedure Load_Hall_Of_Fame is
      use Interfaces.C;
      use Game;

      --## rule off IMPROPER_INITIALIZATION
      Result: chars_ptr;
      --## rule on IMPROPER_INITIALIZATION
      function Load_Ada_Hall_Of_Fame return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "loadAdaHallOfFame";
   begin
      Result := Load_Ada_Hall_Of_Fame;
      if Strlen(Item => Result) > 0 then
         raise Data_Loading_Error with Value(Item => Result);
      end if;
   end Load_Hall_Of_Fame;

end HallOfFame;
