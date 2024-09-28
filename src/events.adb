--    Copyright 2016-2024 Bartek thindil Jasicki
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

with Maps;

package body Events is

   procedure Set_Event(Index: Positive) is
      use Maps;
      X, Y, Time, E_Type, Data: Integer;
      procedure Set_Ada_Event(I: Positive; X1, Y1, T, E, D: out Integer) with
         Import => True,
         Convention => C,
         External_Name => "setAdaEvent";
   begin
      Set_Ada_Event
        (I => Index, X1 => X, Y1 => Y, T => Time, E => E_Type, D => Data);
      if X = -1 then
         return;
      end if;
      Sky_Map(X, Y).Event_Index := Index;
   end Set_Event;

end Events;
