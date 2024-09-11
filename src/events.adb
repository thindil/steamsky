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

with Maps; use Maps;
with Ships;

package body Events is

   function Check_For_Event return Boolean is
      use Ships;

      Result: Integer;
      function Check_Ada_For_Event return Integer with
         Import => True,
         Convention => C,
         External_Name => "checkAdaForEvent";
   begin
      Set_Ship_In_Nim;
      Result := Check_Ada_For_Event;
      Get_Ship_From_Nim(Ship => Player_Ship);
      Set_Map_Cell(X => Player_Ship.Sky_X, Y => Player_Ship.Sky_Y);
      Set_Events_In_Ada_Loop :
      for I in 1 .. Get_Events_Amount loop
         Set_Event(Index => I);
      end loop Set_Events_In_Ada_Loop;
      if Result = 1 then
         return True;
      end if;
      return False;
   end Check_For_Event;

   procedure Set_Event(Index: Positive) is
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
