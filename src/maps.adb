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

package body Maps is

   procedure Set_Map_Cell(X, Y: Integer) is
      Base_Index, Visited, Event_Index, Mission_Index: Integer;
      procedure Set_Ada_Map_Cell
        (Nim_X, Nim_Y: Integer;
         B_Index, Vis, E_Index, M_Index: out Integer) with
         Import => True,
         Convention => C,
         External_Name => "setAdaMapCell";
   begin
      Set_Ada_Map_Cell
        (Nim_X => X, Nim_Y => Y, B_Index => Base_Index, Vis => Visited,
         E_Index => Event_Index, M_Index => Mission_Index);
      Sky_Map(X, Y) :=
        (Base_Index => Base_Index,
         Visited => (if Visited = 1 then True else False),
         Event_Index => Event_Index, Mission_Index => Mission_Index);
   end Set_Map_Cell;

end Maps;
