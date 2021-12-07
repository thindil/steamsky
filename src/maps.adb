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

with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Ships; use Ships;

package body Maps is

   function Count_Distance
     (Destination_X: Map_X_Range; Destination_Y: Map_Y_Range) return Natural is
      Diff_X: Natural range 0 .. Map_X_Range'Last;
      Diff_Y: Natural range 0 .. Map_Y_Range'Last;
      Distance: Float range 0.0 .. Float(Map_X_Range'Last * Map_Y_Range'Last);
   begin
      Diff_X := abs (Player_Ship.Sky_X - Destination_X);
      Diff_Y := abs (Player_Ship.Sky_Y - Destination_Y);
      Distance := Sqrt(X => Float((Diff_X**2) + (Diff_Y**2)));
      return Natural(Float'Floor(Distance));
   end Count_Distance;

   procedure Normalize_Coord
     (Coord: in out Integer; Is_X_Axis: Boolean := True) is
   begin
      if Is_X_Axis then
         if Coord < Map_X_Range'First then
            Coord := Map_X_Range'First;
         elsif Coord > Map_X_Range'Last then
            Coord := Map_X_Range'Last;
         end if;
      else
         if Coord < Map_Y_Range'First then
            Coord := Map_Y_Range'First;
         elsif Coord > Map_Y_Range'Last then
            Coord := Map_Y_Range'Last;
         end if;
      end if;
   end Normalize_Coord;

end Maps;
