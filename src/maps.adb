--    Copyright 2017 Bartek thindil Jasicki
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

   function CountDistance
     (DestinationX, DestinationY: Positive) return Natural is
      DiffX, DiffY: Natural;
      Distance: Float;
   begin
      DiffX := abs (PlayerShip.SkyX - DestinationX);
      DiffY := abs (PlayerShip.SkyY - DestinationY);
      Distance := Sqrt(Float((DiffX**2) + (DiffY**2)));
      return Natural(Float'Floor(Distance));
   end CountDistance;

end Maps;
