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

with Ada.Numerics.Generic_Elementary_Functions;
with Ships; use Ships;

package body Maps is

   function CountDistance
     (DestinationX, DestinationY: Positive) return Natural is
      type Value_Type is digits 2 range 0.0 .. 9999999.0;
      package Value_Functions is new Ada.Numerics.Generic_Elementary_Functions
        (Value_Type);
      DiffX, DiffY: Natural;
      Distance: Value_Type;
   begin
      DiffX := abs (PlayerShip.SkyX - DestinationX);
      DiffY := abs (PlayerShip.SkyY - DestinationY);
      Distance := Value_Functions.Sqrt(Value_Type((DiffX**2) + (DiffY**2)));
      return Natural(Value_Type'Floor(Distance));
   end CountDistance;

end Maps;
