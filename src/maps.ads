--    Copyright 2017-2019 Bartek thindil Jasicki
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

package Maps is

   -- Data structure for cells in game map
   type SkyCell is record
      BaseIndex: Natural;  -- If sky base is in cell > 0
      Visited: Boolean; -- True if player was in this cell
      EventIndex: Natural; -- If event is in cell > 0
      MissionIndex: Natural; -- If accepted mission is in cell > 0
   end record;
   -- Game map
   SkyMap: array(1 .. 1024, 1 .. 1024) of SkyCell;

   -- Return distance between player ship and destination point
   function CountDistance(DestinationX, DestinationY: Positive) return Natural;
   -- Normalize map coordinates
   procedure NormalizeCoord(Coord: in out Integer; IsXAxis: Boolean := True);

end Maps;
