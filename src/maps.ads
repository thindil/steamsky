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

package Maps is

   type SkyCell is -- Data structure for cells in game map
   record
      BaseIndex: Natural;  -- If sky base is in cell > 0
      Visited: Boolean; -- True if player was in this cell
      EventIndex: Natural; -- If event is in cell > 0
      MissionIndex: Natural; -- If accepted mission is in cell > 0
   end record;
   SkyMap: array(1 .. 1024, 1 .. 1024) of SkyCell; -- Game map

end Maps;
