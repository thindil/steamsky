--    Copyright 2017-2020 Bartek thindil Jasicki
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

with Events; use Events;
with Game; use Game;
with Missions; use Missions;

-- ****h* Maps/Maps
-- FUNCTION
-- Provide code for manipulate the game map
-- SOURCE
package Maps is
-- ****

   -- ****s* Maps/Maps.SkyCell
   -- FUNCTION
   -- Data structure for cells in game map
   -- PARAMETERS
   -- BaseIndex    - If sky base is in cell > 0
   -- Visited      - True if player was in this cell
   -- EventIndex   - If event is in cell > 0
   -- MissionIndex - If accepted mission is in cell > 0
   -- SOURCE
   type SkyCell is record
      BaseIndex: Extended_BaseRange;
      Visited: Boolean;
      EventIndex: Events_Container.Extended_Index;
      MissionIndex: Mission_Container.Extended_Index;
   end record;
   -- ****

   -- ****v* Maps/Maps.SkyMap
   -- FUNCTION
   -- Game map
   -- SOURCE
   SkyMap: array(MapXRange, MapYRange) of SkyCell;
   -- ****

   -- ****f* Maps/Maps.CountDistance
   -- FUNCTION
   -- Count distance (in map fields) between player ship and the destination
   -- point
   -- PARAMETERS
   -- DestinationX - X coordinate of the destination point
   -- DestinationY - Y coordinate of the destination point
   -- RESULT
   -- Distance between player ship and destination point
   -- SOURCE
   function CountDistance
     (DestinationX: MapXRange; DestinationY: MapYRange) return Natural with
      Test_Case => ("Test_CountDistance", Robustness);
      -- ****

      -- ****f* Maps/Maps.NormalizeCoord
      -- FUNCTION
      -- Normalize map coordinates
      -- PARAMETERS
      -- Coord   - Coordinate to normalize
      -- IsXAxis - If true, coordinate is in X axis
      -- RESULT
      -- Parameter Coord
      -- SOURCE
   procedure NormalizeCoord
     (Coord: in out Integer; IsXAxis: Boolean := True) with
      Test_Case => ("Test_NormalizeCoord", Robustness);
      -- ****

end Maps;
