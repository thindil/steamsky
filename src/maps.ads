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

with Events; use Events;
with Game; use Game;
with Missions; use Missions;

-- ****h* Maps/Maps
-- FUNCTION
-- Provide code for manipulate the game map
-- SOURCE
package Maps is
-- ****

   -- ****s* Maps/Maps.Sky_Cell
   -- FUNCTION
   -- Data structure for cells in game map
   -- PARAMETERS
   -- Base_Index    - If sky base is in cell > 0
   -- Visited       - True if player was in this cell
   -- Event_Index   - If event is in cell > 0
   -- Mission_Index - If accepted mission is in cell > 0
   -- SOURCE
   type Sky_Cell is record
      Base_Index: Extended_Base_Range := 0;
      Visited: Boolean;
      Event_Index: Events_Container.Extended_Index := 0;
      Mission_Index: Mission_Container.Extended_Index := 0;
   end record;
   -- ****

   -- ****v* Maps/Maps.Sky_Map
   -- FUNCTION
   -- Game map
   -- SOURCE
   Sky_Map: array(Map_X_Range, Map_Y_Range) of Sky_Cell;
   -- ****

   -- ****f* Maps/Maps.Count_Distance
   -- FUNCTION
   -- Count distance (in map fields) between player ship and the destination
   -- point
   -- PARAMETERS
   -- Destination_X - X coordinate of the destination point
   -- Destination_Y - Y coordinate of the destination point
   -- RESULT
   -- Distance between player ship and destination point
   -- SOURCE
   function Count_Distance
     (Destination_X: Map_X_Range; Destination_Y: Map_Y_Range)
      return Natural with
      Test_Case => (Name => "Test_CountDistance", Mode => Robustness);
      -- ****

      -- ****f* Maps/Maps.Normalize_Coord
      -- FUNCTION
      -- Normalize map coordinates
      -- PARAMETERS
      -- Coord   - Coordinate to normalize
      -- Is_X_Axis - If true, coordinate is in X axis
      -- RESULT
      -- Parameter Coord
      -- SOURCE
   procedure Normalize_Coord
     (Coord: in out Integer; Is_X_Axis: Boolean := True) with
      Post =>
      (if Is_X_Axis then Coord in Map_X_Range'Range else Coord in Map_Y_Range),
      Test_Case => (Name => "Test_NormalizeCoord", Mode => Nominal);
      -- ****

end Maps;
