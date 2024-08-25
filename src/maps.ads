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
      Event_Index: Natural := 0;
      Mission_Index: Mission_Container.Extended_Index := 0;
   end record;
   -- ****

   --## rule off REDUCEABLE_SCOPE
   -- ****d* Maps/Maps.Empty_Cell
   -- FUNCTION
   -- Default, empty map cell data
   -- SOURCE
   Empty_Cell: constant Sky_Cell := (others => <>);
   -- ****
   --## rule on REDUCEABLE_SCOPE

   -- ****v* Maps/Maps.Sky_Map
   -- FUNCTION
   -- Game map
   -- SOURCE
   Sky_Map: array(Map_X_Range, Map_Y_Range) of Sky_Cell;
   -- ****

-- Temporary code to interact with Nim

   procedure Get_Ada_Map_Cell
     (X, Y, Base_Index, Visited, Event_Index, Mission_Index: Integer) with
      Import => True,
      Convention => C,
      External_Name => "getAdaMapCell";

   procedure Set_Map_Cell(X, Y: Integer);

end Maps;
