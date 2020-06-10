-- Copyright (c) 2020 Bartek thindil Jasicki <thindil@laeran.pl>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ships; use Ships;

-- ****h* Maps/MUI
-- FUNCTION
-- Provide code for showing and interacting with maps
-- SOURCE
package Maps.UI is
-- ****

   -- ****v* MUI/MenuAccelerators
   -- FUNCTION
   -- Array with the game menu default accelerators
   -- SOURCE
   MenuAccelerators: constant array(1 .. 16) of Unbounded_String :=
     (To_Unbounded_String("s"), To_Unbounded_String("a"),
      To_Unbounded_String("c"), To_Unbounded_String("o"),
      To_Unbounded_String("r"), To_Unbounded_String("m"),
      To_Unbounded_String("b"), To_Unbounded_String("n"),
      To_Unbounded_String("i"), To_Unbounded_String("t"),
      To_Unbounded_String("w"), To_Unbounded_String("g"),
      To_Unbounded_String("F1"), To_Unbounded_String("p"),
      To_Unbounded_String("q"), To_Unbounded_String("x"));
   -- ****

   -- ****f* MUI/UpdateHeader
   -- FUNCTION
   -- Update the game information on the UI header (time, crew, etc)
   -- SOURCE
   procedure UpdateHeader;
   -- ****

   -- ****f* MUI/DrawMap
   -- FUNCTION
   -- Draw map on the screen
   -- SOURCE
   procedure DrawMap;
   -- ****

   -- ****f* MUI/CreateGameUI
   -- FUNCTION
   -- Create the game UI and show sky map to the player
   -- SOURCE
   procedure CreateGameUI;
   -- ****

   -- ****f* MUI/ShowSkyMap
   -- FUNCTION
   -- Show sky map - draw map, update header, etc
   -- SOURCE
   procedure ShowSkyMap;
   -- ****

private

   -- ****v* Maps.UI/StartX
   -- FUNCTION
   -- Top left map coordinate
   -- SOURCE
   StartX, StartY: Integer;
   -- ****

   -- ****iv* MUI/CenterX
   -- FUNCTION
   -- Coordinates of the center point of the map
   -- SOURCE
   CenterX, CenterY: Positive;
   -- ****

   -- ****f* MUI/UpdateMapInfo
   -- FUNCTION
   -- Update information about map cell
   -- PARAMETERS
   -- X - X coordinate of the map cell
   -- Y - Y coordinate of the map cell
   -- SOURCE
   procedure UpdateMapInfo
     (X: Positive := PlayerShip.SkyX; Y: Positive := PlayerShip.SkyY);
   -- ****

   procedure UpdateMoveButtons;

end Maps.UI;
