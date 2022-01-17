-- Copyright (c) 2020-2022 Bartek thindil Jasicki <thindil@laeran.pl>
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

   -- ****v* MUI/MUI.MenuAccelerators
   -- FUNCTION
   -- Array with the game menu default accelerators
   -- SOURCE
   Menu_Accelerators: array(1 .. 11) of Unbounded_String :=
     (1 => To_Unbounded_String(Source => "s"),
      2 => To_Unbounded_String(Source => "o"),
      3 => To_Unbounded_String(Source => "r"),
      4 => To_Unbounded_String(Source => "m"),
      5 => To_Unbounded_String(Source => "k"),
      6 => To_Unbounded_String(Source => "w"),
      7 => To_Unbounded_String(Source => "g"),
      8 => To_Unbounded_String(Source => "F1"),
      9 => To_Unbounded_String(Source => "p"),
      10 => To_Unbounded_String(Source => "q"),
      11 => To_Unbounded_String(Source => "x"));
   -- ****

   -- ****v* MUI/MUI.MapAccelerators
   -- FUNCTION
   -- Array with default map keyboard accelerators
   -- SOURCE
   Map_Accelerators: array(1 .. 37) of Unbounded_String :=
     (1 => To_Unbounded_String("e"), 2 => To_Unbounded_String("v"),
      3 => To_Unbounded_String("plus"), 4 => To_Unbounded_String("minus"),
      5 => To_Unbounded_String("KP_Home"), 6 => To_Unbounded_String("KP_Up"),
      7 => To_Unbounded_String("KP_Prior"), 8 => To_Unbounded_String("KP_Left"),
      9 => To_Unbounded_String("KP_Begin"), 10 => To_Unbounded_String("KP_Right"),
      11 => To_Unbounded_String("KP_End"), 12 => To_Unbounded_String("KP_Down"),
      13 => To_Unbounded_String("KP_Next"), 14 => To_Unbounded_String("KP_Divide"),
      15 => To_Unbounded_String("Shift-Return"), 16 => To_Unbounded_String("Shift-h"),
      17 => To_Unbounded_String("Shift-KP_7"), 18 => To_Unbounded_String("Shift-KP_8"),
      19 => To_Unbounded_String("Shift-KP_9"), 20 => To_Unbounded_String("Shift-KP_4"),
      To_Unbounded_String("Shift-KP_6"), To_Unbounded_String("Shift-KP_1"),
      To_Unbounded_String("Shift-KP_2"), To_Unbounded_String("Shift-KP_3"),
      To_Unbounded_String("Control-KP_Home"),
      To_Unbounded_String("Control-KP_Up"),
      To_Unbounded_String("Control-KP_Prior"),
      To_Unbounded_String("Control-KP_Left"),
      To_Unbounded_String("Control-KP_Right"),
      To_Unbounded_String("Control-KP_End"),
      To_Unbounded_String("Control-KP_Down"),
      To_Unbounded_String("Control-KP_Next"),
      To_Unbounded_String("Control-Return"), To_Unbounded_String("Control-a"),
      To_Unbounded_String("Control-b"), To_Unbounded_String("Control-c"),
      To_Unbounded_String("Control-d"));
   -- ****

   -- ****v* MUI/MUI.FullScreenAccel
   -- FUNCTION
   -- Keyboard shortcut for switching full screen mode
   -- SOURCE
   FullScreenAccel: Unbounded_String := To_Unbounded_String("Control-f");
   -- ****

   -- ****v* MUI/MUI.GeneralAccelerators
   -- FUNCTION
   -- Array with default shortcuts used in many places
   -- HISTORY
   -- 6.9 - Added
   -- SOURCE
   GeneralAccelerators: array(1 .. 4) of Unbounded_String :=
     (To_Unbounded_String("Alt-a"), To_Unbounded_String("Alt-b"),
      To_Unbounded_String("Alt-c"), To_Unbounded_String("Alt-d"));
   -- ****

   -- ****iv* MUI/MUI.CenterX
   -- FUNCTION
   -- Coordinates of the center point of the map
   -- SOURCE
   CenterX, CenterY: Positive;
   -- ****

   -- ****v* MUI/MUI.DefaultFontsSizes
   -- FUNCTION
   -- Default sizes of the game fonts
   -- SOURCE
   DefaultFontsSizes: array(1 .. 3) of Positive;
   -- ****

   -- ****f* MUI/MUI.UpdateHeader
   -- FUNCTION
   -- Update the game information on the UI header (time, crew, etc)
   -- SOURCE
   procedure UpdateHeader;
   -- ****

   -- ****f* MUI/MUI.DrawMap
   -- FUNCTION
   -- Draw map on the screen
   -- SOURCE
   procedure DrawMap;
   -- ****

   -- ****f* MUI/MUI.CreateGameUI
   -- FUNCTION
   -- Create the game UI and show sky map to the player
   -- SOURCE
   procedure CreateGameUI;
   -- ****

   -- ****f* MUI/MUI.ShowSkyMap
   -- FUNCTION
   -- Show sky map - draw map, update header, etc
   -- PARAMETERS
   -- Clear - If true (when back from other screen), remove old subwindow and
   -- add map
   -- SOURCE
   procedure ShowSkyMap(Clear: Boolean := False);
   -- ****

   -- ****f* MUI/MUI.SetKeys
   -- FUNCTION
   -- Set keyboard shortcuts
   -- SOURCE
   procedure SetKeys;
   -- ****

   -- ****f* MUI/MUI.FinishStory
   -- FUNCTION
   -- Finish the current story
   -- SOURCE
   procedure FinishStory;
   -- ****

private

   -- ****v* MUI/MUI.StartX_(private)
   -- FUNCTION
   -- Top left map coordinate
   -- SOURCE
   StartX, StartY: Integer;
   -- ****

   -- ****f* MUI/MUI.UpdateMapInfo_(private)
   -- FUNCTION
   -- Update information about map cell
   -- PARAMETERS
   -- X - X coordinate of the map cell
   -- Y - Y coordinate of the map cell
   -- SOURCE
   procedure UpdateMapInfo
     (X: Positive := Player_Ship.Sky_X; Y: Positive := Player_Ship.Sky_Y);
   -- ****

   -- ****f* MUI/MUI.UpdateMoveButtons_(private)
   -- FUNCTION
   -- Updated the player ship movement buttons
   -- SOURCE
   procedure UpdateMoveButtons;
   -- ****

end Maps.UI;
