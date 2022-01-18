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

   -- ****v* MUI/MUI.Map_Accelerators
   -- FUNCTION
   -- Array with default map keyboard accelerators
   -- SOURCE
   Map_Accelerators: array(1 .. 37) of Unbounded_String :=
     (1 => To_Unbounded_String(Source => "e"),
      2 => To_Unbounded_String(Source => "v"),
      3 => To_Unbounded_String(Source => "plus"),
      4 => To_Unbounded_String(Source => "minus"),
      5 => To_Unbounded_String(Source => "KP_Home"),
      6 => To_Unbounded_String(Source => "KP_Up"),
      7 => To_Unbounded_String(Source => "KP_Prior"),
      8 => To_Unbounded_String(Source => "KP_Left"),
      9 => To_Unbounded_String(Source => "KP_Begin"),
      10 => To_Unbounded_String(Source => "KP_Right"),
      11 => To_Unbounded_String(Source => "KP_End"),
      12 => To_Unbounded_String(Source => "KP_Down"),
      13 => To_Unbounded_String(Source => "KP_Next"),
      14 => To_Unbounded_String(Source => "KP_Divide"),
      15 => To_Unbounded_String(Source => "Shift-Return"),
      16 => To_Unbounded_String(Source => "Shift-h"),
      17 => To_Unbounded_String(Source => "Shift-KP_7"),
      18 => To_Unbounded_String(Source => "Shift-KP_8"),
      19 => To_Unbounded_String(Source => "Shift-KP_9"),
      20 => To_Unbounded_String(Source => "Shift-KP_4"),
      21 => To_Unbounded_String(Source => "Shift-KP_6"),
      22 => To_Unbounded_String(Source => "Shift-KP_1"),
      23 => To_Unbounded_String(Source => "Shift-KP_2"),
      24 => To_Unbounded_String(Source => "Shift-KP_3"),
      25 => To_Unbounded_String(Source => "Control-KP_Home"),
      26 => To_Unbounded_String(Source => "Control-KP_Up"),
      27 => To_Unbounded_String(Source => "Control-KP_Prior"),
      28 => To_Unbounded_String(Source => "Control-KP_Left"),
      29 => To_Unbounded_String(Source => "Control-KP_Right"),
      30 => To_Unbounded_String(Source => "Control-KP_End"),
      31 => To_Unbounded_String(Source => "Control-KP_Down"),
      32 => To_Unbounded_String(Source => "Control-KP_Next"),
      33 => To_Unbounded_String(Source => "Control-Return"),
      34 => To_Unbounded_String(Source => "Control-a"),
      35 => To_Unbounded_String(Source => "Control-b"),
      36 => To_Unbounded_String(Source => "Control-c"),
      37 => To_Unbounded_String(Source => "Control-d"));
   -- ****

   -- ****v* MUI/MUI.Full_Screen_Accel
   -- FUNCTION
   -- Keyboard shortcut for switching full screen mode
   -- SOURCE
   Full_Screen_Accel: Unbounded_String := To_Unbounded_String(Source => "Control-f");
   -- ****

   -- ****v* MUI/MUI.General_Accelerators
   -- FUNCTION
   -- Array with default shortcuts used in many places
   -- HISTORY
   -- 6.9 - Added
   -- SOURCE
   General_Accelerators: array(1 .. 4) of Unbounded_String :=
     (1 => To_Unbounded_String(Source => "Alt-a"), 2 => To_Unbounded_String(Source => "Alt-b"),
      3 => To_Unbounded_String(Source => "Alt-c"), 4 => To_Unbounded_String(Source => "Alt-d"));
   -- ****

   -- ****iv* MUI/MUI.Center_X
   -- FUNCTION
   -- Coordinates of the center point of the map
   -- SOURCE
   Center_X, Center_Y: Positive;
   -- ****

   -- ****v* MUI/MUI.Default_Fonts_Sizes
   -- FUNCTION
   -- Default sizes of the game fonts
   -- SOURCE
   Default_Fonts_Sizes: array(1 .. 3) of Positive;
   -- ****

   -- ****f* MUI/MUI.Update_Header
   -- FUNCTION
   -- Update the game information on the UI header (time, crew, etc)
   -- SOURCE
   procedure Update_Header;
   -- ****

   -- ****f* MUI/MUI.Draw_Map
   -- FUNCTION
   -- Draw map on the screen
   -- SOURCE
   procedure Draw_Map;
   -- ****

   -- ****f* MUI/MUI.Create_Game_UI
   -- FUNCTION
   -- Create the game UI and show sky map to the player
   -- SOURCE
   procedure Create_Game_UI;
   -- ****

   -- ****f* MUI/MUI.Show_Sky_Map
   -- FUNCTION
   -- Show sky map - draw map, update header, etc
   -- PARAMETERS
   -- Clear - If true (when back from other screen), remove old subwindow and
   -- add map
   -- SOURCE
   procedure Show_Sky_Map(Clear: Boolean := False);
   -- ****

   -- ****f* MUI/MUI.Set_Keys
   -- FUNCTION
   -- Set keyboard shortcuts
   -- SOURCE
   procedure Set_Keys;
   -- ****

   -- ****f* MUI/MUI.Finish_Story
   -- FUNCTION
   -- Finish the current story
   -- SOURCE
   procedure Finish_Story;
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
