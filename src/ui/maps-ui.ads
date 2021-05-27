-- Copyright (c) 2020-2021 Bartek thindil Jasicki <thindil@laeran.pl>
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
with Tcl.Tk.Ada.Widgets.Menu; use Tcl.Tk.Ada.Widgets.Menu;
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
   MenuAccelerators: array(1 .. 11) of Unbounded_String :=
     (To_Unbounded_String("s"), To_Unbounded_String("o"),
      To_Unbounded_String("r"), To_Unbounded_String("m"),
      To_Unbounded_String("k"), To_Unbounded_String("w"),
      To_Unbounded_String("g"), To_Unbounded_String("F1"),
      To_Unbounded_String("p"), To_Unbounded_String("q"),
      To_Unbounded_String("x"));
   -- ****

   -- ****v* MUI/MUI.MapAccelerators
   -- FUNCTION
   -- Array with default map keyboard accelerators
   -- SOURCE
   MapAccelerators: array(1 .. 37) of Unbounded_String :=
     (To_Unbounded_String("e"), To_Unbounded_String("v"),
      To_Unbounded_String("plus"), To_Unbounded_String("minus"),
      To_Unbounded_String("KP_7"), To_Unbounded_String("KP_8"),
      To_Unbounded_String("KP_9"), To_Unbounded_String("KP_4"),
      To_Unbounded_String("KP_5"), To_Unbounded_String("KP_6"),
      To_Unbounded_String("KP_1"), To_Unbounded_String("KP_2"),
      To_Unbounded_String("KP_3"), To_Unbounded_String("KP_Divide"),
      To_Unbounded_String("Shift-Return"), To_Unbounded_String("H"),
      To_Unbounded_String("Shift-KP_Home"), To_Unbounded_String("Shift-KP_Up"),
      To_Unbounded_String("Shift-KP_Prior"),
      To_Unbounded_String("Shift-KP_Left"),
      To_Unbounded_String("Shift-KP_Right"),
      To_Unbounded_String("Shift-KP_End"),
      To_Unbounded_String("Control-KP_Down"),
      To_Unbounded_String("Control-KP_Next"),
      To_Unbounded_String("Control-KP_Home"),
      To_Unbounded_String("Control-KP_Up"),
      To_Unbounded_String("Control-KP_Prior"),
      To_Unbounded_String("Control-KP_Left"),
      To_Unbounded_String("Control-KP_Right"),
      To_Unbounded_String("Control-KP_End"),
      To_Unbounded_String("Control-KP_Down"),
      To_Unbounded_String("Control-KP_Next"),
      To_Unbounded_String("Control-KP_Begin"),
      To_Unbounded_String("Control-a"), To_Unbounded_String("Control-b"),
      To_Unbounded_String("Control-c"), To_Unbounded_String("Control-d"));
   -- ****

   -- ****v* MUI/MUI.FullScreenAccel
   -- FUNCTION
   -- Keyboard shortcut for switching full screen mode
   -- SOURCE
   FullScreenAccel: Unbounded_String := To_Unbounded_String("Control-f");
   -- ****

   -- ****iv* MUI/MUI.CenterX
   -- FUNCTION
   -- Coordinates of the center point of the map
   -- SOURCE
   CenterX, CenterY: Positive;
   -- ****

   -- ****iv* MUI/MUI.GameMenu
   -- FUNCTION
   -- The main game menu
   -- SOURCE
   GameMenu: Tk_Menu;
   -- ****

   -- ****v* MUI/MUI.DefaultFontsSizes
   -- FUNCTION
   -- Default sizes of the game fonts
   -- SOURCE
   DefaultFontsSizes: array(1 .. 3) of Positive;
   -- ****

   -- ****f* MUI/MUI.CreateGameMenu
   -- FUNCTION
   -- Create the main game menu. Clear old elements and add all default
   -- SOURCE
   procedure CreateGameMenu;
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
     (X: Positive := PlayerShip.SkyX; Y: Positive := PlayerShip.SkyY);
   -- ****

   -- ****f* MUI/MUI.UpdateMoveButtons_(private)
   -- FUNCTION
   -- Updated the player ship movement buttons
   -- SOURCE
   procedure UpdateMoveButtons;
   -- ****

end Maps.UI;
