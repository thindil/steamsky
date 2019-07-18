--    Copyright 2018-2019 Bartek thindil Jasicki
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Gtkada.Builder; use Gtkada.Builder;
with Gtk.Widget; use Gtk.Widget;
with Ships; use Ships;

package Maps.UI is

-- ****f* Maps.UI/CreateSkyMap;
-- FUNCTION
-- Create sky map
-- SOURCE
   procedure CreateSkyMap;
-- ****
-- ****f* Maps.UI/ShowSkyMap
-- FUNCTION
-- Show sky map
-- SOURCE
   procedure ShowSkyMap
     (X: Integer := PlayerShip.SkyX; Y: Integer := PlayerShip.SkyY);
-- ****
-- ****f* Maps.UI/UpdateHeader;
-- FUNCTION
-- Update game header informations
-- SOURCE
   procedure UpdateHeader;
-- ****
-- ****f* Maps.UI/SetMapMoveButtons;
-- FUNCTION
-- Set icons on move map buttons
-- SOURCE
   procedure SetMapMoveButtons;
-- ****

private

-- ****v* Maps.UI/Builder
-- FUNCTION
-- Gtk builder for user interface
-- SOURCE
   Builder: Gtkada_Builder;
-- ****
   MapWidth, MapHeight, CenterX, CenterY, MapCellWidth, MapCellHeight, MapX,
-- ****v* Maps.UI/MapY
-- SOURCE

   MapY: Positive;
-- ****
-- ****v* Maps.UI/StartX, StartY
-- SOURCE
   StartX, StartY: Integer;
-- ****
-- ****v* Maps.UI/ButtonsVisible
-- SOURCE
   ButtonsVisible: Boolean := False;
-- ****

-- ****f* Maps.UI/DeathConfirm;
-- FUNCTION
-- Show confirmation to show game stats when player died
-- SOURCE
   procedure DeathConfirm;
-- ****
-- ****f* Maps.UI/UpdateMoveButtons;
-- FUNCTION
-- Update move buttons
-- SOURCE
   procedure UpdateMoveButtons;
-- ****
-- ****f* Maps.UI/DrawMap;
-- FUNCTION
-- Draw sky map
-- SOURCE
   procedure DrawMap;
-- ****
-- ****f* Maps.UI/HideButtons
-- FUNCTION
-- Hide selected button
-- SOURCE
   procedure HideButtons(Widget: not null access Gtk_Widget_Record'Class);
-- ****
-- ****f* Maps.UI/CheckButtons
-- FUNCTION
-- Check selected button
-- SOURCE
   procedure CheckButtons(Widget: not null access Gtk_Widget_Record'Class);
-- ****
-- ****f* Maps.UI/GetCurrentCellCoords;
-- FUNCTION
-- Get current map cell coordinates based on mouse position
-- SOURCE
   procedure GetCurrentCellCoords;
-- ****
-- ****f* Maps.UI/UpdateMapInfo
-- FUNCTION
-- Update info about current map cell
-- SOURCE
   procedure UpdateMapInfo(ShowOrdersInfo: Boolean := False);
-- ****
-- ****f* Maps.UI/FinishStory;
-- FUNCTION
-- Finish current story and show confirm dialog to player
-- SOURCE
   procedure FinishStory;
-- ****

end Maps.UI;
