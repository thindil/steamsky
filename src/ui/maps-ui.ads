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

   procedure CreateSkyMap; -- Create sky map
   procedure ShowSkyMap(X: Integer := PlayerShip.SkyX;
      Y: Integer := PlayerShip.SkyY); -- Show sky map
   procedure UpdateHeader; -- Update game header informations
   procedure SetMapMoveButtons; -- Set icons on move map buttons

private

   Builder: Gtkada_Builder; -- Gtk builder for user interface
   MapWidth, MapHeight, CenterX, CenterY, MapCellWidth, MapCellHeight, MapX,
   MapY: Positive;
   StartX, StartY: Integer;
   ButtonsVisible: Boolean := False;

   procedure DeathConfirm; -- Show confirmation to show game stats when player died
   procedure UpdateMoveButtons; -- Update move buttons
   procedure DrawMap; -- Draw sky map
   procedure HideButtons
     (Widget: not null access Gtk_Widget_Record'Class); -- Hide selected button
   procedure CheckButtons
     (Widget: not null access Gtk_Widget_Record'
        Class); -- Check selected button
   procedure GetCurrentCellCoords; -- Get current map cell coordinates based on mouse position
   procedure UpdateMapInfo
     (ShowOrdersInfo: Boolean := False); -- Update info about current map cell
   procedure FinishStory; -- Finish current story and show confirm dialog to player

end Maps.UI;
