# Copyright 2026 Bartek thindil Jasicki
#
# This file is part of Steam Sky.
#
# Steam Sky is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Steam Sky is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Steam Sky.  If not, see <http://www.gnu.org/licenses/>.

## Provides code related to showing debug interface.

import contracts, nuklear/nuklear_sdl_renderer
import coreui

var debugTab: Positive = 1

proc showShipTab() {.raises: [], tags: [RootEffect], contractual.} =
  ## Show the tab which allows changes in the player's ship
  setLayoutRowDynamic(height = 30, cols = 5)
  labelButton(title = "Move ship"):
    discard
  label(str = "X:")
  label(str = "Y:")

proc showDebugUI*(dialog: var GameDialog) {.raises: [], tags: [ReadIOEffect,
    RootEffect], contractual.} =
  ## Show the debug dialog with various development options
  ##
  ## * dialog - the current in-game dialog to show
  ##
  ## Returns parameter dialog.
  const
    width: float = 700
    height: float = 500
    groupHeight: float = height - 90
  updateDialog(width = width, height = height)
  window(name = "Debug options", x = 40, y = 0, w = width, h = height, flags = {
      windowBorder, windowTitle, windowMinimizable, windowMovable,
      windowNoScrollbar}):
    layoutSpaceStatic(height = groupHeight, widgetsCount = 2):
      row(x = 0, y = 0, w = width * 0.25, h = groupHeight):
        group(title = "debugButtons", flags = {windowNoScrollbar}):
          setLayoutRowDynamic(height = 30, cols = 1)
          labelButton(title = "Ship"):
            debugTab = 1
          labelButton(title = "Crew"):
            debugTab = 2
          labelButton(title = "Cargo"):
            debugTab = 3
          labelButton(title = "Bases"):
            debugTab = 4
          labelButton(title = "World"):
            debugTab = 5
          labelButton(title = "Refresh"):
            discard
          labelButton(title = "Save game"):
            discard
      row(x = width * 0.25, y = 0, w = width * 0.70, h = groupHeight):
        group(title = "debugMenus", flags = {windowNoFlags}):
          case debugTab
          of 1:
            showShipTab()
          else:
            discard
  windowSetFocus(name = "Debug options")
