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
import ../game
import coreui

var debugTab, shipX, shipY: Positive = 1

proc setDebugData*() {.raises: [], tags: [], contractual.} =
  ## Set the data for the debug UI
  shipX = playerShip.skyX
  shipY = playerShip.skyY

proc showShipTab() {.raises: [], tags: [RootEffect], contractual.} =
  ## Show the tab which allows changes in the player's ship
  setLayoutRowDynamic(height = 30, cols = 5)
  labelButton(title = "Move ship"):
    playerShip.skyX = shipX
    playerShip.skyY = shipY
  label(str = "X:")
  var newValue: Positive = property2(name = "#", min = 1, val = shipX,
      max = 1_024, step = 1, incPerPixel = 1)
  if newValue != shipX:
    shipX = newValue
  label(str = "Y:")
  newValue = property2(name = "#", min = 1, val = shipY, max = 1_024, step = 1,
      incPerPixel = 1)
  if newValue != shipX:
    shipY = newValue

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
    groupOneWidth: float = width * 0.25
    groupTwoWidth: float = width * 0.72
  updateDialog(width = width, height = height)
  window(name = "Debug options", x = 40, y = 0, w = width, h = height, flags = {
      windowBorder, windowTitle, windowMinimizable, windowMovable,
      windowNoScrollbar}):
    layoutSpaceStatic(height = groupHeight, widgetsCount = 2):
      row(x = 0, y = 0, w = groupOneWidth, h = groupHeight):
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
            setDebugData()
          labelButton(title = "Save game"):
            discard
      row(x = groupOneWidth, y = 0, w = groupTwoWidth, h = groupHeight):
        group(title = "debugMenus", flags = {windowNoFlags}):
          case debugTab
          of 1:
            showShipTab()
          else:
            discard
  windowSetFocus(name = "Debug options")
