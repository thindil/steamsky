# Copyright 2024 Bartek thindil Jasicki
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

## Provides code related to the game's main menu, like showing the
## menu, and selecting its various sections

import std/[os, sequtils]
import contracts, nuklear/nuklear_sdl_renderer
import ../game
import coreui

var
  logo: PImage = nil
  showLoadButton, showHoFButton: bool = false

proc setMainMenu*() {.raises: [NuklearException], tags: [
    ReadDirEffect], contractual.} =
  ## Set the main menu, load logo if needed and set the menu's buttons
  if logo == nil:
    logo = nuklearLoadSVGImage(filePath = dataDirectory & "ui" & DirSep &
        "images" & DirSep & "logo.svg", width = 0, height = 110)
  showLoadButton = walkFiles(pattern = saveDirectory & "*.sav").toSeq.len > 0
  showHoFButton = fileExists(filename = saveDirectory & "halloffame.dat")

proc showNews*(state: var GameState) {.raises: [], tags: [], contractual.} =
  ## Show the game's latest changes
  ## * state - the current game's state
  ##
  ## Returns the modified parameter state.
  setLayoutRowDynamic(height = (windowHeight - 40).float, cols = 1)
  label(str = "here")
  setLayoutRowDynamic(height = 40, cols = 2)
  labelButton(title = "Test"):
    echo "button pressed"
  state = news

proc showMainMenu*(state: var GameState) {.raises: [], tags: [], contractual.} =
  ## Show the game's main menu and set the game's state
  ##
  ## * state - the current game's state
  ##
  ## Returns the modified parameter state.
  layoutSpaceStatic(height = 90, widgetsCount = 1):
    row(x = 50, y = 0, w = 500, h = 90):
      image(image = logo)
  setLayoutRowDynamic(height = 40, cols = 1)
  label(str = gameVersion & " development", alignment = centered)
  layoutSpaceStatic(height = 240, widgetsCount = 6):
    const
      x: float = 225
      w: float = 150
      h: float = 40
    row(x = x, y = 0, w = w, h = h):
      labelButton(title = "New game"):
        echo "button pressed"
    var y: float = h;
    if showLoadButton:
      row(x = x, y = y, w = w, h = h):
        y += h
        labelButton(title = "Load game"):
          echo "button pressed"
    if showHoFButton:
      row(x = x, y = y, w = w, h = h):
        y += h
        labelButton(title = "Hall of Fame"):
          echo "button pressed"
    row(x = x, y = y, w = w, h = h):
      y += h
      labelButton(title = "News"):
        state = news
        return
    row(x = x, y = y, w = w, h = h):
      y += h
      labelButton(title = "About"):
        echo "button pressed"
    row(x = x, y = y, w = w, h = h):
      y += h
      labelButton(title = "Quit"):
        state = quitGame
        return
  state = mainMenu
