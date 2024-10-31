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
  window(name = "News", x = 0, y = 0, w = windowWidth.float, h = (windowHeight -
      50).float, flags = {windowNoFlags}):
    setLayoutRowDynamic(height = (windowHeight - 40).float, cols = 1)
    label(str = "here")
  window(name = "Buttons", x = 0, y = (windowHeight - 40).float,
      w = windowWidth.float, h = 40, flags = {windowNoFlags}):
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
  window(name = "Logo", x = 50, y = 0, w = 500, h = 110, flags = {
      windowNoFlags}):
    setLayoutRowDynamic(height = 90, cols = 1)
    image(image = logo)
  window(name = "VersionInfo", x = 180, y = 90, w = 250, h = 70, flags = {
      windowNoFlags}):
    setLayoutRowDynamic(height = 50, cols = 1)
    label(str = gameVersion & " development", alignment = centered)
  window(name = "MainMenu", x = 220, y = 130, w = 150, h = 280, flags = {
      windowNoFlags}):
    setLayoutRowDynamic(height = 40, cols = 1)
    labelButton(title = "New game"):
      echo "button pressed"
    if showLoadButton:
      labelButton(title = "Load game"):
        echo "button pressed"
    if showHoFButton:
      labelButton(title = "Hall of Fame"):
        echo "button pressed"
    labelButton(title = "News"):
      state = news
      return
    labelButton(title = "About"):
      echo "button pressed"
    labelButton(title = "Quit"):
      state = quitGame
      return
  state = mainMenu
