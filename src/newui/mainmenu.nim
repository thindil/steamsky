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

import std/os
import contracts, nuklear/nuklear_sdl_renderer
import ../game
import coreui

var logo: nk_image

proc setMainMenu*(loadLogo: bool = true) =
  if loadLogo:
    logo = nuklearLoadImage(filePath = (dataDirectory & "ui" & DirSep & "images" & DirSep & "logo.svg").cstring)

proc showMainMenu*(state: var GameState) {.raises: [], tags: [], contractual.} =
  ## Show the game's main menu and set the game's state
  ##
  ## * state - the current game's state
  ##
  ## Returns the modified parameter state.
  window(name = "VersionInfo", x = 180, y = 90, w = 250, h = 70, flags = {
      windowNoFlags}):
    setLayoutRowDynamic(height = 50, cols = 1)
    label(str = gameVersion & " development", alignment = centered)
  window(name = "MainMenu", x = 220, y = 130, w = 150, h = 280, flags = {
      windowNoFlags}):
    setLayoutRowDynamic(height = 40, cols = 1)
    labelButton(title = "New game"):
      echo "button pressed"
    labelButton(title = "Load game"):
      echo "button pressed"
    labelButton(title = "Hall of Fame"):
      echo "button pressed"
    labelButton(title = "News"):
      echo "button pressed"
    labelButton(title = "About"):
      echo "button pressed"
    labelButton(title = "Quit"):
      state = quitGame
      return
  state = mainMenu
