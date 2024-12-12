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
# along with Steam Sky.  if, see <http://www.gnu.org/licenses/>.

## Provides code related to the game's main map, like, creating the game's UI,
## etc.

import std/os
import contracts, nuklear/nuklear_sdl_renderer
import ../[config, game, messages]
import coreui, errordialog

var
  mapImages: array[2, PImage] = [nil, nil]

proc createGameUi*(dialog: var GameDialog) {.raises: [], tags: [RootEffect],
    contractual.} =
  ## Create the game's UI and show the map to the player
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns parameter dialog, modified if any error happened.
  if mapImages[0] == nil:
    # Load images
    try:
      mapImages[0] = nuklearLoadSVGImage(filePath = dataDirectory & "ui" &
          DirSep & "images" & DirSep & "ui" & DirSep & "menu.svg", width = 0,
          height = 10 + gameSettings.interfaceFontSize)
    except:
      dialog = setError(message = "Can't set the game's images.")

proc showMap*(state: var GameState; dialog: var GameDialog) {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Show the game's map
  ##
  ## * state - the current game's state
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter state and dialog. The latter is modified if
  ## any error happened.
  layoutSpaceStatic(height = 35, widgetsCount = 3):
    row(x = 0, y = 0, w = 40, h = 35):
      saveButtonStyle()
      setButtonStyle(field = padding, value = NimVec2(x: 0.0, y: 0.0))
      imageButton(image = mapImages[0]):
        discard
      restoreButtonStyle()
    var
      text: string = formattedTime()
      widgetWidth: float = try:
        getTextWidth(text = text) + 15 * getButtonStyle(field = padding).x;
      except:
        dialog = setError(message = "Can't get widget width.")
        return
    row(x = windowWidth / 3.5, y = 0, w = widgetWidth, h = 30):
      label(str = text)
    row(x = windowWidth - 100, y = 0, w = 30, h = 30):
      label(str = "test")
  dialog = none
  state = map
