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

import std/[colors, tables]
import contracts, nuklear/nuklear_sdl_renderer
import ../[config, game, messages, shipscargo]
import coreui, errordialog, themes

var
  mapImages: array[4, PImage] = [nil, nil, nil, nil]

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
      for index, fileName in themesList[gameSettings.interfaceTheme].icons[4..7]:
        mapImages[index] = nuklearLoadSVGImage(filePath = fileName,
            width = 0, height = 20 + gameSettings.interfaceFontSize)
    except:
      dialog = setError(message = "Can't set the game's images.")

proc showHeader(dialog: var GameDialog) {.raises: [], tags: [RootEffect],
    contractual.} =
  ## Show the game's header
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened or the game's menu is to show.
  let fuelAmount: Natural = try:
        getItemAmount(itemType = fuelType)
      except KeyError:
        dialog = setError(message = "Can't get fuel amount.")
        return
  setRowTemplate(height = 35):
    rowTemplateStatic(width = 40)
    rowTemplateDynamic()
    rowTemplateStatic(width = 30)
    try:
      rowTemplateStatic(width = getTextWidth(text = $fuelAmount))
    except:
      dialog = setError(message = "Can't set fuel text width")
      return
  imageButton(image = mapImages[0]):
    discard
  label(str = formattedTime(), alignment = centered)
  let theme: ThemeData = try:
      themesList[gameSettings.interfaceTheme]
    except:
      dialog = setError(message = "Can't get the game's theme.")
      return
  var
    itemAmount = try:
        getItemAmount(itemType = fuelType)
      except KeyError:
        dialog = setError(message = "Can't get fuel amount.")
        return
    r, g, b: Natural = 0
    image: PImage = nil
  if itemAmount > gameSettings.lowFuel:
    (r, g, b) = theme.colors[2].extractRGB
    image = mapImages[1]
  elif itemAmount > 0:
    (r, g, b) = theme.colors[27].extractRGB
    image = mapImages[3]
  else:
    (r, g, b) = theme.colors[28].extractRGB
    image = mapImages[2]
  image(image = image)
  colorLabel(str = $fuelAmount, r = r, g = g, b = b)

proc showMap*(state: var GameState; dialog: var GameDialog) {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Show the game's map
  ##
  ## * state - the current game's state
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameters state and dialog. The latter is modified if
  ## any error happened.
  showHeader(dialog = dialog)
  state = map
