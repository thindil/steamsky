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

## Provides code related to the game's help, like showing it, etc.

import std/tables
import contracts, nuklear/nuklear_sdl_renderer
import ../[config, help, types]
import coreui, header, themes

var selected: ExtendedNatural = -1

proc showHelp*(state: var GameState; dialog: var GameDialog) {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Show the screen with in-game help
  ##
  ## * state - the current game's state
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameters state and dialog. The latter is modified if
  ## any error happened.
  if showHeader(dialog = dialog, close = previous, state = state):
    return
  setLayoutRowDynamic(height = gameSettings.topicsPosition.float, cols = 1)
  group(title = "TopicsGroup", flags = {windowNoFlags}):
    setLayoutRowDynamic(height = 25, cols = 1)
    var index = 0
    for title, help in helpList:
      index.inc
      var sel: bool = selected == index
      if selectableLabel(str = title, value = sel):
        if sel:
          selected = index
        else:
          selected = -1
  setLayoutRowDynamic(height = 20, cols = 2)
  if gameSettings.showTooltips:
    addTooltip(bounds = getWidgetBounds(),
        text = "Make the list of topics smaller.")
  imageButtonCentered(image = images[contract2Icon]):
    gameSettings.topicsPosition -= gameSettings.interfaceFontSize + 10
  if gameSettings.showTooltips:
    addTooltip(bounds = getWidgetBounds(),
        text = "Make the list of topics bigger.")
  imageButtonCentered(image = images[expand2Icon]):
    gameSettings.topicsPosition += gameSettings.interfaceFontSize + 10
