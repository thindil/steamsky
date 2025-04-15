# Copyright 2025 Bartek thindil Jasicki
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

## Provides code related to trading with bases and ships UI, like showing the
## list of items to trade, info about items, trading itself, etc.

import contracts, nuklear/nuklear_sdl_renderer
import coreui, dialogs, header

var
  typesList: seq[string] = @["All"]
  typeIndex: Natural = 0
  typeSearch: string = ""

proc setTrade*() {.raises: [], tags: [], contractual.} =
  ## Set the data for trades UI
  typesList = @["All"]
  typeIndex = 0
  typeSearch = ""

proc showTrade*(state: var GameState; dialog: var GameDialog) {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Show the trading UI
  ##
  ## * state - the current game's state
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameters state and dialog. The latter is modified if
  ## any error happened.
  showHeader(dialog = dialog, close = CloseDestination.map, state = state,
      options = true)
  if state != GameState.trade:
    return
  # Draw dialogs
  showQuestion(dialog = dialog, state = state)
  if state != GameState.trade:
    return
  showMessage(dialog = dialog)
  showInfo(dialog = dialog)
  # Draw UI
  if showOptions:
    setLayoutRowDynamic(height = 35, cols = 3)
    label(str = "Type:")
    let newType = comboList(items = typesList,
        selected = typeIndex, itemHeight = 25, x = 200, y = 150)
    if newType != typeIndex:
      typeIndex = newType
    editString(text = typeSearch, maxLen = 64)
