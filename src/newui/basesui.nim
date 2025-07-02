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

## Provides code related to various interactions in bases, like buying recipes,
## repair ship, healing wounded crew memebrs, etc.

import contracts, nuklear/nuklear_sdl_renderer
import ../config
import coreui, header, messagesui, setui, table, themes

type
  BaseSortOrders = enum
    nameAsc, nameDesc, costAsc, costDesc, timeAsc, timeDesc, none

const
  headers: array[3, HeaderData[BaseSortOrders]] = [
    HeaderData[BaseSortOrders](label: "Action", sortAsc: nameAsc,
        sortDesc: nameDesc),
    HeaderData[BaseSortOrders](label: "Cost", sortAsc: costAsc,
        sortDesc: costDesc),
    HeaderData[BaseSortOrders](label: "Time", sortAsc: timeAsc,
        sortDesc: timeDesc)]
  ratio: array[3, cfloat] = [400.cfloat, 200, 200]

proc showWounded*(state: var GameState; dialog: var GameDialog) {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Show the UI with the list of wounded the player's ship's crew members
  ##
  ## * state - the current game's state
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameters state and dialog. The latter is modified if
  ## any error happened.
  if showHeader(dialog = dialog, close = CloseDestination.map, state = state):
    return
  let tableHeight: float = windowHeight - gameSettings.messagesPosition.float - 20
  setLayoutRowDynamic(height = tableHeight, cols = 1)
  group(title = "HealGroup", flags = {windowNoFlags}):
    if dialog != none:
      windowDisable()
    # Show information about money owned by the player
    setLayoutRowStatic(height = 30, cols = moneyWidth.len, ratio = moneyWidth)
    for index, text in moneyText:
      if index mod 2 == 0:
        label(str = text)
      else:
        colorLabel(str = text, color = theme.colors[goldenColor])
  showLastMessages(theme = theme, dialog = dialog, height = windowHeight - tableHeight)
