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

import std/algorithm
import contracts, nuklear/nuklear_sdl_renderer
import ../[config, types]
import coreui, header, messagesui, setui, table, themes

type
  BaseSortOrders = enum
    nameAsc, nameDesc, costAsc, costDesc, timeAsc, timeDesc, none

const defaultBaseSortOrder: BaseSortOrders = none

var
  baseSortOrder: BaseSortOrders = defaultBaseSortOrder
  baseState: GameState = healWounded

proc sortItems(x, y: BaseItemData): int {.raises: [], tags: [], contractual.} =
  ## Check how to sort the selected items on the list
  ##
  ## * x - the first item to sort
  ## * y - the second item to sort
  ##
  ## Returns 1 if the x item should go first, otherwise -1
  case baseSortOrder
  of nameAsc:
    if x.name < y.name:
      return 1
    return -1
  of nameDesc:
    if x.name > y.name:
      return 1
    return -1
  of costAsc:
    if x.cost < y.cost:
      return 1
    return -1
  of costDesc:
    if x.cost > y.cost:
      return 1
    return -1
  of timeAsc:
    if x.time < y.time:
      return 1
    return -1
  of timeDesc:
    if x.time > y.time:
      return 1
    return -1
  of none:
    return -1

proc sortItems(sortAsc, sortDesc: BaseSortOrders;
    dialog: var GameDialog) {.raises: [], tags: [RootEffect], contractual.} =
  ## Sort items in the base lists
  ##
  ## * sortAsc  - the sorting value for ascending sort
  ## * sortDesc - the sorting value for descending sort
  ## * dialog   - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  if baseSortOrder == sortAsc:
    baseSortOrder = sortDesc
  else:
    baseSortOrder = sortAsc
  var localItems: seq[BaseItemData] = @[]
  if baseState == healWounded:
    localItems = setWoundedList(dialog = dialog)
  localItems.sort(cmp = sortItems)
  itemsIndexes = @[]
  for item in localItems:
    itemsIndexes.add(y = item.id)

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
  baseState = state
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
    addHeader(headers = headers, ratio = ratio, tooltip = "actions",
      code = sortItems, dialog = dialog)
  showLastMessages(theme = theme, dialog = dialog, height = windowHeight - tableHeight)
