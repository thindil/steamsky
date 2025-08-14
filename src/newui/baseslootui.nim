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

## Provides code related to recruit new crew members in bases, like show the
## UI, start negotiating, show information about a recruit, etc.

import contracts, nuklear/nuklear_sdl_renderer
import ../config
import coreui, header, messagesui, setui, table, themes

proc sortLoot(sortAsc, sortDesc: ItemsSortOrders;
    dialog: var GameDialog) {.raises: [], tags: [RootEffect], contractual.} =
  ## Sort items on the loot list
  ##
  ## * sortAsc  - the sorting value for ascending sort
  ## * sortDesc - the sorting value for descending sort
  ## * dialog   - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  if itemsSortOrder == sortAsc:
    itemsSortOrder = sortDesc
  else:
    itemsSortOrder = sortAsc

const
  headers: array[5, HeaderData[ItemsSortOrders]] = [
    HeaderData[ItemsSortOrders](label: "Name", sortAsc: nameAsc,
        sortDesc: nameDesc),
    HeaderData[ItemsSortOrders](label: "Type", sortAsc: typeAsc,
        sortDesc: typeDesc),
    HeaderData[ItemsSortOrders](label: "Durability", sortAsc: durabilityAsc,
        sortDesc: durabilityDesc),
    HeaderData[ItemsSortOrders](label: "Owned", sortAsc: ownedAsc,
        sortDesc: ownedDesc),
    HeaderData[ItemsSortOrders](label: "Available", sortAsc: availableAsc,
        sortDesc: availableDesc)]
  ratio: array[5, cfloat] = [300.cfloat, 200, 200, 200, 200]

proc showLoot*(state: var GameState; dialog: var GameDialog) {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Show the loot UI
  ##
  ## * state - the current game's state
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameters state and dialog. The latter is modified if
  ## any error happened.
  if showHeader(dialog = dialog, close = CloseDestination.map, state = state):
    return
  setLayoutRowDynamic(height = 30, cols = 3, ratio = [0.1.cfloat, 0.3])
  label(str = "Type:")
  if gameSettings.showTooltips:
    addTooltip(bounds = getWidgetBounds(),
        text = "Show only items of the selected type")
  let newType = comboList(items = typesList,
      selected = typeIndex, itemHeight = 25, x = 200, y = 150)
  if newType != typeIndex:
    typeIndex = newType
  # Show information about free cargo space in the player's ship
  setLayoutRowStatic(height = 30, cols = 2, ratio = [cargoWidth[0], cargoWidth[1]])
  label(str = cargoText[0])
  colorLabel(str = cargoText[1], color = theme.colors[goldenColor])
  # Show the list of items to loot
  let tableHeight: float = windowHeight - gameSettings.messagesPosition.float - 65
  setLayoutRowDynamic(height = tableHeight, cols = 1)
  group(title = "LootGroup", flags = {windowNoFlags}):
    if dialog != none:
      windowDisable()
    addHeader(headers = headers, ratio = ratio, tooltip = "items",
      code = sortLoot, dialog = dialog)
  # Show the last in-game messages
  showLastMessages(theme = theme, dialog = dialog, height = windowHeight - tableHeight)
