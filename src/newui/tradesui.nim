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

import std/tables
import contracts, nuklear/nuklear_sdl_renderer
import ../[config, game, types]
import coreui, dialogs, errordialog, header

type ItemsSortOrders = enum
  nameAsc, nameDesc, typeAsc, typeDesc, durabilityAsc, durabilityDesc, priceAsc,
    priceDesc, profitAsc, profitDesc, weightAsc, weightDesc, ownedAsc,
    ownedDesc, availableAsc, availableDesc, none

const defaultItemsSortOrder: ItemsSortOrders = none

var
  typesList: seq[string] = @["All"]
  typeIndex: Natural = 0
  typeSearch: string = ""
  itemsSortOrder: ItemsSortOrders = defaultItemsSortOrder
  itemsIndexes: seq[int]

proc setTrade*(dialog: var GameDialog) {.raises: [], tags: [RootEffect], contractual.} =
  ## Set the data for trades UI
  typesList = @["All"]
  var baseCargo: seq[BaseCargo]
  if itemsSortOrder == defaultItemsSortOrder:
    itemsIndexes = @[]
    for index in playerShip.cargo.low .. playerShip.cargo.high:
      itemsIndexes.add(y = index)
    itemsIndexes.add(y = -1)
    for index in baseCargo.low .. baseCargo.high:
      itemsIndexes.add(y = index)
  for i in itemsIndexes:
    if i == -1:
      break
    let
      protoIndex = playerShip.cargo[i].protoIndex
      itemType = try:
          if itemsList[protoIndex].showType.len == 0:
            itemsList[protoIndex].itemType
          else:
            itemsList[protoIndex].showType
        except:
          dialog = setError(message = "Can't get item type.")
          return
    try:
      if typesList.find(item = itemType) == -1 and itemsList[
          protoIndex].price > 0:
        typesList.add(y = itemType)
    except:
      dialog = setError(message = "Can't add item type.")
      return
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
  # Show advanced options if needed
  if showOptions:
    setLayoutRowDynamic(height = 35, cols = 3, ratio = [0.1.cfloat, 0.3, 0.6])
    label(str = "Type:")
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Show only items of the selected type")
    let newType = comboList(items = typesList,
        selected = typeIndex, itemHeight = 25, x = 200, y = 150)
    if newType != typeIndex:
      typeIndex = newType
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Enter a name of an item which you looking for")
    editString(text = typeSearch, maxLen = 64)
