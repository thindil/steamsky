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

import std/[strutils, tables]
import contracts, nuklear/nuklear_sdl_renderer
import ../[basescargo, basestypes, config, crewinventory, game, items, maps, types]
import coreui, dialogs, errordialog, header

type ItemsSortOrders = enum
  nameAsc, nameDesc, typeAsc, typeDesc, durabilityAsc, durabilityDesc, priceAsc,
    priceDesc, profitAsc, profitDesc, weightAsc, weightDesc, ownedAsc,
    ownedDesc, availableAsc, availableDesc, none

const defaultItemsSortOrder: ItemsSortOrders = none

var
  typesList: seq[string] = @["All"]
  typeIndex: Natural = 0
  nameSearch: string = ""
  itemsSortOrder: ItemsSortOrders = defaultItemsSortOrder
  itemsIndexes: seq[int]
  currentPage: Positive = 1
  baseType: string
  baseIndex: BasesRange
  baseCargo: seq[BaseCargo]
  eventIndex: int

proc setTrade*(dialog: var GameDialog) {.raises: [], tags: [RootEffect],
    contractual.} =
  ## Set the data for trades UI
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  typesList = @["All"]
  baseCargo = @[]
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
  nameSearch = ""
  currentPage = 1
  baseIndex = skyMap[playerShip.skyX][playerShip.skyY].baseIndex
  eventIndex = skyMap[playerShip.skyX][playerShip.skyY].eventIndex
  if baseIndex > 0:
    baseType = skyBases[baseIndex].baseType
    baseCargo = skyBases[baseIndex].cargo
  else:
    baseType = "0"
    baseCargo = traderCargo

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
    editString(text = nameSearch, maxLen = 64)
  let
    moneyIndex2: int = findItem(inventory = playerShip.cargo,
        protoIndex = moneyIndex)
    moneyText: array[4, string] = ["You have ", $playerShip.cargo[
        moneyIndex2].amount & " " & moneyName, " Base has ", $skyBases[
        baseIndex].cargo[0].amount & " " & moneyName]
    textWidth: array[4, cfloat] = try:
        [moneyText[0].getTextWidth.cfloat, moneyText[1].getTextWidth.cfloat,
            moneyText[2].getTextWidth.cfloat, moneyText[3].getTextWidth.cfloat]
      except:
        dialog = setError(message = "Can't count the money info width.")
        return
  setLayoutRowStatic(height = 35, cols = 4, ratio = textWidth)
  label(str = moneyText[0])
  label(str = moneyText[1])
  label(str = moneyText[2])
  label(str = moneyText[3])
  var
    currentItemIndex = 0
    indexesList: seq[Natural]
    currentRow = 1
  let startRow = ((currentPage - 1) * gameSettings.listsLimit) + 1
  for i in itemsIndexes:
    currentItemIndex.inc
    if i == -1:
      break
    try:
      if getPrice(baseType = baseType, itemIndex = playerShip.cargo[
          i].protoIndex) == 0:
        continue
    except:
      dialog = setError(message = "Can't get price.")
    let
      protoIndex = playerShip.cargo[i].protoIndex
      baseCargoIndex = findBaseCargo(protoIndex = protoIndex,
          durability = playerShip.cargo[i].durability)
    if baseCargoIndex > -1:
      indexesList.add(y = baseCargoIndex)
    let itemType = try:
          if itemsList[protoIndex].showType.len == 0:
            itemsList[protoIndex].itemType
          else:
            itemsList[protoIndex].showType
        except:
          dialog = setError(message = "Can't get item type2.")
          return
    if typeIndex > 0 and itemType != typesList[typeIndex]:
      continue
    let itemName = getItemName(item = playerShip.cargo[i], damageInfo = false,
        toLower = false)
    if nameSearch.len > 0 and itemName.toLowerAscii.find(
        sub = nameSearch.toLowerAscii) == -1:
      continue
    if currentRow < startRow:
      currentRow.inc
      continue
    var price = 0
    if baseCargoIndex == -1:
      try:
        price = getPrice(baseType = baseType, itemIndex = protoIndex)
      except:
        dialog = setError(message = "Can't get price2.")
        return
    else:
      price = if baseIndex > 0:
          skyBases[baseIndex].cargo[baseCargoIndex].price
        else:
          traderCargo[baseCargoIndex].price
    if eventIndex > -1:
      if eventsList[eventIndex].eType == doublePrice and eventsList[
          eventIndex].itemIndex == protoIndex:
        price = price * 2
    let profit = price - playerShip.cargo[i].price
    var baseAmount = 0
    if baseIndex > 0:
      try:
        if baseCargoIndex > -1 and isBuyable(baseType = baseType,
            itemIndex = protoIndex):
          baseAmount = baseCargo[baseCargoIndex].amount
      except:
        dialog = setError(message = "Can't get base amount.")
        return
    else:
      if baseCargoIndex > -1:
        baseAmount = baseCargo[baseCargoIndex].amount
