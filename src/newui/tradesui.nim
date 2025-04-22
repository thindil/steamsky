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

import std/[colors, strutils, tables]
import contracts, nuklear/nuklear_sdl_renderer
import ../[basescargo, basestypes, config, crewinventory, game, items, maps,
    shipscargo, types]
import coreui, dialogs, errordialog, header, themes

type ItemsSortOrders = enum
  nameAsc, nameDesc, typeAsc, typeDesc, durabilityAsc, durabilityDesc, priceAsc,
    priceDesc, profitAsc, profitDesc, weightAsc, weightDesc, ownedAsc,
    ownedDesc, availableAsc, availableDesc, none

const defaultItemsSortOrder: ItemsSortOrders = none

var
  typesList: seq[string] = @["All"]
  typeIndex: Natural = 0
  nameSearch, location, baseType: string = ""
  itemsSortOrder: ItemsSortOrders = defaultItemsSortOrder
  itemsIndexes: seq[int] = @[]
  currentPage: Positive = 1
  baseIndex: BasesRange = 1
  baseCargo: seq[BaseCargo] = @[]
  eventIndex, moneyIndex2: int = -1
  moneyText: seq[string] = @[]
  moneyWidth: seq[cfloat] = @[]
  cargoText: array[2, string] = ["Free cargo space is ", ""]
  cargoWidth: array[2, cfloat] = [0.cfloat, 0]

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
    location = "Base"
  else:
    baseType = "0"
    baseCargo = traderCargo
    location = "Ship"
  moneyIndex2 = findItem(inventory = playerShip.cargo, protoIndex = moneyIndex)
  moneyText = @[]
  moneyWidth = @[]
  if moneyIndex == -1:
    moneyText.add(y = "You don't have " & moneyName & " to buy anything")
  else:
    moneyText.add(y = "You have ")
    moneyText.add(y = $playerShip.cargo[moneyIndex2].amount & " " & moneyName)
  if baseCargo[0].amount == 0:
    moneyText.add(y = " " & location & " doesn't have any " & moneyName & " to buy anything")
  else:
    moneyText.add(y = " " & location & " has ")
    moneyText.add(y = $baseCargo[0].amount & " " & moneyName)
  for text in moneyText:
    try:
      moneyWidth.add(y = text.getTextWidth)
    except:
      dialog = setError(message = "Can't get the width of the money text.")
      return
  var freeSpace = try:
      freeCargo(amount = 0)
    except:
      dialog = setError(message = "Can't get free space.")
      return
  if freeSpace < 0:
    freeSpace = 0
  cargoWidth[0] = try:
      cargoText[0].getTextWidth.cfloat
    except:
      dialog = setError(message = "Can't get the width of the cargo text.")
      0.0
  cargoText[1] = $freeSpace & " kg"
  cargoWidth[1] = try:
      cargoText[1].getTextWidth.cfloat
    except:
      dialog = setError(message = "Can't get the width of the cargo text.")
      0.0

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
    setLayoutRowDynamic(height = 25, cols = 3, ratio = [0.1.cfloat, 0.3, 0.6])
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
  # Show information about money owned by the player and the base
  setLayoutRowStatic(height = 25, cols = moneyWidth.len, ratio = moneyWidth)
  for index, text in moneyText:
    if index mod 2 == 0:
      label(str = text)
    else:
      colorLabel(str = text, color = theme.colors[goldenColor])
  # Show information about free cargo space in the player's ship
  setLayoutRowStatic(height = 25, cols = 2, ratio = cargoWidth)
  label(str = cargoText[0])
  colorLabel(str = cargoText[1], color = theme.colors[goldenColor])
  # Show the list of items for trade
  setLayoutRowDynamic(height = windowHeight - 140 - (
      if showOptions: 45 else: 0), cols = 1)
  group(title = "TradeGroup", flags = {windowNoFlags}):
    setLayoutRowStatic(height = 25, cols = 8, ratio = [200.cfloat, 200, 200,
        200, 200, 200, 200, 200])
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Press mouse button to sort the items.")
    labelButton(title = "Name"):
      if itemsSortOrder == nameAsc:
        itemsSortOrder = nameDesc
      else:
        itemsSortOrder = nameAsc
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Press mouse button to sort the items.")
    labelButton(title = "Type"):
      if itemsSortOrder == typeAsc:
        itemsSortOrder = typeDesc
      else:
        itemsSortOrder = typeAsc
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Press mouse button to sort the items.")
    labelButton(title = "Durability"):
      if itemsSortOrder == durabilityAsc:
        itemsSortOrder = durabilityDesc
      else:
        itemsSortOrder = durabilityAsc
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Press mouse button to sort the items.")
    labelButton(title = "Price"):
      if itemsSortOrder == priceAsc:
        itemsSortOrder = priceDesc
      else:
        itemsSortOrder = priceAsc
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Press mouse button to sort the items.")
    labelButton(title = "Profit"):
      if itemsSortOrder == profitAsc:
        itemsSortOrder = profitDesc
      else:
        itemsSortOrder = profitAsc
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Press mouse button to sort the items.")
    labelButton(title = "Weight"):
      if itemsSortOrder == weightAsc:
        itemsSortOrder = weightDesc
      else:
        itemsSortOrder = weightAsc
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Press mouse button to sort the items.")
    labelButton(title = "Owned"):
      if itemsSortOrder == ownedAsc:
        itemsSortOrder = ownedDesc
      else:
        itemsSortOrder = ownedAsc
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Press mouse button to sort the items.")
    labelButton(title = "Available"):
      if itemsSortOrder == availableAsc:
        itemsSortOrder = availableDesc
      else:
        itemsSortOrder = availableAsc
    var
      currentItemIndex = 0
      indexesList: seq[Natural]
      currentRow = 1
    let startRow = ((currentPage - 1) * gameSettings.listsLimit) + 1
    saveButtonStyle()
    setButtonStyle(field = borderColor, a = 0)
    try:
      setButtonStyle(field = normal, color = "#120d0d".parseColor)
    except:
      dialog = setError(message = "Can't set table color")
      return
    setButtonStyle(field = rounding, value = 0)
    setButtonStyle(field = border, value = 0)
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
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Show available options of item.")
      labelButton(title = itemName):
        discard
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Show available options of item.")
      labelButton(title = itemType):
        discard
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Show available options of item.")
      labelButton(title = "Placeholder"):
        discard
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Show available options of item.")
      labelButton(title = $price):
        discard
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Show available options of item.")
      labelButton(title = $profit):
        discard
      try:
        if gameSettings.showTooltips:
          addTooltip(bounds = getWidgetBounds(),
              text = "Show available options of item.")
        labelButton(title = $itemsList[protoIndex].weight & " kg"):
          discard
      except:
        dialog = setError(message = "Can't show weight")
        return
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Show available options of item.")
      labelButton(title = $playerShip.cargo[i].amount):
        discard
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Show available options of item.")
      labelButton(title = $baseAmount):
        discard
    restoreButtonStyle()
