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

import std/[algorithm, math, strutils, tables]
import contracts, nuklear/nuklear_sdl_renderer
import ../[basescargo, basestypes, config, game, items, types, trades]
import coreui, dialogs, errordialog, header, messagesui, setui, table, themes

var itemIndex: int = -1

type LocalItemData = object
  name, iType: string
  damage: float
  price, owned, available, id: Natural
  profit: int
  weight: Positive = 1
  quality: ObjectQuality

proc sortItems(x, y: LocalItemData): int {.raises: [], tags: [], contractual.} =
  ## Check how to sort the selected items on the list
  ##
  ## * x - the first item to sort
  ## * y - the second item to sort
  ##
  ## Returns 1 if the x item should go first, otherwise -1
  case itemsSortOrder
  of nameAsc:
    if x.name < y.name:
      return 1
    return -1
  of nameDesc:
    if x.name > y.name:
      return 1
    return -1
  of typeAsc:
    if x.iType < y.iType:
      return 1
    return -1
  of typeDesc:
    if x.iType > y.iType:
      return 1
    return -1
  of durabilityAsc:
    if x.damage < y.damage:
      return 1
    return -1
  of durabilityDesc:
    if x.damage > y.damage:
      return 1
    return -1
  of qualityAsc:
    if x.quality < y.quality:
      return 1
    return -1
  of qualityDesc:
    if x.quality > y.quality:
      return 1
    return -1
  of priceAsc:
    if x.price < y.price:
      return 1
    return -1
  of priceDesc:
    if x.price > y.price:
      return 1
    return -1
  of profitAsc:
    if x.profit < y.profit:
      return 1
    return -1
  of profitDesc:
    if x.profit > y.profit:
      return 1
    return -1
  of weightAsc:
    if x.weight < y.weight:
      return 1
    return -1
  of weightDesc:
    if x.weight > y.weight:
      return 1
    return -1
  of ownedAsc:
    if x.owned < y.owned:
      return 1
    return -1
  of ownedDesc:
    if x.owned > y.owned:
      return 1
    return -1
  of availableAsc:
    if x.available < y.available:
      return 1
    return -1
  of availableDesc:
    if x.available > y.available:
      return 1
    return -1
  of none:
    return -1

proc sortTrades(sortAsc, sortDesc: ItemsSortOrders;
    dialog: var GameDialog) {.raises: [], tags: [RootEffect], contractual.} =
  ## Sort items on the trades list
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
  var
    localItems: seq[LocalItemData] = @[]
    indexesList: seq[Natural] = @[]
  for index, item in playerShip.cargo:
    let
      protoIndex: Natural = item.protoIndex
      baseCargoIndex: int = findBaseCargo(protoIndex = protoIndex,
          durability = item.durability, quality = item.quality)
    var price: int = 0
    if baseCargoIndex > -1:
      indexesList.add(y = baseCargoIndex)
      price = baseCargo[baseCargoIndex].price
    else:
      price = try:
          getPrice(baseType = baseType, itemIndex = protoIndex,
              quality = item.quality)
        except:
          dialog = setError(message = "Can't get price.")
          return
    if eventIndex > -1:
      if eventsList[eventIndex].eType == doublePrice and eventsList[
          eventIndex].itemIndex == protoIndex:
        price *= 2
    try:
      localItems.add(y = LocalItemData(name: getItemName(item = item),
          iType: (if itemsList[protoIndex].showType.len == 0: itemsList[
          protoIndex].itemType else: itemsList[protoIndex].showType),
              damage: (
          item.durability.float / defaultItemDurability.float), price: price,
          profit: price - item.price, weight: itemsList[protoIndex].weight,
          owned: item.amount, available: (if baseCargoIndex > -1: baseCargo[
          baseCargoIndex].amount else: 0), id: index, quality: item.quality))
    except:
      dialog = setError(message = "Can't add item from the player's ship's cargo.")
      return
  localItems.sort(cmp = sortItems)
  itemsIndexes = @[]
  for item in localItems:
    itemsIndexes.add(y = item.id)
  itemsIndexes.add(y = -1)
  localItems = @[]
  for index, item in baseCargo:
    if index in indexesList:
      continue
    let protoIndex: Natural = item.protoIndex
    var price: int = item.price
    if eventIndex > -1:
      if eventsList[eventIndex].eType == doublePrice and eventsList[
          eventIndex].itemIndex == protoIndex:
        price *= 2
    try:
      localItems.add(y = LocalItemData(name: itemsList[protoIndex].name,
          iType: (if itemsList[protoIndex].showType.len == 0: itemsList[
          protoIndex].itemType else: itemsList[protoIndex].showType),
              damage: (
          item.durability.float / defaultItemDurability.float), price: price,
          profit: -price, weight: itemsList[protoIndex].weight, owned: 0,
          available: item.amount, id: index, quality: item.quality))
    except:
      dialog = setError(message = "Can't add item from the base's cargo.")
      return
  localItems.sort(cmp = sortItems)
  for item in localItems:
    itemsIndexes.add(y = item.id)

proc setBuyDialog(dialog: var GameDialog) {.raises: [], tags: [RootEffect],
    contractual.} =
  ## Set the dialog for buying items
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  closePopup()
  var baseCargoIndex: int = -1
  if itemIndex > -1:
    let
      protoIndex: Natural = playerShip.cargo[itemIndex].protoIndex
    baseCargoIndex = -findBaseCargo(protoIndex = protoIndex,
        durability = playerShip.cargo[itemIndex].durability,
        quality = playerShip.cargo[itemIndex].quality)
  else:
    baseCargoIndex = itemIndex
  dialog = setManipulate(action = buyAction, iIndex = baseCargoIndex)

proc setSellDialog(dialog: var GameDialog) {.raises: [], tags: [RootEffect],
    contractual.} =
  ## Set the dialog for selling items
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  closePopup()
  dialog = setManipulate(action = sellAction, iIndex = itemIndex)

proc showItemInfo(data: int; dialog: var GameDialog) {.raises: [], tags: [
    RootEffect], contractual.} =
  ## Show the selected item information
  ##
  ## * data   - the index of the selected item
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  itemIndex = itemsIndexes[data]
  if data > playerShip.cargo.len:
    itemIndex *= -1
  let (protoIndex, maxSellAmount, maxBuyAmount, _, quality) = try:
      getTradeData(iIndex = itemIndex)
    except:
      dialog = setError(message = "Can't get the trade's data.")
      return
  var itemInfo: string = ""
  try:
    if itemsList[protoIndex].itemType == weaponType:
      itemInfo.add(y = "Skill: {gold}" & skillsList[itemsList[
          protoIndex].value[3]].name & "/" & attributesList[skillsList[
              itemsList[protoIndex].value[
          3]].attribute].name & (if itemsList[protoIndex].value[4] ==
          1: "\nCan be used with shield." else: "\nCan't be used with shield (two-handed weapon).") & "\n{/gold}Damage type: {gold}")
      case itemsList[protoIndex].value[5]
      of 1:
        itemInfo.add(y = "cutting")
      of 2:
        itemInfo.add(y = "impaling")
      of 3:
        itemInfo.add(y = "blunt")
      else:
        discard
      itemInfo.add(y = "{/gold}")
  except:
    dialog = setError(message = "Can't show weapon info.")
    return
  let itemTypes: array[6, string] = [weaponType, chestArmor, headArmor,
      armsArmor, legsArmor, shieldType]
  for itemType in itemTypes:
    try:
      if itemsList[protoIndex].itemType == itemType:
        if itemInfo.len > 0:
          itemInfo.add(y = "\n")
        itemInfo.add(y = "Damage chance: {gold}" & getItemChanceToDamage(
            itemData = itemsList[protoIndex].value[1]) &
            "\n{/gold}Strength: {gold}" & $itemsList[protoIndex].value[2] & "{/gold}")
        break
    except:
      dialog = setError(message = "Can't get damage chance.")
      return
  try:
    if itemsList[protoIndex].itemType in toolsList:
      if itemInfo.len > 0:
        itemInfo.add(y = "\n")
      itemInfo.add(y = "Damage chance: {gold}" & getItemChanceToDamage(
          itemData = itemsList[protoIndex].value[1]) & "{/gold}")
  except:
    dialog = setError(message = "Can't get tool info.")
    return
  try:
    if itemsList[protoIndex].itemType.len > 4 and (itemsList[
        protoIndex].itemType[0..3] == "Ammo" or itemsList[
        protoIndex].itemType == "Harpoon"):
      if itemInfo.len > 0:
        itemInfo.add(y = "\n")
      itemInfo.add(y = "Strength: {gold}" & $itemsList[protoIndex].value[1] & "{/gold}")
  except:
    dialog = setError(message = "Can't get ammo info.")
    return
  try:
    if itemInfo.len > 0:
      itemInfo.add(y = "\n")
    itemInfo.add(y = "Quality: {gold}" & ($quality).capitalizeAscii & "{/gold}")
  except:
    dialog = setError(message = "Can't get quality info.")
    return
  try:
    if itemsList[protoIndex].description.len > 0:
      itemInfo.add(y = "\n\n" & itemsList[protoIndex].description)
  except:
    dialog = setError(message = "Can't get the description.")
    return
  try:
    dialog = setInfo(text = itemInfo, title = itemsList[protoIndex].name,
        button1 = (if maxBuyAmount ==
        0: emptyButtonSettings else: ButtonSettings(
        tooltip: "Buy item from the base", code: setBuyDialog,
        icon: buyDefaultIcon.ord, text: "Buy", color: "")), button2 = (
        if maxSellAmount == 0: emptyButtonSettings else: ButtonSettings(
        tooltip: "Sell item from the ship cargo", code: setSellDialog,
        icon: sellDefaultIcon.ord, text: "Sell", color: "")))
  except:
    dialog = setError(message = "Can't show the item's info.")

const
  headers: array[9, HeaderData[ItemsSortOrders]] = [
    HeaderData[ItemsSortOrders](label: "Name", sortAsc: nameAsc,
        sortDesc: nameDesc),
    HeaderData[ItemsSortOrders](label: "Type", sortAsc: typeAsc,
        sortDesc: typeDesc),
    HeaderData[ItemsSortOrders](label: "Durability", sortAsc: durabilityAsc,
        sortDesc: durabilityDesc),
    HeaderData[ItemsSortOrders](label: "Quality", sortAsc: qualityAsc,
        sortDesc: qualityDesc),
    HeaderData[ItemsSortOrders](label: "Price", sortAsc: priceAsc,
        sortDesc: priceDesc),
    HeaderData[ItemsSortOrders](label: "Profit", sortAsc: profitAsc,
        sortDesc: profitDesc),
    HeaderData[ItemsSortOrders](label: "Weight", sortAsc: weightAsc,
        sortDesc: weightDesc),
    HeaderData[ItemsSortOrders](label: "Owned", sortAsc: ownedAsc,
        sortDesc: ownedDesc),
    HeaderData[ItemsSortOrders](label: "Available", sortAsc: availableAsc,
        sortDesc: availableDesc)]
  ratio: array[9, cfloat] = [300.cfloat, 200, 200, 200, 200, 200, 200, 200, 200]

proc showPlayerItems(dialog: var GameDialog; indexesList: var seq[Natural];
    currentRow, row: var Positive; startRow: Positive) {.raises: [], tags: [
    RootEffect], contractual.} =
  ## Show the list of items in the player's ship's cargo
  ##
  ## * dialog      - the current in-game dialog displayed on the screen
  ## * indexesList - the list of indexes of added items from the trader's cargo
  ## * currentRow  - the current row in the list
  ## * row         - the amount of rows in the list, used for pagination
  ## * startRow    - the row from which the list will start
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  var currentItemIndex: Natural = 0
  for index, i in itemsIndexes:
    currentItemIndex.inc
    if i == -1:
      break
    try:
      if getPrice(baseType = baseType, itemIndex = playerShip.cargo[
          i].protoIndex, quality = normal) == 0:
        continue
    except:
      dialog = setError(message = "Can't get price.")
      break
    let
      protoIndex: Natural = playerShip.cargo[i].protoIndex
      baseCargoIndex: int = findBaseCargo(protoIndex = protoIndex,
          durability = playerShip.cargo[i].durability,
          quality = playerShip.cargo[i].quality)
    if baseCargoIndex > -1:
      indexesList.add(y = baseCargoIndex)
    let itemType: string = try:
          if itemsList[protoIndex].showType.len == 0:
            itemsList[protoIndex].itemType
          else:
            itemsList[protoIndex].showType
        except:
          dialog = setError(message = "Can't get item type2.")
          return
    if typeIndex > 0 and itemType != typesList[typeIndex]:
      continue
    let itemName: string = getItemName(item = playerShip.cargo[i],
        damageInfo = false, toLower = false)
    if nameSearch.len > 0 and itemName.toLowerAscii.find(
        sub = nameSearch.toLowerAscii) == -1:
      continue
    if currentRow < startRow:
      currentRow.inc
      continue
    var price: int = 0
    if baseCargoIndex == -1:
      try:
        price = getPrice(baseType = baseType, itemIndex = protoIndex,
            quality = playerShip.cargo[i].quality)
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
        price *= 2
    let profit: int = price - playerShip.cargo[i].price
    var baseAmount: Natural = 0
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
    addButton(label = itemName, tooltip = "Show available options of item.",
      data = index, code = showItemInfo, dialog = dialog)
    addButton(label = itemType, tooltip = "Show available options of item.",
      data = index, code = showItemInfo, dialog = dialog)
    addProgressBar(tooltip = (if playerShip.cargo[i].durability < 100:
      getItemDamage(itemDurability = playerShip.cargo[i].durability)
      else: "Unused"), value = playerShip.cargo[i].durability,
      maxValue = defaultItemDurability, data = index, code = showItemInfo,
      dialog = dialog)
    addButton(label = ($playerShip.cargo[i].quality).capitalizeAscii,
        tooltip = "Show available options of item.", data = index,
        code = showItemInfo, dialog = dialog)
    addButton(label = $price, tooltip = "Show available options of item.",
      data = index, code = showItemInfo, dialog = dialog)
    addButton(label = $profit, tooltip = "Show available options of item.",
        data = index, code = showItemInfo, dialog = dialog, color = (
        if profit >
        0: greenColor elif profit < 0: redColor else: tableTextColor))
    setButtonStyle(field = textNormal, color = theme.colors[tableTextColor])
    try:
      addButton(label = $itemsList[protoIndex].weight & " kg",
        tooltip = "Show available options of item.", data = index,
        code = showItemInfo, dialog = dialog)
    except:
      dialog = setError(message = "Can't show weight")
      return
    addButton(label = $playerShip.cargo[i].amount,
      tooltip = "Show available options of item.", data = index,
      code = showItemInfo, dialog = dialog)
    addButton(label = $baseAmount, tooltip = "Show available options of item.",
      data = index, code = showItemInfo, dialog = dialog)
    row.inc
    if row == gameSettings.listsLimit + 1:
      break

proc showTrade*(state: var GameState; dialog: var GameDialog) {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Show the trading UI
  ##
  ## * state - the current game's state
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameters state and dialog. The latter is modified if
  ## any error happened.
  if showHeader(dialog = dialog, close = CloseDestination.map, state = state,
    options = true):
    return
  if updateData:
    refreshItemsList(dialog = dialog)
    baseCargo = skyBases[baseIndex].cargo
  # Show advanced options if needed
  if showOptions:
    setLayoutRowDynamic(height = 30, cols = 3, ratio = [0.1.cfloat, 0.3, 0.6])
    label(str = "Type:")
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Show only items of the selected type")
    let newType: Natural = comboList(items = typesList,
        selected = typeIndex, itemHeight = 25, x = 200, y = 150)
    if newType != typeIndex:
      typeIndex = newType
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Enter a name of an item which you looking for")
    editString(text = nameSearch, maxLen = 64)
  # Show information about money owned by the player and the base
  setLayoutRowStatic(height = 30, cols = moneyWidth.len, ratio = moneyWidth)
  for index, text in moneyText:
    if index mod 2 == 0:
      label(str = text)
    else:
      colorLabel(str = text, color = theme.colors[goldenColor])
  # Show information about free cargo space in the player's ship
  setLayoutRowStatic(height = 30, cols = cargoWidth.len, ratio = cargoWidth)
  for index, text in cargoText:
    if index mod 2 == 0:
      label(str = text)
    else:
      colorLabel(str = text, color = theme.colors[goldenColor])
  # Show the list of items for trade
  let tableHeight: float = windowHeight - 140 - (if showOptions: 45 else: 0) -
      gameSettings.messagesPosition.float
  setLayoutRowDynamic(height = tableHeight, cols = 1)
  group(title = "TradeGroup", flags = {windowNoFlags}):
    if dialog != none:
      windowDisable()
    addHeader(headers = headers, ratio = ratio, tooltip = "items",
      code = sortTrades, dialog = dialog)
    var
      currentItemIndex: Natural = 0
      indexesList: seq[Natural] = @[]
      currentRow: Positive = 1
    let startRow: Positive = ((currentPage - 1) * gameSettings.listsLimit) + 1
    saveButtonStyle()
    setButtonStyle(field = borderColor, a = 0)
    try:
      setButtonStyle(field = normal, color = theme.colors[tableRowColor])
      setButtonStyle(field = textNormal, color = theme.colors[tableTextColor])
    except:
      dialog = setError(message = "Can't set table color")
      return
    setButtonStyle(field = rounding, value = 0)
    setButtonStyle(field = border, value = 0)
    var row: Positive = 1
    # Show the list of items in the player's ship's cargo
    showPlayerItems(dialog = dialog, indexesList = indexesList,
        currentRow = currentRow, row = row, startRow = startRow)
    currentItemIndex = playerShip.cargo.len + 1
    # Show the list of items in the base's cargo
    for i in playerShip.cargo.len + 1..itemsIndexes.high:
      if row == gameSettings.listsLimit + 1:
        break
      try:
        if itemsIndexes[i] in indexesList or not isBuyable(baseType = baseType,
            itemIndex = baseCargo[itemsIndexes[i]].protoIndex,
            baseIndex = baseIndex) or baseCargo[itemsIndexes[i]].amount == 0:
          continue
      except:
        dialog = setError(message = "Can't check if item is buyable2.")
        return
      let
        protoIndex: Natural = baseCargo[itemsIndexes[i]].protoIndex
        itemType: string = try:
            if itemsList[protoIndex].showType.len == 0:
              itemsList[protoIndex].itemType
            else:
              itemsList[protoIndex].showType
          except:
            dialog = setError(message = "Can't get item type4.")
            return
      if typeIndex > 0 and itemType != typesList[typeIndex]:
        continue
      let itemName: string = try:
            itemsList[protoIndex].name
          except:
            dialog = setError(message = "Can't get item name2.")
            return
      if nameSearch.len > 0 and itemName.toLowerAscii.find(
          sub = nameSearch.toLowerAscii) == -1:
        continue
      if currentRow < startRow:
        currentRow.inc
        continue
      var price: Natural = if baseIndex > 0:
          skyBases[baseIndex].cargo[itemsIndexes[i]].price
        else:
          traderCargo[itemsIndexes[i]].price
      if eventIndex > -1:
        if eventsList[eventIndex].eType == doublePrice and eventsList[
            eventIndex].itemIndex == protoIndex:
          price *= 2
      let baseAmount: Natural = (if baseIndex == 0: traderCargo[itemsIndexes[
          i]].amount else: skyBases[baseIndex].cargo[itemsIndexes[i]].amount)
      addButton(label = itemName, tooltip = "Show available options of item.",
        data = i, code = showItemInfo, dialog = dialog)
      addButton(label = itemType, tooltip = "Show available options of item.",
        data = i, code = showItemInfo, dialog = dialog)
      var durability: int = (if baseIndex == 0: traderCargo[itemsIndexes[
          i]].durability else: skyBases[baseIndex].cargo[itemsIndexes[i]].durability)
      addProgressBar(tooltip = (if baseCargo[itemsIndexes[i]].durability < 100:
        getItemDamage(itemDurability = baseCargo[itemsIndexes[i]].durability)
        else: "Unused"), value = durability,
        maxValue = defaultItemDurability, data = i, code = showItemInfo,
        dialog = dialog)
      addButton(label = ($skyBases[baseIndex].cargo[itemsIndexes[
          i]].quality).capitalizeAscii,
          tooltip = "Show available options of item.", data = i,
          code = showItemInfo, dialog = dialog)
      addButton(label = $price, tooltip = "Show available options of item.",
        data = i, code = showItemInfo, dialog = dialog)
      setButtonStyle(field = textNormal, color = theme.colors[redColor])
      addButton(label = $(-price), tooltip = "Show available options of item.",
        data = i, code = showItemInfo, dialog = dialog)
      setButtonStyle(field = textNormal, color = theme.colors[tableTextColor])
      try:
        addButton(label = $itemsList[protoIndex].weight & " kg",
          tooltip = "Show available options of item.", data = i,
          code = showItemInfo, dialog = dialog)
      except:
        dialog = setError(message = "Can't show weight")
        return
      addButton(label = "0", tooltip = "Show available options of item.",
        data = i, code = showItemInfo, dialog = dialog)
      addButton(label = $baseAmount, tooltip = "Show available options of item.",
        data = i, code = showItemInfo, dialog = dialog)
      row.inc
      if row == gameSettings.listsLimit + 1:
        break
    restoreButtonStyle()
    addPagination(page = currentPage, row = row)
  showLastMessages(theme = theme, dialog = dialog, height = windowHeight -
      tableHeight - 20)
