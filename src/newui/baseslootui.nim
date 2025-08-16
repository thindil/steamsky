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

import std/[algorithm, strutils, tables]
import contracts, nuklear/nuklear_sdl_renderer
import ../[basescargo, basestypes, config, game, items, maps, shipscargo, types]
import coreui, dialogs, errordialog, header, messagesui, setui, table, themes

var itemIndex: int = -1

type LocalItemData = object
  name: string
  iType: string
  damage: float
  owned: Natural
  available: Natural
  id: Natural

proc sortItems(x, y: LocalItemData): int =
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
    else:
      return -1
  of nameDesc:
    if x.name > y.name:
      return 1
    else:
      return -1
  of typeAsc:
    if x.iType < y.iType:
      return 1
    else:
      return -1
  of typeDesc:
    if x.iType > y.iType:
      return 1
    else:
      return -1
  of durabilityAsc:
    if x.damage < y.damage:
      return 1
    else:
      return -1
  of durabilityDesc:
    if x.damage > y.damage:
      return 1
    else:
      return -1
  of ownedAsc:
    if x.owned < y.owned:
      return 1
    else:
      return -1
  of ownedDesc:
    if x.owned > y.owned:
      return 1
    else:
      return -1
  of availableAsc:
    if x.available < y.available:
      return 1
    else:
      return -1
  of availableDesc:
    if x.available > y.available:
      return 1
    else:
      return -1
  else:
    return -1

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
  var localItems: seq[LocalItemData]
  var indexesList: seq[Natural]
  for index, item in playerShip.cargo:
    let
      protoIndex = item.protoIndex
      baseCargoIndex = findBaseCargo(protoIndex = protoIndex,
          durability = item.durability)
    try:
      localItems.add(y = LocalItemData(name: getItemName(item = item),
          iType: (if itemsList[protoIndex].showType.len == 0: itemsList[
          protoIndex].itemType else: itemsList[protoIndex].showType),
              damage: (
          item.durability.float / defaultItemDurability.float),
          owned: item.amount, available: (if baseCargoIndex > -1: baseCargo[
          baseCargoIndex].amount else: 0), id: index))
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
    let protoIndex = item.protoIndex
    try:
      localItems.add(y = LocalItemData(name: itemsList[protoIndex].name,
          iType: (if itemsList[protoIndex].showType.len == 0: itemsList[
          protoIndex].itemType else: itemsList[protoIndex].showType),
              damage: (
          item.durability.float / defaultItemDurability.float), owned: 0,
          available: item.amount, id: index))
    except:
      dialog = setError(message = "Can't add item from the base's cargo.")
      return
  localItems.sort(cmp = sortItems)
  for item in localItems:
    itemsIndexes.add(y = item.id)

proc setGiveDialog(dialog: var GameDialog) {.raises: [], tags: [RootEffect],
    contractual.} =
  ## Set the dialog for giving items
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
        durability = playerShip.cargo[itemIndex].durability)
  else:
    baseCargoIndex = itemIndex
  dialog = setManipulate(action = giveAction, iIndex = baseCargoIndex)

proc setDropDialog(dialog: var GameDialog) {.raises: [], tags: [RootEffect],
    contractual.} =
  ## Set the dialog for dropping items
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  closePopup()
  dialog = setManipulate(action = dropAction, iIndex = itemIndex)

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
  var
    cargoIndex: int = -1
    baseCargoIndex: int = 0
  if itemIndex < 0:
    baseCargoIndex = (itemIndex + 1).abs
  else:
    cargoIndex = itemIndex - 1
  let baseIndex: ExtendedBasesRange = skyMap[playerShip.skyX][
      playerShip.skyY].baseIndex
  if cargoIndex > playerShip.cargo.high or baseCargoIndex > skyBases[
      baseIndex].cargo.high:
    return
  let
    protoIndex: int = (if cargoIndex > -1: playerShip.cargo[
      cargoIndex].protoIndex else: skyBases[baseIndex].cargo[
      baseCargoIndex].protoIndex)
    quality: ObjectQuality = (if cargoIndex > -1: playerShip.cargo[
        cargoIndex].quality else: skyBases[baseIndex].cargo[
        baseCargoIndex].quality)
  var itemInfo = ""
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
  var maxAmount: int = (if baseCargoIndex > -1: skyBases[baseIndex].cargo[
      baseCargoIndex].amount else: 0)
  let
    freeAmount: int = try:
        (if baseCargoIndex > -1: (freeCargo(amount = 0).float /
          itemsList[skyBases[baseIndex].cargo[
          baseCargoIndex].protoIndex].weight.float).Natural else: 0)
      except:
        dialog = setError(message = "Can't count free amount.")
        return
    cargoMaxAmount: Natural = (if cargoIndex > -1: playerShip.cargo[
        cargoIndex].amount.Natural else: 0)
  if maxAmount > freeAmount:
    maxAmount = freeAmount
  try:
    dialog = setInfo(text = itemInfo, title = itemsList[protoIndex].name,
        button1 = (if maxAmount == 0: emptyButtonSettings else: ButtonSettings(
        tooltip: "Take item from the base", code: setGiveDialog,
        icon: giveIcon.ord, text: "Take", color: "")), button2 = (
        if cargoMaxAmount == 0: emptyButtonSettings else: ButtonSettings(
        tooltip: "Drop item from the ship cargo", code: setDropDialog,
        icon: dropIcon.ord, text: "Drop", color: "")))
  except:
    dialog = setError(message = "Can't show the item's info.")

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
    var
      currentItemIndex = 0
      indexesList: seq[Natural]
      currentRow = 1
    let startRow = ((currentPage - 1) * gameSettings.listsLimit) + 1
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
        break
      let
        protoIndex: Natural = playerShip.cargo[i].protoIndex
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
      if currentRow < startRow:
        currentRow.inc
        continue
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
      addButton(label = itemName, tooltip = "Show available options of item.",
        data = i, code = showItemInfo, dialog = dialog)
      addButton(label = itemType, tooltip = "Show available options of item.",
        data = i, code = showItemInfo, dialog = dialog)
      addProgressBar(tooltip = (if playerShip.cargo[i].durability < 100:
        getItemDamage(itemDurability = playerShip.cargo[i].durability)
        else: "Unused"), value = playerShip.cargo[i].durability,
        maxValue = defaultItemDurability, data = i, code = showItemInfo,
        dialog = dialog)
      addButton(label = $playerShip.cargo[i].amount,
        tooltip = "Show available options of item.", data = i,
        code = showItemInfo, dialog = dialog)
      addButton(label = $baseAmount, tooltip = "Show available options of item.",
        data = i, code = showItemInfo, dialog = dialog)
      row.inc
      if row == gameSettings.listsLimit + 1:
        break
    currentItemIndex = playerShip.cargo.len + 1
    # Show the list of items in the base's cargo
    for i in playerShip.cargo.len + 1 .. itemsIndexes.high:
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
        protoIndex = baseCargo[itemsIndexes[i]].protoIndex
        itemType = try:
            if itemsList[protoIndex].showType.len == 0:
              itemsList[protoIndex].itemType
            else:
              itemsList[protoIndex].showType
          except:
            dialog = setError(message = "Can't get item type4.")
            return
      if typeIndex > 0 and itemType != typesList[typeIndex]:
        continue
      let itemName = try:
            itemsList[protoIndex].name
          except:
            dialog = setError(message = "Can't get item name2.")
            return
      if currentRow < startRow:
        currentRow.inc
        continue
      let baseAmount = (if baseIndex == 0: traderCargo[itemsIndexes[
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
      addButton(label = "0", tooltip = "Show available options of item.",
        data = i, code = showItemInfo, dialog = dialog)
      addButton(label = $baseAmount, tooltip = "Show available options of item.",
        data = i, code = showItemInfo, dialog = dialog)
      row.inc
    restoreButtonStyle()
    addPagination(page = currentPage, row = row)
  # Show the last in-game messages
  showLastMessages(theme = theme, dialog = dialog, height = windowHeight - tableHeight)
