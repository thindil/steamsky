# Copyright 2024 Bartek thindil Jasicki
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

import std/[algorithm, os, strutils, tables]
import ../[basestypes, basescargo, config, crewinventory, events, game, items,
    maps, shipscargo, tk, types]
import coreui, mapsui, table, utilsui2

type ItemsSortOrders = enum
  nameAsc, nameDesc, typeAsc, typeDesc, durabilityAsc, durabilityDesc, priceAsc,
    priceDesc, profitAsc, profitDesc, weightAsc, weightDesc, ownedAsc,
    ownedDesc, availableAsc, availableDesc, none

const defaultItemsSortOrder: ItemsSortOrders = none

var
  tradeTable: TableWidget
  itemsSortOrder: ItemsSortOrders = defaultItemsSortOrder
  itemsIndexes: seq[int]

proc showTradeCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [
    RootEffect], exportc.} =
  ## Show information about trading
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ShowTrade ?itemtype? ?searchstring?
  ## Itemtype is type of items to show, searchstring is string which is
  ## looking for in items names
  var tradeFrame = mainPaned & ".tradeframe"
  let
    tradeCanvas = tradeFrame & ".canvas"
    baseIndex = skyMap[playerShip.skyX][playerShip.skyY].baseIndex
    eventIndex = skyMap[playerShip.skyX][playerShip.skyY].eventIndex
  var label = tradeCanvas & ".trade.options.typelabel"
  if tclEval2(script = "winfo exists " & label) == "0":
    tclEvalFile(fileName = dataDirectory & "ui" & DirSep & "trade.tcl")
    tclEval(script = "bind " & tradeFrame & " <Configure> {ResizeCanvas %W.canvas %w %h}")
    tradeFrame = tradeCanvas & ".trade"
    tradeTable = createTable(parent = tradeFrame, headers = @["Name", "Type",
        "Durability", "Price", "Profit", "Weight", "Owned", "Available"],
        scrollbar = mainPaned & ".tradeframe.scrolly",
        command = "SortTradeItems",
        tooltipTExt = "Press mouse button to sort the items.")
  elif tclEval2(script = "winfo ismapped " & label) == "1" and argc == 1:
    itemsSortOrder = defaultItemsSortOrder
    tclEval(script = "grid remove " & closeButton)
    tclEval(script = closeButton & " configure -command ShowSkyMap")
    if baseIndex == 0 and eventIndex > -1:
      deleteEvent(eventIndex = eventIndex)
    showSkyMap(clear = true)
    return tclOk
  tclSetVar(varName = "gamestate", newValue = "trade")
  let searchEntry = tradeCanvas & ".trade.options.search"
  if argc < 3:
    tclEval(script = searchEntry & " delete 0 end")
  tclEval(script = closeButton & " configure -command {ShowSkyMap ShowTrade}")
  tradeFrame = tradeCanvas & ".trade"
  let comboBox = tradeFrame & ".options.type"
  clearTable(table = tradeTable)
  var
    baseType: string
    baseCargo: seq[BaseCargo]
  if baseIndex > 0:
    baseType = skyBases[baseIndex].baseType
    baseCargo = skyBases[baseIndex].cargo
  else:
    baseType = "0"
    baseCargo = traderCargo
  if itemsSortOrder == defaultItemsSortOrder:
    itemsIndexes = @[]
    for index in playerShip.cargo.low .. playerShip.cargo.high:
      itemsIndexes.add(y = index)
    itemsIndexes.add(y = -1)
    for index in baseCargo.low .. baseCargo.high:
      itemsIndexes.add(y = index)
  var itemsTypes: string = "All"
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
          return showError(message = "Can't get item type.")
    try:
      if itemsTypes.find(sub = "{" & itemType & "}") == -1 and itemsList[
          protoIndex].price > 0:
        itemsTypes.add(y = " {" & itemType & "}")
    except:
      return showError(message = "Can't add item type.")
  var
    currentItemIndex = 0
    indexesList: seq[Natural]
    currentRow = 1
  let
    page = try:
        (if argc == 4: ($argv[3]).parseInt else: 1)
      except:
        return showError(message = "Can't get page.")
    startRow = ((page - 1) * gameSettings.listsLimit) + 1
  for i in itemsIndexes:
    currentItemIndex.inc
    if i == -1:
      break
    try:
      if getPrice(baseType = baseType, itemIndex = playerShip.cargo[
          i].protoIndex) == 0:
        continue
    except:
      return showError(message = "Can't get price.")
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
          return showError(message = "Can't get item type2.")
    if argc > 1 and argv[1] != "All" and itemType != $argv[1]:
      continue
    let itemName = getItemName(item = playerShip.cargo[i], damageInfo = false,
        toLower = false)
    if argc == 3 and itemName.toLowerAscii.find(sub = ($argv[
        2]).toLowerAscii) == -1:
      continue
    if currentRow < startRow:
      currentRow.inc
      continue
    var price = 0
    if baseCargoIndex == -1:
      try:
        price = getPrice(baseType = baseType, itemIndex = protoIndex)
      except:
        return showError(message = "Can't get price2.")
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
        return showError(message = "Can't get base amount.")
    else:
      if baseCargoIndex > -1:
        baseAmount = baseCargo[baseCargoIndex].amount
    addButton(table = tradeTable, text = itemName,
        tooltip = "Show available options for item",
        command = "ShowTradeItemInfo " & $(i + 1), column = 1)
    addButton(table = tradeTable, text = itemType,
        tooltip = "Show available options for item",
        command = "ShowTradeItemInfo " & $(i + 1), column = 2)
    let itemDurability = (if playerShip.cargo[i].durability <
        100: getItemDamage(itemDurability = playerShip.cargo[
        i].durability) else: "Unused")
    addProgressbar(table = tradeTable, value = playerShip.cargo[i].durability,
        maxValue = defaultItemDurability, tooltip = itemDurability,
        command = "ShowTradeItemInfo " & $(i + 1), column = 3)
    addButton(table = tradeTable, text = $price,
        tooltip = "Show available options for item",
        command = "ShowTradeItemInfo " & $(i + 1), column = 4)
    addButton(table = tradeTable, text = $profit,
        tooltip = "Show available options for item",
        command = "ShowTradeItemInfo " & $(i + 1), column = 5, color = (
        if profit >
        0: tclGetVar(varName = "ttk::theme::" & gameSettings.interfaceTheme &
        "::colors(-green)") elif profit < 0: tclGetVar(
        varName = "ttk::theme::" & gameSettings.interfaceTheme &
        "::colors(-green)") else: ""))
    try:
      addButton(table = tradeTable, text = $itemsList[protoIndex].weight &
          " kg", tooltip = "Show available options for item",
          command = "ShowTradeItemInfo " & $(i + 1), column = 6)
    except:
      return showError(message = "Can't show weight")
    addButton(table = tradeTable, text = $playerShip.cargo[i].amount,
        tooltip = "Show available options for item",
        command = "ShowTradeItemInfo " & $(i + 1), column = 7)
    addButton(table = tradeTable, text = $baseAmount,
        tooltip = "Show available options for item",
        command = "ShowTradeItemInfo " & $(i + 1), column = 8, newRow = true)
    if tradeTable.row == gameSettings.listsLimit + 1:
      break
  currentItemIndex = playerShip.cargo.len + 1
  for i in currentItemIndex .. itemsIndexes.high:
    let
      protoIndex = baseCargo[itemsIndexes[i]].protoIndex
      itemType = try:
          if itemsList[protoIndex].showType.len == 0:
            itemsList[protoIndex].itemType
          else:
            itemsList[protoIndex].showType
        except:
          return showError(message = "Can't get item type3.")
    try:
      if isBuyable(baseType = baseType, itemIndex = protoIndex,
          baseIndex = baseIndex) and baseCargo[itemsIndexes[i]].amount > 0 and
          itemsTypes.find(sub = "{" & itemType & "}") == -1:
        itemsTypes.add(y = " {" & itemType & "}")
    except:
      return showError(message = "Can't check if item is buyable.")
  for i in currentItemIndex .. itemsIndexes.high:
    if tradeTable.row == gameSettings.listsLimit + 1:
      break
    try:
      if itemsIndexes[i] in indexesList or not isBuyable(baseType = baseType,
          itemIndex = baseCargo[itemsIndexes[i]].protoIndex,
          baseIndex = baseIndex) or baseCargo[itemsIndexes[i]].amount == 0:
        continue
    except:
      return showError(message = "Can't check if item is buyable2.")
    let
      protoIndex = baseCargo[itemsIndexes[i]].protoIndex
      itemType = try:
          if itemsList[protoIndex].showType.len == 0:
            itemsList[protoIndex].itemType
          else:
            itemsList[protoIndex].showType
        except:
          return showError(message = "Can't get item type4.")
    if argc > 1 and argv[1] != "All" and itemType != $argv[1]:
      continue
    let itemName = try:
          itemsList[protoIndex].name
        except:
          return showError(message = "Can't get item name2.")
    if argc == 3 and itemName.toLowerAscii.find(sub = ($argv[
        2]).toLowerAscii) == -1:
      continue
    if currentRow < startRow:
      currentRow.inc
      continue
    var price = if baseIndex > 0:
        skyBases[baseIndex].cargo[itemsIndexes[i]].price
      else:
        traderCargo[itemsIndexes[i]].price
    if eventIndex > -1:
      if eventsList[eventIndex].eType == doublePrice and eventsList[
          eventIndex].itemIndex == protoIndex:
        price = price * 2
    let baseAmount = (if baseIndex == 0: traderCargo[itemsIndexes[
        i]].amount else: skyBases[baseIndex].cargo[itemsIndexes[i]].amount)
    addButton(table = tradeTable, text = itemName,
        tooltip = "Show available options for item",
        command = "ShowTradeItemInfo -" & $(itemsIndexes[i] + 1), column = 1)
    addButton(table = tradeTable, text = itemType,
        tooltip = "Show available options for item",
        command = "ShowTradeItemInfo -" & $(itemsIndexes[i] + 1), column = 2)
    let itemDurability = (if baseCargo[itemsIndexes[i]].durability <
        100: getItemDamage(itemDurability = baseCargo[itemsIndexes[
        i]].durability) else: "Unused")
    addProgressbar(table = tradeTable, value = baseCargo[itemsIndexes[
        i]].durability, maxValue = defaultItemDurability,
        tooltip = itemDurability, command = "ShowTradeItemInfo -" &
        $(itemsIndexes[i] + 1), column = 3)
    addButton(table = tradeTable, text = $price,
        tooltip = "Show available options for item",
        command = "ShowTradeItemInfo -" & $(itemsIndexes[i] + 1), column = 4)
    addButton(table = tradeTable, text = $(-price),
        tooltip = "Show available options for item",
        command = "ShowTradeItemInfo -" & $(itemsIndexes[i] + 1), column = 5,
        newRow = false, color = tclGetVar(varName = "ttk::theme::" &
        gameSettings.interfaceTheme & "::colors(-red)"))
    try:
      addButton(table = tradeTable, text = $itemsList[protoIndex].weight &
          " kg", tooltip = "Show available options for item",
          command = "ShowTradeItemInfo -" & $(itemsIndexes[i] + 1), column = 6)
    except:
      return showError(message = "Can't show item weight2.")
    addButton(table = tradeTable, text = "0",
        tooltip = "Show available options for item",
        command = "ShowTradeItemInfo -" & $(itemsIndexes[i] + 1), column = 7)
    addButton(table = tradeTable, text = $baseAmount,
        tooltip = "Show available options for item",
        command = "ShowTradeItemInfo -" & $(itemsIndexes[i] + 1), column = 8, newRow = true)
  let arguments = (if argc > 2: "{" & $argv[1] & "} {" & $argv[2] &
      "}" elif argc == 2: $argv[1] & " {}" else: "All {}")
  if page > 1:
    if tradeTable.row < gameSettings.listsLimit + 1:
      addPagination(table = tradeTable, previousCommand = "ShowTrade " &
          arguments & " " & $(page - 1), nextCommand = "")
    else:
      addPagination(table = tradeTable, previousCommand = "ShowTrade " &
          arguments & " " & $(page - 1), nextCommand = "ShowTrade " &
          arguments & " " & $(page + 1))
  elif tradeTable.row == gameSettings.listsLimit + 1:
    addPagination(table = tradeTable, previousCommand = "",
        nextCommand = "ShowTrade " & arguments & " " & $(page + 1))
  updateTable(table = tradeTable, grabFocus = tclEval2(script = "focus") == searchEntry)
  tclEval(script = "update")
  tclEval(script = comboBox & " configure -values [list " & itemsTypes & "]")
  if argc == 1:
    tclEval(script = comboBox & " current 0")
  let moneyIndex2 = findItem(inventory = playerShip.cargo,
      protoIndex = moneyIndex)
  var tradeInfo = ""
  if moneyIndex2 > -1:
    tradeInfo = "You have " & $playerShip.cargo[moneyIndex2].amount & " " &
        moneyName & "."
  else:
    tradeInfo = "You don't have any " & moneyName & " to buy anything."
  var freeSpace = try:
      freeCargo(amount = 0)
    except:
      return showError(message = "Can't get free space.")
  if freeSpace < 0:
    freeSpace = 0
  tradeInfo.add(y = "\nFree cargo space: " & $freeSpace & " kg.")
  label = tradeFrame & ".options.playerinfo"
  tclEval(script = label & " configure -text {" & tradeInfo & "}")
  tradeInfo = ""
  if baseIndex > 0:
    if skyBases[baseIndex].cargo[0].amount == 0:
      tradeInfo.add(y = "Base doesn't have any " & moneyName & " to buy anything.")
    else:
      tradeInfo.add(y = "Base has " & $skyBases[baseIndex].cargo[0].amount &
          " " & moneyName & ".")
  else:
    if traderCargo[0].amount == 0:
      tradeInfo.add(y = "Ship doesn't have any " & moneyName & " to buy anything.")
    else:
      tradeInfo.add(y = "Ship has " & $traderCargo[0].amount & " " & moneyName & ".")
  label = tradeFrame & ".options.baseinfo"
  tclEval(script = label & " configure -text {" & tradeInfo & "}")
  tclEval(script = "grid " & closeButton & " -row 0 -column 1")
  tclEval(script = tradeCanvas & " configure -height [expr " & tclEval2(
      script = mainPaned & "sashpos 0") & " - 20] -width " & tclEval2(
      script = mainPaned & " cget -width"))
  tclEval(script = "update")
  tclEval(script = tradeCanvas & " create window 0 0 -anchor nw -window " & tradeFrame)
  tclEval(script = "update")
  tclEval(script = tradeCanvas & " configure -scrollregion [list " & tclEval2(
      script = tradeCanvas & " bbox all") & "]")
  tclEval(script = tradeCanvas & " xview moveto 0.0")
  tclEval(script = tradeCanvas & " yview moveto 0.0")
  showScreen(newScreenName = "tradeframe")
  tclSetResult(value = "1")
  return tclOk

proc sortTradeItemsCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [
    RootEffect], exportc.} =
  ## Sort the trading list
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## SortTradeItems x
  ## X is X axis coordinate where the player clicked the mouse button
  let
    xPos = try:
        ($argv[1]).parseInt
      except:
        return showError(message = "Can't get X position on table.")
    column = (if xPos > -1: getColumnNumber(table = tradeTable,
        xPosition = xPos) else: itemsSortOrder.ord + 1)
  case column
  of 1:
    if itemsSortOrder == nameAsc:
      itemsSortOrder = nameDesc
    else:
      itemsSortOrder = nameAsc
  of 2:
    if itemsSortOrder == typeAsc:
      itemsSortOrder = typeDesc
    else:
      itemsSortOrder = typeAsc
  of 3:
    if itemsSortOrder == durabilityAsc:
      itemsSortOrder = durabilityDesc
    else:
      itemsSortOrder = durabilityAsc
  of 4:
    if itemsSortOrder == priceAsc:
      itemsSortOrder = priceDesc
    else:
      itemsSortOrder = priceAsc
  of 5:
    if itemsSortOrder == profitAsc:
      itemsSortOrder = profitDesc
    else:
      itemsSortOrder = profitAsc
  of 6:
    if itemsSortOrder == weightAsc:
      itemsSortOrder = weightDesc
    else:
      itemsSortOrder = weightAsc
  of 7:
    if itemsSortOrder == ownedAsc:
      itemsSortOrder = ownedDesc
    else:
      itemsSortOrder = ownedAsc
  of 8:
    if itemsSortOrder == availableAsc:
      itemsSortOrder = availableDesc
    else:
      itemsSortOrder = availableAsc
  else:
    discard
  if itemsSortOrder == defaultItemsSortOrder:
    return tclOk
  let baseIndex = skyMap[playerShip.skyX][playerShip.skyY].baseIndex
  var
    baseCargo: seq[BaseCargo]
    baseType: string
  if baseIndex > 0:
    baseCargo = skyBases[baseIndex].cargo
    baseType = skyBases[baseIndex].baseType
  else:
    baseCargo = traderCargo
    baseType = "0"
  var indexesList: seq[Natural]
  let eventIndex = skyMap[playerShip.skyX][playerShip.skyY].eventIndex
  type LocalItemData = object
    name: string
    iType: string
    damage: float
    price: Natural
    profit: int
    weight: Positive = 1
    owned: Natural
    available: Natural
    id: Natural
  var localItems: seq[LocalItemData]
  for index, item in playerShip.cargo:
    let
      protoIndex = item.protoIndex
      baseCargoIndex = findBaseCargo(protoIndex = protoIndex,
          durability = item.durability)
    var price: int
    if baseCargoIndex > -1:
      indexesList.add(y = baseCargoIndex)
      price = baseCargo[baseCargoIndex].price
    else:
      price = try:
          getPrice(baseType = baseType, itemIndex = protoIndex)
        except:
          return showError(message = "Can't get price.")
    if eventIndex > -1:
      if eventsList[eventIndex].eType == doublePrice and eventsList[
          eventIndex].itemIndex == protoIndex:
        price = price * 2
    try:
      localItems.add(y = LocalItemData(name: getItemName(item = item), iType: (
          if itemsList[protoIndex].showType.len == 0: itemsList[
          protoIndex].itemType else: itemsList[protoIndex].showType), damage: (
          item.durability.float / defaultItemDurability.float), price: price,
          profit: price - item.price, weight: itemsList[protoIndex].weight,
          owned: item.amount, available: (if baseCargoIndex > -1: baseCargo[
          baseCargoIndex].amount else: 0), id: index))
    except:
      return showError(message = "Can't add item from the player's ship's cargo.")

  proc sortItems(x, y: LocalItemData): int =
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
    of priceAsc:
      if x.price < y.price:
        return 1
      else:
        return -1
    of priceDesc:
      if x.price > y.price:
        return 1
      else:
        return -1
    of profitAsc:
      if x.profit < y.profit:
        return 1
      else:
        return -1
    of profitDesc:
      if x.profit > y.profit:
        return 1
      else:
        return -1
    of weightAsc:
      if x.weight < y.weight:
        return 1
      else:
        return -1
    of weightDesc:
      if x.weight > y.weight:
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
    of none:
      return -1

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
    var price = item.price
    if eventIndex > -1:
      if eventsList[eventIndex].eType == doublePrice and eventsList[
          eventIndex].itemIndex == protoIndex:
        price = price * 2
    try:
      localItems.add(y = LocalItemData(name: itemsList[protoIndex].name,
          iType: (if itemsList[protoIndex].showType.len == 0: itemsList[
          protoIndex].itemType else: itemsList[protoIndex].showType), damage: (
          item.durability.float / defaultItemDurability.float), price: price,
          profit: -price, weight: itemsList[protoIndex].weight, owned: 0,
          available: item.amount, id: index))
    except:
      return showError(message = "Can't add item from the base's cargo.")
  localItems.sort(cmp = sortItems)
  for item in localItems:
    itemsIndexes.add(y = item.id)
  let typeBox = mainPaned & ".tradeframe.canvas.trade.options.type"
  return showTradeCommand(clientData = clientData, interp = interp, argc = 2,
      argv = @["ShowTrade", tclEval2(script = typeBox &
      " get")].allocCStringArray)

var itemIndex = -1

proc tradeItemCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.exportc.} =
  var baseCargoIndex, cargoIndex: int = -1
  if itemIndex < 0:
    baseCargoIndex = itemIndex.abs
  else:
    cargoIndex = itemIndex
  var protoIndex = 0
  let baseIndex = skyMap[playerShip.skyX][playerShip.skyY].baseIndex
  if cargoIndex > -1:
    protoIndex = playerShip.cargo[cargoIndex].protoIndex
    if baseCargoIndex == -1:
      baseCargoIndex = findBaseCargo(protoIndex = protoIndex)
  else:
    protoIndex = (if baseIndex == 0: traderCargo[
        baseCargoIndex].protoIndex else: skyBases[baseIndex].cargo[
        baseCargoIndex].protoIndex)
  let trader = (if baseIndex > 0: "base" else: "ship")
  return tclOk

proc addCommands*() {.sideEffect, raises: [], tags: [].} =
  ## Adds Tcl commands related to the trades UI
  try:
    discard
#    addCommand("ShowTrade", showTradeCommand)
#    addCommand("SortTradeItems", sortTradeItemsCommand)
#    addCommand("TradeItem", tradeItemCommand)
  except:
    showError(message = "Can't add a Tcl command.")

# Temporary code for interfacing with Ada

proc getTradeItemIndex(iIndex: cint) {.exportc.} =
  itemIndex = iIndex
