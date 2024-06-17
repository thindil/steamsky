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

import std/[os, strutils, tables]
import ../[basestypes, basescargo, config, events, game, items, maps, tk, types]
import coreui, mapsui, table

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
    argv: cstringArray): TclResults {.exportc.} =
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
      itemType = if itemsList[protoIndex].showType.len == 0:
          itemsList[protoIndex].itemType
        else:
          itemsList[protoIndex].showType
    if itemsTypes.find(sub = "{" & itemType & "}") == -1 and itemsList[
        protoIndex].price > 0:
      itemsTypes.add(y = " {" & itemType & "}")
  var
    currentItemIndex = 0
    indexesList: seq[Natural]
    currentRow = 1
  let
    page = (if argc == 4: ($argv[3]).parseInt else: 1)
    startRow = ((page - 1) * gameSettings.listsLimit) + 1
  for i in itemsIndexes:
    currentItemIndex.inc
    if i == -1:
      break
    if getPrice(baseType = baseType, itemIndex = playerShip.cargo[
        i].protoIndex) == 0:
      continue
    let
      protoIndex = playerShip.cargo[i].protoIndex
      baseCargoIndex = findBaseCargo(protoIndex = protoIndex,
          durability = playerShip.cargo[i].durability)
    if baseCargoIndex > -1:
      indexesList.add(y = baseCargoIndex)
    let itemType = if itemsList[protoIndex].showType.len == 0:
          itemsList[protoIndex].itemType
        else:
          itemsList[protoIndex].showType
    if argc > 1 and argv[1] != "All" and itemType != $argv[1]:
      continue
    let itemName = getItemName(item = playerShip.cargo[i], damageInfo = false,
        toLower = false)
    if argc == 3 and itemName.find(sub = $argv[2]) == -1:
      continue
    if currentRow < startRow:
      currentRow.inc
      continue
    var price = 0
    if baseCargoIndex == -1:
      price = getPrice(baseType = baseType, itemIndex = protoIndex)
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
      if baseCargoIndex > -1 and isBuyable(baseType = baseType,
          itemIndex = protoIndex):
        baseAmount = baseCargo[baseCargoIndex].amount
    else:
      if baseCargoIndex > -1:
        baseAmount = baseCargo[baseCargoIndex].amount
    addButton(table = tradeTable, text = itemName,
        tooltip = "Show available options for item",
        command = "ShowTradeItemInfo " & $i, column = 1)
    addButton(table = tradeTable, text = itemType,
        tooltip = "Show available options for item",
        command = "ShowTradeItemInfo " & $i, column = 2)
    let itemDurability = (if playerShip.cargo[i].durability <
        100: getItemDamage(itemDurability = playerShip.cargo[
        i].durability) else: "Unused")
    addProgressbar(table = tradeTable, value = playerShip.cargo[i].durability,
        maxValue = defaultItemDurability, tooltip = itemDurability,
        command = "ShowTradeItemInfo " & $i, column = 3)
    addButton(table = tradeTable, text = $price,
        tooltip = "Show available options for item",
        command = "ShowTradeItemInfo " & $i, column = 4)
    addButton(table = tradeTable, text = $profit,
        tooltip = "Show available options for item",
        command = "ShowTradeItemInfo " & $i, column = 5, color = (if profit >
        0: tclGetVar(varName = "ttk::theme::" & gameSettings.interfaceTheme &
        "::colors(-green)") elif profit < 0: tclGetVar(
        varName = "ttk::theme::" & gameSettings.interfaceTheme &
        "::colors(-green)") else: ""))
    addButton(table = tradeTable, text = $itemsList[protoIndex].weight & " kg",
        tooltip = "Show available options for item",
        command = "ShowTradeItemInfo " & $i, column = 6)
    addButton(table = tradeTable, text = $playerShip.cargo[i].amount,
        tooltip = "Show available options for item",
        command = "ShowTradeItemInfo " & $i, column = 7)
    addButton(table = tradeTable, text = $baseAmount,
        tooltip = "Show available options for item",
        command = "ShowTradeItemInfo " & $i, column = 8, newRow = true)
    if tradeTable.row == gameSettings.listsLimit + 1:
      break
  currentItemIndex = playerShip.cargo.len + 1
  for i in currentItemIndex .. itemsIndexes.high:
    let
      protoIndex = baseCargo[itemsIndexes[i]].protoIndex
      itemType = if itemsList[protoIndex].showType.len == 0:
          itemsList[protoIndex].itemType
        else:
          itemsList[protoIndex].showType
    if isBuyable(baseType = baseType, itemIndex = protoIndex,
        baseIndex = baseIndex) and baseCargo[itemsIndexes[i]].amount > 0 and
        itemsTypes.find(sub = "{" & itemType & "}") == -1:
      itemsTypes.add(y = " {" & itemType & "}")
  return tclOk

proc addCommands*() {.sideEffect, raises: [], tags: [].} =
  ## Adds Tcl commands related to the trades UI
  try:
    discard
#    addCommand("ShowTrade", showTradeCommand)
  except:
    showError(message = "Can't add a Tcl command.")
