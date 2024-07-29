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
import ../[basescargo, config, game, items, maps, tk]
import coreui, mapsui, table

var
  lootTable: TableWidget
  itemsIndexes: seq[int]

type ItemsSortOrders = enum
  none, nameAsc, nameDesc, typeAsc, typeDesc, durabilityAsc, durabilityDesc,
    ownedAsc, ownedDesc, availableAsc, availableDesc

const defaultItemsSortOrder: ItemsSortOrders = none

var itemsSortOrder: ItemsSortOrders = defaultItemsSortOrder

proc showLootCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.exportc.} =
  var lootFrame = mainPaned & ".lootframe"
  let lootCanvas = lootFrame & ".canvas"
  var label = lootCanvas & ".loot.options.typelabel"
  if tclEval2(script = "winfo exists " & label) == "0":
    tclEvalFile(fileName = dataDirectory & "ui" & DirSep & "loot.tcl")
    tclEval(script = "bind " & lootFrame & " <Configure> {ResizeCanvas %W.canvas %w %h}")
    lootFrame = lootCanvas & ".loot"
    lootTable = createTable(parent = lootFrame, headers = @["Name", "Type",
        "Durability", "Owned", "Available"],
        scrollbar = ".gameframe.paned.lootframe.scrolly",
        command = "SortLootItems",
        tooltipText = "Press mouse button to sort the items.")
  elif tclEval2(script = "winfo ismapped " & label) == "1" and argc == 1:
    tclEval(script = "grid remove " & closeButton)
    showSkyMap(clear = true)
  lootFrame = lootCanvas & ".loot"
  var comboBox = lootFrame & ".options.type"
  let
    baseIndex = skyMap[playerShip.skyX][playerShip.skyY].baseIndex
    currentBaseCargo = skyBases[baseIndex].cargo
  if itemsSortOrder == defaultItemsSortOrder:
    itemsIndexes = @[]
    for index, _ in playerShip.cargo:
      itemsIndexes.add(y = index)
    itemsIndexes.add(y = -1)
    for index, _ in currentBaseCargo:
      itemsIndexes.add(y = index)
  clearTable(table = lootTable)
  var itemsTypes = "All"
  for index in itemsIndexes:
    if index == -1:
      break
    let
      protoIndex = playerShip.cargo[index].protoIndex
      itemType = (if itemsList[protoIndex].showType.len == 0: itemsList[
          protoIndex].itemType else: itemsList[protoIndex].showType)
    if not itemsTypes.contains(sub = "{" & itemType & "}"):
      itemsTypes.add(y = " {" & itemType & "}")
  var
    currentItemIndex = 1
    indexesList: seq[Natural]
    currentRow = 1
  let
    page = (if argc == 3: ($argv[2]).parseInt else: 1)
    startRow = ((page - 1) * gameSettings.listsLimit) + 1
  const tableTooltip = "Show item's description and actions"
  for index in itemsIndexes:
    currentItemIndex.inc
    if index == -1:
      break
    let
      protoIndex = playerShip.cargo[index].protoIndex
      baseCargoIndex = findBaseCargo(protoIndex = protoIndex,
          durability = playerShip.cargo[index].durability)
    if baseCargoIndex > -1:
      indexesList.add(y = baseCargoIndex)
    let itemType = (if itemsList[protoIndex].showType.len == 0: itemsList[
        protoIndex].itemType else: itemsList[protoIndex].showType)
    if argc > 1 and argv[1] != "All" and itemType != $argv[1]:
      continue
    if currentRow < startRow:
      currentRow.inc
      continue
    let itemName = getItemName(item = playerShip.cargo[index],
        damageInfo = false, toLower = false)
    addButton(table = lootTable, text = itemName, tooltip = tableTooltip,
        command = "ShowLootItemInfo " & $(index + 1), column = 1)
    addButton(table = lootTable, text = itemType, tooltip = tableTooltip,
        command = "ShowLootItemInfo " & $(index + 1), column = 2)
    let itemDurability = (if playerShip.cargo[index].durability <
        100: getItemDamage(itemDurability = playerShip.cargo[
        index].durability) else: "Unused")
    addProgressbar(table = lootTable, value = playerShip.cargo[
        index].durability, maxValue = defaultItemDurability,
        tooltip = itemDurability, command = "ShowLootItemInfo " & $(index + 1), column = 3)
    addButton(table = lootTable, text = $playerShip.cargo[index].amount,
        tooltip = tableTooltip, command = "ShowLootItemInfo " & $(index + 1), column = 4)
    let baseAmount = if baseCargoIndex > 0: skyBases[baseIndex].cargo[
        baseCargoIndex].amount else: 0
    addButton(table = lootTable, text = $baseAmount, tooltip = tableTooltip,
        command = "ShowLootItemInfo " & $(index + 1), column = 5, newRow = true)
    if lootTable.row == gameSettings.listsLimit + 1:
      break
  currentItemIndex = playerShip.cargo.len + 2
  for index in currentItemIndex .. itemsIndexes.high:
    let
      protoIndex = currentBaseCargo[itemsIndexes[index]].protoIndex
      itemType = (if itemsList[protoIndex].showType.len == 0: itemsList[
          protoIndex].itemType else: itemsList[protoIndex].showType)
    if not itemsTypes.contains(sub = "{" & itemType & "}"):
      itemsTypes.add(y = " {" & itemType & "}")
  for index in currentItemIndex .. itemsIndexes.high:
    if lootTable.row == gameSettings.listsLimit + 1:
      break
    if indexesList.contains(item = itemsIndexes[index]):
      continue
    let
      protoIndex = currentBaseCargo[itemsIndexes[index]].protoIndex
      itemType = (if itemsList[protoIndex].showType.len == 0: itemsList[
          protoIndex].itemType else: itemsList[protoIndex].showType)
    if argc == 2 and argv[1] != "All" and itemType != $argv[1]:
      continue
    if currentRow < startRow:
      currentRow.inc
      continue
    let itemName = itemsList[protoIndex].name
    addButton(table = lootTable, text = itemName, tooltip = tableTooltip,
        command = "ShowLootItemInfo -" & $(index + 1), column = 1)
    addButton(table = lootTable, text = itemType, tooltip = tableTooltip,
        command = "ShowLootItemInfo -" & $(index + 1), column = 2)
    let itemDurability = (if currentBaseCargo[itemsIndexes[index]].durability <
        100: getItemDamage(itemDurability = currentBaseCargo[itemsIndexes[
        index]].durability) else: "Unused")
    addProgressbar(table = lootTable, value = currentBaseCargo[
        itemsIndexes[index]].durability, maxValue = defaultItemDurability,
        tooltip = itemDurability, command = "ShowLootItemInfo -" & $(index + 1), column = 3)
  return tclOk

proc addCommands*() {.sideEffect, raises: [], tags: [].} =
  ## Adds Tcl commands related to the trades UI
  try:
    discard
#    addCommand("ShowRecruit", showRecruitCommand)
  except:
    showError(message = "Can't add a Tcl command.")
