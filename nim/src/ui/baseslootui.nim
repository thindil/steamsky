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
import ../[basescargo, config, game, items, maps, shipscargo, tk]
import coreui, mapsui, table, utilsui2

var
  lootTable: TableWidget
  itemsIndexes: seq[int]

type ItemsSortOrders = enum
  none, nameAsc, nameDesc, typeAsc, typeDesc, durabilityAsc, durabilityDesc,
    ownedAsc, ownedDesc, availableAsc, availableDesc

const defaultItemsSortOrder: ItemsSortOrders = none

var itemsSortOrder: ItemsSortOrders = defaultItemsSortOrder

proc showLootCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [
    RootEffect], exportc.} =
  ## Show information about looting
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ShowLoot
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
      itemType = try:
          (if itemsList[protoIndex].showType.len == 0: itemsList[
            protoIndex].itemType else: itemsList[protoIndex].showType)
        except:
          return showError(message = "Can't get item type.")
    if not itemsTypes.contains(sub = "{" & itemType & "}"):
      itemsTypes.add(y = " {" & itemType & "}")
  var
    currentItemIndex = 1
    indexesList: seq[Natural]
    currentRow = 1
  let
    page = try:
        (if argc == 3: ($argv[2]).parseInt else: 1)
      except:
        return showError(message = "Can't get page number.")
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
    let itemType = try:
        (if itemsList[protoIndex].showType.len == 0: itemsList[
          protoIndex].itemType else: itemsList[protoIndex].showType)
      except:
        return showError(message = "Can't get item type2.")
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
      itemType = try:
          (if itemsList[protoIndex].showType.len == 0: itemsList[
            protoIndex].itemType else: itemsList[protoIndex].showType)
        except:
          return showError(message = "Can't get item type3.")
    if not itemsTypes.contains(sub = "{" & itemType & "}"):
      itemsTypes.add(y = " {" & itemType & "}")
  for index in currentItemIndex .. itemsIndexes.high:
    if lootTable.row == gameSettings.listsLimit + 1:
      break
    if indexesList.contains(item = itemsIndexes[index]):
      continue
    let
      protoIndex = currentBaseCargo[itemsIndexes[index]].protoIndex
      itemType = try:
          (if itemsList[protoIndex].showType.len == 0: itemsList[
            protoIndex].itemType else: itemsList[protoIndex].showType)
        except:
          return showError(message = "Can't get item type4.")
    if argc == 2 and argv[1] != "All" and itemType != $argv[1]:
      continue
    if currentRow < startRow:
      currentRow.inc
      continue
    let itemName = try:
        itemsList[protoIndex].name
      except:
        return showError(message = "Can't get item name.")
    addButton(table = lootTable, text = itemName, tooltip = tableTooltip,
        command = "ShowLootItemInfo -" & $(itemsIndexes[index] + 1), column = 1)
    addButton(table = lootTable, text = itemType, tooltip = tableTooltip,
        command = "ShowLootItemInfo -" & $(itemsIndexes[index] + 1), column = 2)
    let itemDurability = (if currentBaseCargo[itemsIndexes[index]].durability <
        100: getItemDamage(itemDurability = currentBaseCargo[itemsIndexes[
        index]].durability) else: "Unused")
    addProgressbar(table = lootTable, value = currentBaseCargo[itemsIndexes[
        index]].durability, maxValue = defaultItemDurability,
        tooltip = itemDurability, command = "ShowLootItemInfo -" & $(
        itemsIndexes[index] + 1), column = 3)
    addButton(table = lootTable, text = "0", tooltip = tableTooltip,
        command = "ShowLootItemInfo -" & $(itemsIndexes[index] + 1), column = 4)
    let baseAmount = skyBases[baseIndex].cargo[itemsIndexes[index]].amount
    addButton(table = lootTable, text = $baseAmount, tooltip = tableTooltip,
        command = "ShowLootItemInfo -" & $(itemsIndexes[index] + 1), column = 5, newRow = true)
  let arguments = (if argc > 1: "{" & $argv[1] & "}" else: "All")
  if page > 1:
    if lootTable.row < gameSettings.listsLimit + 1:
      addPagination(table = lootTable, previousCommand = "ShowLoot " &
          arguments & " " & $(page - 1))
    else:
      addPagination(table = lootTable, previousCommand = "ShowLoot " &
          arguments & " " & $(page - 1), nextCommand = "ShowLoot " & arguments &
          " " & $(page + 1))
  elif lootTable.row == gameSettings.listsLimit + 1:
    addPagination(table = lootTable, nextCommand = "ShowLoot " & arguments &
        " " & $(page + 1))
  updateTable(table = lootTable)
  tclEval(script = "update")
  tclEval(script = lootTable.canvas & " configure -scrollregion [list " &
      tclEval2(script = lootTable.canvas & " bbox all") & "]")
  tclEval(script = comboBox & " configure -values [list " & itemsTypes & "]")
  if argc == 1:
    tclEval(script = comboBox & " current 0")
  var freeSpace = try:
      freeCargo(amount = 0)
    except:
      return showError(message = "Can't count free space.")
  if freeSpace < 0:
    freeSpace = 0
  let tradeInfo = "Free cargo space: " & $(freeSpace) & " kg."
  label = lootCanvas & ".loot.options.playerinfo"
  tclEval(script = label & " configure -text {" & tradeInfo & "}")
  tclEval(script = "grid " & closeButton & " -row 0 -column 1")
  tclEval(script = lootCanvas & " configure -height [expr " & tclEval2(
      script = mainPaned & " sashpos 0") & " - 20] -width " & tclEval2(
      script = mainPaned & " cget -width"))
  tclEval(script = "update")
  tclEval(script = lootCanvas & " create window 0 0 -anchor nw -window " & lootFrame)
  tclEval(script = "update")
  tclEval(script = lootCanvas & " configure -scrollregion [list " & tclEval2(
      script = lootCanvas & " bbox all") & "]")
  tclEval(script = lootCanvas & " xview moveto 0.0")
  tclEval(script = lootCanvas & " yview moveto 0.0")
  showScreen(newScreenName = "lootframe")
  tclSetResult(value = "1")
  return tclOk

var itemIndex = -1

proc showLootItemInfoCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.exportc.} =
  itemIndex = ($argv[1]).parseInt
  if itemIndex < 0:
    itemIndex.inc
  else:
    itemIndex.dec
  var baseCargoIndex, cargoIndex = -1
  if itemIndex < 0:
    baseCargoIndex = itemIndex.abs
  else:
    cargoIndex = itemIndex
  let baseIndex = skyMap[playerShip.skyX][playerShip.skyY].baseIndex
  if cargoIndex > playerShip.cargo.high or baseCargoIndex > skyBases[
      baseIndex].cargo.high:
    return tclOk
  let protoIndex = (if cargoIndex > -1: playerShip.cargo[
      cargoIndex].protoIndex else: skyBases[baseIndex].cargo[
      baseCargoIndex].protoIndex)
  var itemInfo = "Weight: {gold}" & $itemsList[protoIndex].weight & " kg{/gold}"
  if itemsList[protoIndex].itemType == weaponType:
    itemInfo.add(y = "\nSkill: {gold}" & skillsList[itemsList[protoIndex].value[
        3]].name & "/" & attributesList[skillsList[itemsList[protoIndex].value[
        3]].attribute].name & "{/gold}")
    if itemsList[protoIndex].value[4] == 1:
      itemInfo.add(y = "\n{gold}Can be used with shield.{/gold}")
    else:
      itemInfo.add(y = "\n{gold}Can't be used with shield (two-handed weapon).{/gold}")
    itemInfo.add(y = "\nDamage type: {gold}")
    case itemsList[protoIndex].value[5]
    of 1:
      itemInfo.add(y = "cutting{/gold}")
    of 2:
      itemInfo.add(y = "impaling{/gold}")
    of 3:
      itemInfo.add(y = "blunt{/gold}")
    else:
      discard
  let itemTypes: array[6, string] = [weaponType, chestArmor, headArmor,
      armsArmor, legsArmor, shieldType]
  for itemType in itemTypes:
    if itemsList[protoIndex].itemType == itemType:
      itemInfo.add(y = "\nDamage chance: {gold}" & getItemChanceToDamage(
          itemData = itemsList[protoIndex].value[1]) & "{/gold}")
      itemInfo.add(y = "\nStrength: {gold}" & $itemsList[protoIndex].value[2] & "{/gold}")
      break
  return tclOk

proc addCommands*() {.sideEffect, raises: [], tags: [].} =
  ## Adds Tcl commands related to the trades UI
  try:
    discard
#    addCommand("ShowLoot", showLootCommand)
#    addCommand("ShowLootItemInfo", showLootItemInfoCommand)
  except:
    showError(message = "Can't add a Tcl command.")
