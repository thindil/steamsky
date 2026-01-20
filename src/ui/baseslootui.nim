# Copyright 2024-2026 Bartek thindil Jasicki
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

## Provides code related to looting empty bases UI, like the list of items to
## loot, selecting amount of items to get or drop, etc.

import std/[algorithm, strutils, tables]
import contracts, nimalyzer
import ../[basescargo, config, game, items, maps, messages, shipscargo, tk, types]
import coreui, dialogs, errordialog, dialogs2, mapsui, table, updateheader, utilsui2

{.push ruleOff: "varDeclared".}
var
  lootTable: TableWidget
  itemsIndexes: seq[int] = @[]
{.pop ruleOn: "varDeclared".}

type ItemsSortOrders = enum
  none, nameAsc, nameDesc, typeAsc, typeDesc, durabilityAsc, durabilityDesc,
    qualityAsc, qualityDesc, ownedAsc, ownedDesc, availableAsc, availableDesc

const defaultItemsSortOrder: ItemsSortOrders = none

var itemsSortOrder: ItemsSortOrders = defaultItemsSortOrder

proc showLootCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [
    RootEffect], cdecl, contractual, contractual, ruleOff: "params".} =
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
  var lootFrame: string = mainPaned & ".lootframe"
  let lootCanvas: string = lootFrame & ".canvas"
  var label: string = lootCanvas & ".loot.options.typelabel"
  if tclEval2(script = "winfo exists " & label) == "0":
    tclEval(script = """
      ttk::frame .gameframe.paned.lootframe
      set lootcanvas [canvas .gameframe.paned.lootframe.canvas \
         -yscrollcommand [list .gameframe.paned.lootframe.scrolly set] \
         -xscrollcommand [list .gameframe.paned.lootframe.scrollx set]]
      pack [ttk::scrollbar .gameframe.paned.lootframe.scrolly -orient vertical \
         -command [list $lootcanvas yview]] -side right -fill y
      pack $lootcanvas -side top -fill both
      SetScrollbarBindings $lootcanvas .gameframe.paned.lootframe.scrolly
      pack [ttk::scrollbar .gameframe.paned.lootframe.scrollx -orient horizontal \
         -command [list $lootcanvas xview]] -fill x
      ::autoscroll::autoscroll .gameframe.paned.lootframe.scrolly
      ::autoscroll::autoscroll .gameframe.paned.lootframe.scrollx
      set lootframe [ttk::frame $lootcanvas.loot]
      SetScrollbarBindings $lootframe .gameframe.paned.lootframe.scrolly
      # Type of items to show
      grid [ttk::frame $lootframe.options] -sticky w -padx 5 -pady 5
      SetScrollbarBindings $lootframe.options .gameframe.paned.lootframe.scrolly
      grid [ttk::label $lootframe.options.typelabel -text {Type:}]
      SetScrollbarBindings $lootframe.options.typelabel \
         .gameframe.paned.lootframe.scrolly
      grid [ttk::combobox $lootframe.options.type -state readonly] -column 1 -row 0
      bind $lootframe.options.type <<ComboboxSelected>> \
         {ShowLoot [$lootframe.options.type get]}
      grid [ttk::frame $lootframe.options.info] -sticky nw \
         -columnspan 2
      grid [ttk::label $lootframe.options.info.playerinfo -wraplength 300 \
         -text {Free cargo space: }]
      grid [ttk::label $lootframe.options.info.playerinfo2 -wraplength 300 \
         -style Golden.TLabel] -row 0 -column 1
      SetScrollbarBindings $lootframe.options.info \
         .gameframe.paned.tradeframe.scrolly
    """)
    tclEval(script = "bind " & lootFrame & " <Configure> {ResizeCanvas %W.canvas %w %h}")
    lootFrame = lootCanvas & ".loot"
    lootTable = createTable(parent = lootFrame, headers = @["Name", "Type",
        "Durability", "Quality", "Owned", "Available"],
        scrollbar = ".gameframe.paned.lootframe.scrolly",
        command = "SortLootItems",
        tooltipText = "Press mouse button to sort the items.")
  elif tclEval2(script = "winfo ismapped " & label) == "1" and argc == 1:
    tclEval(script = "grid remove " & closeButton)
    showSkyMap(clear = true)
  lootFrame = lootCanvas & ".loot"
  var comboBox: string = lootFrame & ".options.type"
  let
    baseIndex: ExtendedBasesRange = skyMap[playerShip.skyX][
        playerShip.skyY].baseIndex
    currentBaseCargo: seq[BaseCargo] = skyBases[baseIndex].cargo
  if itemsSortOrder == defaultItemsSortOrder:
    itemsIndexes = @[]
    for index, _ in playerShip.cargo:
      itemsIndexes.add(y = index)
    itemsIndexes.add(y = -1)
    for index, _ in currentBaseCargo:
      itemsIndexes.add(y = index)
  clearTable(table = lootTable)
  var itemsTypes: string = "All"
  for index in itemsIndexes:
    if index == -1:
      break
    let
      protoIndex: int = playerShip.cargo[index].protoIndex
      itemType: string = try:
          (if itemsList[protoIndex].showType.len == 0: itemsList[
            protoIndex].itemType else: itemsList[protoIndex].showType)
        except:
          return showError(message = "Can't get item type.")
    if not itemsTypes.contains(sub = "{" & itemType & "}"):
      itemsTypes.add(y = " {" & itemType & "}")
  var
    currentItemIndex: int = 1
    indexesList: seq[Natural] = @[]
    currentRow: Positive = 1
  let
    page: int = try:
        (if argc == 3: ($argv[2]).parseInt else: 1)
      except:
        return showError(message = "Can't get page number.")
    startRow: Natural = ((page - 1) * gameSettings.listsLimit) + 1
  const tableTooltip: string = "Show item's description and actions"
  for index in itemsIndexes:
    currentItemIndex.inc
    if index == -1:
      break
    let
      protoIndex: int = playerShip.cargo[index].protoIndex
      baseCargoIndex: int = findBaseCargo(protoIndex = protoIndex,
          durability = playerShip.cargo[index].durability,
          quality = playerShip.cargo[index].quality)
    if baseCargoIndex > -1:
      indexesList.add(y = baseCargoIndex)
    let itemType: string = try:
        (if itemsList[protoIndex].showType.len == 0: itemsList[
          protoIndex].itemType else: itemsList[protoIndex].showType)
      except:
        return showError(message = "Can't get item type2.")
    if argc > 1 and argv[1] != "All" and itemType != $argv[1]:
      continue
    if currentRow < startRow:
      currentRow.inc
      continue
    let itemName: string = getItemName(item = playerShip.cargo[index],
        damageInfo = false, toLower = false)
    addButton(table = lootTable, text = itemName, tooltip = tableTooltip,
        command = "ShowLootItemInfo " & $(index + 1), column = 1)
    addButton(table = lootTable, text = itemType, tooltip = tableTooltip,
        command = "ShowLootItemInfo " & $(index + 1), column = 2)
    let itemDurability: string = (if playerShip.cargo[index].durability <
        100: getItemDamage(itemDurability = playerShip.cargo[
        index].durability) else: "Unused")
    addProgressbar(table = lootTable, value = playerShip.cargo[
        index].durability, maxValue = defaultItemDurability,
        tooltip = itemDurability, command = "ShowLootItemInfo " & $(index + 1), column = 3)
    addButton(table = lootTable, text = ($playerShip.cargo[
        index].quality).capitalizeAscii, tooltip = tableTooltip,
        command = "ShowLootItemInfo " & $(index + 1), column = 4)
    addButton(table = lootTable, text = $playerShip.cargo[index].amount,
        tooltip = tableTooltip, command = "ShowLootItemInfo " & $(index + 1), column = 5)
    let baseAmount: int = if baseCargoIndex > -1: skyBases[baseIndex].cargo[
        baseCargoIndex].amount else: 0
    addButton(table = lootTable, text = $baseAmount, tooltip = tableTooltip,
        command = "ShowLootItemInfo " & $(index + 1), column = 6, newRow = true)
    if lootTable.row == gameSettings.listsLimit + 1:
      break
  currentItemIndex = playerShip.cargo.len + 2
  for index in currentItemIndex..itemsIndexes.high:
    let
      protoIndex: int = currentBaseCargo[itemsIndexes[index]].protoIndex
      itemType: string = try:
          (if itemsList[protoIndex].showType.len == 0: itemsList[
            protoIndex].itemType else: itemsList[protoIndex].showType)
        except:
          return showError(message = "Can't get item type3.")
    if not itemsTypes.contains(sub = "{" & itemType & "}"):
      itemsTypes.add(y = " {" & itemType & "}")
  for index in currentItemIndex..itemsIndexes.high:
    if lootTable.row == gameSettings.listsLimit + 1:
      break
    if indexesList.contains(item = itemsIndexes[index]):
      continue
    let
      protoIndex: int = currentBaseCargo[itemsIndexes[index]].protoIndex
      itemType: string = try:
          (if itemsList[protoIndex].showType.len == 0: itemsList[
            protoIndex].itemType else: itemsList[protoIndex].showType)
        except:
          return showError(message = "Can't get item type4.")
    if argc == 2 and argv[1] != "All" and itemType != $argv[1]:
      continue
    if currentRow < startRow:
      currentRow.inc
      continue
    let itemName: string = try:
        itemsList[protoIndex].name
      except:
        return showError(message = "Can't get item name.")
    addButton(table = lootTable, text = itemName, tooltip = tableTooltip,
        command = "ShowLootItemInfo -" & $(itemsIndexes[index] + 1), column = 1)
    addButton(table = lootTable, text = itemType, tooltip = tableTooltip,
        command = "ShowLootItemInfo -" & $(itemsIndexes[index] + 1), column = 2)
    let itemDurability: string = (if currentBaseCargo[itemsIndexes[
        index]].durability < 100: getItemDamage(
        itemDurability = currentBaseCargo[itemsIndexes[
        index]].durability) else: "Unused")
    addProgressbar(table = lootTable, value = currentBaseCargo[itemsIndexes[
        index]].durability, maxValue = defaultItemDurability,
        tooltip = itemDurability, command = "ShowLootItemInfo -" & $(
        itemsIndexes[index] + 1), column = 3)
    addButton(table = lootTable, text = ($currentBaseCargo[itemsIndexes[
        index]].quality).capitalizeAscii, tooltip = tableTooltip,
        command = "ShowLootItemInfo -" & $(itemsIndexes[index] + 1), column = 4)
    addButton(table = lootTable, text = "0", tooltip = tableTooltip,
        command = "ShowLootItemInfo -" & $(itemsIndexes[index] + 1), column = 5)
    let baseAmount: int = skyBases[baseIndex].cargo[itemsIndexes[index]].amount
    addButton(table = lootTable, text = $baseAmount, tooltip = tableTooltip,
        command = "ShowLootItemInfo -" & $(itemsIndexes[index] + 1), column = 6, newRow = true)
  let arguments: string = (if argc > 1: "{" & $argv[1] & "}" else: "All")
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
  var freeSpace: int = try:
      freeCargo(amount = 0)
    except:
      return showError(message = "Can't count free space.")
  if freeSpace < 0:
    freeSpace = 0
  let tradeInfo: string = $(freeSpace) & " kg"
  label = lootCanvas & ".loot.options.info.playerinfo2"
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

var itemIndex: int = -1

proc showLootItemInfoCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [WriteIOEffect,
        TimeEffect, RootEffect], cdecl, contractual.} =
  ## Show information about the selected item
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ShowLootItemInfo itemindex
  ## ItemIndex is a index of the item which info will be shown.
  itemIndex = try:
      ($argv[1]).parseInt
    except:
      return showError(message = "Can't get item's index.")
  let (protoIndex, maxAmount, cargoMaxAmount, quality, _, _) = try:
      getLootData(itemIndex = itemIndex)
    except:
      return showError(message = "Can't get the trade's data.")
  var itemInfo: string = try:
      "Weight: {gold}" & $itemsList[protoIndex].weight & " kg{/gold}"
    except:
      return showError(message = "Can't get item's weight.")
  try:
    if itemsList[protoIndex].itemType == weaponType:
      itemInfo.add(y = "\nSkill: {gold}" & skillsList[itemsList[
          protoIndex].value[3]].name & "/" & attributesList[skillsList[
              itemsList[protoIndex].value[
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
  except:
    return showError(message = "Can't show weapon info.")
  let itemTypes: array[6, string] = [weaponType, chestArmor, headArmor,
      armsArmor, legsArmor, shieldType]
  for itemType in itemTypes:
    try:
      if itemsList[protoIndex].itemType == itemType:
        itemInfo.add(y = "\nDamage chance: {gold}" & getItemChanceToDamage(
            itemData = itemsList[protoIndex].value[1]) & "{/gold}")
        itemInfo.add(y = "\nStrength: {gold}" & $itemsList[protoIndex].value[
            2] & "{/gold}")
        break
    except:
      return showError(message = "Can't show weapon damage chance.")
  try:
    if itemsList[protoIndex].itemType in toolsList:
      itemInfo.add(y = "\nDamage chance: {gold}" & getItemChanceToDamage(
          itemData = itemsList[protoIndex].value[1]) & "{/gold}")
  except:
    return showError(message = "Can't show tool damage chance.")
  try:
    if itemsList[protoIndex].itemType.len > 4 and itemsList[
        protoIndex].itemType[0..3] == "Ammo" or itemsList[
        protoIndex].itemType == "Harpoon":
      itemInfo.add(y = "\nStrength: {gold}" & $itemsList[protoIndex].value[1] & "{/gold}")
  except:
    return showError(message = "Can't show ammo info.")
  try:
    if itemInfo.len > 0:
      itemInfo.add(y = "\n")
    itemInfo.add(y = "Quality: {gold}" & ($quality).capitalizeAscii & "{/gold}")
  except:
    return showError(message = "Can't get quality info.")
  try:
    if itemsList[protoIndex].description.len > 0:
      itemInfo.add(y = "\n\n" & itemsList[protoIndex].description)
  except:
    return showError(message = "Can't show item's description.")
  try:
    showInfo(text = itemInfo, title = itemsList[protoIndex].name, button1 = (
        if maxAmount == 0: emptyButtonSettings else: ButtonSettings(
        tooltip: "Take item from the base", command: "LootAmount take " &
        $maxAmount, icon: "giveicon", text: "Take", color: "")), button2 = (
        if cargoMaxAmount == 0: emptyButtonSettings else: ButtonSettings(
        tooltip: "Drop item from the ship cargo", command: "LootAmount drop " &
        $cargoMaxAmount, icon: "dropicon", text: "Drop", color: "")))
  except:
    return showError(message = "Can't show the item's info.")
  return tclOk

proc lootItemCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [
    RootEffect], cdecl, contractual.} =
  ## Take or drop the selected item
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## LootItem actiontype
  ## actiontype can be: drop, dropall, take, takeall
  var baseCargoIndex, cargoIndex: int = -1
  if itemIndex < 0:
    baseCargoIndex = (itemIndex + 1).abs
  else:
    cargoIndex = itemIndex - 1
  var protoIndex: int = 0
  let baseIndex: ExtendedBasesRange = skyMap[playerShip.skyX][
      playerShip.skyY].baseIndex
  if cargoIndex > -1:
    protoIndex = playerShip.cargo[cargoIndex].protoIndex
    if baseCargoIndex == -1:
      baseCargoIndex = findBaseCargo(protoIndex = protoIndex,
          quality = playerShip.cargo[cargoIndex].quality)
  else:
    protoIndex = skyBases[baseIndex].cargo[baseCargoIndex].protoIndex
  var amount: int = 0
  const amountBox: string = ".itemdialog.amount"
  if $argv[1] in ["drop", "dropall"]:
    let item: InventoryData = playerShip.cargo[cargoIndex]
    amount = try:
        (if argv[1] == "drop": tclEval2(script = amountBox &
          " get").parseInt else: item.amount)
      except:
        return showError(message = "Can't get drop amount.")
    if baseCargoIndex > -1:
      try:
        updateBaseCargo(cargoIndex = baseCargoIndex, amount = amount,
            durability = item.durability, quality = item.quality,
            maxDurability = item.maxDurability, weight = item.weight)
      except:
        return showError(message = "Can't update the base's cargo.")
    else:
      try:
        updateBaseCargo(protoIndex = protoIndex, amount = amount,
            durability = item.durability, quality = item.quality,
            maxDurability = item.maxDurability, weight = item.weight)
      except:
        return showError(message = "Can't update the base's cargo2.")
    updateCargo(ship = playerShip, cargoIndex = cargoIndex, amount = -amount,
        durability = item.durability, quality = item.quality,
        maxDurability = item.maxDurability, weight = item.weight)
    try:
      addMessage(message = "You drop " & $amount & " " & itemsList[
          protoIndex].name & ".", mType = orderMessage)
    except:
      return showError(message = "Can't add message.")
  else:
    amount = try:
        (if argv[1] == "take": tclEval2(script = amountBox &
          " get").parseInt else: ($argv[2]).parseInt)
      except:
        return showError(message = "Can't get take amount.")
    try:
      if freeCargo(amount = -(amount * itemsList[protoIndex].weight)) < 0:
        showMessage(text = "You can't take that much " & itemsList[
            protoIndex].name & ".", title = "Too much taken")
        return tclOk
    except:
      return showError(message = "Can't count free cargo.")
    let item: BaseCargo = skyBases[baseIndex].cargo[baseCargoIndex]
    if cargoIndex > -1:
      updateCargo(ship = playerShip, cargoIndex = cargoIndex, amount = amount,
          durability = item.durability, quality = item.quality,
          maxDurability = item.maxDurability, weight = item.weight)
    else:
      updateCargo(ship = playerShip, protoIndex = protoIndex, amount = amount,
          durability = item.durability, quality = item.quality,
          maxDurability = item.maxDurability, weight = item.weight)
    try:
      updateBaseCargo(cargoIndex = baseCargoIndex, amount = -(amount),
          durability = item.durability, quality = item.quality,
          maxDurability = item.maxDurability, weight = item.weight)
    except:
      return showError(message = "Can't update the base's cargo3.")
    try:
      addMessage(message = "You took " & $amount & " " & itemsList[
          protoIndex].name & ".", mType = orderMessage)
    except:
      return showError(message = "Can't add message2.")
  if $argv[1] in ["take", "drop"]:
    if closeDialogCommand(clientData = clientData, interp = interp, argc = 2,
        argv = @["CloseDialog", ".itemdialog"].allocCStringArray) == tclError:
      return tclError
  updateHeader()
  updateMessages()
  let typeBox: string = mainPaned & ".lootframe.canvas.loot.options.type"
  return showLootCommand(clientData = clientData, interp = interp, argc = 2,
      argv = @["ShowLoot", tclEval2(script = typeBox &
      " get")].allocCStringArray)

proc lootAmountCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [WriteIOEffect,
        TimeEffect, RootEffect], cdecl, contractual.} =
  ## Show dialog to enter amount of items to drop or take
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## LootAmount action maxamount
  ## Action which will be taken. Can be take or drop. Maxamount is the
  ## maximum allowed amount of items to take
  if argv[1] == "drop":
    showManipulateItem(title = "Drop " & getItemName(item = playerShip.cargo[
        itemIndex - 1]), command = "LootItem drop", action = "drop",
        itemIndex = itemIndex - 1)
  else:
    if itemIndex > 0:
      try:
        showManipulateItem(title = "Take " & getItemName(
            item = playerShip.cargo[itemIndex - 1]), command = "LootItem take",
            action = "take", itemIndex = itemIndex - 1, maxAmount = ($argv[2]).parseInt)
      except:
        return showError(message = "Can't take item from base.")
    else:
      let baseIndex: ExtendedBasesRange = skyMap[playerShip.skyX][
          playerShip.skyY].baseIndex
      try:
        showManipulateItem(title = "Take " & itemsList[skyBases[
            baseIndex].cargo[(itemIndex + 1).abs].protoIndex].name,
            command = "LootItem take", action = "take", itemIndex = (itemIndex +
            1).abs, maxAmount = ( $argv[2]).parseInt)
      except:
        return showError(message = "Can't take item from base2.")
  return tclOk

proc sortLootItemsCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [
    RootEffect], cdecl, contractual.} =
  ## Sort the looting list
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## SortLootItems x
  ## X is X axis coordinate where the player clicked the mouse button
  let column: int = try:
        getColumnNumber(table = lootTable, xPosition = ($argv[1]).parseInt)
      except:
        return showError(message = "Can't get the column number.")
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
    if itemsSortOrder == qualityAsc:
      itemsSortOrder = qualityDesc
    else:
      itemsSortOrder = ownedAsc
  of 5:
    if itemsSortOrder == ownedAsc:
      itemsSortOrder = ownedDesc
    else:
      itemsSortOrder = ownedAsc
  of 6:
    if itemsSortOrder == availableAsc:
      itemsSortOrder = availableDesc
    else:
      itemsSortOrder = availableAsc
  else:
    discard
  if itemsSortOrder == none:
    return tclOk
  type LocalItemData = object
    name: string
    iType: string
    damage: float
    owned: Natural
    available: Natural
    quality: ObjectQuality
    id: Natural
  var
    localItems: seq[LocalItemData] = @[]
    indexesList: seq[Natural] = @[]
  let baseIndex: ExtendedBasesRange = skyMap[playerShip.skyX][
      playerShip.skyY].baseIndex
  var localBaseCargo: seq[BaseCargo] = skyBases[baseIndex].cargo
  for index, item in playerShip.cargo:
    let
      protoIndex: int = item.protoIndex
      baseCargoIndex: int = findBaseCargo(protoIndex = protoIndex,
          durability = item.durability, quality = item.quality)
    if baseCargoIndex > -1:
      indexesList.add(y = baseCargoIndex)
    try:
      localItems.add(y = LocalItemData(name: getItemName(item = item), iType: (
          if itemsList[protoIndex].showType.len == 0: itemsList[
          protoIndex].itemType else: itemsList[protoIndex].showType), damage: (
          item.durability.float / defaultItemDurability.float),
          owned: item.amount, available: (if baseCargoIndex >
          -1: localBaseCargo[
          baseCargoIndex].amount else: 0), quality: item.quality, id: index))
    except:
      return showError(message = "Can't add player's ship's item.")
  proc sortItems(x, y: LocalItemData): int {.raises: [], tags: [],
      contractual.} =
    ## Compare two items and return which should go first, based on the sort
    ## order of the items
    ##
    ## * x - the first item to compare
    ## * y - the second item to compare
    ##
    ## Returns 1 if the first item should go first, -1 if the second item
    ## should go first.
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
  localItems.sort(cmp = sortItems)
  itemsIndexes = @[]
  for item in localItems:
    itemsIndexes.add(y = item.id)
  itemsIndexes.add(y = -1)
  localItems = @[]
  for index, item in localBaseCargo:
    if indexesList.contains(item = index):
      continue
    let protoIndex: int = item.protoIndex
    try:
      localItems.add(y = LocalItemData(name: itemsList[protoIndex].name,
          iType: (if itemsList[protoIndex].showType.len == 0: itemsList[
          protoIndex].itemType else: itemsList[protoIndex].showType), damage: (
          item.durability.float / defaultItemDurability.float), owned: 0,
          available: item.amount, quality: item.quality, id: index))
    except:
      return showError(message = "Can't add the base's item.")
  localItems.sort(cmp = sortItems)
  for item in localItems:
    itemsIndexes.add(y = item.id)
  return showLootCommand(clientData = clientData, interp = interp, argc = 2,
      argv = @["ShowLoot", "All"].allocCStringArray)

proc addCommands*() {.raises: [], tags: [WriteIOEffect, TimeEffect, RootEffect],
    contractual.} =
  ## Adds Tcl commands related to the trades UI
  try:
    addCommand(name = "ShowLoot", nimProc = showLootCommand)
    addCommand(name = "ShowLootItemInfo", nimProc = showLootItemInfoCommand)
    addCommand(name = "LootItem", nimProc = lootItemCommand)
    addCommand(name = "LootAmount", nimProc = lootAmountCommand)
    addCommand(name = "SortLootItems", nimProc = sortLootItemsCommand)
  except:
    showError(message = "Can't add a Tcl command.")
