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

import std/[algorithm, strutils, tables]
import ../[config, crewinventory, game, items, shipscargo, shipscrew, tk, types]
import coreui, dialogs, table, utilsui2

var
  inventoryTable: TableWidget
    ## The UI table with the list of items in the crew member's inventory
  memberIndex: Natural
    ## The index of the selected crew member
  inventoryIndexes: seq[Natural]
    ## The list of indexes of items in the crew member's inventory

proc updateInventoryCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [
    RootEffect], exportc.} =
  ## Update inventory list of the selected crew member
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## UpdateInventory memberindex page
  ## MemberIndex is the index of the crew member to show inventory, page
  ## is a number of the page of inventory list to show
  memberIndex = try:
      ($argv[1]).parseInt - 1
    except:
      return showError(message = "Can't get the member index.")
  if inventoryTable.row > 1:
    inventoryTable.clearTable
  let member = playerShip.crew[memberIndex]
  if inventoryIndexes.len != member.inventory.len:
    inventoryIndexes = @[]
    for index, _ in member.inventory:
      inventoryIndexes.add(y = index)
  let
    page = try:
        (if argc == 3: ($argv[2]).parseInt else: 1)
      except:
        return showError(message = "Can't get the page number.")
    startRow = ((page - 1) * gameSettings.listsLimit) + 1
  var currentRow = 1
  for index, item in inventoryIndexes:
    if currentRow < startRow:
      currentRow.inc
      continue
    addCheckButton(table = inventoryTable,
        tooltip = "Select the item for move or equip it.",
        command = "ToggleInventoryItem " & $(index + 1) & " " & $(item + 1),
        checked = tclGetVar(varName = "invindex" & $(item + 1)) == "1",
        column = 1, emptyUnchecked = true)
    addButton(table = inventoryTable, text = getItemName(
        item = member.inventory[item], damageInfo = false, toLower = false),
        tooltip = "Show the selected item's info",
        command = "ShowInventoryItemInfo " & $(item + 1), column = 2)
    addProgressbar(table = inventoryTable, value = member.inventory[
        item].durability, maxValue = defaultItemDurability,
        tooltip = "The current durability level of the selected item.",
        command = "ShowInventoryItemInfo " & $(item + 1), column = 3)
    if itemIsUsed(memberIndex = memberIndex, itemIndex = item):
      addCheckButton(table = inventoryTable,
          tooltip = "The item is used by the crew member",
          command = "ShowInventoryItemInfo " & $(item + 1), checked = true, column = 4)
    else:
      addCheckButton(table = inventoryTable,
          tooltip = "The item isn't used by the crew member",
          command = "ShowInventoryItemInfo " & $(item + 1), checked = false, column = 4)
    addButton(table = inventoryTable, text = $member.inventory[item].amount,
        tooltip = "The amount of the item owned by the crew member.",
        command = "ShowInventoryItemInfo " & $(item + 1), column = 5)
    try:
      addButton(table = inventoryTable, text = $(member.inventory[item].amount *
          itemsList[member.inventory[item].protoIndex].weight) & " kg",
          tooltip = "The total weight of the items",
          command = "ShowInventoryItemInfo " & $(item + 1), column = 6, newRow = true)
    except:
      return showError(message = "Can't count the total weight of the item.")
    if inventoryTable.row == gameSettings.listsLimit + 1:
      break
  if page > 1:
    addPagination(table = inventoryTable, previousCommand = "UpdateInventory " &
        $argv[1] & " " & $(page - 1), nextCommand = (if inventoryTable.row <
        gameSettings.listsLimit + 1: "" else: "UpdateInventory " & $argv[1] &
        " " & $(page + 1)))
  elif inventoryTable.row == gameSettings.listsLimit + 1:
    addPagination(table = inventoryTable, previousCommand = "",
        nextCommand = "UpdateInventory " & $argv[1] & " " & $(page + 1))
  updateTable(table = inventoryTable)
  return tclOk

proc resetSelection() =
  ## Reset the currently selected items in the crew member inventory
  for index, _ in playerShip.crew[memberIndex].inventory:
    if tclGetVar(varName = "invindex" & $(index + 1)) == "1":
      tclUnsetVar(varName = "invindex" & $(index + 1))

proc showMemberInventoryCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [
        RootEffect], exportc.} =
  ## Show inventory of the selected crew member
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ShowMemberInventory memberindex
  ## MemberIndex is the index of the crew member to show inventory
  let localMemberIndex = try:
        ($argv[1]).parseInt - 1
      except:
        return showError(message = "Can't get the member index.")
  if playerShip.crew[localMemberIndex].inventory.len == 0:
    tclEval(script = "CloseDialog .memberdialog")
    showMessage(text = playerShip.crew[localMemberIndex].name &
        " doesn't own any items.", title = "Inventory of " & playerShip.crew[
        localMemberIndex].name)
    return tclOk
  memberIndex = localMemberIndex
  resetSelection()
  let
    memberDialog = createDialog(name = ".memberdialog",
        title = "Inventory of " & playerShip.crew[memberIndex].name, columns = 2)
    dialogCloseButton = memberDialog & ".button"
  tclEval(script = "ttk::button " & dialogCloseButton &
      " -image exiticon -command {CloseDialog " & memberDialog & "} -text {Close} -style Dialog.TButton")
  tclEval(script = "tooltip::tooltip " & dialogCloseButton & " \"Close inventory \\[Escape key\\]\"")
  let yScroll = memberDialog & ".yscroll"
  tclEval(script = "ttk::scrollbar " & yScroll & " -orient vertical -command [list .memberdialog.canvas yview]")
  let memberCanvas = memberDialog & ".canvas"
  tclEval(script = "canvas " & memberCanvas & " -yscrollcommand [list " &
      yScroll & " set]")
  tclEval(script = "grid " & memberCanvas & " -padx 5 -pady 5")
  tclEval(script = "grid " & yScroll & " -row 1 -column 1 -padx 5 -pady 5 -sticky ns")
  tclEval(script = "::autoscroll::autoscroll " & yScroll)
  let memberFrame = memberCanvas & ".frame"
  tclEval(script = "ttk::frame " & memberFrame)
  let freeSpaceFrame = memberFrame & ".freeframe"
  tclEval(script = "ttk::frame " & freeSpaceFrame)
  let freeSpaceLabel = freeSpaceFrame & ".freespace"
  tclEval(script = "ttk::label " & freeSpaceLabel &
      " -text {Free inventory space: " & $freeInventory(
      memberIndex = memberIndex, amount = 0) & " kg} -wraplength 400")
  tclEval(script = "grid " & freeSpaceLabel)
  var height = try:
      10 + tclEval2(script = "winfo reqheight " &
        freeSpaceLabel).parseInt
    except:
      return showError(message = "Can't count the height of the label.")
  tclEval(script = "grid " & freeSpaceFrame)
  let buttonsBox = memberFrame & ".selectbox"
  tclEval(script = "ttk::frame " & buttonsBox)
  let selectAllButton = buttonsBox & ".selectallbutton"
  tclEval(script = "ttk::button " & selectAllButton & " -image selectallicon -command {ToggleAllInventory select} -style Small.TButton")
  tclEval(script = "tooltip::tooltip " & selectAllButton & " \"Select all items.\"")
  tclEval(script = "grid " & selectAllButton & " -sticky w")
  let unselectAllButton = buttonsBox & ".unselectallbutton"
  tclEval(script = "ttk::button " & unselectAllButton & " -image unselectallicon -command {ToggleAllInventory unselect} -style Small.TButton")
  tclEval(script = "tooltip::tooltip " & selectAllButton & " \"Unselect all items.\"")
  tclEval(script = "grid " & unselectAllButton & " -sticky w -row 0 -column 1")
  height = try:
      height + tclEval2(script = "winfo reqheight " &
        selectAllButton).parseInt
    except:
      return showError(message = "Can't count the height of the button.")
  tclEval(script = "grid " & buttonsBox & " -sticky w -padx 5")
  inventoryTable = createTable(parent = memberFrame, headers = @["", "Name",
      "Durability", "Used", "Amount", "Weight"], scrollbar = yScroll,
      command = "SortCrewInventory",
      tooltipText = "Press mouse button to sort the inventory.")
  discard updateInventoryCommand(clientData = clientData, interp = interp,
      argc = argc, argv = argv)
  height = try:
      height + tclEval2(script = "winfo reqheight " &
        inventoryTable.canvas).parseInt
    except:
      return showError(message = "Can't count the height of the table.")
  var width = try:
      tclEval2(script = "winfo reqwidth " &
        inventoryTable.canvas).parseInt
    except:
      return showError(message = "Can't count the width.")
  tclEval(script = "grid " & dialogCloseButton & " -pady 5")
  tclEval(script = "focus " & inventoryTable.canvas)
  tclEval(script = "bind " & dialogCloseButton & " <Tab> {focus " &
      selectAllButton & ";break}")
  tclEval(script = "bind " & dialogCloseButton & " <Escape> {" &
      dialogCloseButton & " invoke;break}")
  tclEval(script = "bind " & selectAllButton & " <Escape> {" &
      dialogCloseButton & " invoke;break}")
  tclEval(script = "bind " & unselectAllButton & " <Escape> {" &
      dialogCloseButton & " invoke;break}")
  tclEval(script = "bind " & inventoryTable.canvas & " <Escape> {" &
      dialogCloseButton & " invoke;break}")
  tclEval(script = "focus " & dialogCloseButton)
  if height > 500:
    height = 500
  tclEval(script = memberFrame & " configure -height " & $height & " -width " & $width)
  tclEval(script = memberCanvas & " configure -height " & $height & " -width " &
      $(width + 15))
  tclEval(script = memberCanvas & " create window 0 0 -anchor nw -window " & memberFrame)
  tclEval(script = "update")
  tclEval(script = memberCanvas & " configure -scrollregion [list " & tclEval2(
      script = memberCanvas & " bbox all") & "]")
  showDialog(dialog = memberDialog, relativeX = 0.2, relativeY = 0.2)
  return tclOk

type InventorySortOrders = enum
  selectedAsc, selectedDesc, nameAsc, nameDesc, durabilityAsc, durabilityDesc,
    typeAsc, typeDesc, amountAsc, amountDesc, weightAsc, weightDesc, useAsc,
    useDesc, none

const defaultInventorySortOrder: InventorySortOrders = none

var inventorySortOrder = defaultInventorySortOrder

proc sortCrewInventoryCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [
        RootEffect], exportc.} =
  ## Sort the selected crew member inventory
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## SortCrewInventory x
  ## X is X axis coordinate where the player clicked the mouse button
  let column = try:
      (if argv[1] == "-1": Positive.high else: getColumnNumber(
        table = inventoryTable, xPosition = ($argv[1]).parseInt))
    except:
      return showError(message = "Can't get column number.")
  case column
  of 1:
    if inventorySortOrder == selectedAsc:
      inventorySortOrder = selectedDesc
    else:
      inventorySortOrder = selectedAsc
  of 2:
    if inventorySortOrder == nameAsc:
      inventorySortOrder = nameDesc
    else:
      inventorySortOrder = nameAsc
  of 3:
    if inventorySortOrder == durabilityAsc:
      inventorySortOrder = durabilityDesc
    else:
      inventorySortOrder = durabilityAsc
  of 4:
    if inventorySortOrder == useAsc:
      inventorySortOrder = useDesc
    else:
      inventorySortOrder = useAsc
  of 5:
    if inventorySortOrder == amountAsc:
      inventorySortOrder = amountDesc
    else:
      inventorySortOrder = amountAsc
  of 6:
    if inventorySortOrder == weightAsc:
      inventorySortOrder = weightDesc
    else:
      inventorySortOrder = weightAsc
  else:
    discard
  if inventorySortOrder == none:
    return updateInventoryCommand(clientData = clientData, interp = interp,
        argc = 2, argv = @["UpdateInventory", ($(memberIndex + 1))].allocCStringArray)
  type LocalItemData = object
    selected: bool = false
    name: string = ""
    damage: float = 0.0
    itemType: string = ""
    amount: Positive = 1
    weight: Positive = 1
    used: bool = false
    id: Natural = 0
  var localInventory: seq[LocalItemData]
  for index, _ in inventoryIndexes:
    try:
      localInventory.add(LocalItemData(selected: tclGetVar(
          varName = "invindex" & $(index + 1)) == "1", name: getItemName(
          item = playerShip.crew[memberIndex].inventory[index],
          damageInfo = false, toLower = false), damage: playerShip.crew[
          memberIndex].inventory[index].durability.float /
          defaultItemDurability.float, itemType: (if itemsList[playerShip.crew[
          memberIndex].inventory[index].protoIndex].showType.len > 0: itemsList[
          playerShip.crew[memberIndex].inventory[
          index].protoIndex].showType else: itemsList[playerShip.crew[
          memberIndex].inventory[index].protoIndex].itemType),
          amount: playerShip.crew[memberIndex].inventory[index].amount,
          weight: playerShip.crew[memberIndex].inventory[index].amount *
          itemsList[playerShip.crew[memberIndex].inventory[
          index].protoIndex].weight, used: itemIsUsed(memberIndex = memberIndex,
          itemIndex = index), id: index))
    except:
      return showError(message = "Can't add item to local inventory.")
  proc sortInventory(x, y: LocalItemData): int =
    case inventorySortOrder
    of selectedAsc:
      if x.selected < y.selected:
        return 1
      else:
        return -1
    of selectedDesc:
      if x.selected > y.selected:
        return 1
      else:
        return -1
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
    of typeAsc:
      if x.itemType < y.itemType:
        return 1
      else:
        return -1
    of typeDesc:
      if x.itemType > y.itemType:
        return 1
      else:
        return -1
    of amountAsc:
      if x.amount < y.amount:
        return 1
      else:
        return -1
    of amountDesc:
      if x.amount > y.amount:
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
    of useAsc:
      if x.used < y.used:
        return 1
      else:
        return -1
    of useDesc:
      if x.used > y.used:
        return 1
      else:
        return -1
    of none:
      return -1
  localInventory.sort(cmp = sortInventory)
  inventoryIndexes = @[]
  for item in localInventory:
    inventoryIndexes.add(y = item.id)
  return updateInventoryCommand(clientData = clientData, interp = interp,
      argc = 2, argv = @["UpdateInventory", ($memberIndex)].allocCStringArray)

proc setUseItemCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [
    RootEffect], exportc.} =
  ## Set if item is used by a crew member or not
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## SetUseItem itemindex
  ## itemindex is the index of the item which will be set
  let itemIndex = try:
      ($argv[1]).parseInt - 1
    except:
      return showError(message = "Can't get the item index.")
  if itemIsUsed(memberIndex = memberIndex, itemIndex = itemIndex):
    takeOffItem(memberIndex = memberIndex, itemIndex = itemIndex)
    return sortCrewInventoryCommand(clientData = clientData, interp = interp,
        argc = 2, argv = @["SortCrewInventory", "-1"].allocCStringArray)
  let itemType = try:
      itemsList[playerShip.crew[memberIndex].inventory[
          itemIndex].protoIndex].itemType
    except:
      return showError(message = "Can't get the item type.")
  if itemType == weaponType:
    try:
      if itemsList[playerShip.crew[memberIndex].inventory[
          itemIndex].protoIndex].value[4] == 2 and playerShip.crew[
          memberIndex].equipment[shield] > -1:
        showMessage(text = playerShip.crew[memberIndex].name &
            " can't use this weapon because have shield equiped. Take off shield first.",
            title = "Shield in use")
        return tclOk
    except:
      return showError(message = "Can't do check for shield.")
    playerShip.crew[memberIndex].equipment[weapon] = itemIndex
  elif itemType == shieldType:
    if playerShip.crew[memberIndex].equipment[weapon] > -1:
      try:
        if itemsList[playerShip.crew[memberIndex].inventory[playerShip.crew[
            memberIndex].equipment[weapon]].protoIndex].value[4] == 2:
          showMessage(text = playerShip.crew[memberIndex].name &
              " can't use shield because have equiped two-hand weapon. Take off weapon first.",
              title = "Two handed weapon in use")
          return tclOk
      except:
        return showError(message = "Can't do check for two handed weapon.")
    playerShip.crew[memberIndex].equipment[shield] = itemIndex
  elif itemType == headArmor:
    playerShip.crew[memberIndex].equipment[helmet] = itemIndex
  elif itemType == chestArmor:
    playerShip.crew[memberIndex].equipment[torso] = itemIndex
  elif itemType == armsArmor:
    playerShip.crew[memberIndex].equipment[arms] = itemIndex
  elif itemType == legsArmor:
    playerShip.crew[memberIndex].equipment[legs] = itemIndex
  elif itemType in toolsList:
    playerShip.crew[memberIndex].equipment[tool] = itemIndex
  return sortCrewInventoryCommand(clientData = clientData, interp = interp,
      argc = 2, argv = @["SortCrewInventory", "-1"].allocCStringArray)

proc showMoveItemCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [], exportc.} =
  ## Show UI to move the selected item to the ship cargo
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ShowMoveItem itemindex
  ## itemindex is the index of the item which will be set
  let
    itemIndex = try:
        ($argv[1]).parseInt - 1
      except:
        return showError(message = "Can't get the item index.")
    itemDialog = createDialog(name = ".itemdialog", title = "Move " &
        getItemName(item = playerShip.crew[memberIndex].inventory[itemIndex]) &
        " to ship cargo", titleWidth = 400, columns = 2,
        parentName = ".memberDialog")
    maxAmount = playerShip.crew[memberIndex].inventory[itemIndex].amount
    amountBox = itemDialog & ".amount"
  var button = itemDialog & ".movebutton"
  tclEval(script = "ttk::button " & button & " -text Move -command {MoveItem " &
      $argv[1] & "} -image moveicon -style Dialoggreen.TButton")
  tclEval(script = "ttk::spinbox " & amountBox & " -width 5 -from 1 -to " &
      $maxAmount & " -validate key -validatecommand {ValidateMoveAmount " &
      $maxAmount & " %P " & button & " %W}")
  var maxAmountButton = itemDialog & ".amountlbl"
  tclEval(script = "ttk::button " & maxAmountButton & " -text {Amount (max: " &
      $maxAmount & "):} -command {" & amountBox & " set " & $maxAmount & ";" &
      amountBox & " validate}")
  tclEval(script = "tooltip::tooltip " & maxAmountButton & " \"Max amount of the item to move.\"")
  tclEval(script = "grid " & maxAmountButton & " -padx 5")
  tclEval(script = amountBox & " set 1")
  tclEval(script = "tooltip::tooltip " & amountBox & " \"Amount of the item to move.\"")
  tclEval(script = "grid " & amountBox & " -column 1 -row 1")
  tclEval(script = "bind " & amountBox & " <Escape> {" & itemDialog & ".cancelbutton invoke;break}")
  tclEval(script = "tooltip::tooltip " & button & " \"Move the itemm to the cargo.\"")
  tclEval(script = "grid " & button & " -padx {5 0} -pady {0 5}")
  tclEval(script = "bind " & button & " <Escape> {" & itemDialog & ".cancelbutton invoke;break}")
  button = itemDialog & ".cancelbutton"
  tclEval(script = "ttk::button " & button &
      " -text Cancel -command {CloseDialog " & itemDialog & " .memberdialog;focus .memberdialog.button} -image cancelicon -style Dialogred.TButton")
  tclEval(script = "tooltip::tooltip " & button & " \"Cancel moving and close dialog.\\[Escape key\\]\"")
  tclEval(script = "grid " & button & " -column 1 -row 2 -padx {0 5} -pady {0 5}")
  tclEval(script = "focus " & button)
  tclEval(script = "bind " & button & " <Tab> {focus " & itemDialog & ".movebutton;break}")
  tclEval(script = "bind " & button & " <Escape> {" & button & " invoke;break}")
  showDialog(dialog = itemDialog)
  return tclOk

proc toggleAllInventoryCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [
        RootEffect], exportc.} =
  ## Select or deselect all items in the crew member inventory
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ToggleAllInventory action
  ## Action is the action which will be performed. Possible values are
  ## select or deselect
  if argv[1] == "unselect":
    resetSelection()
  else:
    for index, _ in playerShip.crew[memberIndex].inventory:
      tclSetVar(varName = "invindex" & $(index + 1), newValue = "1")
  return sortCrewInventoryCommand(clientData = clientData, interp = interp,
      argc = 2, argv = @["SortCrewInventory", "-1"].allocCStringArray)

proc moveItem(itemIndex: Natural; amount: Positive) {.sideEffect, raises: [],
    tags: [RootEffect].} =
  ## Move the selected item to the player's ship's cargo
  ##
  ## * itemIndex - the index in the crew member's inventory of item to move
  ## * amount    - the amount of the item to move
  try:
    if freeCargo(amount = 0 - (itemsList[playerShip.crew[memberIndex].inventory[
        itemIndex].protoIndex].weight * amount)) < 0:
      showMessage(text = "No free space in ship cargo for tha amout of " &
          getItemName(item = playerShip.crew[memberIndex].inventory[itemIndex]),
          title = "No free space in cargo")
      return
  except:
    showError(message = "Can't check free cargo space.")
    return
  updateCargo(ship = playerShip, protoIndex = playerShip.crew[
      memberIndex].inventory[itemIndex].protoIndex, amount = amount,
      durability = playerShip.crew[memberIndex].inventory[itemIndex].durability,
      price = playerShip.crew[memberIndex].inventory[itemIndex].price)
  try:
    updateInventory(memberIndex = memberIndex, amount = -amount,
        inventoryIndex = itemIndex, ship = playerShip)
  except:
    showError(message = "Can't update the crew member inventory.")
    return
  if (playerShip.crew[memberIndex].order == clean and findItem(
      inventory = playerShip.crew[memberIndex].inventory,
      itemType = cleaningTools) == -1) or (playerShip.crew[memberIndex].order in
      {upgrading, repair} and findItem(inventory = playerShip.crew[
      memberIndex].inventory, itemType = repairTools) == -1):
    try:
      giveOrders(ship = playerShip, memberIndex = memberIndex,
          givenOrder = rest)
    except CrewOrderError:
      showMessage(text = getCurrentExceptionMsg(),
          title = "Can't give an order.")
      return
    except:
      showError(message = "Can't give order to the crew member.")
      return
  let typeBox = mainPaned & ".shipinfoframe.cargo.canvas.frame.selecttype.combo"
  tclEval(script = "event generate " & typeBox & " <<ComboboxSelected>>")

proc moveItemCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [
    RootEffect], exportc.} =
  ## Move the selected item to the ship cargo
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## MoveItem itemindex
  ## itemindex is the index of the item which will be set
  let
    itemIndex = try:
        ($argv[1]).parseInt - 1
      except:
        return showError(message = "Can't get item index.")
    itemDialog = ".itemdialog"
    amountBox = itemDialog & ".amount"
    amount = try:
        tclEval2(script = amountBox & " get").parseInt
      except:
        return showError(message = "Can't get the amount of item to move.")
  moveItem(itemIndex = itemIndex, amount = amount)
  tclEval(script = itemDialog & " destroy")
  tclEval(script = "CloseDialog " & itemDialog & " .memberdialog")
  if playerShip.crew[memberIndex].inventory.len == 0:
    tclEval(script = "CloseDialog .memberdialog")
    return tclOk
  return sortCrewInventoryCommand(clientData = clientData, interp = interp,
      argc = 2, argv = @["SortCrewInventory", "-1"].allocCStringArray)

proc moveItemsCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [
    RootEffect], exportc.} =
  ## Move the selected items to the ship cargo
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## MoveItems
  for index in countdown(playerShip.crew[memberIndex].inventory.high,
      playerShip.crew[memberIndex].inventory.low):
    if tclGetVar(varName = "invindex" & $(index + 1)) == "1":
      moveItem(itemIndex = index, amount = playerShip.crew[
          memberIndex].inventory[index].amount)
  if playerShip.crew[memberIndex].inventory.len == 0:
    tclEval(script = "CloseDialog .memberdialog")
    return tclOk
  resetSelection()
  return sortCrewInventoryCommand(clientData = clientData, interp = interp,
      argc = 2, argv = @["SortCrewInventory", "-1"].allocCStringArray)

proc toggleInventoryItemsCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [
    RootEffect], exportc.} =
  ## Equip or unequip the selected items
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ToggleInventoryItems action
  ## Action is the action to do with the selected items. Possible values are
  ## equip and unequip
  var isUsed = false
  let equip = argv[1] == "equip"
  for index, _ in playerShip.crew[memberIndex].inventory:
    if tclGetVar(varName = "invindex" & $(index + 1)) == "1":
      isUsed = itemIsUsed(memberIndex = memberIndex, itemIndex = index)
      if equip and not isUsed:
        discard setUseItemCommand(clientData = clientData, interp = interp,
            argc = 2, argv = @["SetUseItem", $index].allocCStringArray)
    elif not equip and isUsed:
      takeOffItem(memberIndex = memberIndex, itemIndex = index)
  resetSelection()
  return sortCrewInventoryCommand(clientData = clientData, interp = interp,
      argc = 2, argv = @["SortCrewInventory", "-1"].allocCStringArray)

proc toggleInventoryItemCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [], exportc.} =
  ## Select or deselect the selected item in the inventory
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ToggleInventoryItem rowindex, itemindex
  ## Rowindex is the index of the row in which is the selected item,
  ## itemindex is the index of the selected item in crew member inventory.
  let row = try:
      ($argv[1]).parseInt
    except:
      return showError(message = "Can't take the number of the row.")
  toggleCheckedButton(table = inventoryTable, row = row, column = 1)
  if isChecked(table = inventoryTable, row = row, column = 1):
    tclSetVar(varName = "invindex" & $argv[2], newValue = "1")
  else:
    tclUnsetVar(varName = "invindex" & $argv[2])
  return tclOk

proc showInventoryItemInfoCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [], exportc.} =
  ## Show detailed information about the selected item in crew member
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ShowInventoryItemInfo memberindex itemindex
  ## itemindex is the index of the item which will be show
  var selection = false
  for index, _ in playerShip.crew[memberIndex].inventory:
    if tclGetVar(varName = "invindex" & $(index + 1)) == "1":
      selection = true
      break
  if selection:
    let itemsMenu = createDialog(name = ".itemsmenu",
        title = "Selected items actions", parentName = ".memberdialog")

    proc addButton(name, label, command: string) =
      let button = itemsMenu & name
      tclEval(script = "ttk::button " & button & " -text {" & label &
          "} -command {CloseDialog " & itemsMenu & " .memberdialog;" & command & "}")
      tclEval(script = "grid " & button & " -sticky we -padx 5" & (
          if command.len == 0: " -pady {0 3}" else: ""))
      tclEval(script = "bind " & button & " <Escape> {CloseDialog " &
          itemsMenu & " .memberdialog;break}")
      if command.len == 0:
        tclEval(script = "bind " & button & " <Tab> {focus " & itemsMenu & ".equip;break}")
        tclEval(script = "focus " & button)

    addButton(name = ".equip", label = "Equip items",
        command = "ToggleInventoryItems equip")
    addButton(name = ".unequip", label = "Unequip items",
        command = "ToggleInventoryItems unequip")
    addButton(name = ".move", label = "Move items to the ship's cargo",
        command = "MoveItems")
    addButton(name = ".close", label = "Close", command = "")
    showDialog(dialog = itemsMenu, parentFrame = ".memberdialog")
    return tclOk
  let
    itemIndex = try:
        ($argv[1]).parseInt - 1
      except:
        return showError(message = "Can't get the index of the item.")
    itemType = try:
          itemsList[playerShip.crew[memberIndex].inventory[
              itemIndex].protoIndex].itemType
        except:
          return showError(message = "Can't get the type of the item.")
    typesArray: array[1 .. 6, string] = [weaponType, shieldType, headArmor,
        chestArmor, armsArmor, legsArmor]
  var equipable = itemType in toolsList
  for iType in typesArray:
    if iType == itemType:
      equipable = true
      break
  let used = itemIsUsed(memberIndex = memberIndex, itemIndex = itemIndex)
  try:
    showInventoryItemInfo(parent = ".memberdialog", memberIndex = memberIndex,
        itemIndex = itemIndex, button1 = ButtonSettings(text: "Move",
        command: "ShowMoveItem " & $argv[1], icon: "cargoicon",
        tooltip: "Move the selected item to the ship's cargo", color: ""),
        button2 = (if equipable: ButtonSettings(text: (
        if used: "Unequip" else: "Equip"), command: "SetUseItem " & $argv[1],
        icon: (if used: "unequipicon" else: "equipicon"), tooltip: (
        if used: "Stop" else: "Start") & " using the selected item",
        color: "green") else: emptyButtonSettings))
  except:
    return showError(message = "Can't show the information about the item.")
  return tclOk

proc validateMoveAmountCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [], exportc.} =
  ## Validate amount of the item to move
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ValidateMoveAmount maxvalue amount button spinbox
  try:
    var amount = 0
    if argv[2].len > 0:
      amount = ($argv[2]).parseInt
    let
      button = $argv[3]
      maxVal = ($argv[1]).parseInt
      spinBox = $argv[4]
    if amount < 1:
      tclEval(script = button & " configure -state disabled")
      tclSetResult(value = "1")
      return tclOk
    elif amount > maxVal:
      tclEval(script = spinBox & " set " & $maxVal)
    tclEval(script = button & " configure -state normal")
    tclSetResult(value = "1")
  except:
    tclSetResult(value = "0")
  return tclOk

proc addCommands*() {.sideEffect, raises: [], tags: [].} =
  ## Adds Tcl commands related to the crew UI
  try:
    discard
#    addCommand("ShowMemberInventory", showMemberInventoryCommand)
#    addCommand("SortCrewInventory", sortCrewInventoryCommand)
#    addCommand("ShowMoveItem", showMoveItemCommand)
#    addCommand("ToggleAllInventory", toggleAllInventoryCommand)
#    addCommand("SetUseItem", setUseItemCommand)
#    addCommand("UpdateInventory", updateInventoryCommand)
#    addCommand("MoveItem", moveItemCommand)
#    addCommand("MoveItems", moveItemsCommand)
#    addCommand("ToggleInventoryItems", toggleInventoryItemsCommand)
#    addCommand("ToggleInventoryItem", toggleInventoryItemCommand)
#    addCommand("ShowInventoryItemInfo", showInventoryItemInfoCommand)
#    addCommand("ValidateMoveAmount", validateMoveAmountCommand)
  except:
    showError(message = "Can't add a Tcl command.")

# Temporary code for interfacing with Ada

proc moveAdaItem(itemIndex, amount: cint) {.sideEffect, raises: [], tags: [
    RootEffect], exportc.} =
  try:
    moveItem(itemIndex = itemIndex - 1, amount = amount)
  except:
    echo getCurrentExceptionMsg()
