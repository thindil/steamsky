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

import std/[strutils, tables]
import ../[config, crewinventory, game, items, tk]
import dialogs, table

var
  inventoryTable: TableWidget
    ## The UI table with the list of items in the crew member's inventory
  memberIndex: Natural
    ## The index of the selected crew member
  inventoryIndexes: seq[Natural]
    ## The list of indexes of items in the crew member's inventory

proc updateInventoryCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults {.sideEffect, raises: [], tags: [].} =
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
      ($argv[1]).parseInt
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
    argv: openArray[cstring]): TclResults =
  let localMemberIndex = ($argv[1]).parseInt
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
  return tclOk

proc addCommands*() {.sideEffect, raises: [], tags: [].} =
  ## Adds Tcl commands related to the crew UI
  try:
    addCommand("UpdateInventory", updateInventoryCommand)
  except:
    showError(message = "Can't add a Tcl command.")
