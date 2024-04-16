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
    argv: openArray[cstring]): TclResults {.sideEffect, raises: [], tags: [].} =
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
        ($argv[1]).parseInt
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
  let freeSpaceLabel = memberFrame & ".freespace"
  tclEval(script = "ttk::label " & freeSpaceLabel &
      " -text {Free inventory space: " & $freeInventory(
      memberIndex = memberIndex, amount = 0) & " kg} -wraplength 400")
  tclEval(script = "grid " & freeSpaceLabel)
  var height = try:
      10 + tclEval2(script = "winfo reqheight " &
        freeSpaceLabel).parseInt
    except:
      return showError(message = "Can't count the height of the label.")
  let buttonsBox = memberFrame & ".selectbox"
  tclEval(script = "ttk::frame " & buttonsBox)
  let selectAllButton = buttonsBox & ".selectallbutton"
  tclEval(script = "ttk::button " & selectAllButton & " -image selectallicon -command {ToggleAllInventory select} -style Small.TButton")
  tclEval(script = "tooltip::tooltip " & selectAllButton & " \"Select all items.\"")
  tclEval(script = "grid " & selectAllButton & " -sticky w")
  let unselectAllButton = buttonsBox & ".unselectallbutton"
  tclEval(script = "ttk::button " & unselectAllButton & " -image unselectallicon -command {ToggleInventory unselect} -style Small.TButton")
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

proc addCommands*() {.sideEffect, raises: [], tags: [].} =
  ## Adds Tcl commands related to the crew UI
  try:
    addCommand("UpdateInventory", updateInventoryCommand)
    addCommand("ShowMemberInventory", showMemberInventoryCommand)
  except:
    showError(message = "Can't add a Tcl command.")
