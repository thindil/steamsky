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
import ../[config, game, items, shipscargo, tk]
import coreui, table

var
  cargoTable: TableWidget
    ## The UI table with all items of the player's ship's cargo
  cargoIndexes: seq[Natural]
    ## The list of indexes of the items in the cargo

proc showCargoCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults {.exportc.} =
  let
    shipCanvas = mainPaned & ".shipinfoframe.cargo.canvas"
    cargoInfoFrame = shipCanvas & ".frame"
    res = tclEval2(script = "grid size " & cargoInfoFrame).split
    rows = res[1].parseInt
  deleteWidgets(startIndex = 3, endIndex = rows - 1, frame = cargoInfoFrame)
  cargoTable = createTable(parent = cargoInfoFrame, headers = @["", "Name",
      "Durability", "Type", "Amount", "Weight"],
      scrollbar = mainPaned & ".shipinfoframe.cargo.scrolly",
      command = "SortShipCargo",
      tooltipText = "Press mouse button to sort the cargo.")
  if cargoIndexes.len != playerShip.cargo.len:
    cargoIndexes = @[]
    for index, _ in playerShip.cargo:
      cargoIndexes.add(y = index)
  let freeSpaceLabel = cargoInfoFrame & ".freespace"
  tclEval(script = freeSpaceLabel & " configure -text {Free cargo space: " &
      $freeCargo(amount = 0) & " kg}")
  var itemsTypes = "All"
  for index in cargoIndexes:
    let
      item = playerShip.cargo[index]
      protoItem = itemsList[item.protoIndex]
      itemType = (if protoItem.showType.len >
          0: protoItem.showType else: protoItem.itemType)
    if "{" & itemType & "}" notin itemsTypes:
      itemsTypes.add(y = " {" & itemType & "}")
  let
    page = (if argc == 2: ($argv[1]).parseInt else: 1)
    startRow = ((page - 1) * gameSettings.listsLimit) + 1
    typeBox = cargoInfoFrame & ".selecttype.combo"
    itemsType = tclEval2(script = typeBox & " get")
  var currentRow = 1
  for index in cargoIndexes:
    if currentRow < startRow:
      currentRow.inc
      continue
    let
      item = playerShip.cargo[index]
      protoItem = itemsList[item.protoIndex]
      itemType = (if protoItem.showType.len >
          0: protoItem.showType else: protoItem.itemType)
    if itemsType != "All" and itemType != itemsType:
      continue
    addButton(table = cargoTable, text = getItemName(item = item),
        tooltip = "Show item's description and actions",
        command = "ShowCargoItemInfo " & $(index + 1), column = 1)
    addProgressbar(table = cargoTable, value = item.durability,
        maxValue = defaultItemDurability,
        tooltip = "The current durability of the selected item",
        command = "ShowCargoItemInfo " & $(index + 1), column = 2)
  return tclOk

proc addCommands*() {.sideEffect, raises: [], tags: [].} =
  ## Adds Tcl commands related to the crew UI
  try:
    discard
#    addCommand("ShowCargo", showCargoCommand)
  except:
    showError(message = "Can't add a Tcl command.")
