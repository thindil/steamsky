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
import ../[config, crewinventory, game, items, messages, shipscargo, tk, types]
import coreui, dialogs, table, updateheader, utilsui2

var
  cargoTable: TableWidget
    ## The UI table with all items of the player's ship's cargo
  cargoIndexes: seq[Natural]
    ## The list of indexes of the items in the cargo

proc showCargoCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults {.sideEffect, raises: [], tags: [
    RootEffect], exportc.} =
  ## Show the cargo of the player ship
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ShowCargo ?page?
  ## Optional paramater page is the number of the page of cargo list to show
  let
    shipCanvas = mainPaned & ".shipinfoframe.cargo.canvas"
    cargoInfoFrame = shipCanvas & ".frame"
    res = tclEval2(script = "grid size " & cargoInfoFrame).split
    rows = try:
        res[1].parseInt
      except:
        return showError(message = "Can't get the amount of rows")
  deleteWidgets(startIndex = 3, endIndex = rows - 1, frame = cargoInfoFrame)
  cargoTable = createTable(parent = cargoInfoFrame, headers = @["Name",
      "Durability", "Type", "Amount", "Weight"], scrollbar = mainPaned &
      ".shipinfoframe.cargo.scrolly", command = "SortShipCargo",
      tooltipText = "Press mouse button to sort the cargo.")
  if cargoIndexes.len != playerShip.cargo.len:
    cargoIndexes = @[]
    for index, _ in playerShip.cargo:
      cargoIndexes.add(y = index)
  let freeSpaceLabel = cargoInfoFrame & ".freespace"
  try:
    tclEval(script = freeSpaceLabel & " configure -text {Free cargo space: " &
        $freeCargo(amount = 0) & " kg}")
  except:
    return showError(message = "Can't show the amount of free space.")
  var itemsTypes = "All"
  for index in cargoIndexes:
    let
      item = playerShip.cargo[index]
      protoItem = try:
          itemsList[item.protoIndex]
        except:
          return showError(message = "Can't get proto item")
      itemType = (if protoItem.showType.len >
          0: protoItem.showType else: protoItem.itemType)
    if "{" & itemType & "}" notin itemsTypes:
      itemsTypes.add(y = " {" & itemType & "}")
  let
    page = try:
        (if argc == 2: ($argv[1]).parseInt else: 1)
      except:
        return showError(message = "Can't get the number of page.")
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
      protoItem = try:
          itemsList[item.protoIndex]
        except:
          return showError(message = "Can't get the proto item.")
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
    addButton(table = cargoTable, text = itemType,
        tooltip = "The type of the selected item",
        command = "ShowCargoItemInfo " & $(index + 1), column = 3)
    addButton(table = cargoTable, text = $item.amount,
        tooltip = "The amount of the selected item",
        command = "ShowCargoItemInfo " & $(index + 1), column = 4)
    addButton(table = cargoTable, text = $(item.amount * protoItem.weight) &
        " kg", tooltip = "The total weight of the selected item",
        command = "ShowCargoItemInfo " & $(index + 1), column = 5, newRow = true)
    if cargoTable.row == gameSettings.listsLimit + 1:
      break
  if page > 1:
    addPagination(table = cargoTable, previousCommand = "ShowCargo " & $(page -
        1), nextCommand = (if cargoTable.row < gameSettings.listsLimit +
        1: "" else: "ShowCargo " & $(page + 1)))
  elif cargoTable.row == gameSettings.listsLimit + 1:
    addPagination(table = cargoTable, previousCommand = "",
        nextCommand = "ShowCargo " & $(page + 1))
  updateTable(table = cargoTable)
  tclEval(script = typeBox & " configure -values [list " & itemsTypes & "]")
  tclEval(script = "update")
  tclEval(script = shipCanvas & " configure -scrollregion [list " & tclEval2(
      script = shipCanvas & " bbox all") & "]")
  tclEval(script = shipCanvas & " xview moveto 0.0")
  tclEval(script = shipCanvas & " yview moveto 0.0")
  return tclOk

type cargoSortOrders = enum
  nameAsc, nameDesc, durabilityAsc, durabilityDesc, typeAsc, typeDesc,
    amountAsc, amountDesc, weightAsc, weightDesc, none

const defaultCargoSortOrder = none

var cargoSortOrder = defaultCargoSortOrder

proc sortCargoCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults {.sideEffect, raises: [], tags: [
    RootEffect], exportc.} =
  ## Sort the player's ship's cargo list
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## SortShipCargo x
  ## X is X axis coordinate where the player clicked the mouse button
  let column: Positive = try:
      getColumnNumber(table = cargoTable, xPosition = ($argv[1]).parseInt)
    except:
      Positive.high
  case column
  of 1:
    if cargoSortOrder == nameAsc:
      cargoSortOrder = nameDesc
    else:
      cargoSortOrder = nameAsc
  of 2:
    if cargoSortOrder == durabilityAsc:
      cargoSortOrder = durabilityDesc
    else:
      cargoSortOrder = durabilityAsc
  of 3:
    if cargoSortOrder == typeAsc:
      cargoSortOrder = typeDesc
    else:
      cargoSortOrder = typeAsc
  of 4:
    if cargoSortOrder == amountAsc:
      cargoSortOrder = amountDesc
    else:
      cargoSortOrder = amountAsc
  of 5:
    if cargoSortOrder == weightAsc:
      cargoSortOrder = weightDesc
    else:
      cargoSortOrder = weightAsc
  else:
    discard
  if cargoSortOrder == none:
    return showCargoCommand(clientData = clientData, interp = interp, argc = 1,
        argv = @["ShowCargo".cstring])
  type LocalCargoData = object
    name: string
    damage: float
    itemType: string
    amount: Positive = 1
    weight: Positive = 1
    id: Natural
  var localCargo: seq[LocalCargoData]
  for index, item in playerShip.cargo:
    try:
      localCargo.add(y = LocalCargoData(name: getItemName(item = item,
          damageInfo = false, toLower = false), damage: (item.durability.float /
          defaultItemDurability.float), itemType: (if itemsList[
          item.protoIndex].showType.len > 0: itemsList[
          item.protoIndex].showType else: itemsList[item.protoIndex].itemType),
          amount: item.amount, weight: item.amount * itemsList[
          item.protoIndex].weight, id: index))
    except:
      return showError(message = "Can't add local item to cargo.")
  proc sortCargo(x, y: LocalCargoData): int =
    case cargoSortOrder
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
    of none:
      return -1
  localCargo.sort(cmp = sortCargo)
  cargoIndexes = @[]
  for item in localCargo:
    cargoIndexes.add(y = item.id)
  return showCargoCommand(clientData = clientData, interp = interp, argc = 1,
      argv = @["ShowCargo".cstring])

proc showGiveItemCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults {.sideEffect, raises: [], tags: [], exportc.} =
  ## Show UI to give the selected item from the ship cargo to the selected
  ## crew member
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ShowGiveItem itemindex
  ## Itemindex is the index of the item which will be set
  let
    itemIndex = try:
        ($argv[1]).parseInt - 1
      except:
        return showError(message = "Can't get the item's index.")
    itemDialog = createDialog(name = ".itemdialog", title = "Give " &
        getItemName(item = playerShip.cargo[itemIndex]) &
        " from the ship's cargo to the selected crew member", titleWidth = 370, columns = 3)
  var label = itemDialog & ".memberlbl"
  tclEval(script = "ttk::label " & label & " -text {To:}")
  tclEval(script = "grid " & label)
  var membersNames = ""
  for member in playerShip.crew:
    membersNames.add(y = " " & member.name)
  let crewBox = itemDialog & ".member"
  tclEval(script = "ttk::combobox " & crewBox & " -state readonly -width 14")
  tclEval(script = crewBox & " configure -values [list " & membersNames & "]")
  tclEval(script = crewBox & " current 0")
  tclEval(script = "grid " & crewBox & " -column 1 -row 1")
  tclEval(script = "bind " & crewBox & " <Escape> {" & itemDialog & ".cancelbutton invoke;break}")
  tclEval(script = "bind " & crewBox & " <<ComboboxSelected>> {UpdateMaxGiveAmount " &
      $argv[1] & "}")
  var button = itemDialog & ".maxbutton"
  tclEval(script = "ttk::button " & button)
  tclEval(script = "grid " & button & " -row 2 -pady {0 5}")
  tclEval(script = "bind " & button & " <Escape> {" & itemDialog & ".cancelbutton invoke;break}")
  tclEval(script = "tooltip::tooltip " & button & " \"Set the max amount as amount to give for the selected crew member.\"")
  let amountBox = itemDialog & ".giveamount"
  tclEval(script = "ttk::spinbox " & amountBox & " -width 14 -from 1 -to " &
      $playerShip.cargo[itemIndex].amount &
      " -validate key -validatecommand {CheckAmount %W " & $(itemIndex + 1) &
      " %P " & itemDialog & ".givebutton} -command {ValidateAmount " &
      itemDialog & ".giveamount " & $(itemIndex + 1) & " " & itemDialog & ".givebutton}")
  tclEval(script = amountBox & " set 1")
  tclEval(script = "grid " & amountBox & " -column 1 -row 2 -pady {0 5}")
  tclEval(script = "bind " & amountBox & " <Escape> {" & itemDialog & ".cancelbutton invoke;break}")
  label = itemDialog & ".errorlbl"
  tclEval(script = "ttk::label " & label & " -style Headerred.TLabel -wraplength 350")
  tclEval(script = "grid " & label & " -columnspan 2 -padx 5")
  tclEval(script = "grid remove " & label)
  button = itemDialog & ".givebutton"
  tclEval(script = "ttk::button " & button &
      " -image give2icon -command {GiveItem " & $argv[1] & "} -style Dialoggreen.TButton -text Give")
  tclEval(script = "grid " & button & " -column 0 -row 4 -padx 5 -pady 5 -sticky e")
  tclEval(script = "tooltip::tooltip " & button & " \"Give the item\"")
  tclEval(script = "bind " & button & " <Escape> {" & itemDialog & ".cancelbutton invoke;break}")
  button = itemDialog & ".cancelbutton"
  tclEval(script = "ttk::button " & button &
      " -image cancelicon -command {CloseDialog " & itemDialog & "} -style Dialogred.TButton -text Close")
  tclEval(script = "grid " & button & " -column 1 -row 4 -padx {5 15} -pady 5 -sticky w")
  tclEval(script = "tooltip::tooltip " & button & " \"Cancel giving and close dialog. \\[Escape key\\]\"")
  tclEval(script = "focus " & button)
  tclEval(script = "bind " & button & " <Tab> {focus .itemdialog.maxbutton;break}")
  tclEval(script = "bind" & button & " <Escape> {" & button & " invoke;break}")
  showDialog(dialog = itemDialog)
  tclEval(script = "event generate " & crewBox & " <<ComboboxSelected>>")
  return tclOk

proc giveItemCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults {.sideEffect, raises: [], tags: [
    RootEffect], exportc.} =
  ## Give selected amount of the selected item from the ship's cargo to the
  ## selected crew member
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## GiveItem
  let
    itemDialog = ".itemdialog"
    spinBox = itemDialog & ".giveamount"
    amount = try:
        tclEval2(script = spinBox & " get").parseInt
      except:
        return showError(message = "Can't get the amount.")
    comboBox = itemDialog & ".member"
    memberIndex = try:
        tclEval2(script = comboBox & " current").parseInt
      except:
        return showError(message = "Can't ge the member's index.")
    itemIndex = try:
        ($argv[1]).parseInt - 1
      except:
        return showError(message = "Can't get the item's index.")
    item = playerShip.cargo[itemIndex]
  try:
    if freeInventory(memberIndex = memberIndex, amount = -(itemsList[
        item.protoIndex].weight * amount)) < 0:
      showMessage(text = "No free space in " & playerShip.crew[
          memberIndex].name & "'s inventory for that amount of " & getItemName(
          item = item), title = "Can't give item")
      return tclOk
  except:
    return showError(message = "Can't get the item.")
  addMessage(message = "You gave " & $amount & " " & getItemName(item = item) &
      " to " & playerShip.crew[memberIndex].name & ".", mType = otherMessage)
  try:
    updateInventory(memberIndex = memberIndex, amount = amount,
        protoIndex = item.protoIndex, durability = item.durability,
        price = item.price, ship = playerShip)
  except:
    return showError(message = "Can't update the member's inventory.")
  updateCargo(ship = playerShip, amount = -amount, cargoIndex = itemIndex,
      price = item.price)
  tclEval(script = "destroy " & itemDialog)
  tclEval(script = "busy forget " & mainPaned)
  tclEval(script = "busy forget " & gameHeader)
  updateHeader()
  updateMessages()
  return sortCargoCommand(clientData = clientData, interp = interp, argc = 2,
      argv = @["SortShipCargo".cstring, "-1"])

proc showDropItemCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults {.sideEffect, raises: [], tags: [], exportc.} =
  let itemIndex = try:
      ($argv[1]).parseInt - 1
    except:
      return showError(message = "Can't get the item's index.")
  showManipulateItem(title = "Drop " & getItemName(item = playerShip.cargo[
      itemIndex]) & " from the ship's cargo", command = "DropItem " & $argv[1],
      action = "drop", itemIndex = itemIndex)
  return tclOk

proc addCommands*() {.sideEffect, raises: [], tags: [].} =
  ## Adds Tcl commands related to the crew UI
  try:
    discard
#    addCommand("ShowCargo", showCargoCommand)
#    addCommand("SortShipCargo", sortCargoCommand)
#    addCommand("ShowGiveItem", showGiveItemCommand)
#    addCommand("GiveItem", giveItemCommand)
#    addCommand("ShowDropItem", showDropItemCommand)
  except:
    showError(message = "Can't add a Tcl command.")
