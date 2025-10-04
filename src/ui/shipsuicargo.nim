# Copyright 2024-2025 Bartek thindil Jasicki
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

## Provides code related to the information about the player's ship's cargo,
## like shoing its list, moving to crew members' inventory, dropping items
## from it, etc.

import std/[algorithm, strutils, tables]
import contracts, nimalyzer
import ../[config, crewinventory, game, items, messages, missions, shipscargo,
    stories, tk, types]
import coreui, dialogs, dialogs2, errordialog, table, updateheader, utilsui2

{.push ruleOff: "varDeclared".}
var
  cargoTable: TableWidget
    ## The UI table with all items of the player's ship's cargo
  cargoIndexes: seq[Natural] = @[]
    ## The list of indexes of the items in the cargo
{.pop ruleOn: "varDeclared".}

proc showCargoCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [
    RootEffect], cdecl, contractual, ruleOff: "params".} =
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
    shipCanvas: string = mainPaned & ".shipinfoframe.cargo.canvas"
    cargoInfoFrame: string = shipCanvas & ".frame"
    res: seq[string] = tclEval2(script = "grid size " & cargoInfoFrame).split
    rows: int = try:
        res[1].parseInt
      except:
        return showError(message = "Can't get the amount of rows")
  deleteWidgets(startIndex = 3, endIndex = rows - 1, frame = cargoInfoFrame)
  cargoTable = createTable(parent = cargoInfoFrame, headers = @["Name",
      "Durability", "Quality", "Type", "Amount", "Weight"],
      scrollbar = mainPaned & ".shipinfoframe.cargo.scrolly",
      command = "SortShipCargo",
      tooltipText = "Press mouse button to sort the cargo.")
  if cargoIndexes.len != playerShip.cargo.len:
    cargoIndexes = @[]
    for index, _ in playerShip.cargo:
      cargoIndexes.add(y = index)
  let freeSpaceLabel: string = cargoInfoFrame & ".freeframe.freespace2"
  try:
    tclEval(script = freeSpaceLabel & " configure -text {" &
        $freeCargo(amount = 0) & " kg}")
  except:
    return showError(message = "Can't show the amount of free space.")
  var itemsTypes: string = "All"
  for index in cargoIndexes:
    let
      item: InventoryData = playerShip.cargo[index]
      protoItem: ObjectData = try:
          itemsList[item.protoIndex]
        except:
          return showError(message = "Can't get proto item")
      itemType: string = (if protoItem.showType.len >
          0: protoItem.showType else: protoItem.itemType)
    if "{" & itemType & "}" notin itemsTypes:
      itemsTypes.add(y = " {" & itemType & "}")
  let
    page: Positive = try:
        (if argc == 2: ($argv[1]).parseInt else: 1)
      except:
        return showError(message = "Can't get the number of page.")
    startRow: Positive = ((page - 1) * gameSettings.listsLimit) + 1
    typeBox: string = cargoInfoFrame & ".selecttype.combo"
    itemsType: string = tclEval2(script = typeBox & " get")
  tclEval(script = "grid configure " & cargoTable.canvas & " -row 3")
  var currentRow: Positive = 1
  for index in cargoIndexes:
    if currentRow < startRow:
      currentRow.inc
      continue
    let
      item: InventoryData = playerShip.cargo[index]
      protoItem: ObjectData = try:
          itemsList[item.protoIndex]
        except:
          return showError(message = "Can't get the proto item.")
      itemType: string = (if protoItem.showType.len >
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
    addButton(table = cargoTable, text = ($item.quality).capitalizeAscii,
        tooltip = "The quality of the selected item",
        command = "ShowCargoItemInfo " & $(index + 1), column = 3)
    addButton(table = cargoTable, text = itemType,
        tooltip = "The type of the selected item",
        command = "ShowCargoItemInfo " & $(index + 1), column = 4)
    addButton(table = cargoTable, text = $item.amount,
        tooltip = "The amount of the selected item",
        command = "ShowCargoItemInfo " & $(index + 1), column = 5)
    addButton(table = cargoTable, text = $(item.amount * protoItem.weight) &
        " kg", tooltip = "The total weight of the selected item",
        command = "ShowCargoItemInfo " & $(index + 1), column = 6, newRow = true)
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
  nameAsc, nameDesc, durabilityAsc, durabilityDesc, qualityAsc, qualityDesc,
    typeAsc, typeDesc, amountAsc, amountDesc, weightAsc, weightDesc, none

const defaultCargoSortOrder: cargoSortOrders = none

var cargoSortOrder: cargoSortOrders = defaultCargoSortOrder

proc sortCargoCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [
    RootEffect], cdecl, contractual.} =
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
        (if argv[1] == "-1": Positive.high else: getColumnNumber(
            table = cargoTable, xPosition = ($argv[1]).parseInt))
      except:
        return showError(message = "Can't get the column number.")
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
    if cargoSortOrder == qualityAsc:
      cargoSortOrder = qualityDesc
    else:
      cargoSortOrder = qualityAsc
  of 4:
    if cargoSortOrder == typeAsc:
      cargoSortOrder = typeDesc
    else:
      cargoSortOrder = typeAsc
  of 5:
    if cargoSortOrder == amountAsc:
      cargoSortOrder = amountDesc
    else:
      cargoSortOrder = amountAsc
  of 6:
    if cargoSortOrder == weightAsc:
      cargoSortOrder = weightDesc
    else:
      cargoSortOrder = weightAsc
  else:
    discard
  if cargoSortOrder == none:
    return showCargoCommand(clientData = clientData, interp = interp, argc = 1,
        argv = @["ShowCargo"].allocCStringArray)
  type LocalCargoData = object
    name: string
    damage: float
    itemType: string
    amount: Positive = 1
    weight: Positive = 1
    quality: ObjectQuality
    id: Natural
  var localCargo: seq[LocalCargoData] = @[]
  for index, item in playerShip.cargo:
    try:
      localCargo.add(y = LocalCargoData(name: getItemName(item = item,
          damageInfo = false, toLower = false), damage: (item.durability.float /
          defaultItemDurability.float), itemType: (if itemsList[
          item.protoIndex].showType.len > 0: itemsList[
          item.protoIndex].showType else: itemsList[item.protoIndex].itemType),
          amount: item.amount, weight: item.amount * itemsList[
          item.protoIndex].weight, quality: item.quality, id: index))
    except:
      return showError(message = "Can't add local item to cargo.")

  proc sortCargo(x, y: LocalCargoData): int {.raises: [], tags: [],
      contractual.} =
    ## Compare two items and return which should go first, based on the sort
    ## order of the items
    ##
    ## * x - the first item to compare
    ## * y - the second item to compare
    ##
    ## Returns 1 if the first item should go first, -1 if the second item
    ## should go first.
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
    of qualityAsc:
      if x.quality < y.quality:
        return 1
      else:
        return -1
    of qualityDesc:
      if x.quality > y.quality:
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
      argv = @["ShowCargo"].allocCStringArray)

proc showGiveItemCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [WriteIOEffect,
        TimeEffect, RootEffect], cdecl, contractual.} =
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
    itemIndex: Natural = try:
        ($argv[1]).parseInt - 1
      except:
        return showError(message = "Can't get the item's index.")
    itemDialog: string = createDialog(name = ".itemdialog", title = "Give " &
        getItemName(item = playerShip.cargo[itemIndex]) &
        " from the ship's cargo to the selected crew member", titleWidth = 370, columns = 3)
  var label: string = itemDialog & ".memberlbl"
  tclEval(script = "ttk::label " & label & " -text {To:}")
  tclEval(script = "grid " & label)
  var membersNames: string = ""
  for member in playerShip.crew:
    membersNames.add(y = " " & member.name)
  let crewBox: string = itemDialog & ".member"
  tclEval(script = "ttk::combobox " & crewBox & " -state readonly -width 14")
  tclEval(script = crewBox & " configure -values [list " & membersNames & "]")
  tclEval(script = crewBox & " current 0")
  tclEval(script = "grid " & crewBox & " -column 1 -row 1")
  tclEval(script = "bind " & crewBox & " <Escape> {" & itemDialog & ".cancelbutton invoke;break}")
  tclEval(script = "bind " & crewBox & " <<ComboboxSelected>> {UpdateMaxGiveAmount " &
      $argv[1] & "}")
  var button: string = itemDialog & ".maxbutton"
  tclEval(script = "ttk::button " & button)
  tclEval(script = "grid " & button & " -row 2 -pady {0 5}")
  tclEval(script = "bind " & button & " <Escape> {" & itemDialog & ".cancelbutton invoke;break}")
  tclEval(script = "tooltip::tooltip " & button & " \"Set the max amount as amount to give for the selected crew member.\"")
  let amountBox: string = itemDialog & ".giveamount"
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
      " -image cancelicon -command {CloseDialog " & itemDialog & "} -style Dialogred.TButton -text Cancel")
  tclEval(script = "grid " & button & " -column 1 -row 4 -padx {5 15} -pady 5 -sticky w")
  tclEval(script = "tooltip::tooltip " & button & " \"Cancel giving and close dialog. \\[Escape key\\]\"")
  tclEval(script = "focus " & button)
  tclEval(script = "bind " & button & " <Tab> {focus .itemdialog.maxbutton;break}")
  tclEval(script = "bind " & button & " <Escape> {" & button & " invoke;break}")
  showDialog(dialog = itemDialog)
  tclEval(script = "event generate " & crewBox & " <<ComboboxSelected>>")
  return tclOk

proc giveItemCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [
    RootEffect], cdecl, contractual.} =
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
  const
    itemDialog: string = ".itemdialog"
    spinBox: string = itemDialog & ".giveamount"
    comboBox: string = itemDialog & ".member"
  let
    amount: Natural = try:
        tclEval2(script = spinBox & " get").parseInt
      except:
        return showError(message = "Can't get the amount.")
    memberIndex: Natural = try:
        tclEval2(script = comboBox & " current").parseInt
      except:
        return showError(message = "Can't ge the member's index.")
    itemIndex: int = try:
        ($argv[1]).parseInt - 1
      except:
        return showError(message = "Can't get the item's index.")
    item: InventoryData = playerShip.cargo[itemIndex]
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
        price = item.price, ship = playerShip, quality = item.quality)
  except:
    return showError(message = "Can't update the member's inventory.")
  updateCargo(ship = playerShip, amount = -amount, cargoIndex = itemIndex,
      price = item.price, quality = item.quality)
  discard closeDialogCommand(clientData = clientData, interp = interp, argc = 2,
      argv = @["CloseDialog", ".itemdialog"].allocCStringArray)
  updateHeader()
  updateMessages()
  return sortCargoCommand(clientData = clientData, interp = interp, argc = 2,
      argv = @["SortShipCargo", "-1"].allocCStringArray)

proc showDropItemCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [WriteIOEffect,
        TimeEffect, RootEffect], cdecl, contractual.} =
  ## Show UI to drop the selected item from the ship cargo
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ShowDropItem itemindex
  ## Itemindex is the index of the item which will be set
  let itemIndex: Natural = try:
      ($argv[1]).parseInt - 1
    except:
      return showError(message = "Can't get the item's index.")
  showManipulateItem(title = "Drop " & getItemName(item = playerShip.cargo[
      itemIndex]) & " from the ship's cargo", command = "DropItem " & $argv[1],
      action = "drop", itemIndex = itemIndex)
  return tclOk

proc dropItemCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [
    RootEffect], cdecl, contractual.} =
  ## Drop selected amount of the selected item from the ship's cargo
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## DropItem
  const
    itemDialog: string = ".itemdialog"
    spinBox: string = itemDialog & ".amount"
  var dropAmount, dropAmount2: Natural = try:
      tclEval2(script = spinBox & " get").parseInt
    except:
      return showError(message = "Can't get the drop amount.")
  let itemIndex: Natural = try:
      ($argv[1]).parseInt - 1
    except:
      return showError(message = "Can't get the item's index.")
  try:
    if itemsList[playerShip.cargo[itemIndex].protoIndex].itemType == missionItemsType:
      for j in 1 .. dropAmount2:
        for index, mission in acceptedMissions:
          if mission.mType == deliver and mission.itemIndex == playerShip.cargo[
              itemIndex].protoIndex:
            deleteMission(missionIndex = index)
            dropAmount.dec
            break
    elif currentStory.index.len > 0 and storiesList[
        currentStory.index].startData[0].parseInt == playerShip.cargo[
            itemIndex].protoIndex:
      clearCurrentStory()
  except:
    return showError(message = "Can't check the drop amount.")
  if dropAmount > 0:
    addMessage(message = "You dropped " & $dropAmount & " " & getItemName(
        item = playerShip.cargo[itemIndex]) & ".", mtype = otherMessage)
    updateCargo(ship = playerShip, protoIndex = playerShip.cargo[
        itemIndex].protoIndex, amount = -dropAmount,
        durability = playerShip.cargo[itemIndex].durability,
        price = playerShip.cargo[itemIndex].price,
        quality = playerShip.cargo[itemIndex].quality)
  discard closeDialogCommand(clientData = clientData, interp = interp, argc = 2,
      argv = @["CloseDialog", ".itemdialog"].allocCStringArray)
  updateHeader()
  updateMessages()
  return sortCargoCommand(clientData = clientData, interp = interp, argc = 2,
      argv = @["SortShipCargo", "-1"].allocCStringArray)

proc showCargoItemInfoCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [WriteIOEffect,
        TimeEffect, RootEffect], cdecl, contractual.} =
  ## Show information about the selected item in the player's ship cargo
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ShowCargoItemInfo itemindex
  ## Itemindex is the index of the item which information will be show
  let itemIndex: Natural = try:
      ($argv[1]).parseInt - 1
    except:
      return showError(message = "Can't get the item's index.")
  try:
    showInventoryItemInfo(parent = ".", itemIndex = itemIndex, memberIndex = -1,
        button1 = ButtonSettings(tooltip: "Give item to a crew member",
        command: "ShowGiveItem " & $argv[1], icon: "giveicon", text: "Give",
        color: ""), button2 = ButtonSettings(
        tooltip: "Drop item from the ship cargo", command: "ShowDropItem " &
        $argv[1], icon: "dropicon", text: "Drop", color: ""))
  except:
    return showError(message = "Can't show the item's info.")
  return tclOk

proc updateMaxGiveAmountCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [WriteIOEffect,
        TimeEffect, RootEffect], cdecl, contractual.} =
  ## Update max give amount after selecting the crew member
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## UpdateMaxGiveAmount itemindex
  ## ItemIndex is the index of the item to give
  const crewBox: string = ".itemdialog.member"
  let
    itemIndex: Natural = try:
        ($argv[1]).parseInt
      except:
        return showError(message = "Can't get the item's index.")
    item: InventoryData = playerShip.cargo[itemIndex]
    memberIndex: Natural = try:
        tclEval2(script = crewBox & " current").parseInt
      except:
        return showError(message = "Can't get the member's index.")
  var maxAmount: Natural = try:
        (freeInventory(memberIndex = memberIndex, amount = 0).float / itemsList[
            item.protoIndex].weight.float).Natural
      except:
        return showError(message = "Can't count the max amount.")
  if item.amount < maxAmount:
    maxAmount = item.amount
  const amountBox: string = ".itemdialog.giveamount"
  try:
    if tclEval2(script = amountBox & " get").parseInt > maxAmount:
      tclEval(script = amountBox & " set " & $maxAmount)
  except:
    showError(message = "Can't set the max amount.")
  tclEval(script = amountBox & " configure -to " & $maxAmount)
  const maxButton: string = ".itemdialog.maxbutton"
  tclEval(script = maxButton & " configure -text {Amount (max: " & $maxAmount &
      "):} -command {" & amountBox & " set " & $maxAmount & ";" & amountBox & " validate}")
  return tclOk

proc addCommands*() {.raises: [], tags: [WriteIOEffect, TimeEffect,
    RootEffect], contractual.} =
  ## Adds Tcl commands related to the crew UI
  try:
    addCommand(name = "ShowCargo", nimProc = showCargoCommand)
    addCommand(name = "SortShipCargo", nimProc = sortCargoCommand)
    addCommand(name = "ShowGiveItem", nimProc = showGiveItemCommand)
    addCommand(name = "GiveItem", nimProc = giveItemCommand)
    addCommand(name = "ShowDropItem", nimProc = showDropItemCommand)
    addCommand(name = "DropItem", nimProc = dropItemCommand)
    addCommand(name = "ShowCargoItemInfo", nimProc = showCargoItemInfoCommand)
    addCommand(name = "UpdateMaxGiveAmount",
        nimProc = updateMaxGiveAmountCommand)
  except:
    showError(message = "Can't add a Tcl command.")
