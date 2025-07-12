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

import std/[algorithm, math, strutils, tables]
import ../[basestypes, basescargo, config, crewinventory, events, game, items,
    maps, shipscargo, tk, trades, types]
import coreui, dialogs, dialogs2, errordialog, mapsui, table, updateheader, utilsui2

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
    argv: cstringArray): TclResults {.raises: [], tags: [
    RootEffect], cdecl.} =
  ## Show information about trading
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ShowTrade ?itemtype? ?searchstring?
  ## Itemtype is type of items to show, searchstring is string which is
  ## looking for in items names
  var tradeFrame = mainPaned & ".tradeframe"
  let
    tradeCanvas = tradeFrame & ".canvas"
    baseIndex = skyMap[playerShip.skyX][playerShip.skyY].baseIndex
    eventIndex = skyMap[playerShip.skyX][playerShip.skyY].eventIndex
  var label = tradeCanvas & ".trade.options.playerinfo.moneyinfo"
  if tclEval2(script = "winfo exists " & label) == "0":
    tclEval(script = """
      ttk::frame .gameframe.paned.tradeframe
      set tradecanvas [canvas .gameframe.paned.tradeframe.canvas \
         -yscrollcommand [list .gameframe.paned.tradeframe.scrolly set] \
         -xscrollcommand [list .gameframe.paned.tradeframe.scrollx set]]
      pack [ttk::scrollbar .gameframe.paned.tradeframe.scrolly -orient vertical \
         -command [list $tradecanvas yview]] -side right -fill y
      pack $tradecanvas -side top -fill both
      pack [ttk::scrollbar .gameframe.paned.tradeframe.scrollx -orient horizontal \
         -command [list $tradecanvas xview]] -fill x
      SetScrollbarBindings $tradecanvas .gameframe.paned.tradeframe.scrolly
      ::autoscroll::autoscroll .gameframe.paned.tradeframe.scrolly
      ::autoscroll::autoscroll .gameframe.paned.tradeframe.scrollx
      set tradeframe [ttk::frame $tradecanvas.trade]
      SetScrollbarBindings $tradeframe .gameframe.paned.tradeframe.scrolly
      # Type of items to show
      grid [ttk::frame $tradeframe.options] -sticky w
      SetScrollbarBindings $tradeframe.options .gameframe.paned.tradeframe.scrolly
      ttk::label $tradeframe.options.typelabel -text {Type:}
      SetScrollbarBindings $tradeframe.options.typelabel \
         .gameframe.paned.tradeframe.scrolly
      ttk::combobox $tradeframe.options.type -state readonly
      bind $tradeframe.options.type <<ComboboxSelected>> \
         {ShowTrade [$tradeframe.options.type get]}
      tooltip::tooltip $tradeframe.options.type {Show only items of the selected type}
      ttk::entry $tradeframe.options.search -validate key \
         -validatecommand {SearchTrade %P}
      tooltip::tooltip $tradeframe.options.search {Enter a name of an item which you looking for}
      grid [ttk::frame $tradeframe.options.playerinfo] -sticky nw -columnspan 2 -row 1
      SetScrollbarBindings $tradeframe.options.playerinfo \
         .gameframe.paned.tradeframe.scrolly
      grid [ttk::label $tradeframe.options.playerinfo.moneyinfo -wraplength 300] \
         -sticky w
      SetScrollbarBindings $tradeframe.options.playerinfo.moneyinfo \
         .gameframe.paned.tradeframe.scrolly
      ttk::label $tradeframe.options.playerinfo.moneyinfo2 -wraplength 300 \
         -style Golden.TLabel
      SetScrollbarBindings $tradeframe.options.playerinfo.moneyinfo2 \
         .gameframe.paned.tradeframe.scrolly
      grid [ttk::frame $tradeframe.options.playerinfo.cargoinfo] -columnspan 2
      SetScrollbarBindings $tradeframe.options.playerinfo.cargoinfo \
         .gameframe.paned.tradeframe.scrolly
      grid [ttk::label $tradeframe.options.playerinfo.cargoinfo.cargoinfo -wraplength 300 -text {Free cargo space is }] \
         -sticky w
      SetScrollbarBindings $tradeframe.options.playerinfo.cargoinfo.cargoinfo \
         .gameframe.paned.tradeframe.scrolly
      grid [ttk::label $tradeframe.options.playerinfo.cargoinfo.cargoinfo2 -wraplength 300 \
         -style Golden.TLabel] -sticky w -row 0 -column 1
      SetScrollbarBindings $tradeframe.options.playerinfo.cargoinfo.cargoinfo2 \
         .gameframe.paned.tradeframe.scrolly
      grid [ttk::frame $tradeframe.options.baseinfo] -sticky nw -column 2 -row 1
      SetScrollbarBindings $tradeframe.options.baseinfo \
         .gameframe.paned.tradeframe.scrolly
      grid [ttk::label $tradeframe.options.baseinfo.baseinfo -wraplength 300]
      SetScrollbarBindings $tradeframe.options.baseinfo.baseinfo \
         .gameframe.paned.tradeframe.scrolly
      grid [ttk::label $tradeframe.options.baseinfo.baseinfo2 -wraplength 300 \
         -style Golden.TLabel] -sticky w -row 0 -column 1
      SetScrollbarBindings $tradeframe.options.baseinfo.baseinfo2 \
         .gameframe.paned.tradeframe.scrolly
      grid [ttk::frame $tradeframe.options.baseinfo.basecargoinfo]
      SetScrollbarBindings $tradeframe.options.baseinfo.basecargoinfo \
         .gameframe.paned.tradeframe.scrolly
      grid [ttk::label $tradeframe.options.baseinfo.basecargoinfo.baseinfo -wraplength 300 -text {Base has}]
      SetScrollbarBindings $tradeframe.options.basecargoinfo.baseinfo \
         .gameframe.paned.tradeframe.scrolly
      grid [ttk::label $tradeframe.options.baseinfo.basecargoinfo.baseinfo2 -wraplength 300 \
         -style Golden.TLabel] -sticky w -row 0 -column 1
      SetScrollbarBindings $tradeframe.options.baseinfo.basecargoinfo.baseinfo2 \
         .gameframe.paned.tradeframe.scrolly
    """)
    tclEval(script = "bind " & tradeFrame & " <Configure> {ResizeCanvas %W.canvas %w %h}")
    tradeFrame = tradeCanvas & ".trade"
    tradeTable = createTable(parent = tradeFrame, headers = @["Name", "Type",
        "Durability", "Price", "Profit", "Weight", "Owned", "Available"],
        scrollbar = mainPaned & ".tradeframe.scrolly",
        command = "SortTradeItems",
        tooltipTExt = "Press mouse button to sort the items.")
    tclEval(script = "grid configure " & tradeTable.canvas & " -row 1")
  elif tclEval2(script = "winfo ismapped " & label) == "1" and argc == 1:
    itemsSortOrder = defaultItemsSortOrder
    tclEval(script = "grid remove " & closeButton)
    tclEval(script = "grid remove " & gameHeader & ".morebutton")
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
  tclEval(script = gameHeader & ".morebutton configure -command {TradeMore}")
  tradeFrame = tradeCanvas & ".trade"
  let comboBox = tradeFrame & ".options.type"
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
      itemType = try:
          if itemsList[protoIndex].showType.len == 0:
            itemsList[protoIndex].itemType
          else:
            itemsList[protoIndex].showType
        except:
          return showError(message = "Can't get item type.")
    try:
      if itemsTypes.find(sub = "{" & itemType & "}") == -1 and itemsList[
          protoIndex].price > 0:
        itemsTypes.add(y = " {" & itemType & "}")
    except:
      return showError(message = "Can't add item type.")
  var
    currentItemIndex = 0
    indexesList: seq[Natural]
    currentRow = 1
  let
    page = try:
        (if argc == 4: ($argv[3]).parseInt else: 1)
      except:
        return showError(message = "Can't get page.")
    startRow = ((page - 1) * gameSettings.listsLimit) + 1
  for i in itemsIndexes:
    currentItemIndex.inc
    if i == -1:
      break
    try:
      if getPrice(baseType = baseType, itemIndex = playerShip.cargo[
          i].protoIndex) == 0:
        continue
    except:
      return showError(message = "Can't get price.")
    let
      protoIndex = playerShip.cargo[i].protoIndex
      baseCargoIndex = findBaseCargo(protoIndex = protoIndex,
          durability = playerShip.cargo[i].durability)
    if baseCargoIndex > -1:
      indexesList.add(y = baseCargoIndex)
    let itemType = try:
          if itemsList[protoIndex].showType.len == 0:
            itemsList[protoIndex].itemType
          else:
            itemsList[protoIndex].showType
        except:
          return showError(message = "Can't get item type2.")
    if argc > 1 and argv[1] != "All" and itemType != $argv[1]:
      continue
    let itemName = getItemName(item = playerShip.cargo[i], damageInfo = false,
        toLower = false)
    if argc == 3 and itemName.toLowerAscii.find(sub = ($argv[
        2]).toLowerAscii) == -1:
      continue
    if currentRow < startRow:
      currentRow.inc
      continue
    var price = 0
    if baseCargoIndex == -1:
      try:
        price = getPrice(baseType = baseType, itemIndex = protoIndex)
      except:
        return showError(message = "Can't get price2.")
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
      try:
        if baseCargoIndex > -1 and isBuyable(baseType = baseType,
            itemIndex = protoIndex):
          baseAmount = baseCargo[baseCargoIndex].amount
      except:
        return showError(message = "Can't get base amount.")
    else:
      if baseCargoIndex > -1:
        baseAmount = baseCargo[baseCargoIndex].amount
    addButton(table = tradeTable, text = itemName,
        tooltip = "Show available options for item",
        command = "ShowTradeItemInfo " & $(i + 1), column = 1)
    addButton(table = tradeTable, text = itemType,
        tooltip = "Show available options for item",
        command = "ShowTradeItemInfo " & $(i + 1), column = 2)
    let itemDurability = (if playerShip.cargo[i].durability <
        100: getItemDamage(itemDurability = playerShip.cargo[
        i].durability) else: "Unused")
    addProgressbar(table = tradeTable, value = playerShip.cargo[i].durability,
        maxValue = defaultItemDurability, tooltip = itemDurability,
        command = "ShowTradeItemInfo " & $(i + 1), column = 3)
    addButton(table = tradeTable, text = $price,
        tooltip = "Show available options for item",
        command = "ShowTradeItemInfo " & $(i + 1), column = 4)
    addButton(table = tradeTable, text = $profit,
        tooltip = "Show available options for item",
        command = "ShowTradeItemInfo " & $(i + 1), column = 5, color = (
        if profit >
        0: tclGetVar(varName = "ttk::theme::" & gameSettings.interfaceTheme &
        "::colors(-green)") elif profit < 0: tclGetVar(
        varName = "ttk::theme::" & gameSettings.interfaceTheme &
        "::colors(-green)") else: ""))
    try:
      addButton(table = tradeTable, text = $itemsList[protoIndex].weight &
          " kg", tooltip = "Show available options for item",
          command = "ShowTradeItemInfo " & $(i + 1), column = 6)
    except:
      return showError(message = "Can't show weight")
    addButton(table = tradeTable, text = $playerShip.cargo[i].amount,
        tooltip = "Show available options for item",
        command = "ShowTradeItemInfo " & $(i + 1), column = 7)
    addButton(table = tradeTable, text = $baseAmount,
        tooltip = "Show available options for item",
        command = "ShowTradeItemInfo " & $(i + 1), column = 8, newRow = true)
    if tradeTable.row == gameSettings.listsLimit + 1:
      break
  currentItemIndex = playerShip.cargo.len + 1
  for i in currentItemIndex .. itemsIndexes.high:
    let
      protoIndex = baseCargo[itemsIndexes[i]].protoIndex
      itemType = try:
          if itemsList[protoIndex].showType.len == 0:
            itemsList[protoIndex].itemType
          else:
            itemsList[protoIndex].showType
        except:
          return showError(message = "Can't get item type3.")
    try:
      if isBuyable(baseType = baseType, itemIndex = protoIndex,
          baseIndex = baseIndex) and baseCargo[itemsIndexes[i]].amount > 0 and
          itemsTypes.find(sub = "{" & itemType & "}") == -1:
        itemsTypes.add(y = " {" & itemType & "}")
    except:
      return showError(message = "Can't check if item is buyable.")
  for i in currentItemIndex .. itemsIndexes.high:
    if tradeTable.row == gameSettings.listsLimit + 1:
      break
    try:
      if itemsIndexes[i] in indexesList or not isBuyable(baseType = baseType,
          itemIndex = baseCargo[itemsIndexes[i]].protoIndex,
          baseIndex = baseIndex) or baseCargo[itemsIndexes[i]].amount == 0:
        continue
    except:
      return showError(message = "Can't check if item is buyable2.")
    let
      protoIndex = baseCargo[itemsIndexes[i]].protoIndex
      itemType = try:
          if itemsList[protoIndex].showType.len == 0:
            itemsList[protoIndex].itemType
          else:
            itemsList[protoIndex].showType
        except:
          return showError(message = "Can't get item type4.")
    if argc > 1 and argv[1] != "All" and itemType != $argv[1]:
      continue
    let itemName = try:
          itemsList[protoIndex].name
        except:
          return showError(message = "Can't get item name2.")
    if argc == 3 and itemName.toLowerAscii.find(sub = ($argv[
        2]).toLowerAscii) == -1:
      continue
    if currentRow < startRow:
      currentRow.inc
      continue
    var price = if baseIndex > 0:
        skyBases[baseIndex].cargo[itemsIndexes[i]].price
      else:
        traderCargo[itemsIndexes[i]].price
    if eventIndex > -1:
      if eventsList[eventIndex].eType == doublePrice and eventsList[
          eventIndex].itemIndex == protoIndex:
        price = price * 2
    let baseAmount = (if baseIndex == 0: traderCargo[itemsIndexes[
        i]].amount else: skyBases[baseIndex].cargo[itemsIndexes[i]].amount)
    addButton(table = tradeTable, text = itemName,
        tooltip = "Show available options for item",
        command = "ShowTradeItemInfo -" & $(itemsIndexes[i] + 1), column = 1)
    addButton(table = tradeTable, text = itemType,
        tooltip = "Show available options for item",
        command = "ShowTradeItemInfo -" & $(itemsIndexes[i] + 1), column = 2)
    let itemDurability = (if baseCargo[itemsIndexes[i]].durability <
        100: getItemDamage(itemDurability = baseCargo[itemsIndexes[
        i]].durability) else: "Unused")
    addProgressbar(table = tradeTable, value = baseCargo[itemsIndexes[
        i]].durability, maxValue = defaultItemDurability,
        tooltip = itemDurability, command = "ShowTradeItemInfo -" &
        $(itemsIndexes[i] + 1), column = 3)
    addButton(table = tradeTable, text = $price,
        tooltip = "Show available options for item",
        command = "ShowTradeItemInfo -" & $(itemsIndexes[i] + 1), column = 4)
    addButton(table = tradeTable, text = $(-price),
        tooltip = "Show available options for item",
        command = "ShowTradeItemInfo -" & $(itemsIndexes[i] + 1), column = 5,
        newRow = false, color = tclGetVar(varName = "ttk::theme::" &
        gameSettings.interfaceTheme & "::colors(-red)"))
    try:
      addButton(table = tradeTable, text = $itemsList[protoIndex].weight &
          " kg", tooltip = "Show available options for item",
          command = "ShowTradeItemInfo -" & $(itemsIndexes[i] + 1), column = 6)
    except:
      return showError(message = "Can't show item weight2.")
    addButton(table = tradeTable, text = "0",
        tooltip = "Show available options for item",
        command = "ShowTradeItemInfo -" & $(itemsIndexes[i] + 1), column = 7)
    addButton(table = tradeTable, text = $baseAmount,
        tooltip = "Show available options for item",
        command = "ShowTradeItemInfo -" & $(itemsIndexes[i] + 1), column = 8, newRow = true)
  let arguments = (if argc > 2: "{" & $argv[1] & "} {" & $argv[2] &
      "}" elif argc == 2: $argv[1] & " {}" else: "All {}")
  if page > 1:
    if tradeTable.row < gameSettings.listsLimit + 1:
      addPagination(table = tradeTable, previousCommand = "ShowTrade " &
          arguments & " " & $(page - 1), nextCommand = "")
    else:
      addPagination(table = tradeTable, previousCommand = "ShowTrade " &
          arguments & " " & $(page - 1), nextCommand = "ShowTrade " &
          arguments & " " & $(page + 1))
  elif tradeTable.row == gameSettings.listsLimit + 1:
    addPagination(table = tradeTable, previousCommand = "",
        nextCommand = "ShowTrade " & arguments & " " & $(page + 1))
  updateTable(table = tradeTable, grabFocus = tclEval2(script = "focus") != searchEntry)
  tclEval(script = "update")
  tclEval(script = comboBox & " configure -values [list " & itemsTypes & "]")
  if argc == 1:
    tclEval(script = comboBox & " current 0")
  let moneyIndex2 = findItem(inventory = playerShip.cargo,
      protoIndex = moneyIndex)
  var tradeInfo = ""
  if moneyIndex2 > -1:
    tradeInfo = "You have"
    label = tradeFrame & ".options.playerinfo.moneyinfo2"
    tclEval(script = label & " configure -text {" & $playerShip.cargo[
        moneyIndex2].amount & " " & moneyName & "}")
    tclEval(script = "grid " & label & " -row 0 -column 1 -sticky w")
  else:
    tradeInfo = "You don't have any " & moneyName & " to buy anything"
    label = tradeFrame & ".options.playerinfo.moneyinfo2"
    tclEval(script = "grid remove " & label)
  label = tradeFrame & ".options.playerinfo.moneyinfo"
  tclEval(script = label & " configure -text {" & tradeInfo & "}")
  var freeSpace = try:
      freeCargo(amount = 0)
    except:
      return showError(message = "Can't get free space.")
  if freeSpace < 0:
    freeSpace = 0
  tradeInfo = $freeSpace & " kg"
  label = tradeFrame & ".options.playerinfo.cargoinfo.cargoinfo2"
  tclEval(script = label & " configure -text {" & tradeInfo & "}")
  tradeInfo = ""
  if baseIndex > 0:
    if skyBases[baseIndex].cargo[0].amount == 0:
      tradeInfo.add(y = "Base doesn't have any " & moneyName & " to buy anything")
      label = tradeFrame & ".options.baseinfo.baseinfo2"
      tclEval(script = "grid remove " & label)
    else:
      tradeInfo.add(y = "Base has ")
  else:
    if traderCargo[0].amount == 0:
      tradeInfo.add(y = "Ship doesn't have any " & moneyName & " to buy anything")
      label = tradeFrame & ".options.baseinfo.baseinfo2"
      tclEval(script = "grid remove " & label)
    else:
      tradeInfo.add(y = "Ship has ")
  label = tradeFrame & ".options.baseinfo.baseinfo"
  tclEval(script = label & " configure -text {" & tradeInfo & "}")
  label = tradeFrame & ".options.baseinfo.baseinfo2"
  if baseIndex > 0:
    if skyBases[baseIndex].cargo[0].amount > 0:
      tradeInfo = $skyBases[baseIndex].cargo[0].amount & " " & moneyName
  else:
    if traderCargo[0].amount > 0:
      tradeInfo = $traderCargo[0].amount & " " & moneyName
  tclEval(script = label & " configure -text {" & tradeInfo & "}")
  tclEval(script = "grid " & closeButton & " -row 0 -column 1")
  tclEval(script = "grid " & gameHeader & ".morebutton -row 0 -column 2")
  tclEval(script = tradeCanvas & " configure -height [expr " & tclEval2(
      script = mainPaned & " sashpos 0") & " - 20] -width " & tclEval2(
      script = mainPaned & " cget -width"))
  tclEval(script = "update")
  tclEval(script = tradeCanvas & " create window 0 0 -anchor nw -window " & tradeFrame)
  tclEval(script = "update")
  tclEval(script = tradeCanvas & " configure -scrollregion [list " & tclEval2(
      script = tradeCanvas & " bbox all") & "]")
  tclEval(script = tradeCanvas & " xview moveto 0.0")
  tclEval(script = tradeCanvas & " yview moveto 0.0")
  showScreen(newScreenName = "tradeframe")
  tclSetResult(value = "1")
  return tclOk

proc sortTradeItemsCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [
    RootEffect], cdecl.} =
  ## Sort the trading list
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## SortTradeItems x
  ## X is X axis coordinate where the player clicked the mouse button
  let
    xPos = try:
        ($argv[1]).parseInt
      except:
        return showError(message = "Can't get X position on table.")
    column = (if xPos > -1: getColumnNumber(table = tradeTable,
        xPosition = xPos) else: itemsSortOrder.ord + 1)
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
    if itemsSortOrder == priceAsc:
      itemsSortOrder = priceDesc
    else:
      itemsSortOrder = priceAsc
  of 5:
    if itemsSortOrder == profitAsc:
      itemsSortOrder = profitDesc
    else:
      itemsSortOrder = profitAsc
  of 6:
    if itemsSortOrder == weightAsc:
      itemsSortOrder = weightDesc
    else:
      itemsSortOrder = weightAsc
  of 7:
    if itemsSortOrder == ownedAsc:
      itemsSortOrder = ownedDesc
    else:
      itemsSortOrder = ownedAsc
  of 8:
    if itemsSortOrder == availableAsc:
      itemsSortOrder = availableDesc
    else:
      itemsSortOrder = availableAsc
  else:
    discard
  if itemsSortOrder == defaultItemsSortOrder:
    return tclOk
  let baseIndex = skyMap[playerShip.skyX][playerShip.skyY].baseIndex
  var
    baseCargo: seq[BaseCargo]
    baseType: string
  if baseIndex > 0:
    baseCargo = skyBases[baseIndex].cargo
    baseType = skyBases[baseIndex].baseType
  else:
    baseCargo = traderCargo
    baseType = "0"
  var indexesList: seq[Natural]
  let eventIndex = skyMap[playerShip.skyX][playerShip.skyY].eventIndex
  type LocalItemData = object
    name: string
    iType: string
    damage: float
    price: Natural
    profit: int
    weight: Positive = 1
    owned: Natural
    available: Natural
    id: Natural
  var localItems: seq[LocalItemData]
  for index, item in playerShip.cargo:
    let
      protoIndex = item.protoIndex
      baseCargoIndex = findBaseCargo(protoIndex = protoIndex,
          durability = item.durability)
    var price: int
    if baseCargoIndex > -1:
      indexesList.add(y = baseCargoIndex)
      price = baseCargo[baseCargoIndex].price
    else:
      price = try:
          getPrice(baseType = baseType, itemIndex = protoIndex)
        except:
          return showError(message = "Can't get price.")
    if eventIndex > -1:
      if eventsList[eventIndex].eType == doublePrice and eventsList[
          eventIndex].itemIndex == protoIndex:
        price = price * 2
    try:
      localItems.add(y = LocalItemData(name: getItemName(item = item), iType: (
          if itemsList[protoIndex].showType.len == 0: itemsList[
          protoIndex].itemType else: itemsList[protoIndex].showType), damage: (
          item.durability.float / defaultItemDurability.float), price: price,
          profit: price - item.price, weight: itemsList[protoIndex].weight,
          owned: item.amount, available: (if baseCargoIndex > -1: baseCargo[
          baseCargoIndex].amount else: 0), id: index))
    except:
      return showError(message = "Can't add item from the player's ship's cargo.")

  proc sortItems(x, y: LocalItemData): int =
    case itemsSortOrder
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
    of typeAsc:
      if x.iType < y.iType:
        return 1
      else:
        return -1
    of typeDesc:
      if x.iType > y.iType:
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
    of priceAsc:
      if x.price < y.price:
        return 1
      else:
        return -1
    of priceDesc:
      if x.price > y.price:
        return 1
      else:
        return -1
    of profitAsc:
      if x.profit < y.profit:
        return 1
      else:
        return -1
    of profitDesc:
      if x.profit > y.profit:
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
    of ownedAsc:
      if x.owned < y.owned:
        return 1
      else:
        return -1
    of ownedDesc:
      if x.owned > y.owned:
        return 1
      else:
        return -1
    of availableAsc:
      if x.available < y.available:
        return 1
      else:
        return -1
    of availableDesc:
      if x.available > y.available:
        return 1
      else:
        return -1
    of none:
      return -1

  localItems.sort(cmp = sortItems)
  itemsIndexes = @[]
  for item in localItems:
    itemsIndexes.add(y = item.id)
  itemsIndexes.add(y = -1)
  localItems = @[]
  for index, item in baseCargo:
    if index in indexesList:
      continue
    let protoIndex = item.protoIndex
    var price = item.price
    if eventIndex > -1:
      if eventsList[eventIndex].eType == doublePrice and eventsList[
          eventIndex].itemIndex == protoIndex:
        price = price * 2
    try:
      localItems.add(y = LocalItemData(name: itemsList[protoIndex].name,
          iType: (if itemsList[protoIndex].showType.len == 0: itemsList[
          protoIndex].itemType else: itemsList[protoIndex].showType), damage: (
          item.durability.float / defaultItemDurability.float), price: price,
          profit: -price, weight: itemsList[protoIndex].weight, owned: 0,
          available: item.amount, id: index))
    except:
      return showError(message = "Can't add item from the base's cargo.")
  localItems.sort(cmp = sortItems)
  for item in localItems:
    itemsIndexes.add(y = item.id)
  let typeBox = mainPaned & ".tradeframe.canvas.trade.options.type"
  return showTradeCommand(clientData = clientData, interp = interp, argc = 2,
      argv = @["ShowTrade", tclEval2(script = typeBox &
      " get")].allocCStringArray)

var itemIndex = -1

proc tradeItemCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [
    WriteIOEffect, TimeEffect, RootEffect, RootEffect], cdecl.} =
  ## Buy or sell the selected item
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## TradeItem tradetype
  ## Tradetype is type of trade action. Can be buy, buymax, sell, sellmax
  var baseCargoIndex, cargoIndex: int = -1
  if itemIndex < 0:
    baseCargoIndex = itemIndex.abs
  else:
    cargoIndex = itemIndex
  var protoIndex = 0
  let baseIndex = skyMap[playerShip.skyX][playerShip.skyY].baseIndex
  if cargoIndex > -1:
    protoIndex = playerShip.cargo[cargoIndex].protoIndex
    if baseCargoIndex == -1:
      baseCargoIndex = findBaseCargo(protoIndex = protoIndex)
  else:
    protoIndex = (if baseIndex == 0: traderCargo[
        baseCargoIndex].protoIndex else: skyBases[baseIndex].cargo[
        baseCargoIndex].protoIndex)
  let trader = (if baseIndex > 0: "base" else: "ship")
  try:
    if argc > 2:
      if argv[1] == "buy":
        buyItems(baseItemIndex = baseCargoIndex, amount = $argv[2])
      else:
        sellItems(itemIndex = cargoIndex, amount = $argv[2])
    else:
      let amountBox = ".itemdialog.amount"
      if argv[1] == "buy":
        buyItems(baseItemIndex = baseCargoIndex, amount = tclEval2(
            script = amountBox & " get"))
      else:
        sellItems(itemIndex = cargoIndex, amount = tclEval2(script = amountBox & " get"))
    discard closeDialogCommand(clientData = clientData, interp = interp,
        argc = 2, argv = @["CloseDialog", ".itemdialog"].allocCStringArray)
  except CantBuyError:
    tclEval(script = "destroy .itemdialog")
    showMessage(text = "You can't buy " & getCurrentExceptionMsg() &
        " in this " & trader & ".", title = "Can't buy items")
  except NoFreeCargoError:
    tclEval(script = "destroy .itemdialog")
    showMessage(text = "You don't have enough free space in your ship's cargo.",
        title = "Can't buy items")
  except NoMoneyError:
    tclEval(script = "destroy .itemdialog")
    showMessage(text = "You don't have any " & moneyName & " to buy " &
        getCurrentExceptionMsg() & ".", title = "No money to buy items")
  except NotEnoughMoneyError:
    tclEval(script = "destroy .itemdialog")
    showMessage(text = "You don't have enough " & moneyName &
        " to buy so many " & getCurrentExceptionMsg() & ".",
        title = "Not enough money to buy items")
  except NoMoneyInBaseError:
    tclEval(script = "destroy .itemdialog")
    showMessage(text = "You can't sell so many " & getCurrentExceptionMsg() &
        " because " & trader & " don't have that many " & moneyName &
        " to buy it.", title = "Too much items for sale")
  except NoTraderError:
    tclEval(script = "destroy .itemdialog")
    showMessage(text = "You don't have assigned anyone in the crew to the trader's duty.",
        title = "No trader assigned")
  except NoFreeSpaceError:
    tclEval(script = "destroy .itemdialog")
    showMessage(text = "The " & trader &
        " doesn't have free space in cargo to buy it.",
        title = "No space in the " &
        trader & "'s cargo")
  except:
    tclEval(script = "destroy .itemdialog")
    return showError(message = "Can't trade item.")
  updateHeader()
  updateMessages()
  let typeBox = ".gameframe.paned.tradeframe.canvas.trade.options.type"
  return showTradeCommand(clientData = clientData, interp = interp, argc = 2,
      argv = @["ShowTrade", tclEval2(script = typeBox &
      " get")].allocCStringArray)

proc showTradeItemInfoCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [WriteIOEffect,
        TimeEffect, RootEffect], cdecl.} =
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
  ## ShowTradeItemInfo itemindex
  ## ItemIndex is the index of the item which menu will be show. If index
  ## starts with minus means item in base/trader cargo only. Otherwise it is
  ## index in the player ship cargo.
  itemIndex = try:
      ($argv[1]).parseInt
    except:
      return showError(message = "Can't get itemIndex.")
  if itemIndex < 0:
    itemIndex.inc
  else:
    itemIndex.dec
  let (protoIndex, maxSellAmount, maxBuyAmount, price) = try:
      getTradeData(iIndex = itemIndex)
    except:
      return showError(message = "Can't get the trade's data.")
  var itemInfo = ""
  try:
    if itemsList[protoIndex].itemType == weaponType:
      itemInfo.add(y = "Skill: {gold}" & skillsList[itemsList[protoIndex].value[
          3]].name & "/" & attributesList[skillsList[itemsList[
              protoIndex].value[
          3]].attribute].name & (if itemsList[protoIndex].value[4] ==
          1: "\nCan be used with shield." else: "\nCan't be used with shield (two-handed weapon).") & "\n{/gold}Damage type: {gold}")
      case itemsList[protoIndex].value[5]
      of 1:
        itemInfo.add(y = "cutting")
      of 2:
        itemInfo.add(y = "impaling")
      of 3:
        itemInfo.add(y = "blunt")
      else:
        discard
      itemInfo.add(y = "{/gold}")
  except:
    return showError(message = "Can't show weapon info.")
  let itemTypes: array[6, string] = [weaponType, chestArmor, headArmor,
      armsArmor, legsArmor, shieldType]
  for itemType in itemTypes:
    try:
      if itemsList[protoIndex].itemType == itemType:
        if itemInfo.len > 0:
          itemInfo.add(y = "\n")
        itemInfo.add(y = "Damage chance: {gold}" & getItemChanceToDamage(
            itemData = itemsList[protoIndex].value[1]) &
            "\n{/gold}Strength: {gold}" & $itemsList[protoIndex].value[2] & "{/gold}")
        break
    except:
      return showError(message = "Can't get damage chance.")
  try:
    if itemsList[protoIndex].itemType in toolsList:
      if itemInfo.len > 0:
        itemInfo.add(y = "\n")
      itemInfo.add(y = "Damage chance: {gold}" & getItemChanceToDamage(
          itemData = itemsList[protoIndex].value[1]) & "{/gold}")
  except:
    return showError(message = "Can't get tool info.")
  try:
    if itemsList[protoIndex].itemType.len > 4 and (itemsList[
        protoIndex].itemType[0..3] == "Ammo" or itemsList[
        protoIndex].itemType == "Harpoon"):
      if itemInfo.len > 0:
        itemInfo.add(y = "\n")
      itemInfo.add(y = "Strength: {gold}" & $itemsList[protoIndex].value[1] & "{/gold}")
  except:
    return showError(message = "Can't get ammo info.")
  try:
    if itemsList[protoIndex].description.len > 0:
      if itemInfo.len > 0:
        itemInfo.add(y = "\n\n")
      itemInfo.add(y = itemsList[protoIndex].description)
  except:
    return showError(message = "Can't get the description.")
  try:
    showInfo(text = itemInfo, title = itemsList[protoIndex].name, button1 = (
        if maxBuyAmount == 0: emptyButtonSettings else: ButtonSettings(
        tooltip: "Buy item from the base", command: "TradeAmount buy " &
        $maxBuyAmount & " " & $price, icon: "buy2icon", text: "Buy",
        color: "")),
        button2 = (if maxSellAmount ==
        0: emptyButtonSettings else: ButtonSettings(
        tooltip: "Sell item from the ship cargo", command: "TradeAmount sell " &
        $maxSellAmount & " " & $price, icon: "sell2icon", text: "Sell", color: "")))
  except:
    return showError(message = "Can't show the item's info.")
  return tclOk

proc tradeAmountCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [WriteIOEffect,
        TimeEffect, RootEffect], cdecl.} =
  ## Show dialog to enter amount of items to sell or buy
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## TradeAmount action baseindex
  ## Action which will be taken. Can be buy or sell. BaseIndex is the index
  ## of the base from which item will be bought. If zero it mean buying from
  ## trader ship.
  try:
    if argv[1] == "sell":
      showManipulateItem(title = "Sell " & getItemName(item = playerShip.cargo[
          itemIndex]), command = "TradeItem sell", action = "sell",
          itemIndex = itemIndex, maxAmount = ($argv[2]).parseInt, cost = ($argv[3]).parseInt)
    else:
      if itemIndex > 0:
        showManipulateItem(title = "Buy " & getItemName(item = playerShip.cargo[
            itemIndex]), command = "TradeItem buy", action = "buy",
            itemIndex = itemIndex, maxAmount = ($argv[2]).parseInt, cost = (
                $argv[3]).parseInt)
      else:
        let baseIndex = skyMap[playerShip.skyX][playerShip.skyY].baseIndex
        if baseIndex > 0:
          showManipulateItem(title = "Buy " & itemsList[skyBases[
              baseIndex].cargo[itemIndex.abs].protoIndex].name,
                  command = "TradeItem buy",
              action = "buy", itemIndex = itemIndex.abs, maxAmount = ($argv[
              2]).parseInt, cost = ($argv[3]).parseInt)
        else:
          showManipulateItem(title = "Buy " & itemsList[traderCargo[
              itemIndex.abs].protoIndex].name, command = "TradeItem buy",
              action = "buy", itemIndex = itemIndex.abs, maxAmount = ($argv[
              2]).parseInt, cost = ($argv[3]).parseInt)
  except:
    return showError(message = "Can't show setting trade amount.")
  return tclOk

proc searchTradeCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [
    RootEffect], cdecl.} =
  ## Show only this items which contains the selected sequence
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## SearchTrade
  let
    typeBox = mainPaned & ".tradeframe.canvas.trade.options.type"
    searchText = $argv[1]
  if searchText.len == 0:
    return showTradeCommand(clientData = clientData, interp = interp, argc = 2,
        argv = @["ShowTrade", tclEval2(script = typeBox &
            " get")].allocCStringArray)
  return showTradeCommand(clientData = clientData, interp = interp, argc = 3,
      argv = @["ShowTrade", tclEval2(script = typeBox & " get"),
          searchText].allocCStringArray)

proc tradeMoreCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [], cdecl.} =
  ## Maximize or minimize the options for the list of items to trade
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## TradeMore
  ## If th argument is set to show, show the options, otherwise hide them.
  let
    tradeFrame = mainPaned & ".tradeframe"
    button = gameHeader & ".morebutton"
  if tclEval2(script = "winfo ismapped " & tradeFrame &
      ".canvas.trade.options.typelabel") == "1":
    tclEval(script = "grid remove " & tradeFrame & ".canvas.trade.options.typelabel")
    tclEval(script = "grid remove " & tradeFrame & ".canvas.trade.options.type")
    tclEval(script = "grid remove " & tradeFrame & ".canvas.trade.options.search")
    tclEval(script = button & " configure -command {TradeMore}")
  else:
    tclEval(script = "grid " & tradeFrame & ".canvas.trade.options.typelabel -sticky w -row 0")
    tclEval(script = "grid " & tradeFrame & ".canvas.trade.options.type -row 0 -column 1")
    tclEval(script = "grid " & tradeFrame & ".canvas.trade.options.search -row 0 -column 2")
    tclEval(script = button & " configure -command {TradeMore}")
  return tclOk

proc addCommands*() {.raises: [], tags: [WriteIOEffect, TimeEffect,
    RootEffect].} =
  ## Adds Tcl commands related to the trades UI
  try:
    addCommand("ShowTrade", showTradeCommand)
    addCommand("SortTradeItems", sortTradeItemsCommand)
    addCommand("TradeItem", tradeItemCommand)
    addCommand("ShowTradeItemInfo", showTradeItemInfoCommand)
    addCommand("TradeAmount", tradeAmountCommand)
    addCommand("SearchTrade", searchTradeCommand)
    addCommand("TradeMore", tradeMoreCommand)
  except:
    showError(message = "Can't add a Tcl command.")
