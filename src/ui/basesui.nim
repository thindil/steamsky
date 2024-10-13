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

import std/[algorithm, os, strutils, tables]
import ../[bases, basesship, basesship2, basestrade, basestypes, config,
    crewinventory, game, maps, shipscrew, tk, types]
import coreui, dialogs, errordialog, mapsui, table, updateheader, utilsui2

var
  baseTable: TableWidget
  itemsIndexes: seq[string]

proc showBaseUiCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [
        RootEffect], cdecl.} =
  ## Show the selected base action
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ShowBaseUI UIType search page
  ## UIType can be heal, repair, recipes. Search is a string which will be
  ## looked for in names of recipes (only). Page is the number of current
  ## page on the list to show
  var baseFrame = mainPaned & ".baseframe"
  let baseCanvas = baseFrame & ".canvas"
  if tclEval2(script = "winfo exists " & baseCanvas) == "0":
    tclEvalFile(fileName = dataDirectory & "ui" & DirSep & "base.tcl")
    tclEval(script = "bind " & baseFrame & " <Configure> {ResizeCanvas %W.canvas %h}")
  elif tclEval2(script = "winfo ismapped " & baseCanvas) == "1" and argc == 1:
    tclEval(script = "grid remove " & closeButton)
    showSkyMap(clear = true)
    return tclOk
  baseFrame = baseCanvas & ".base"
  if tclEval2(script = "winfo exists " & baseFrame & ".table") == "1":
    tclEval(script = "destroy " & baseTable.canvas)
  let
    searchFrame = baseCanvas & ".base.searchframe"
    searchEntry = searchFrame & ".search"
    baseIndex = skyMap[playerShip.skyX][playerShip.skyY].baseIndex
  if argv[1] == "recipes":
    tclEval(script = "grid " & searchFrame)
    if argc != 3:
      tclEval(script = searchEntry & " configure -validatecommand {}")
      tclEval(script = searchEntry & " delete 0 end")
      tclEval(script = searchEntry & " configure -validatecommand {SearchRecipes %P}")
    baseTable = createTable(parent = baseFrame, headers = @["Name", "Cost"],
        scrollbar = mainPaned & ".baseframe.scrolly",
        command = "SortBaseItems " & $argv[1],
        tooltipText = "Press mouse button to sort the recipes.")
    if itemsIndexes.len != recipesList.len:
      itemsIndexes = @[]
      for index in recipesList.keys:
        itemsIndexes.add(y = index)
  else:
    tclEval(script = "grid remove " & searchFrame)
    baseTable = createTable(parent = baseFrame, headers = @["Action", "Cost",
        "Time"], scrollbar = mainPaned & ".baseframe.scrolly",
        command = "SortBaseItems " & $argv[1],
        tooltipText = "Press mouse button to sort the actions.")
    if argv[1] == "heal" and itemsIndexes.len != playerShip.crew.len + 1:
      itemsIndexes = @[]
      for index, _ in playerShip.crew:
        itemsIndexes.add(y = $(index + 1))
      itemsIndexes.add(y = "0")
    elif argv[1] == "repair" and itemsIndexes.len != playerShip.modules.len + 3:
      itemsIndexes = @[]
      for index, _ in playerShip.modules:
        itemsIndexes.add(y = $(index + 1))
      itemsIndexes.add(y = "0")
      itemsIndexes.add(y = (if skyBases[baseIndex].population >
          149: "-1" else: "-3"))
      itemsIndexes.add(y = (if skyBases[baseIndex].population >
          299: "-2" else: "-3"))
  let
    moneyIndex2 = findItem(inventory = playerShip.cargo,
        protoIndex = moneyIndex)
  var moneyLabel = baseCanvas & ".base.moneyframe.lblmoney"
  if moneyIndex2 > -1:
    tclEval(script = moneyLabel & " configure -text {You have } -style TLabel")
    moneyLabel = baseCanvas & ".base.moneyframe.lblmoney2"
    tclEval(script = moneyLabel & " configure -text {" & $playerShip.cargo[
        moneyIndex2].amount & " " & moneyName & "}")
    tclEval(script = "grid " & moneyLabel & " -column 1 -row 0")
  else:
    tclEval(script = moneyLabel & " configure -text {You don't have any " &
        moneyName & " to buy anything.} -style Headerred.TLabel")
    moneyLabel = baseCanvas & ".base.moneyframe.lblmoney2"
    tclEval(script = "grid remove " & moneyLabel)
  let
    page = try:
        (if argc == 4: ($argv[3]).parseInt else: 1)
      except:
        return showError(message = "Can't get the page number")
    startRow = ((page - 1) * gameSettings.listsLimit) + 1

  proc getColor(actionCost: Natural): string =
    if moneyIndex2 == -1 or playerShip.cargo[moneyIndex2].amount < actionCost:
      return "red"
    return ""

  var
    cost, time: Natural = 0
    formattedTime = ""

  proc formatTime() =
    if time < 60:
      formattedTime = $time & " minute"
      if time > 1:
        formattedTime.add(y = "s")
    else:
      formattedTime = $(time / 60) & " hour"
      if time / 60 > 1:
        formattedTime.add(y = "s")
      if time mod 60 > 0:
        formattedTime.add(y = " and " & $(time mod 60) & " minute")
        if time mod 60 > 1:
          formattedTime.add(y = "s")

  var
    currentRow = 1
    firstIndex = ""
  if argv[1] == "heal":
    for index in itemsIndexes:
      let crewIndex = try:
          index.parseInt - 1
        except:
          return showError(message = "Can't get the crew index.")
      if crewIndex > -1:
        if playerShip.crew[crewIndex].health == 100:
          continue
        if firstIndex.len == 0:
          firstIndex = index
      if currentRow < startRow:
        currentRow.inc
        continue
      cost = 0
      time = 0
      try:
        healCost(cost = cost, time = time, memberIndex = crewIndex)
      except:
        return showError(message = "Can't count heal cost")
      addButton(table = baseTable, text = (if crewIndex > -1: playerShip.crew[
          crewIndex].name else: "Heal all wounded crew members"),
          tooltip = "Show available options", command = "ShowBaseMenu heal " &
          index, column = 1)
      addButton(table = baseTable, text = $cost & " " & moneyName,
          tooltip = "Show available options", command = "ShowBaseMenu heal " &
          index, column = 2, color = getColor(actionCost = cost))
      formatTime()
      addButton(table = baseTable, text = formattedTime,
          tooltip = "Show available options", command = "ShowBaseMenu heal " &
          index, column = 3, newRow = true)
      if baseTable.row == gameSettings.listsLimit + 1:
        break
  elif argv[1] == "repair":
    for index in itemsIndexes:
      let moduleIndex = try:
          index.parseInt - 1
        except:
          return showError(message = "Can't get module index.")
      if moduleIndex > -1:
        if playerShip.modules[moduleIndex].durability == playerShip.modules[
            moduleIndex].maxDurability:
          continue
        if firstIndex.len == 0:
          firstIndex = index
      if currentRow < startRow:
        currentRow.inc
        continue
      if index == "-3":
        continue
      cost = 0
      time = 0
      try:
        repairCost(cost = cost, time = time, moduleIndex = moduleIndex)
      except:
        return showError(message = "Can't count repair cost")
      try:
        countPrice(price = cost, traderIndex = findMember(order = talk))
      except:
        return showError(message = "Can't count repair price")
      addButton(table = baseTable, text = (case index
        of "0":
          "Slowly repair the whole ship"
        of "-1":
          "Repair the whole ship"
        of "-2":
          "Quickly repair the whole ship"
        else:
          playerShip.modules[moduleIndex].name),
        tooltip = "Show available options", command = "ShowBaseMenu repair " &
        index, column = 1)
      addButton(table = baseTable, text = $cost & " " & moneyName,
          tooltip = "Show available options", command = "ShowBaseMenu repair " &
          index, column = 2, color = getColor(actionCost = cost))
      formatTime()
      addButton(table = baseTable, text = formattedTime,
          tooltip = "Show available options", command = "ShowBaseMenu repair " &
          index, column = 3, newRow = true)
      if baseTable.row == gameSettings.listsLimit + 1:
        break
  elif argv[1] == "recipes":
    let baseType = skyBases[baseIndex].baseType
    for index in itemsIndexes:
      try:
        if index notin basesTypesList[baseType].recipes or index in
            knownRecipes or recipesList[index].reputation > skyBases[
            baseIndex].reputation.level:
          continue
      except:
        return showError(message = "Can't check recipe index")
      try:
        if argc > 2 and argv[2].len > 0 and not itemsList[recipesList[
            index].resultIndex].name.toLowerAscii.contains(sub = ($argv[
            2]).toLowerAscii):
          continue
      except:
        return showError(message = "Can't check recipe index2")
      if firstIndex.len == 0:
        firstIndex = index
      if currentRow < startRow:
        currentRow.inc
        continue
      try:
        addButton(table = baseTable, text = itemsList[recipesList[
            index].resultIndex].name,
          tooltip = "Show available options",
            command = "ShowBaseMenu recipes " &
          index, column = 1)
      except:
        return showError(message = "Can't add button")
      try:
        cost = if getPrice(baseType = baseType, itemIndex = recipesList[
            index].resultIndex) > 0:
            getPrice(baseType = baseType, itemIndex = recipesList[
                index].resultIndex) * recipesList[index].difficulty * 10
          else:
            recipesList[index].difficulty * 10
      except:
        return showError(message = "Can't count recipe cost")
      cost = (cost.float * newGameSettings.pricesBonus).int
      if cost < 1:
        cost = 1
      try:
        countPrice(price = cost, traderIndex = findMember(order = talk))
      except:
        return showError(message = "Can't count recipe price")
      addButton(table = baseTable, text = $cost & " " & moneyName,
          tooltip = "Show available options",
          command = "ShowBaseMenu recipes " &
          index, column = 2, color = getColor(actionCost = cost), newRow = true)
      if baseTable.row == gameSettings.listsLimit + 1:
        break
  let arguments: string = (if argc > 2: "{" & $argv[1] & "} {" & $argv[2] &
      "}" else: "{" & $argv[1] & "} {}")
  addPagination(table = baseTable, previousCommand = (if page >
      1: "ShowBaseUI " & arguments & " " & $(page - 1) else: ""),
      nextCommand = (if baseTable.row < gameSettings.listsLimit +
      1: "" else: "ShowBaseUI " & arguments & " " & $(page + 1)))
  updateTable(table = baseTable, grabFocus = tclEval2(script = "focus") != searchEntry)
  if firstIndex.len == 0 and argc < 3:
    tclEval(script = "grid remove " & closeButton)
    showSkyMap(clear = true)
    return tclOk
  tclEval(script = "grid " & closeButton & " -row 0 -column 1")
  baseFrame = baseCanvas & ".base"
  tclEval(script = baseCanvas & " configure -height [expr " & tclEval2(
      script = mainPaned & " sashpos 0") & " - 20] -width " & tclEval2(
      script = mainPaned & " cget -width"))
  tclEval(script = "update")
  tclEval(script = baseCanvas & " create window 0 0 -anchor nw -window " & baseFrame)
  tclEval(script = "update")
  tclEval(script = baseCanvas & " configure -scrollregion [list " & tclEval2(
      script = baseCanvas & " bbox all") & "]")
  showScreen(newScreenName = "baseframe")
  tclSetResult(value = "1")
  return tclOk

proc baseActionCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [
    RootEffect], cdecl.} =
  ## Show the selected base action
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## BaseAction ActionType
  ## ActionType can be heal, repair, recipes
  let itemIndex = $argv[2]
  if argv[1] == "heal":
    try:
      healWounded(memberIndex = itemIndex.parseInt - 1)
    except:
      return showError(message = "Can't heal wounded.")
  elif argv[1] == "repair":
    try:
      repairShip(moduleIndex = itemIndex.parseInt - 1)
    except:
      return showError(message = "Can't repair the ship.")
  elif argv[1] == "recipes":
    try:
      buyRecipe(recipeIndex = itemIndex)
    except:
      return showError(message = "Can't buy the recipe.")
  updateHeader()
  updateMessages()
  return showBaseUiCommand(clientData = clientData, interp = interp, argc = 2,
      argv = @["ShowBaseUI", $argv[1]].allocCStringArray)

proc searchRecipesCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [
    RootEffect], cdecl.} =
  ## Show only this recipes which contains the selected sequence
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## SearchRecipes TextToSearch
  let searchText = $argv[1]
  if searchText.len == 0:
    return showBaseUiCommand(clientData = clientData, interp = interp, argc = 2,
        argv = @["ShowBaseUI", "recipes"].allocCStringArray)
  return showBaseUiCommand(clientData = clientData, interp = interp, argc = 3,
      argv = @["ShowBaseUI", "recipes", searchText].allocCStringArray)

proc showBaseMenuCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [WriteIOEffect, TimeEffect], cdecl.} =
  ## Show menu with options for the selected item
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ShowBaseMenu action index
  ## Action is name of action (heal,repair or recipe) and index is the index
  ## of the item
  var cost, time: Natural = 0
  let
    action = $argv[1]
    itemIndex = $argv[2]
    baseIndex = skyMap[playerShip.skyX][playerShip.skyY].baseIndex
  if action == "heal":
    try:
      healCost(cost = cost, time = time, memberIndex = itemIndex.parseInt)
    except:
      return showError(message = "Can't count heal cost")
  elif action == "repair":
    try:
      repairCost(cost = cost, time = time, moduleIndex = itemIndex.parseInt)
      countPrice(price = cost, traderIndex = findMember(order = talk))
    except:
      return showError(message = "Can't count repair cost")
  else:
    try:
      cost = (if getPrice(baseType = skyBases[baseIndex].baseType,
          itemIndex = recipesList[itemIndex].resultIndex) > 0: getPrice(
          baseType = skyBases[baseIndex].baseType, itemIndex = recipesList[
          itemIndex].resultIndex) * recipesList[itemIndex].difficulty *
          10 else: recipesList[itemIndex].difficulty * 10)
    except:
      return showError(message = "Can't count the recipe cost")
    cost = (cost.float * newGameSettings.pricesBonus).Natural
    if cost < 1:
      cost = 1
    try:
      countPrice(price = cost, traderIndex = findMember(order = talk))
    except:
      return showError(message = "Can't count the recipe's price")
  let
    moneyIndex2 = findItem(inventory = playerShip.cargo,
        protoIndex = moneyIndex)
    baseMenu = createDialog(name = ".basemenu", title = "Actions",
        parentName = ".")

  proc addButton(name, label, command: string) =
    let button = baseMenu & name
    tclEval(script = "ttk::button " & button & " -text {" & label &
        "} -command {CloseDialog " & baseMenu & " .;" & command & "}")
    tclEval(script = "grid " & button & " -sticky we -padx 5" & (
        if command.len == 0: " -pady {0 3}" else: ""))
    tclEval(script = "bind " & button & " <Escape> {CloseDialog " & baseMenu & ".;break}")
    if command.len == 0:
      tclEval(script = "bind " & button & " <Tab> {focus " & baseMenu & ".action;break}")
      tclEval(script = "focus " & button)

  if moneyIndex2 == -1 or playerShip.cargo[moneyIndex2].amount < cost:
    addButton(name = ".action", label = "You don't have money for this", command = "")
  else:
    addButton(name = ".action", label = (if action ==
        "heal": "Buy healing" elif action ==
        "repair": "Buy repair" else: "Buy recipe"), command = "BaseAction " &
        action & " " & itemIndex)
    addButton(name = ".close", label = "Close", command = "")
  showDialog(dialog = baseMenu, parentFrame = ".")
  return tclOk

type BaseSortOrders = enum
  nameAsc, nameDesc, costAsc, costDesc, timeAsc, timeDesc, none

const defaultBaseSortOrder: BaseSortOrders = none
var baseSortOrder: BaseSortOrders = defaultBaseSortOrder

proc sortBaseItemsCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [
    RootEffect], cdecl.} =
  let column = try:
        getColumnNumber(
            table = baseTable, xPosition = ($argv[2]).parseInt)
      except:
        return showError(message = "Can't get the column number.")
  case column
  of 1:
    if baseSortOrder == nameAsc:
      baseSortOrder = nameDesc
    else:
      baseSortOrder = nameAsc
  of 2:
    if baseSortOrder == costAsc:
      baseSortOrder = costDesc
    else:
      baseSortOrder = costAsc
  of 3:
    if baseSortOrder == timeAsc:
      baseSortOrder = timeDesc
    else:
      baseSortOrder = timeAsc
  else:
    discard
  if baseSortOrder == none:
    return tclOk
  type LocalItemData = object
    name: string
    cost: Positive = 1
    time: Positive = 1
    id: string
  var localItems: seq[LocalItemData] = @[]
  let baseIndex = skyMap[playerShip.skyX][playerShip.skyY].baseIndex
  if argv[1] == "heal":
    var cost, time: Natural = 0
    for index, member in playerShip.crew:
      try:
        healCost(cost = cost, time = time, memberIndex = index)
      except:
        return showError(message = "Can't count heal cost.")
      localItems.add(y = LocalItemData(name: member.name, cost: cost,
          time: time, id: $(index + 1)))
    cost = 0
    time = 0
    try:
      healCost(cost = cost, time = time, memberIndex = -1)
    except:
      return showError(message = "Can't count heal cost2.")
    localItems.add(y = LocalItemData(name: "Heal all wounded crew members",
        cost: cost, time: time, id: "0"))
  elif argv[1] == "repair":
    var cost, time: Natural = 0

    proc countRepairCost(i: int) {.raises: [KeyError], tags: [].} =
      cost = 0
      time = 0
      repairCost(cost = cost, time = time, moduleIndex = i)
      countPrice(price = cost, traderIndex = findMember(order = talk))

    for index, module in playerShip.modules:
      try:
        countRepairCost(i = index)
      except:
        return showError(message = "Can't count repair cost.")
      localItems.add(y = LocalItemData(name: module.name, cost: cost,
          time: time, id: $(index + 1)))
    if skyBases[baseIndex].population > 299:
      try:
        countRepairCost(i = -1)
      except:
        return showError(message = "Can't count repair cost2.")
      localItems.add(y = LocalItemData(name: "Slowly repair the whole ship",
          cost: cost, time: time, id: "0"))
      try:
        countRepairCost(i = -2)
      except:
        return showError(message = "Can't count repair cost3.")
      localItems.add(y = LocalItemData(name: "Repair the whole ship",
          cost: cost, time: time, id: "-1"))
      try:
        countRepairCost(i = -3)
      except:
        return showError(message = "Can't count repair cost4.")
      localItems.add(y = LocalItemData(name: "Quickly repair the whole ship",
          cost: cost, time: time, id: "-2"))
    elif skyBases[baseIndex].population > 149:
      try:
        countRepairCost(i = -1)
      except:
        return showError(message = "Can't count repair cost5.")
      localItems.add(y = LocalItemData(name: "Slowly repair the whole ship",
          cost: cost, time: time, id: "0"))
      try:
        countRepairCost(i = -2)
      except:
        return showError(message = "Can't count repair cost6.")
      localItems.add(y = LocalItemData(name: "Repair the whole ship",
          cost: cost, time: time, id: "-1"))
    else:
      try:
        countRepairCost(i = -1)
      except:
        return showError(message = "Can't count repair cost7.")
      localItems.add(y = LocalItemData(name: "Slowly repair the whole ship",
          cost: cost, time: time, id: "0"))
  elif argv[1] == "recipes":
    for index, recipe in recipesList:
      var cost: Natural = 0
      try:
        cost = (if getPrice(baseType = skyBases[baseIndex].baseType,
            itemIndex = recipe.resultIndex) > 0: getPrice(baseType = skyBases[
            baseIndex].baseType, itemIndex = recipe.resultIndex) *
            recipe.difficulty * 10 else: recipe.difficulty * 10)
      except:
        return showError(message = "Can't get recipe cost.")
      cost = (cost.float * newGameSettings.pricesBonus).Natural
      if cost < 1:
        cost = 1
      try:
        countPrice(price = cost, traderIndex = findMember(order = talk))
      except:
        return showError(message = "Can't count recipe cost.")
      try:
        localItems.add(y = LocalItemData(name: itemsList[
            recipe.resultIndex].name, cost: cost, time: 1, id: index))
      except:
        return showError(message = "Can't add recipe.")
  proc sortItems(x, y: LocalItemData): int =
    case baseSortOrder
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
    of costAsc:
      if x.cost < y.cost:
        return 1
      else:
        return -1
    of costDesc:
      if x.cost > y.cost:
        return 1
      else:
        return -1
    of timeAsc:
      if x.time < y.time:
        return 1
      else:
        return -1
    of timeDesc:
      if x.time > y.time:
        return 1
      else:
        return -1
    of none:
      return -1
  localItems.sort(cmp = sortItems)
  itemsIndexes = @[]
  for item in localItems:
    itemsIndexes.add(y = item.id)
  return showBaseUiCommand(clientData = clientData, interp = interp, argc = 2,
      argv = @["ShowBaseUI", $argv[1]].allocCStringArray)

proc addCommands*() {.raises: [], tags: [WriteIOEffect, TimeEffect].} =
  ## Adds Tcl commands related to the trades UI
  try:
    addCommand("ShowBaseUI", showBaseUiCommand)
    addCommand("BaseAction", baseActionCommand)
    addCommand("SearchRecipes", searchRecipesCommand)
    addCommand("ShowBaseMenu", showBaseMenuCommand)
    addCommand("SortBaseItems", sortBaseItemsCommand)
  except:
    showError(message = "Can't add a Tcl command.")
