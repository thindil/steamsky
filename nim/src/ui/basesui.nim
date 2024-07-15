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
import ../[bases, basesship, basestrade, basestypes, config, crewinventory,
    game, maps, shipscrew, tk, types]
import coreui, mapsui, table, utilsui2

var
  baseTable: TableWidget
  itemsIndexes: seq[string]

proc showBaseUiCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [
        RootEffect], exportc.} =
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
    moneyLabel = baseCanvas & ".base.lblmoney"
  if moneyIndex2 > -1:
    tclEval(script = moneyLabel & " configure -text {You have " &
        $playerShip.cargo[moneyIndex2].amount & " " & moneyName & ".}")
  else:
    tclEval(script = moneyLabel & " configure -text {You don't have " &
        moneyName & " to buy anything.}")
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
      1: "" else: "ShowBaseUI " & arguments & $(page + 1)))
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

proc addCommands*() {.sideEffect, raises: [], tags: [].} =
  ## Adds Tcl commands related to the trades UI
  try:
    discard
#    addCommand("ShowBaseUI", showBaseUiCommand)
  except:
    showError(message = "Can't add a Tcl command.")
