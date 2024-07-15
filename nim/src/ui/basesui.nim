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
import ../[basestrade, config, crewinventory, game, maps, tk]
import coreui, mapsui, table

var
  baseTable: TableWidget
  itemsIndexes: seq[string]

proc showBaseUiCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.exportc.} =
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
    baseTable = createTable(parent = baseFrame, headers = @["Name", "Cost", ""],
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
    page = (if argc == 4: ($argv[3]).parseInt else: 1)
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

  var currentRow = 1
  if argv[1] == "heal":
    var firstIndex = ""
    for index in itemsIndexes:
      let crewIndex = index.parseInt - 1
      if crewIndex > -1:
        if playerShip.crew[crewIndex].health == 100:
          continue
        if firstIndex.len == 0:
          firstIndex = $index
      if currentRow < startRow:
        currentRow.inc
        continue
      healCost(cost = cost, time = time, memberIndex = crewIndex)
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
  return tclOk

proc addCommands*() {.sideEffect, raises: [], tags: [].} =
  ## Adds Tcl commands related to the trades UI
  try:
    discard
#    addCommand("ShowBaseUi", showBaseUiCommand)
  except:
    showError(message = "Can't add a Tcl command.")
