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
import ../[basestypes, config, game, maps, tk]
import coreui, table

proc getReputationText(reputationLevel: int): string {.sideEffect, raises: [],
    tags: [].} =
  ## Get the name of the reputation level in the selected base
  ##
  ## * reputationLevel - the numerical level of reputation in a base
  ##
  ## Returns the name of the reputation level in the selected base
  case reputationLevel
  of -100 .. -75:
    return "Hated"
  of -74 .. -50:
    return "Outlaw"
  of -49 .. -25:
    return "Hostile"
  of -24 .. -1:
    return "Unfriendly"
  of 0:
    return "Unknown"
  of 1..25:
    return "Visitor"
  of 26..50:
    return "Trader"
  of 51..75:
    return "Friend"
  of 76..100:
    return "Well known"
  else:
    return ""

var
  basesTable: TableWidget
  basesIndexes: seq[Positive]

proc updateBasesList*(baseName: string = "", page: Positive = 1) {.sideEffect,
    raises: [], tags: [RootEffect].} =
  ## Update and show list of known bases
  ##
  ## * baseName - the name of the base to find in the list
  ## * page     - the current page of the bases' list to show
  if basesTable.row > 1:
    clearTable(table = basesTable)
  let
    basesCanvas = mainPaned & ".knowledgeframe.bases.canvas"
    basesFrame = basesCanvas & ".frame"
  var rows = try:
      tclEval2(script = "grid size " & basesFrame).split(" ")[1].parseInt
    except:
      showError(message = "Can't get the amount of rows.")
      return
  deleteWidgets(startIndex = 2, endIndex = rows - 1, frame = basesFrame)
  basesTable = createTable(parent = basesFrame, headers = @["Name", "Distance",
      "Coordinates", "Population", "Size", "Owner", "Type", "Reputation"],
      scrollbar = ".gameframe.paned.knowledgeframe.bases.scrolly",
      command = "SortKnownBases {" & baseName & "}",
      tooltipText = "Press mouse button to sort the bases.")
  if basesIndexes.len == 0:
    for index, _ in skyBases:
      basesIndexes.add(y = index)
  let searchEntry = basesFrame & ".options.search"
  if baseName.len == 0:
    tclEval(script = searchEntry & " configure -validatecommand {}")
    tclEval(script = searchEntry & " delete 0 end")
    tclEval(script = searchEntry & " configure -validatecommand {ShowBases %P}")
  var comboBox = basesFrame & ".options.types"
  let basesType = tclEval2(script = comboBox & " get")
  comboBox = basesFrame & ".options.status"
  let basesStatus = tclEval2(script = comboBox & " get")
  comboBox = basesFrame & ".options.owner"
  let basesOwner = tclEval2(script = comboBox & " get")
  rows = 0
  let startRow = ((page - 1) * gameSettings.listsLimit) + 1
  var currentRow = 1
  for index in basesIndexes:
    if not skyBases[index].known:
      continue
    if baseName.len > 0 and not skyBases[index].name.toLowerAscii.contains(
        sub = baseName.toLowerAscii):
      continue
    if basesStatus == "Only not visited" and skyBases[index].visited.year != 0:
      continue
    if basesStatus == "Only visited" and skyBases[index].visited.year == 0:
      continue
    if skyBases[index].visited.year == 0 and (basesType != "Any" or
        basesOwner != "Any"):
      continue
    if currentRow < startRow:
      currentRow.inc
      continue
    var color = (if skyBases[index].visited.year > 0: "green3" else: "")
    if skyBases[index].skyX == playerShip.destinationX and skyBases[
        index].skyY == playerShip.destinationY:
      color = "yellow"
    addButton(table = basesTable, text = skyBases[index].name,
        tooltip = "Show the base's details", command = "ShowBaseInfo " & $index,
        column = 1, color = color)
    addButton(table = basesTable, text = $countDistance(destinationX = skyBases[
        index].skyX, destinationY = skyBases[index].skyY),
        tooltip = "The distance to the base", command = "ShowBaseInfo " &
        $index, column = 2, color = color)
    addButton(table = basesTable, text = "X: " & $skyBases[index].skyX &
        " Y: " & $skyBases[index].skyY, tooltip = "The coordinates of the base",
        command = "ShowBaseInfo " & $index, column = 3, color = color)
    if skyBases[index].visited.year > 0:
      addButton(table = basesTable, text = case skyBases[index].population
        of 0:
          "empty"
        of 1..150:
          "small"
        of 151..299:
          "medium"
        else:
          "large", tooltip = "The population size of the base",
              command = "ShowBaseInfo " & $index, column = 4, color = color)
      addButton(table = basesTable, text = ($skyBases[index].size).toLowerAscii,
          tooltip = "The size of the base", command = "ShowBaseInfo " & $index,
          column = 5, color = color)
      try:
        addButton(table = basesTable, text = factionsList[skyBases[
            index].owner].name, tooltip = "The faction which own the base",
            command = "ShowBaseInfo " & $index, column = 6, color = color)
      except:
        showError(message = "Can't show the faction name.")
        return
      try:
        addButton(table = basesTable, text = basesTypesList[skyBases[
            index].baseType].name, tooltip = "The type of the base",
            command = "ShowBaseInfo " & $index, column = 7, color = color)
      except:
        showError(message = "Can't show the type of the base.")
        return
      addButton(table = basesTable, text = getReputationText(
          reputationLevel = skyBases[index].reputation.level),
          tooltip = "Your reputation in the base", command = "ShowBaseInfo " &
          $index, column = 8, newRow = true, color = color)
    else:
      addButton(table = basesTable, text = "not",
          tooltip = "Show the base's details", command = "ShowBaseInfo " &
          $index, column = 4, color = color)
      addButton(table = basesTable, text = "",
          tooltip = "Show the base's details", command = "ShowBaseInfo " &
          $index, column = 5, color = color)
      addButton(table = basesTable, text = "visited",
          tooltip = "Show the base's details", command = "ShowBaseInfo " &
          $index, column = 6, color = color)
      addButton(table = basesTable, text = "",
          tooltip = "Show the base's details", command = "ShowBaseInfo " &
          $index, column = 7, color = color)
      addButton(table = basesTable, text = "yet",
          tooltip = "Show the base's details", command = "ShowBaseInfo " &
          $index, column = 8, newRow = true, color = color)
    rows.inc
    if rows == gameSettings.listsLimit + 1 and index < skyBases.high:
      break
  if page > 1:
    addPagination(table = basesTable, previousCommand = "ShowBases {" &
        baseName & "} " & $(page - 1), nextCommand = (if basesTable.row <
        gameSettings.listsLimit + 1: "" else: "ShowBases {" & baseName & "} " &
        $(page + 1)))
  elif basesTable.row == gameSettings.listsLimit + 2:
    addPagination(table = basesTable, previousCommand = "",
        nextCommand = "ShowBases {" & baseName & "} " & $(page + 1))
  updateTable(table = basesTable, grabFocus = tclEval2(script = "focus") != searchEntry)
  tclEval(script = basesCanvas & " xview moveto 0.0")
  tclEval(script = basesCanvas & " yview moveto 0.0")
  tclEval(script = "update")
  tclEval(script = basesCanvas & " configure -scrollregion [list " & tclEval2(
      script = basesCanvas & " bbox all") & "]")

proc showBasesCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [
    RootEffect], exportc.} =
  case argc
  of 3:
    try:
      updateBasesList(baseName = $argv[1], page = ($argv[2]).parseInt)
    except:
      return showError(message = "Can't update the list of bases.")
  of 2:
    updateBasesList(baseName = $argv[1])
  else:
    updateBasesList()
  tclSetResult(value = "1")
  return tclOk

proc addCommands*() {.sideEffect, raises: [], tags: [].} =
  ## Adds Tcl commands related to the trades UI
  try:
    discard
#    addCommand("ShowBases", showBasesCommand)
  except:
    showError(message = "Can't add a Tcl command.")

# Temporary code for interfacing with Ada

proc getAdaReputationText(reputationLevel: cint): cstring {.sideEffect,
    raises: [], tags: [], exportc.} =
  return getReputationText(reputationLevel = reputationLevel).cstring

proc updateAdaBasesList(baseName: cstring; page: cint) {.sideEffect, raises: [],
    tags: [RootEffect], exportc.} =
  try:
    updateBasesList(baseName = $baseName, page = page.Positive)
  except:
    echo getCurrentExceptionMsg()
    echo getStackTrace(getCurrentException())
