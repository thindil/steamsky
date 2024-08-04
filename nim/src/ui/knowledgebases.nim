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

import std/strutils
import ../[config, game, maps, tk]
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

proc updateBasesList(baseName: string = "", page: Positive = 1) =
  if basesTable.row > 1:
    clearTable(table = basesTable)
  let
    basesCanvas = mainPaned & ".knowledgeframe.bases.canvas"
    basesFrame = basesCanvas & ".frame"
  var rows = tclEval2(script = "grid size " & basesFrame).split(" ")[2].parseInt
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

proc addCommands*() {.sideEffect, raises: [], tags: [].} =
  ## Adds Tcl commands related to the trades UI
  try:
    discard
#    addCommand("ShowLoot", showLootCommand)
  except:
    showError(message = "Can't add a Tcl command.")

# Temporary code for interfacing with Ada

proc getAdaReputationText(reputationLevel: cint): cstring {.sideEffect,
    raises: [], tags: [], exportc.} =
  return getReputationText(reputationLevel = reputationLevel).cstring

proc updateAdaBasesList(baseName: cstring; page: cint) {.sideEffect, raises: [],
    tags: [], exportc.} =
  try:
    updateBasesList(baseName = $baseName, page = page.Positive)
  except:
    discard
