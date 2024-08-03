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
import ../tk
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

var basesTable: TableWidget

proc updateBasesList(baseName: string = "", page: Positive = 1) =
  if basesTable.row > 1:
    clearTable(table = basesTable)
  let
    basesCanvas = mainPaned & ".knowledgeframe.bases.canvas"
    basesFrame = basesCanvas & ".frame"
    rows = tclEval2(script = "grid size " & basesFrame).split(" ")[2].parseInt
  deleteWidgets(startIndex = 2, endIndex = rows - 1, frame = basesFrame)
  basesTable = createTable(parent = basesFrame, headers = @["Name", "Distance",
      "Coordinates", "Population", "Size", "Owner", "Type", "Reputation"],
      scrollbar = ".gameframe.paned.knowledgeframe.bases.scrolly",
      command = "SortKnownBases {" & baseName & "}",
      tooltipText = "Press mouse button to sort the bases.")

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
