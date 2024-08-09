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
import ../[config, game, maps, tk]
import coreui, dialogs, table

proc showEventInfoCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [], exportc.} =
  ## Show information about the selected event
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ShowEventInfo eventindex
  ## EventIndex is the index of the event to show
  let
    eventIndex = try:
        ($argv[1]).parseInt - 1
      except:
        return showError(message = "Can't get the event index.")
    baseIndex = skyMap[eventsList[eventIndex].skyX][eventsList[
        eventIndex].skyY].baseIndex
  var eventInfo = "X: {gold}" & $eventsList[eventIndex].skyX &
      "{/gold} Y: {gold}" & $eventsList[eventIndex].skyY & "{/gold}"
  case eventsList[eventIndex].eType
  of enemyShip, enemyPatrol, trader, friendlyShip:
    try:
      eventInfo.add(y = "\nShip type: {gold}" & protoShipsList[eventsList[
          eventIndex].shipIndex].name & "{/gold}")
    except:
      return showError(message = "Can't get the ship info")
  of fullDocks, attackOnBase, disease:
    eventInfo.add(y = "\nBase name: {gold}" & skyBases[baseIndex].name & "{/gold}")
  of doublePrice:
    eventInfo.add(y = "\nBase name: {gold}" & skyBases[baseIndex].name & "{/gold}")
    try:
      eventInfo.add(y = "\nItem: {gold}" & itemsList[eventsList[
          eventIndex].itemIndex].name & "{/gold}")
    except:
      return showError(message = "Can't get the item info")
  of none, baseRecovery:
    discard
  showInfo(text = eventInfo, title = "Event information",
      button1 = ButtonSettings(tooltip: "Set the event as the ship destination",
      command: "SetDestination2 " & $eventsList[eventIndex].skyX & " " &
      $eventsList[eventIndex].skyY, icon: "destinationicon", text: "Target",
      color: "green"), button2 = ButtonSettings(
      tooltip: "Show the event on the map", command: "ShowOnMap " & $eventsList[
      eventIndex].skyX & " " & $eventsList[eventIndex].skyY, icon: "show2icon",
      text: "Show", color: "green"))
  return tclOk

var
  eventsTable: TableWidget
  eventsIndexes: seq[Natural]

proc updateEventsList*(page: Positive = 1) =
  if eventsTable.row > 1:
    clearTable(table = eventsTable)
  let
    eventsCanvas = mainPaned & ".knowledgeframe.events.canvas"
    eventsFrame = eventsCanvas & ".frame"
  var rows = try:
      tclEval2(script = "grid size " & eventsFrame).split(" ")[1].parseInt
    except:
      showError(message = "Can't get the amount of rows.")
      return
  deleteWidgets(startIndex = 2, endIndex = rows - 1, frame = eventsFrame)
  var
    label = ""
    row = 1
  if eventsList.len == 0:
    label = eventsFrame & ".noevents"
    tclEval(script = "ttk::label " & label & " -text {You don't know any event yet. You may ask for events in bases. When your ship is docked to base, select Ask for Events from ship orders menu.} -wraplength 350")
    tclEval(script = "grid " & label & " -padx 10")
    tclEval(script = "bind " & eventsCanvas & " <Configure> {" & label &
        " configure -wraplength [expr [winfo width " & eventsCanvas & "] - 10]}")
  else:
    tclEval(script = "bind " & eventsCanvas & " <Configure> {}")
    row = 2
    eventsTable = createTable(parent = eventsFrame, headers = @["Name",
        "Distance", "Coordinates", "Details"], scrollbar = mainPaned &
        ".knowledgeframe.evnets.scrolly", command = "SortKnownEvents",
        tooltipText = "Press mouse button to sort the events.")
    if eventsIndexes.len != eventsList.len:
      eventsIndexes = @[]
      for index, _ in eventsList:
        eventsIndexes.add(y = index)
    let startRow = ((page - 1) * gameSettings.listsLimit) + 1
    var
      currentRow = 1
      color = ""
    for event in eventsIndexes:
      if currentRow < startRow:
        currentRow.inc
        continue
      case eventsList[event].eType
      of enemyShip:
        color = (if eventsList[event].skyX == playerShip.skyX and eventsList[
            event].skyY == playerShip.skyY: "yellow" else: "red")
        addButton(table = eventsTable, text = "Enemy ship spotted",
            tooltip = "Show the event's details", command = "ShowEventInfo " &
            $(row - 1), column = 1, color = color)
      of fullDocks:
        color = (if eventsList[event].skyX == playerShip.skyX and eventsList[
            event].skyY == playerShip.skyY: "yellow" else: "cyan")
        addButton(table = eventsTable, text = "Full docks in base",
            tooltip = "Show the event's details", command = "ShowEventInfo " &
            $(row - 1), column = 1, color = color)
      else:
        discard

proc addCommands*() {.sideEffect, raises: [], tags: [].} =
  ## Adds Tcl commands related to the known events UI
  try:
    discard
#    addCommand("ShowEventInfo", showEventInfoCommand)
  except:
    showError(message = "Can't add a Tcl command.")

# Temporary code for interfacing with Ada

proc updateAdaEventsList(page: cint) {.sideEffect, raises: [],
    tags: [], exportc.} =
  try:
    updateEventsList(page = page.Positive)
  except:
    echo getCurrentExceptionMsg()
    echo getStackTrace(getCurrentException())
