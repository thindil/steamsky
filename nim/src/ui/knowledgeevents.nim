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
import ../[config, game, maps, tk, types]
import coreui, dialogs, errordialog, table

proc showEventInfoCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [WriteIOEffect, TimeEffect].} =
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
  of EventsTypes.none, baseRecovery:
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

proc updateEventsList*(page: Positive = 1) {.raises: [], tags: [RootEffect].} =
  ## Update and show list of known events
  ##
  ## * page     - the current page of the events' list to show
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
  deleteWidgets(startIndex = 1, endIndex = rows - 1, frame = eventsFrame)
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
        ".knowledgeframe.events.scrolly", command = "SortKnownEvents",
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
      of attackOnBase:
        color = (if eventsList[event].skyX == playerShip.skyX and eventsList[
            event].skyY == playerShip.skyY: "yellow" else: "red")
        addButton(table = eventsTable, text = "Base is under attack",
            tooltip = "Show the event's details", command = "ShowEventInfo " &
            $(row - 1), column = 1, color = color)
      of disease:
        color = (if eventsList[event].skyX == playerShip.skyX and eventsList[
            event].skyY == playerShip.skyY: "yellow" else: "yellow3")
        addButton(table = eventsTable, text = "Disease in base",
            tooltip = "Show the event's details", command = "ShowEventInfo " &
            $(row - 1), column = 1, color = color)
      of enemyPatrol:
        color = (if eventsList[event].skyX == playerShip.skyX and eventsList[
            event].skyY == playerShip.skyY: "yellow" else: "red3")
        addButton(table = eventsTable, text = "Enemy patrol",
            tooltip = "Show the event's details", command = "ShowEventInfo " &
            $(row - 1), column = 1, color = color)
      of doublePrice:
        color = (if eventsList[event].skyX == playerShip.skyX and eventsList[
            event].skyY == playerShip.skyY: "yellow" else: "lime")
        addButton(table = eventsTable, text = "Double price in base",
            tooltip = "Show the event's details", command = "ShowEventInfo " &
            $(row - 1), column = 1, color = color)
      of trader:
        color = (if eventsList[event].skyX == playerShip.skyX and eventsList[
            event].skyY == playerShip.skyY: "yellow" else: "green")
        addButton(table = eventsTable, text = "Friendly trader spotted",
            tooltip = "Show the event's details", command = "ShowEventInfo " &
            $(row - 1), column = 1, color = color)
      of friendlyShip:
        color = (if eventsList[event].skyX == playerShip.skyX and eventsList[
            event].skyY == playerShip.skyY: "yellow" else: "green")
        addButton(table = eventsTable, text = "Friendly ship spotted",
            tooltip = "Show the event's details", command = "ShowEventInfo " &
            $(row - 1), column = 1, color = color)
      of EventsTypes.none, baseRecovery:
        discard
      addButton(table = eventsTable, text = $countDistance(
          destinationX = eventsList[event].skyX, destinationY = eventsList[
          event].skyY), tooltip = "The distance to the event",
          command = "ShowEventInfo " & $(row - 1), column = 2, color = color)
      addButton(table = eventsTable, text = "X: " & $eventsList[event].skyX &
          " Y: " & $eventsList[event].skyY,
          tooltip = "The coordinates of the event on the map",
          command = "ShowEventInfo " & $(row - 1), column = 3, color = color)
      case eventsList[event].eType
      of doublePrice:
        try:
          addButton(table = eventsTable, text = itemsList[eventsList[
              event].itemIndex].name & " in " & skyBases[skyMap[eventsList[
              event].skyX][eventsList[event].skyY].baseIndex].name,
              tooltip = "Show the event's details", command = "ShowEventInfo " &
              $(row - 1), column = 4, newRow = true, color = color)
        except:
          showError(message = "Can't add item info button.")
          return
      of attackOnBase, disease, fullDocks, enemyPatrol:
        addButton(table = eventsTable, text = skyBases[skyMap[eventsList[
            event].skyX][eventsList[event].skyY].baseIndex].name,
            tooltip = "Show the event's details", command = "ShowEventInfo " &
            $(row - 1), column = 4, newRow = true, color = color)
      of enemyShip, trader, friendlyShip:
        try:
          addButton(table = eventsTable, text = protoShipsList[eventsList[
              event].shipIndex].name, tooltip = "Show the event's details",
              command = "ShowEventInfo " & $(row - 1), column = 4,
                  newRow = true, color = color)
        except:
          showError(message = "Can't add ship info button.")
          return
      of EventsTypes.none, baseRecovery:
        discard
      row.inc
      if eventsTable.row == gameSettings.listsLimit + 1:
        break
    if page > 1:
      if eventsTable.row < gameSettings.listsLimit + 1:
        addPagination(table = eventsTable, previousCommand = "ShowEvents " & $(
            page - 1), nextCommand = "")
      else:
        addPagination(table = eventsTable, previousCommand = "ShowEvents " & $(
            page - 1), nextCommand = "ShowEvents " & $(page + 1))
    elif eventsTable.row > gameSettings.listsLimit:
      addPagination(table = eventsTable, previousCommand = "",
          nextCommand = "ShowEvents " & $(page + 1))
    updateTable(table = eventsTable)
  tclEval(script = "update")
  tclEval(script = eventsCanvas & " configure -scrollregion [list " & tclEval2(
      script = eventsCanvas & " bbox all") & "]")
  tclEval(script = eventsCanvas & " xview moveto 0.0")
  tclEval(script = eventsCanvas & " yview moveto 0.0")

proc showEventsCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [
    RootEffect].} =
  ## Show the list of known events to the player
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ShowEvents ?startindex?
  ## Page parameter is a page number which will be show
  if argc == 2:
    try:
      updateEventsList(page = ($argv[1]).parseInt)
    except:
      return showError(message = "Can't show the list of known events.")
  else:
    updateEventsList()
  tclSetResult(value = "1")
  return tclOk

type EventsSortOrders = enum
  none, typeAsc, typeDesc, distanceAsc, distanceDesc, detailsAsc, detailsDesc,
    coordAsc, coordDesc

const defaultEventsSortOrder: EventsSortOrders = none

var eventsSortOrder: EventsSortOrders = defaultEventsSortOrder

proc sortEventsCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [
    RootEffect].} =
  ## Show the list of known events to the player
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## SortKnownEvents x
  ## X is X axis coordinate where the player clicked the mouse button
  let column = try:
        getColumnNumber(table = eventsTable, xPosition = ($argv[1]).parseInt)
      except:
        return showError(message = "Can't get the column number.")
  case column
  of 1:
    if eventsSortOrder == typeAsc:
      eventsSortOrder = typeDesc
    else:
      eventsSortOrder = typeAsc
  of 2:
    if eventsSortOrder == distanceAsc:
      eventsSortOrder = distanceDesc
    else:
      eventsSortOrder = distanceAsc
  of 3:
    if eventsSortOrder == detailsAsc:
      eventsSortOrder = detailsDesc
    else:
      eventsSortOrder = detailsAsc
  of 4:
    if eventsSortOrder == coordAsc:
      eventsSortOrder = coordDesc
    else:
      eventsSortOrder = coordAsc
  else:
    discard
  type LocalEventData = object
    eType: EventsTypes
    distance: Natural
    coords: string
    details: string
    id: Natural
  var localEvents: seq[LocalEventData]
  for index, event in eventsList:
    try:
      localEvents.add(y = LocalEventData(eType: event.eType,
          distance: countDistance(destinationX = event.skyX,
          destinationY = event.skyY), coords: "X: " & $event.skyX & " Y: " &
          $event.skyY, details: (case event.eType
      of doublePrice:
        itemsList[event.itemIndex].name & " in " & skyBases[skyMap[event.skyX][
            event.skyY].baseIndex].name
      of attackOnBase, disease, fullDocks, enemyPatrol:
        skyBases[skyMap[event.skyX][event.skyY].baseIndex].name
      of enemyShip, trader, friendlyShip:
        protoShipsList[event.shipIndex].name
      of EventsTypes.none, baseRecovery:
        ""), id: index))
    except:
      return showError(message = "Can't add local event.")
  proc sortEvents(x, y: LocalEventData): int =
    case eventsSortOrder
    of typeAsc:
      if x.eType < y.eType:
        return 1
      else:
        return -1
    of typeDesc:
      if x.eType > y.eType:
        return 1
      else:
        return -1
    of distanceAsc:
      if x.distance < y.distance:
        return 1
      else:
        return -1
    of distanceDesc:
      if x.distance > y.distance:
        return 1
      else:
        return -1
    of detailsAsc:
      if x.details < y.details:
        return 1
      else:
        return -1
    of detailsDesc:
      if x.details > y.details:
        return 1
      else:
        return -1
    of coordAsc:
      if x.coords < y.coords:
        return 1
      else:
        return -1
    of coordDesc:
      if x.coords > y.coords:
        return 1
      else:
        return -1
    of none:
      return -1
  localEvents.sort(cmp = sortEvents)
  eventsIndexes = @[]
  for event in localEvents:
    eventsIndexes.add(y = event.id)
  updateEventsList()
  return tclOk

proc addCommands*() {.raises: [], tags: [WriteIOEffect, TimeEffect].} =
  ## Adds Tcl commands related to the known events UI
  try:
    addCommand("ShowEventInfo", showEventInfoCommand)
    addCommand("ShowEvents", showEventsCommand)
    addCommand("SortKnownEvents", sortEventsCommand)
  except:
    showError(message = "Can't add a Tcl command.")

# Temporary code for interfacing with Ada

proc updateAdaEventsList(page: cint) {.raises: [],
    tags: [RootEffect], exportc.} =
  try:
    updateEventsList(page = page.Positive)
  except:
    echo getCurrentExceptionMsg()
    echo getStackTrace(getCurrentException())
