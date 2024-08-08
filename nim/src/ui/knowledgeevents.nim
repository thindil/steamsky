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
import ../[game, maps, tk]
import dialogs

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

proc addCommands*() {.sideEffect, raises: [], tags: [].} =
  ## Adds Tcl commands related to the known events UI
  try:
    discard
#    addCommand("ShowEventInfo", showEventInfoCommand)
  except:
    showError(message = "Can't add a Tcl command.")
