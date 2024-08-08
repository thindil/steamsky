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
import ../[game, maps, tk]

proc showEventInfoCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {. exportc.} =
  let
    eventIndex = ($argv[1]).parseInt - 1
    baseIndex = skyMap[eventsList[eventIndex].skyX][eventsList[eventIndex].skyX].baseIndex
  var eventInfo = "X: {gold}" & $eventsList[eventIndex].skyX & "{/gold} Y: {gold}" & $eventsList[eventIndex].skyY
  case eventsList[eventIndex].eType
  of enemyShip, enemyPatrol, trader, friendlyShip:
    eventInfo.add(y = "\nShip type: {gold}" & protoShipsList[eventsList[eventIndex].shipIndex].name & "{/gold}")
  of fullDocks, attackOnBase, disease:
    eventInfo.add(y = "\nBase name: {gold}" & skyBases[baseIndex].name & "{/gold}")
  of doublePrice:
    eventInfo.add(y = "\nBase name: {gold}" & skyBases[baseIndex].name & "{/gold}")
    eventInfo.add(y = "\nItem: {gold}" & itemsList[eventsList[eventIndex].itemIndex].name & "{/gold}")
  return tclOk

proc addCommands*() {.sideEffect, raises: [], tags: [].} =
  ## Adds Tcl commands related to the known events UI
  try:
    discard
#    addCommand("ShowEventInfo", showEventInfoCommand)
  except:
    showError(message = "Can't add a Tcl command.")
