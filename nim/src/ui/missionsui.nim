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
import ../[game, maps, missions, tk]

var baseIndex = 0

proc showMissionCommand(clientData: cint; interp: PInterp; argc: cint;
   argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [], exportc.} =
  ## Show mission on map
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ShowMission missionindex
  ## MissionIndex is the index of the mission to show on map
  let missionIndex = try:
      ($argv[1]).parseInt - 1
    except:
      return showError(message = "Can't get the mission index.")
  tclEval(script = "ShowOnMap " & $skyBases[baseIndex].missions[
      missionIndex].targetX & " " & $skyBases[baseIndex].missions[
      missionIndex].targetX)
  return tclOk

proc countMissionsAmount(): Natural {.sideEffect, raises: [], tags: [], exportc.} =
  ## Count the amount of missions which the player can get from the selected
  ##
  ## Returns the amount of missions which the player can get from the base
  result = (case skyBases[skyMap[playerShip.skyX][
      playerShip.skyY].baseIndex].reputation.level
    of 0..25:
      1
    of 26..50:
      3
    of 51..75:
      5
    of 76..100:
      10
    else:
      0)
  for mission in acceptedMissions:
    if mission.startBase == skyMap[playerShip.skyX][playerShip.skyY].baseIndex:
      result.dec
      if result == 0:
        break

proc addCommands*() {.sideEffect, raises: [], tags: [].} =
  ## Adds Tcl commands related to the list of available missions
  try:
    discard
#    addCommand("ShowMission", showMissionCommand)
  except:
    showError(message = "Can't add a Tcl command.")

# Temporary code for interfacing with Ada

proc getMissionBaseIndex(bIndex: cint) {.exportc.} =
  baseIndex = bIndex

proc countAdaMissionsAmount(): cint {.exportc.} =
  return countMissionsAmount().cint
