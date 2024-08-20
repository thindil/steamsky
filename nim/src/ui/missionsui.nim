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
import ../[events, game, maps, missions, tk, types, utils]
import coreui, mapsui, table, utilsui2

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

var
  missionsTable: TableWidget
  missionsIndexes: seq[Natural]

proc refreshMissionsList(page: Positive = 1) {.sideEffect, raises: [], tags: [].} =
  ## Refresh the list of available missions
  ##
  ## * page - The current page of the list to show. Default value is 1.
  if skyBases[baseIndex].missions.len == 0:
    tclEval(script = "grid remove " & closeButton)
    showSkyMap(clear = true)
    return
  let
    missionsLimit = countMissionsAmount()
    missionLabel = mainPaned & ".missionsframe.canvas.missions.missionslabel.missionslbl2"
  tclEval(script = missionLabel & " configure -text {" & $missionsLimit & "}")
  if missionsTable.row > 1:
    clearTable(table = missionsTable)
  if missionsIndexes.len != skyBases[baseIndex].missions.len:
    missionsIndexes = @[]
    for index, _ in skyBases[baseIndex].missions:
      missionsIndexes.add(y = index)
  let startRow = ((page - 1) * 25) + 1
  var
    currentRow = 1
    row = 2
    rows = 0
  for index in missionsIndexes:
    if currentRow < startRow:
      currentRow.inc
      continue
    var
      canAccept = true
      cabinTaken = false
    var mission = skyBases[baseIndex].missions[index]
    if mission.mType == passenger:
      canAccept = false
      for module in playerShip.modules:
        if (module.mType == ModuleType2.cabin and not canAccept) and
            module.quality >= mission.data:
          canAccept = false
          cabinTaken = true
          for owner in module.owner:
            if owner == -1:
              cabinTaken = false
              canAccept = true
              break
          if canAccept:
            break
    addButton(table = missionsTable, text = getMissionType(
        mType = mission.mType), tooltip = "Show more info about the mission",
        command = "MissionMoreInfo " & $(index + 1), column = 1, color = (
        if canAccept: "" elif cabinTaken: "yellow" else: "red"))
    canAccept = true
    cabinTaken = false
    case mission.mType
    of deliver:
      try:
        addButton(table = missionsTable, text = itemsList[skyBases[
            baseIndex].missions[index].itemIndex].name & " to " & skyBases[skyMap[
            mission.targetX][mission.targetY].baseIndex].name,
            tooltip = "Show more info about the mission",
            command = "MissionMoreInfo " & $(index + 1), column = 4)
      except:
        showError(message = "Can't add delivery button.")
        return
    of patrol, explore:
      addButton(table = missionsTable, text = "X: " & $mission.targetX &
          " Y: " & $mission.targetY, tooltip = "Show more info about the mission",
          command = "MissionMoreInfo " & $(index + 1), column = 4)
    of destroy:
      if mission.shipIndex == -1:
        var enemies: seq[Positive]
        try:
          generateEnemies(enemies = enemies, withTraders = false)
        except:
          showError(message = "Can't generate enemies.")
          return
        mission.shipIndex = enemies[getRandom(min = enemies.low,
            max = enemies.high)]
        skyBases[baseIndex].missions[index].shipIndex = mission.shipIndex
      try:
        addButton(table = missionsTable, text = protoShipsList[
            mission.shipIndex].name, tooltip = "Show more info about the mission",
            command = "MissionMoreInfo " & $(index + 1), column = 4)
      except:
        showError(message = "Can't add destroy button.")
        return
    of passenger:
      addButton(table = missionsTable, text = "To " & skyBases[skyMap[
          mission.targetX][mission.targetY].baseIndex].name,
          tooltip = "Show more info about the mission",
          command = "MissionMoreInfo " & $(index + 1), column = 4)
    addButton(table = missionsTable, text = $countDistance(
        destinationX = mission.targetX, destinationY = mission.targetY),
        tooltip = "The distance to the mission", command = "MissionMoreInfo " &
        $(index + 1), column = 2)
    addButton(table = missionsTable, text = "X: " & $mission.targetX & " Y: " &
        $mission.targetY, tooltip = "Show more info about the mission",
        command = "MissionMoreInfo " & $(index + 1), column = 3)
    var missionTime = ""
    minutesToDate(minutes = mission.time, infoText = missionTime)
    addButton(table = missionsTable, text = missionTime,
        tooltip = "The time limit for finish and return the mission",
        command = "MissionMoreInfo " & $(index + 1), column = 5)
    addButton(table = missionsTable, text = $((mission.reward.float *
        mission.multiplier).Natural) & " " & moneyName,
        tooltip = "The base money reward for the mission",
        command = "MissionMoreInfo " & $(index + 1), column = 6, newRow = true)
    row.inc
    rows.inc
    if rows == 25 and index != skyBases[baseIndex].missions.high:
      break
  if page > 1:
    if rows < 25:
      addPagination(table = missionsTable,
          previousCommand = "ShowBaseMissions " & $(page - 1), nextCommand = "")
    else:
      addPagination(table = missionsTable,
          previousCommand = "ShowBaseMissions " & $(page - 1),
          nextCommand = "ShowBaseMissions " & $(page + 1))
  elif rows > 24:
    addPagination(table = missionsTable, previousCommand = "",
        nextCommand = "ShowBaseMissions " & $(page + 1))

proc showBaseMissionsCommand(clientData: cint; interp: PInterp; argc: cint;
   argv: cstringArray): TclResults {.exportc.} =
  var missionsFrame = mainPaned & ".missionsframe"
  let
    missionsCanvas = missionsFrame & ".canvas"
    label = missionsCanvas & ".missions.missionslabel"
  if tclEval2(script = "winfo exists " & label) == "0":
    tclEvalFile(fileName = dataDirectory & "ui" & DirSep & "missions.tcl")
    tclEval(script = "bind " & missionsFrame & " <Configure> {ResizeCanvas %W.canvas %w %h}")
#    addCommand("ShowMission", showMissionCommand)
#    addCommand("SetMission", setMissionCommand)
    missionsTable = createTable(parent = missionsCanvas & ".missions",
        headers = @["Name", "Distance", "Coordinates", "Details", "Time limit",
        "Base reward"], scrollbar = mainPaned & ".missionsframe.scrolly",
        command = "SortAvailableMissions",
        tooltipText = "Press mouse button to sort the missions.")
  elif tclEval2(script = "winfo ismapped " & label) == "1" and argc == 1:
    showSkyMap(clear = true)
    return tclOk
  return tclOk

proc addCommands*() {.sideEffect, raises: [], tags: [].} =
  ## Adds Tcl commands related to the list of available missions
  try:
    discard
#    addCommand("ShowBaseMissions", showBaseMissionsCommand)
  except:
    showError(message = "Can't add a Tcl command.")

# Temporary code for interfacing with Ada

proc getMissionBaseIndex(bIndex: cint) {.exportc.} =
  baseIndex = bIndex

proc countAdaMissionsAmount(): cint {.exportc.} =
  return countMissionsAmount().cint

proc refreshAdaMissionsList(page: cint) {.exportc.} =
  refreshMissionsList(page = page.Positive)
