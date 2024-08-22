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
import ../[config, events, game, maps, missions, missions2, ships, tk, types, utils]
import coreui, dialogs, mapsui, table, utilsui2

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

proc setMissionCommand(clientData: cint; interp: PInterp; argc: cint;
   argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [RootEffect], exportc.} =
  ## Accept the mission in a base
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## SetMission missionindex
  ## MissionIndex is the index of the mission to accept
  let missionIndex = try:
      ($argv[1]).parseInt - 1
    except:
      return showError(message = "Can't get the mission index.")
  try:
    skyBases[baseIndex].missions[missionIndex].multiplier = (tclGetVar(
        varName = "reward").parseFloat / 100.0)
  except:
    return showError(message = "Can't set the mission's multiplier.")
  try:
    acceptMission(missionIndex = missionIndex)
  except MissionAcceptingError:
    showMessage(text = getCurrentExceptionMsg(), title = "Can't accept mission")
    return tclOk
  except:
    return showError(message = "Can't accept mission.")
  if countMissionsAmount() > 0:
    refreshMissionsList()
    updateTable(table = missionsTable)
    updateMessages()
  else:
    tclEval(script = "grid remove " & closeButton)
    showSkyMap(clear = true)
  return tclOk

proc showBaseMissionsCommand(clientData: cint; interp: PInterp; argc: cint;
   argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [RootEffect], exportc.} =
  ## Show the list of available missions in the base
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ShowBaseMissions ?page?
  ## Page is the number of page of the missions list to show. If not
  ## set then it is 1
  var missionsFrame = mainPaned & ".missionsframe"
  let
    missionsCanvas = missionsFrame & ".canvas"
    label = missionsCanvas & ".missions.missionslabel"
  if tclEval2(script = "winfo exists " & label) == "0":
    tclEvalFile(fileName = dataDirectory & "ui" & DirSep & "missions.tcl")
    tclEval(script = "bind " & missionsFrame & " <Configure> {ResizeCanvas %W.canvas %w %h}")
    # addCommand("ShowMission", showMissionCommand)
    # addCommand("SetMission", setMissionCommand)
    missionsTable = createTable(parent = missionsCanvas & ".missions",
        headers = @["Name", "Distance", "Coordinates", "Details", "Time limit",
        "Base reward"], scrollbar = mainPaned & ".missionsframe.scrolly",
        command = "SortAvailableMissions",
        tooltipText = "Press mouse button to sort the missions.")
  elif tclEval2(script = "winfo ismapped " & label) == "1" and argc == 1:
    showSkyMap(clear = true)
    return tclOk
  tclSetVar(varName = "gamestate", newValue = "missions")
  tclEval(script = "grid " & closeButton & " -row 0 -column 1")
  baseIndex = skyMap[playerShip.skyX][playerShip.skyY].baseIndex
  if skyBases[baseIndex].missions.len == 0:
    showSkyMap(clear = true)
    return tclOk
  try:
    refreshMissionsList(page = (if argc > 1: ($argv[1]).parseInt else: 1))
  except:
    return showError(message = "Can't referesh the list of missions.")
  updateTable(table = missionsTable)
  tclEval(script = missionsCanvas & " configure -height [expr " & tclEval2(
      script = mainPaned & " sashpos 0") & " - 20] -width " & tclEval2(
      script = mainPaned & " cget -width"))
  tclEval(script = "update")
  missionsFrame = missionsCanvas & ".missions"
  tclEval(script = missionsCanvas & " create window 0 0 -anchor nw -window " & missionsFrame)
  tclEval(script = "update")
  tclEval(script = missionsCanvas & " configure -scrollregion [list " &
      tclEval2(script = missionsCanvas & " bbox all") & "]")
  showScreen(newScreenName = "missionsframe")
  return tclOk

proc missionMoreInfoCommand(clientData: cint; interp: PInterp; argc: cint;
   argv: cstringArray): TclResults {.exportc.} =
  let
    missionIndex = ($argv[1]).parseInt - 1
    mission = skyBases[baseIndex].missions[missionIndex]
    missionDialog = createDialog(name = ".missiondialog",
        title = "More info about " & getMissionType(mType = mission.mType))
    label = missionDialog & ".infolabel"
  tclEval(script = "text " & label & " -height 5 -width 30")
  tclEval(script = label & " tag configure gold -foreground " & tclGetVar(
      varName = "ttk::theme::" & gameSettings.interfaceTheme &
      "::colors(-goldenyellow)"))
  tclEval(script = label & " tag configure red -foreground " & tclGetVar(
      varName = "ttk::theme::" & gameSettings.interfaceTheme &
      "::colors(-red)"))
  tclEval(script = label & " tag configure green -foreground " & tclGetVar(
      varName = "ttk::theme::" & gameSettings.interfaceTheme &
      "::colors(-green)"))
  case mission.mType
  of deliver:
    tclEval(script = label & " insert end {Item: }")
    tclEval(script = label & " insert end {" & itemsList[
        mission.itemIndex].name & "} [list gold]")
    tclEval(script = label & " insert end {\nWeight: }")
    tclEval(script = label & " insert end {" & $itemsList[
        mission.itemIndex].weight & "} [list gold]")
    tclEval(script = label & " insert end {\nTo base: }")
    tclEval(script = label & " insert end {" & skyBases[skyMap[mission.targetX][
        mission.targetY].baseIndex].name & "} [list gold]")
  of patrol:
    tclEval(script = label & " insert end {Patrol selected area} [list gold]")
  of destroy:
    tclEval(script = label & " insert end {Target: }")
    tclEval(script = label & " insert end {" & protoShipsList[
        mission.shipIndex].name & "} [list gold]")
  of explore:
    tclEval(script = label & " insert end {Explore selected area} [list gold]")
  of passenger:
    var canAccept, cabinTaken = false
    for module in playerShip.modules:
      if (module.mType == ModuleType2.cabin and not canAccept) and
          module.quality >= mission.data:
        canAccept = true
        cabinTaken = false
        for owner in module.owner:
          if owner > -1:
            cabinTaken = true
            canAccept = false
            break
        if canAccept:
          break
    if baseIndex == 0:
      canAccept = true
    tclEval(script = label & " insert end {Needed quality of cabin: }")
    tclEval(script = label & " insert end {" & getCabinQuality(
        quality = mission.data) & (
        if canAccept: "} [list green]" elif cabinTaken: " (taken)} [list gold]" else: " (no cabin)} [list red]"))
    tclEval(script = label & " insert end {\nTo base: }")
    tclEval(script = label & " insert end {" & skyBases[skyMap[mission.targetX][
        mission.targetY].baseIndex].name & "} [list gold]")
  let travelValues = travelInfo(distance = (if mission.mType in {deliver,
      passenger}: countDistance(destinationX = mission.targetX,
      destinationY = mission.targetY) else: countDistance(
      destinationX = mission.targetX, destinationY = mission.targetY) * 2))
  if travelValues[1] > 0:
    var missionInfo = ""
    minutesToDate(minutes = travelValues[1], infoText = missionInfo)
    tclEval(script = label & " insert end {\nETA:}")
    tclEval(script = label & " insert end {" & missionInfo & "} [list gold]")
    tclEval(script = label & " insert end {\nApprox fuel usage: }")
    tclEval(script = label & " insert end {" & $travelValues[2] & "} [list gold]")
  return tclOk

proc addCommands*() {.sideEffect, raises: [], tags: [].} =
  ## Adds Tcl commands related to the list of available missions
  try:
    discard
#    addCommand("ShowBaseMissions", showBaseMissionsCommand)
#    addCommand("MissionMoreInfo", missionMoreInfoCommand)
  except:
    showError(message = "Can't add a Tcl command.")

# Temporary code for interfacing with Ada

proc getMissionBaseIndex(bIndex: cint) {.exportc.} =
  baseIndex = bIndex

proc countAdaMissionsAmount(): cint {.exportc.} =
  return countMissionsAmount().cint

proc refreshAdaMissionsList(page: cint) {.exportc.} =
  refreshMissionsList(page = page.Positive)
