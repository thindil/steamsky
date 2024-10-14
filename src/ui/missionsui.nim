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

import std/[algorithm, os, strutils, tables]
import ../[config, events, game, items, maps, missions, missions2, ships, tk,
    types, utils]
import coreui, dialogs, errordialog, mapsui, table, utilsui2

var baseIndex = 0

proc showMissionCommand(clientData: cint; interp: PInterp; argc: cint;
   argv: cstringArray): TclResults {.raises: [], tags: [WriteIOEffect, TimeEffect], cdecl.} =
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

proc countMissionsAmount(): Natural {.raises: [], tags: [].} =
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

proc refreshMissionsList(page: Positive = 1) {.raises: [], tags: [WriteIOEffect, TimeEffect].} =
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
   argv: cstringArray): TclResults {.raises: [], tags: [RootEffect], cdecl.} =
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
   argv: cstringArray): TclResults {.raises: [], tags: [RootEffect], cdecl.} =
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
    try:
      addCommand("ShowMission", showMissionCommand)
      addCommand("SetMission", setMissionCommand)
    except:
      return showError(message = "Can't add Tcl commands.")
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
   argv: cstringArray): TclResults {.raises: [], tags: [WriteIOEffect, TimeEffect], cdecl.} =
  ## Show more info about the selected mission
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## MissionMoreInfo missionindex
  ## MissionIndex is the index of the mission's info to show
  let
    missionIndex = try:
        ($argv[1]).parseInt - 1
      except:
        return showError(message = "Can't get mission index.")
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
  var canAccept = true
  case mission.mType
  of deliver:
    tclEval(script = label & " insert end {Item: }")
    try:
      tclEval(script = label & " insert end {" & itemsList[
          mission.itemIndex].name & "} [list gold]")
    except:
      return showError(message = "Can't get item name.")
    tclEval(script = label & " insert end {\nWeight: }")
    try:
      tclEval(script = label & " insert end {" & $itemsList[
          mission.itemIndex].weight & "} [list gold]")
    except:
      return showError(message = "Can't get item weight.")
    tclEval(script = label & " insert end {\nTo base: }")
    tclEval(script = label & " insert end {" & skyBases[skyMap[mission.targetX][
        mission.targetY].baseIndex].name & "} [list gold]")
  of patrol:
    tclEval(script = label & " insert end {Patrol selected area} [list gold]")
  of destroy:
    tclEval(script = label & " insert end {Target: }")
    try:
      tclEval(script = label & " insert end {" & protoShipsList[
          mission.shipIndex].name & "} [list gold]")
    except:
      return showError(message = "Can't get ship's name.")
  of explore:
    tclEval(script = label & " insert end {Explore selected area} [list gold]")
  of passenger:
    var cabinTaken = false
    canAccept = false
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
    tclEval(script = label & " insert end {" & $travelValues[2] & " } [list gold]")
    try:
      tclEval(script = label & " insert end {" & itemsList[findProtoItem(
          itemType = fuelType)].name & "}")
    except:
      return showError(message = "Can't get fuel name.")
  tclEval(script = label & " configure -state disabled")
  tclEval(script = "grid " & label & " -padx 5")
  let buttonsFrame = missionDialog & ".buttons"
  tclEval(script = "ttk::frame " & buttonsFrame)
  var button = buttonsFrame & ".button1"
  tclEval(script = "ttk::button " & button &
      " -text Show -image show2icon -command {CloseDialog " & missionDialog &
      ";set mappreview 1;ShowOnMap " & $mission.targetX & " " &
      $mission.targetY & "} -style Dialoggreen.TButton")
  tclEval(script = "tooltip::tooltip " & button & " \"Show the mission on the map\"")
  tclEval(script = "grid " & button & " -padx 5")
  tclEval(script = "bind " & button & " <Tab> {focus " & buttonsFrame & ".button;break}")
  tclEval(script = "bind " & button & " <Escape> {" & buttonsFrame & ".button invoke;break}")
  addCloseButton(name = buttonsFrame & ".button", text = "Close",
      command = "CloseDialog " & missionDialog, column = 1)
  if canAccept:
    button = buttonsFrame & ".button"
    tclEval(script = "bind " & button & " <Tab> {focus " & buttonsFrame & ".button2;break}")
    button = buttonsFrame & ".button2"
    tclEval(script = "ttk::button " & button &
        " -text Accept -image negotiateicon -command {CloseDialog " &
        missionDialog & ";AcceptMission " & $argv[1] & "} -style Dialog.TButton")
    tclEval(script = "tooltip::tooltip " & button & " \"Start negiotiating accepting the mission\"")
    tclEval(script = "grid " & button & " -row 0 -column 2 -padx 5")
    tclEval(script = "bind " & button & " <Tab> {focus " & buttonsFrame & ".button1;break}")
    tclEval(script = "bind " & button & " <Escape> {" & buttonsFrame & ".button invoke;break}")
  tclEval(script = "grid " & buttonsFrame & " -padx 5 -pady 5")
  showDialog(dialog = missionDialog)
  return tclOk

proc acceptMissionCommand(clientData: cint; interp: PInterp; argc: cint;
   argv: cstringArray): TclResults {.raises: [], tags: [WriteIOEffect, TimeEffect], cdecl.} =
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
  ## AcceptMission missionindex
  ## MissionIndex is the index of the mission to accept
  let
    missionIndex = try:
        ($argv[1]).parseInt - 1
      except:
        return showError(message = "Can't get the mission index.")
    mission = skyBases[baseIndex].missions[missionIndex]
    missionDialog = createDialog(name = ".missiondialog", title = "Accept " &
        getMissionType(mType = mission.mType), columns = 2)
    rewardScale = missionDialog & ".reward"
  tclEval(script = "ttk::scale " & rewardScale &
      " -from 0 -to 200 -variable reward -command {UpdateMissionReward " &
      $argv[1] & "} -length 300")
  tclEval(script = "tooltip::tooltip " & rewardScale & " \"Move left - more reputation from mission but less money,\nmove right - more money from mission but less reputation.\"")
  let buttonsBox = missionDialog & ".buttons"
  tclEval(script = "ttk::frame " & buttonsBox)
  var button = buttonsBox & ".accept"
  tclEval(script = "ttk::button " & button &
      " -text Accept -command {CloseDialog " & missionDialog & ";SetMission " &
      $argv[1] & "} -image negotiate2icon -style Dialoggreen.TButton")
  let rewardField = missionDialog & ".rewardfield"
  tclEval(script = "ttk::spinbox " & rewardField &
      " -from 0 -to 200 -textvariable reward -validate key -validatecommand {ValidateSpinbox %W %P " &
      button & "} -width 3")
  tclEval(script = "tooltip::tooltip " & rewardField & " \"Lower value - more reputation from mission but less money,\nhigher value - more money from mission but less reputation.\"")
  let rewardBox = missionDialog & ".rewardbox"
  tclEval(script = "ttk::frame " & rewardBox)
  var rewardLabel = rewardBox & ".rewardlbl"
  tclEval(script = "ttk::label " & rewardLabel & " -text {Reward: }")
  tclEval(script = "grid " & rewardLabel & " -stick w")
  rewardLabel = rewardBox & ".rewardlbl2"
  tclEval(script = "ttk::label " & rewardLabel & " -text {" & $((
      mission.reward.float * mission.multiplier).Natural) & "} -style Golden.TLabel")
  tclEval(script = "grid " & rewardLabel & " -row 0 -column 1 -stick w")
  tclEval(script = "grid " & rewardBox & " -columnspan 2 -padx 5 -stick w")
  rewardLabel = missionDialog & ".rewardinfo"
  tclEval(script = "ttk::label " & rewardLabel & " -text {Percent of " &
      moneyName & " as reward:}")
  tclEval(script = "grid " & rewardLabel & " -columnspan 2 -padx 5 -stick w")
  tclSetVar(varName = "reward", newValue = "100")
  tclEval(script = "grid " & rewardScale & " -padx {5 0} -stick w")
  tclEval(script = "grid " & rewardField & " -row 3 -column 1 -padx {0 5} -stick w")
  tclEval(script = "bind " & rewardField & " <Tab> {focus " & button & ";break}")
  tclEval(script = "bind " & rewardField & " <Escape> {" & missionDialog & ".buttons.cancel invoke;break}")
  tclEval(script = "grid " & button & " -pady 5")
  tclEval(script = "bind " & button & " <Escape> {" & missionDialog & ".buttons.cancel invoke;break}")
  button = buttonsBox & ".cancel"
  tclEval(script = "ttk::button " & button &
      " -text Cancel -command {CloseDialog " & missionDialog & "} -image cancelicon -style Dialogred.TButton")
  tclEval(script = "grid " & button & " -row 0 -column 1 -pady 5 -padx 5")
  tclEval(script = "bind " & button & " <Tab> {focus " & rewardScale & ";break}")
  tclEval(script = "bind " & button & " <Escape> {" & button & " invoke;break}")
  tclEval(script = "bind " & rewardScale & " <Escape> {" & button & " invoke;break}")
  tclEval(script = "grid " & buttonsBox & " -columnspan 2 -pady 5")
  showDialog(dialog = missionDialog)
  tclEval(script = "focus " & button)
  return tclOk

proc updateMissionRewardCommand(clientData: cint; interp: PInterp; argc: cint;
   argv: cstringArray): TclResults {.raises: [], tags: [WriteIOEffect, TimeEffect], cdecl.} =
  ## Update the information about the selected mission reward
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## UpdateMissionReward missionindex
  ## MissionIndex is the index of the mission to update info
  let value = try:
      tclGetVar(varName = "reward").parseFloat.Natural
    except:
      return showError(message = "Can't get the value.")
  tclSetVar(varName = "reward", newValue = $(value.Natural))
  let
    missionIndex = try:
        ($argv[1]).parseInt - 1
      except:
        return showError(message = "Can't get the mission's index.")
    mission = skyBases[baseIndex].missions[missionIndex]
    rewardLabel = ".missiondialog.rewardbox.rewardlbl2"
  tclEval(script = rewardLabel & " configure -text {" & $((
      mission.reward.float * value.float) / 100.0).Natural & " " & moneyName & "}")
  return tclOk

type MissionsSortOrders = enum
  none, typeAsc, typeDesc, distanceAsc, distanceDesc, detailsAsc, detailsDesc,
    timeAsc, timeDesc, rewardAsc, rewardDesc, coordAsc, coordDesc

const defaultMissionsSortOrder: MissionsSortOrders = none

var missionsSortOrder: MissionsSortOrders = defaultMissionsSortOrder

proc sortAvailableMissionsCommand(clientData: cint; interp: PInterp; argc: cint;
   argv: cstringArray): TclResults {.raises: [], tags: [RootEffect], cdecl.} =
  ## Sort the list of available missions
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## SortAvailableMissions x
  ## X is X axis coordinate where the player clicked the mouse button
  let column = try:
        getColumnNumber(table = missionsTable, xPosition = ($argv[1]).parseInt)
      except:
        return showError(message = "Can't get the column number.")
  case column
  of 1:
    if missionsSortOrder == typeAsc:
      missionsSortOrder = typeDesc
    else:
      missionsSortOrder = typeAsc
  of 2:
    if missionsSortOrder == distanceAsc:
      missionsSortOrder = distanceDesc
    else:
      missionsSortOrder = distanceAsc
  of 3:
    if missionsSortOrder == detailsAsc:
      missionsSortOrder = detailsDesc
    else:
      missionsSortOrder = detailsAsc
  of 4:
    if missionsSortOrder == timeAsc:
      missionsSortOrder = timeDesc
    else:
      missionsSortOrder = timeAsc
  of 5:
    if missionsSortOrder == rewardAsc:
      missionsSortOrder = rewardDesc
    else:
      missionsSortOrder = rewardAsc
  of 6:
    if missionsSortOrder == coordAsc:
      missionsSortOrder = coordDesc
    else:
      missionsSortOrder = coordAsc
  else:
    discard
  if missionsSortOrder == none:
    return tclOk
  type LocalMissionData = object
    mType: MissionsTypes
    distance: Natural
    coords: string
    details: string
    time: Natural
    reward: Natural
    id: Natural
  var localMissions: seq[LocalMissionData] = @[]
  for index, mission in skyBases[baseIndex].missions:
    try:
      localMissions.add(y = LocalMissionData(mType: mission.mType,
          distance: countDistance(destinationX = mission.targetX,
          destinationY = mission.targetY), coords: "X: " & $mission.targetX &
          " Y: " & $mission.targetY, details: (case mission.mType
        of deliver: itemsList[mission.itemIndex].name & " to " & skyBases[
            skyMap[mission.targetX][mission.targetY].baseIndex].name
        of patrol, explore: "X: " & $mission.targetX & " Y: " & $mission.targetY
        of destroy: protoShipsList[mission.shipIndex].name
        of passenger: "To " & skyBases[skyMap[mission.targetX][
            mission.targetY].baseIndex].name), time: mission.time,
            reward: mission.reward, id: index))
    except:
      return showError(message = "Can't add mission to list.")
  proc sortMissions(x, y: LocalMissionData): int =
    case missionsSortOrder
    of typeAsc:
      if x.mType < y.mType:
        return 1
      else:
        return -1
    of typeDesc:
      if x.mType > y.mType:
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
    of timeAsc:
      if x.time < y.time:
        return 1
      else:
        return -1
    of timeDesc:
      if x.time > y.time:
        return 1
      else:
        return -1
    of rewardAsc:
      if x.reward < y.reward:
        return 1
      else:
        return -1
    of rewardDesc:
      if x.reward > y.reward:
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
  localMissions.sort(cmp = sortMissions)
  missionsIndexes = @[]
  for mission in localMissions:
    missionsIndexes.add(y = mission.id)
  refreshMissionsList()
  updateTable(table = missionsTable)
  return tclOk

proc addCommands*() {.raises: [], tags: [WriteIOEffect, TimeEffect].} =
  ## Adds Tcl commands related to the list of available missions
  try:
    addCommand("ShowBaseMissions", showBaseMissionsCommand)
    addCommand("MissionMoreInfo", missionMoreInfoCommand)
    addCommand("AcceptMission", acceptMissionCommand)
    addCommand("UpdateMissionReward", updateMissionRewardCommand)
    addCommand("SortAvailableMissions", sortAvailableMissionsCommand)
  except:
    showError(message = "Can't add a Tcl command.")
