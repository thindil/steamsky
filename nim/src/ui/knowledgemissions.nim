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
import ../[config, game, maps, missions, tk, types]
import coreui, dialogs, table, utilsui2

proc showMissionsMenuCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [], exportc.} =
  ## Show the menu with available the selected mission options
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ShowMissionMenu missionindex
  ## MissionIndex is the index of the mission's menu to show
  let
    missionIndex = try:
        ($argv[1]).parseInt - 1
      except:
        return showError(message = "Can't get the mission's index.")
    acceptedMission = acceptedMissions[missionIndex]
    missionMenu = createDialog(name = ".missionslistmenu", title = (
      case acceptedMission.mType
      of deliver:
        "Deliver item"
      of destroy:
        "Destroy enemy"
      of patrol:
        "Patrol area"
      of explore:
        "Explore area"
      of passenger:
        "Transport passenger") & " mission actions", parentName = ".", columns = 3)

  proc addButton(name, label, command, icon, tooltipText: string;
      column: Natural; color: string = "") =
    let button = missionMenu & name
    tclEval(script = "ttk::button " & button & " -text {" & label &
        "} -command {CloseDialog " & missionMenu & " .;" & command &
        "} -image " & icon & "icon -style Dialog" & color & ".TButton")
    tclEval(script = "grid " & button & " -sticky we -padx 5 -pady {0 5} -row 1 -column " & $column)
    tclEval(script = "tooltip::tooltip " & button & " \"" & tooltipText & "\"")
    tclEval(script = "bind " & button & " <Escape> {CloseDialog " &
        missionMenu & " .;break}")
    if name == ".show":
      tclEval(script = "bind " & button & " <Tab> {focus " & missionMenu & ".destination;break}")
      tclEval(script = "focus " & missionMenu & ".destination;break")

  addButton(name = ".destination", tooltipText = "Set the mission as destination for the ship.",
      command = "SetDestination2 " & $acceptedMission.targetX & " " &
      $acceptedMission.targetY, icon = "destination", label = "Target",
      column = 0, color = "green")
  addButton(name = ".close", label = "Close", command = "", icon = "exit",
      tooltipText = "Close the dialog.", column = 1)
  addButton(name = ".show", tooltipText = "Show the mission on map.",
      command = "ShowOnMap " & $acceptedMission.targetX & " " &
      $acceptedMission.targetY, icon = "show2", label = "Show", column = 2,
      color = "green")
  showDialog(dialog = missionMenu, parentFrame = ".")
  return tclOk

var
  missionsTable: TableWidget
  missionsIndexes: seq[Natural]

proc updateMissionsList(page: Positive = 1) {.sideEffect, raises: [], tags: [RootEffect].} =
  ## Update and show list of accepted missions
  ##
  ## * page     - the current page of the missions' list to show
  if missionsTable.row > 1:
    clearTable(table = missionsTable)
  let
    missionsCanvas = mainPaned & ".knowledgeframe.missions.canvas"
    missionsFrame = missionsCanvas & ".frame"
  var rows = try:
      tclEval2(script = "grid size " & missionsFrame).split(" ")[1].parseInt
    except:
      showError(message = "Can't get the amount of rows.")
      return
  deleteWidgets(startIndex = 1, endIndex = rows - 1, frame = missionsFrame)
  if acceptedMissions.len == 0:
    let label = missionsFrame & ".nomissions"
    tclEval(script = "ttk::label " & label & " -text {You didn't accept any mission yet. You may ask for missions in bases. When your ship is docked to base, check Missions from ship orders menu.} -wraplength 350")
    tclEval(script = "grid " & label & " -padx 10")
    tclEval(script = "bind " & missionsCanvas & " <Configure> {" & label &
        " configure -wraplength [expr [winfo width " & missionsCanvas & "] - 15]}")
  else:
    tclEval(script = "bind " & missionsCanvas & " <Configure> {}")
    missionsTable = createTable(parent = missionsFrame, headers = @["Name",
        "Distance", "Coordinates", "Details", "Time limit", "Base reward"],
        scrollbar = ".gameframe.paned.knowledgeframe.missions.scrolly",
        command = "SortAccepted_Missions",
        tooltipText = "Press mouse button to sort the missions.")
    if missionsIndexes.len != acceptedMissions.len:
      missionsIndexes = @[]
      for index, _ in acceptedMissions:
        missionsIndexes.add(y = index)
    var
      row = 2
      rows = 0
      currentRow = 1
    let startRow = ((page - 1) * gameSettings.listsLimit) + 1
    for index in missionsIndexes:
      if currentRow < startRow:
        currentRow.inc
        continue
      let
        acceptedMission = acceptedMissions[index]
        color = (if acceptedMission.targetX == playerShip.skyX and
            acceptedMission.targetY == playerShip.skyY: "yellow" else: "")
      addButton(table = missionsTable, text = getMissionType(
          mType = acceptedMission.mType), tooltip = "Show the mission's menu",
          command = "ShowMissionMenu " & $(row - 1), column = 1, color = color)
      case acceptedMission.mType
      of deliver:
        try:
          addButton(table = missionsTable, text = itemsList[
              acceptedMission.itemIndex].name & " to " & skyBases[skyMap[
              acceptedMission.targetX][acceptedMission.targetY].baseIndex].name,
              tooltip = "Show the mission's menu",
                  command = "ShowMissionMenu " &
              $(row - 1), column = 4, color = color)
        except:
          showError(message = "Can't add delivery button.")
          return
      of patrol, explore:
        addButton(table = missionsTable, text = "X: " &
            $acceptedMission.targetX & " Y: " & $acceptedMission.targetY,
            tooltip = "Show the mission's menu", command = "ShowMissionMenu " &
            $(row - 1), column = 4, color = color)
      of destroy:
        try:
          addButton(table = missionsTable, text = protoShipsList[
              acceptedMission.shipIndex].name,
              tooltip = "Show the mission's menu",
              command = "ShowMissionMenu " &
              $(row - 1), column = 4, color = color)
        except:
          showError(message = "Can't add destroy button.")
          return
      of passenger:
        addButton(table = missionsTable, text = "To " & skyBases[skyMap[
            acceptedMission.targetX][acceptedMission.targetY].baseIndex].name,
            tooltip = "Show the mission's menu", command = "ShowMissionMenu " &
            $(row - 1), column = 4, color = color)
      addButton(table = missionsTable, text = $countDistance(
          destinationX = acceptedMission.targetX,
          destinationY = acceptedMission.targetY),
          tooltip = "The distance to the mission",
          command = "ShowMissionMenu " & $(row - 1), column = 2, color = color)
      addButton(table = missionsTable, text = "X: " & $acceptedMission.targetX &
          " Y: " & $acceptedMission.targetY,
          tooltip = "The coordinates of the mission on the map",
          command = "ShowMissionMenu " & $(row - 1), column = 3, color = color)
      var missionTime = ""
      minutesToDate(minutes = acceptedMission.time, infoText = missionTime)
      addButton(table = missionsTable, text = missionTime,
          tooltip = "The time limit for finish and return the mission",
          command = "ShowMissionMenu " & $(row - 1), column = 5, color = color)
      addButton(table = missionsTable, text = $((acceptedMission.reward.float *
          acceptedMission.multiplier).Natural) & " " & moneyName,
          tooltip = "The base money reward for the mission",
          command = "ShowMissionMenu " & $(row - 1), column = 6, newRow = true, color = color)
      row.inc
      rows.inc
      if rows == gameSettings.listsLimit and index != acceptedMissions.len:
        break
    if page > 1:
      if rows < gameSettings.listsLimit:
        addPagination(table = missionsTable, previousCommand = "ShowMissions " &
            $(page - 1), nextCommand = "")
      else:
        addPagination(table = missionsTable, previousCommand = "ShowMissions " &
            $(page - 1), nextCommand = "ShowMissions " & $(page + 1))
    elif rows > gameSettings.listsLimit - 1:
      addPagination(table = missionsTable, previousCommand = "",
          nextCommand = "ShowMissions " & $(page + 1))
    updateTable(table = missionsTable)
  tclEval(script = "update")
  tclEval(script = missionsCanvas & " configure -scrollregion [list " &
      tclEval2(script = missionsCanvas & " bbox all") & "]")
  tclEval(script = missionsCanvas & " xview moveto 0.0")
  tclEval(script = missionsCanvas & " yview moveto 0.0")

proc showMissionsCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [
    RootEffect], exportc.} =
  ## Show the list of known missions to the player
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ShowMissions ?startindex?
  ## Page parameter is a page number which will be show
  if argc == 2:
    try:
      updateMissionsList(page = ($argv[1]).parseInt)
    except:
      tclSetResult(value = "1")
      return showError(message = "Can't show the list of missions.")
  else:
    updateMissionsList()
  tclSetResult(value = "1")
  return tclOk

type MissionsSortOrders = enum
  none, typeAsc, typeDesc, distanceAsc, distanceDesc, detailsAsc, detailsDesc,
    timeAsc, timeDesc, rewardAsc, rewardDesc, coordAsc, coordDesc

const defaultMissionsSortOrder: MissionsSortOrders = none

var missionsSortOrder: MissionsSortOrders = defaultMissionsSortOrder

proc sortMissionsCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.exportc.} =
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
    if missionsSortOrder == coordAsc:
      missionsSortOrder = coordDesc
    else:
      missionsSortOrder = coordAsc
  of 4:
    if missionsSortOrder == detailsAsc:
      missionsSortOrder = detailsDesc
    else:
      missionsSortOrder = detailsAsc
  of 5:
    if missionsSortOrder == timeAsc:
      missionsSortOrder = timeDesc
    else:
      missionsSortOrder = timeAsc
  of 6:
    if missionsSortOrder == rewardAsc:
      missionsSortOrder = rewardDesc
    else:
      missionsSortOrder = rewardAsc
  else:
    discard
  type LocalMissionData = object
    mType: MissionsTypes
    distance: Natural
    coords: string
    details: string
    time: Natural
    reward: Natural
    id: Natural
  var localMissions: seq[LocalMissionData]
  for index, mission in acceptedMissions:
    localMissions.add(y = LocalMissionData(mType: mission.mType,
        distance: countDistance(destinationX = mission.targetX,
        destinationY = mission.targetY), coords: "X: " & $mission.targetX &
        " Y: " & $mission.targetY, details: (case mission.mType
      of deliver:
        itemsList[mission.itemIndex].name & " to " & skyBases[skyMap[
            mission.targetX][mission.targetY].baseIndex].name
      of patrol, explore:
        "X: " & $mission.targetX & " Y: " & $mission.targetY
      of destroy:
        protoShipsList[mission.shipIndex].name
      of passenger:
        "To " & skyBases[skyMap[mission.targetX][
            mission.targetY].baseIndex].name),
      time: mission.time, reward: mission.reward, id: index))
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
  updateMissionsList()
  return tclOk

proc addCommands*() {.sideEffect, raises: [], tags: [].} =
  ## Adds Tcl commands related to the accepted missions UI
  try:
    discard
#    addCommand("ShowMissionMenu", showMissionsMenuCommand)
#    addCommand("ShowMissions", showMissionsCommand)
#    addCommand("SortMissions", sortMissionsCommand)
  except:
    showError(message = "Can't add a Tcl command.")

# Temporary code for interfacing with Ada

proc updateAdaMissionsList(page: cint) {.sideEffect, raises: [],
    tags: [RootEffect], exportc.} =
  try:
    updateMissionsList(page = page.Positive)
  except:
    echo getCurrentExceptionMsg()
    echo getStackTrace(getCurrentException())
