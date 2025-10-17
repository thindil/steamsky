# Copyright 2025 Bartek thindil Jasicki
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
# along with Steam Sky.  if, see <http://www.gnu.org/licenses/>.

## Provides code related to UI in bases' available missions list, like show
## the list, accept a mission, show a mission on the map, etc.

import std/[algorithm, tables]
import contracts, nuklear/nuklear_sdl_renderer
import ../[config, events, game, items, maps, missions, missions2, ships, types, utils]
import coreui, dialogs, errordialog, header, mapsui, messagesui, setui, table,
    themes, utilsui2

type
  MissionsSortOrders = enum
    none, typeAsc, typeDesc, distanceAsc, distanceDesc, detailsAsc, detailsDesc,
      timeAsc, timeDesc, rewardAsc, rewardDesc, coordAsc, coordDesc
  LocalMissionData = object
    mType: MissionsTypes
    distance: Natural
    coords: string
    details: string
    time: Natural
    reward: Natural
    id: Natural

const defaultMissionsSortOrder: MissionsSortOrders = none

var missionsSortOrder: MissionsSortOrders = defaultMissionsSortOrder

proc sortMissions(x, y: LocalMissionData): int {.raises: [], tags: [],
    contractual.} =
  ## Compare two missions and return which should go first, based on the sort
  ## order of the missions
  ##
  ## * x - the first mission to compare
  ## * y - the second mission to compare
  ##
  ## Returns 1 if the first mission should go first, -1 if the second mission
  ## should go first.
  case missionsSortOrder
  of typeAsc:
    if x.mType < y.mType:
      return 1
    return -1
  of typeDesc:
    if x.mType > y.mType:
      return 1
    return -1
  of distanceAsc:
    if x.distance < y.distance:
      return 1
    return -1
  of distanceDesc:
    if x.distance > y.distance:
      return 1
    return -1
  of detailsAsc:
    if x.details < y.details:
      return 1
    return -1
  of detailsDesc:
    if x.details > y.details:
      return 1
    return -1
  of timeAsc:
    if x.time < y.time:
      return 1
    return -1
  of timeDesc:
    if x.time > y.time:
      return 1
    return -1
  of rewardAsc:
    if x.reward < y.reward:
      return 1
    return -1
  of rewardDesc:
    if x.reward > y.reward:
      return 1
    return -1
  of coordAsc:
    if x.coords < y.coords:
      return 1
    return -1
  of coordDesc:
    if x.coords > y.coords:
      return 1
    return -1
  of none:
    return -1

proc sortMissions(sortAsc, sortDesc: MissionsSortOrders;
    dialog: var GameDialog) {.raises: [], tags: [RootEffect], contractual.} =
  ## Sort missions on the available missions list
  ##
  ## * sortAsc  - the sorting value for ascending sort
  ## * sortDesc - the sorting value for descending sort
  ## * dialog   - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  if missionsSortOrder == sortAsc:
    missionsSortOrder = sortDesc
  else:
    missionsSortOrder = sortAsc
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
      dialog = setError(message = "Can't add mission to list.")
  localMissions.sort(cmp = sortMissions)
  missionsIndexes = @[]
  for mission in localMissions:
    missionsIndexes.add(y = mission.id)

const
  headers: array[6, HeaderData[MissionsSortOrders]] = [
    HeaderData[MissionsSortOrders](label: "Name", sortAsc: typeAsc,
        sortDesc: typeDesc),
    HeaderData[MissionsSortOrders](label: "Distance", sortAsc: distanceAsc,
        sortDesc: distanceDesc),
    HeaderData[MissionsSortOrders](label: "Coordinates", sortAsc: coordAsc,
        sortDesc: coordDesc),
    HeaderData[MissionsSortOrders](label: "Details", sortAsc: detailsAsc,
        sortDesc: detailsDesc),
    HeaderData[MissionsSortOrders](label: "Time limit", sortAsc: timeAsc,
        sortDesc: timeDesc),
    HeaderData[MissionsSortOrders](label: "Base reward", sortAsc: rewardAsc,
        sortDesc: rewardDesc)]
  ratio: array[6, cfloat] = [300.cfloat, 200, 200, 300, 200, 200]

var
  missionIndex: int = -1
  missionReward, missionPercent: Natural = 0

proc showMissionInfo*(dialog: var GameDialog) {.raises: [], tags: [
    RootEffect], contractual.} =
  ## Show the selected mission information
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  const
    width: float = 400
    height: float = 300

  let
    mission: MissionData = skyBases[baseIndex].missions[missionIndex]
    windowName: string = "More info about " & getMissionType(
        mType = mission.mType)
  updateDialog(width = width, height = height)
  window(name = windowName, x = dialogX, y = dialogY, w = width, h = height,
      flags = {windowBorder, windowTitle, windowNoScrollbar, windowMovable}):
    var canAccept: bool = true
    case mission.mType
    of deliver:
      setLayoutRowDynamic(height = 30, cols = 2)
      label(str = "Item:")
      try:
        colorLabel(str = itemsList[mission.itemIndex].name,
            color = theme.colors[goldenColor])
      except:
        dialog = setError(message = "Can't get item name.")
        return
      label(str = "Weight:")
      try:
        colorLabel(str = $itemsList[mission.itemIndex].weight,
            color = theme.colors[goldenColor])
      except:
        dialog = setError(message = "Can't get item weight.")
        return
      label(str = "To base:")
      colorLabel(str = skyBases[skyMap[mission.targetX][
          mission.targetY].baseIndex].name, color = theme.colors[goldenColor])
    of patrol:
      setLayoutRowDynamic(height = 30, cols = 1)
      colorLabel(str = "Patrol selected area", color = theme.colors[goldenColor])
    of destroy:
      setLayoutRowDynamic(height = 60, cols = 2)
      label(str = "Target:")
      try:
        colorWrapLabel(str = protoShipsList[mission.shipIndex].name,
            color = theme.colors[goldenColor])
      except:
        dialog = setError(message = "Can't get ship's name.")
        return
    of explore:
      setLayoutRowDynamic(height = 30, cols = 1)
      colorLabel(str = "Explore selected area", color = theme.colors[goldenColor])
    of passenger:
      setLayoutRowDynamic(height = 60, cols = 2)
      var cabinTaken: bool = false
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
      wrapLabel(str = "Needed quality of cabin:")
      colorWrapLabel(str = getCabinQuality(quality = mission.data) & (
          if canAccept: "" elif cabinTaken: " (taken)" else: " (no cabin)"),
          color = (if canAccept: theme.colors[
          greenColor] elif cabinTaken: theme.colors[
          goldenColor] else: theme.colors[redColor]))
      label(str = "To base:")
      colorLabel(str = skyBases[skyMap[mission.targetX][
          mission.targetY].baseIndex].name, color = theme.colors[goldenColor])
    let travelValues: TravelArray = try:
            travelInfo(distance = (if mission.mType in {
            deliver, passenger}: countDistance(destinationX = mission.targetX,
            destinationY = mission.targetY) else: countDistance(
            destinationX = mission.targetX, destinationY = mission.targetY) * 2))
          except:
            dialog = setError(message = "Can't count travel values.")
            return
    if travelValues[1] > 0:
      var missionInfo: string = ""
      minutesToDate(minutes = travelValues[1], infoText = missionInfo)
      setLayoutRowDynamic(height = 30, cols = 2)
      label(str = "ETA:")
      colorLabel(str = missionInfo, color = theme.colors[goldenColor])
      label(str = "Approx fuel usage:")
      try:
        colorLabel(str = $travelValues[2] & " " & itemsList[findProtoItem(
            itemType = fuelType)].name, color = theme.colors[goldenColor])
      except:
        dialog = setError(message = "Can't get fuel name.")
        return
    setLayoutRowDynamic(height = 30, cols = (if canAccept: 3 else: 2))
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Show the mission on the map")
    setButtonStyle(field = textNormal, color = theme.colors[greenColor])
    imageLabelButton(image = images[showColoredIcon], text = "Show",
        alignment = right):
      centerX = mission.targetX
      centerY = mission.targetY
      dialog = none
      mapPreview = true
    restoreButtonStyle()
    addCloseButton(dialog = dialog, isPopup = false)
    if canAccept:
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Start negotiating accepting the mission")
      imageLabelButton(image = images[negotiateIcon], text = "Accept",
          alignment = right):
        dialog = acceptMissionDialog
        missionReward = (mission.reward.float * mission.multiplier).Natural
        missionPercent = 100

  windowSetFocus(name = windowName)

proc showAcceptMission*(dialog: var GameDialog) {.raises: [], tags: [
    RootEffect], contractual.} =
  ## Show the dialog to accept the selected mission
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  const
    width: float = 400
    height: float = 200

  let
    mission: MissionData = skyBases[baseIndex].missions[missionIndex]
    windowName: string = "Accept " & getMissionType(mType = mission.mType)
  updateDialog(width = width, height = height)
  window(name = windowName, x = dialogX, y = dialogY, w = width, h = height,
      flags = {windowBorder, windowTitle, windowNoScrollbar, windowMovable}):
    setLayoutRowDynamic(height = 30, cols = 2)
    label(str = "Reward:")
    colorLabel(str = $(missionReward) & " " & moneyName, color = theme.colors[goldenColor])
    setLayoutRowDynamic(height = 30, cols = 1)
    label(str = "Percent of " & moneyName & " as reward:")
    setLayoutRowDynamic(height = 30, cols = 2, ratio = [0.75.cfloat, 0.25])
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Move left - more reputation from mission but less money, move right - more money from mission but less reputation.")
    slider(min = 0, val = missionPercent, max = 200, step = 1)
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Move left - more reputation from mission but less money, move right - more money from mission but less reputation.")
    let newPercent: int = property2(name = "#", min = 0, val = missionPercent,
        max = 200, step = 1, incPerPixel = 1)
    if newPercent != missionPercent:
      missionPercent = newPercent
    missionReward = ((mission.reward.float * missionPercent.float) / 100.0).Natural
    setLayoutRowDynamic(height = 30, cols = 2)
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Accept the mission")
    setButtonStyle(field = textNormal, color = theme.colors[greenColor])
    imageLabelButton(image = images[negotiateColoredIcon], text = "Accept",
        alignment = right):
      dialog = none
      skyBases[baseIndex].missions[missionIndex].multiplier = (
        missionPercent.float / 100.0)
      try:
        acceptMission(missionIndex = missionIndex)
      except MissionAcceptingError:
        dialog = setMessage(message = getCurrentExceptionMsg(),
            title = "Can't accept mission")
        return
      except:
        dialog = setError(message = "Can't accept mission.")
        return
      setMissions(dialog = dialog)
    restoreButtonStyle()
    addCloseButton(dialog = dialog, icon = cancelIcon, color = redColor,
        isPopup = false, label = "Cancel")

  windowSetFocus(name = windowName)

proc setMissionInfo(data: int; dialog: var GameDialog) {.raises: [], tags: [],
    contractual.} =
  ## Set the data needed for show information about the selected mission
  ##
  ## * data   - the index of the selected mission
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog.
  missionIndex = data
  dialog = missionDialog

proc showMissions*(state: var GameState; dialog: var GameDialog) {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Show the UI with the list of available missions in the base
  ##
  ## * state - the current game's state
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameters state and dialog. The latter is modified if
  ## any error happened.
  if mapPreview or missionsText[1] == "0":
    state = map
    return
  if showHeader(dialog = dialog, close = CloseDestination.map, state = state):
    return
  # Show information about amount of missions which the player can take
  setLayoutRowStatic(height = 30, cols = 3, ratio = missionsWidth)
  label(str = missionsText[0])
  colorLabel(str = missionsText[1], color = theme.colors[goldenColor])
  label(str = missionsText[2])
  let tableHeight: float = windowHeight - gameSettings.messagesPosition.float - 50
  setLayoutRowDynamic(height = tableHeight, cols = 1)
  group(title = "MissionsGroup", flags = {windowNoFlags}):
    if dialog != none:
      windowDisable()
    # Show the list of missions
    addHeader(headers = headers, ratio = ratio, tooltip = "missions",
      code = sortMissions, dialog = dialog)
    var currentRow: Positive = 1
    let startRow: Positive = ((currentPage - 1) * gameSettings.listsLimit) + 1
    saveButtonStyle()
    setButtonStyle(field = borderColor, a = 0)
    try:
      setButtonStyle(field = normal, color = theme.colors[tableRowColor])
      setButtonStyle(field = textNormal, color = theme.colors[tableTextColor])
    except:
      dialog = setError(message = "Can't set table color")
      return
    setButtonStyle(field = rounding, value = 0)
    setButtonStyle(field = border, value = 0)
    var row: Positive = 1
    for index in missionsIndexes:
      if currentRow < startRow:
        currentRow.inc
        continue
      var
        canAccept: bool = true
        cabinTaken: bool = false
        mission: MissionData = skyBases[baseIndex].missions[index]
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
      addButton(label = getMissionType(mType = mission.mType),
          tooltip = "Show more info about the mission", data = index,
          code = setMissionInfo, dialog = dialog, color = (
          if canAccept: tableTextColor elif cabinTaken: goldenColor else: redColor))
      addButton(label = $countDistance(destinationX = mission.targetX,
          destinationY = mission.targetY),
          tooltip = "The distance to the mission", data = index,
          code = setMissionInfo, dialog = dialog)
      addButton(label = "X: " & $mission.targetX & " Y: " & $mission.targetY,
          tooltip = "Show more info about the mission", data = index,
          code = setMissionInfo, dialog = dialog)
      canAccept = true
      cabinTaken = false
      case mission.mType
      of deliver:
        try:
          addButton(label = itemsList[skyBases[baseIndex].missions[
              index].itemIndex].name & " to " & skyBases[skyMap[
              mission.targetX][mission.targetY].baseIndex].name,
              tooltip = "Show more info about the mission", data = index,
              code = setMissionInfo, dialog = dialog)
        except:
          dialog = setError(message = "Can't add delivery button.")
          return
      of patrol, explore:
        addButton(label = "X: " & $mission.targetX & " Y: " & $mission.targetY,
            tooltip = "Show more info about the mission", data = index,
            code = setMissionInfo, dialog = dialog)
      of destroy:
        if mission.shipIndex == -1:
          var enemies: seq[Positive] = @[]
          try:
            generateEnemies(enemies = enemies, withTraders = false)
          except:
            dialog = setError(message = "Can't generate enemies.")
            return
          mission.shipIndex = enemies[getRandom(min = enemies.low,
              max = enemies.high)]
          skyBases[baseIndex].missions[index].shipIndex = mission.shipIndex
        try:
          addButton(label = protoShipsList[mission.shipIndex].name,
              tooltip = "Show more info about the mission", data = index,
              code = setMissionInfo, dialog = dialog)
        except:
          dialog = setError(message = "Can't add destroy button.")
          return
      of passenger:
        addButton(label = "To " & skyBases[skyMap[mission.targetX][
            mission.targetY].baseIndex].name,
            tooltip = "Show more info about the mission", data = index,
            code = setMissionInfo, dialog = dialog)
      var missionTime: string = ""
      minutesToDate(minutes = mission.time, infoText = missionTime)
      addButton(label = missionTime, tooltip = "The time limit for finish and return the mission",
          data = index, code = setMissionInfo, dialog = dialog)
      addButton(label = $((mission.reward.float * mission.multiplier).Natural) &
          " " & moneyName, tooltip = "The base money reward for the mission",
          data = index, code = setMissionInfo, dialog = dialog)
    restoreButtonStyle()
    addPagination(page = currentPage, row = row)
  # Show the last in-game messages
  showLastMessages(theme = theme, dialog = dialog, height = windowHeight - tableHeight)
