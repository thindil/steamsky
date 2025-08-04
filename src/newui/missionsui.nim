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

import contracts, nuklear/nuklear_sdl_renderer
import ../[config, types]
import coreui, header, messagesui, setui, themes

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

proc showMissions*(state: var GameState; dialog: var GameDialog) {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Show the UI with the list of available missions in the base
  ##
  ## * state - the current game's state
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameters state and dialog. The latter is modified if
  ## any error happened.
  if showHeader(dialog = dialog, close = CloseDestination.map, state = state):
    return
  let tableHeight: float = windowHeight - gameSettings.messagesPosition.float - 20
  setLayoutRowDynamic(height = tableHeight, cols = 1)
  group(title = "MissionsGroup", flags = {windowNoFlags}):
    if dialog != none:
      windowDisable()
    # Show information about amount of missions which the player can take
    setLayoutRowStatic(height = 30, cols = 3, ratio = missionsWidth)
    label(str = missionsText[0])
    colorLabel(str = missionsText[1], color = theme.colors[goldenColor])
    label(str = missionsText[2])
  showLastMessages(theme = theme, dialog = dialog, height = windowHeight - tableHeight)
