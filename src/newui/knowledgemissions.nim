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
# along with Steam Sky.  If not, see <http://www.gnu.org/licenses/>.

## Provides code related to the information about the list of accepted
## missions, like sorting them, showing information about them, etc.

import contracts, nuklear/nuklear_sdl_renderer
import ../config
import coreui, errordialog, setui, table, themes

type MissionsSortOrders = enum
  none, typeAsc, typeDesc, distanceAsc, distanceDesc, timeAsc, timeDesc,
    rewardAsc, rewardDesc, coordAsc, coordDesc

const defaultMissionsSortOrder: MissionsSortOrders = none

var
  missionsSortOrder: MissionsSortOrders = defaultMissionsSortOrder
  missionIndex: Natural = 0

proc showMissionInfo(data: int; dialog: var GameDialog) {.raises: [], tags: [
    RootEffect], contractual.} =
  ## Show the selected mission's actions menu
  ##
  ## * data   - the index of the selected item
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  missionIndex = data

proc sortMissions(sortAsc, sortDesc: MissionsSortOrders;
    dialog: var GameDialog) {.raises: [], tags: [RootEffect], contractual.} =
  ## Sort missions on the list
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

proc showMissionsInfo*(dialog: var GameDialog) {.raises: [], tags: [RootEffect],
    contractual.} =
  ## Show the list of the accepted missions
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  # No missions
  if missionsUIList.len == 0:
    setLayoutRowDynamic(height = 100, cols = 1)
    wrapLabel(str = "You didn't accept any mission yet. You may ask for missions in bases. When your ship is docked to base, check Missions from ship orders menu.")
  else:
    const
      headers: array[5, HeaderData[MissionsSortOrders]] = [
        HeaderData[MissionsSortOrders](label: "Name", sortAsc: typeAsc,
            sortDesc: typeDesc),
        HeaderData[MissionsSortOrders](label: "Distance",
            sortAsc: distanceAsc, sortDesc: distanceDesc),
        HeaderData[MissionsSortOrders](label: "Coordinates", sortAsc: coordAsc,
            sortDesc: coordDesc),
        HeaderData[MissionsSortOrders](label: "Time limit",
            sortAsc: timeAsc, sortDesc: timeDesc),
        HeaderData[MissionsSortOrders](label: "Base reward",
            sortAsc: rewardAsc, sortDesc: rewardDesc)]
      ratio: array[5, cfloat] = [200.cfloat, 100, 150, 250, 150]

    addHeader(headers = headers, ratio = ratio, tooltip = "missions",
        code = sortMissions, dialog = dialog)
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
    var
      row, currentRow: Positive = 1
    # Show the list of accepted missions
    for mission in missionsUIList:
      if currentRow < startRow:
        currentRow.inc
        continue
      setButtonStyle(field = textNormal, color = theme.colors[mission.color])
      addButton(label = mission.name, tooltip = "Show the mission's menu",
          data = mission.index, code = showMissionInfo, dialog = dialog)
      addButton(label = $mission.distance, tooltip = "Show the mission's menu",
          data = mission.index, code = showMissionInfo, dialog = dialog)
      addButton(label = mission.coords, tooltip = "Show the mission's menu",
          data = mission.index, code = showMissionInfo, dialog = dialog)
      addButton(label = mission.timeLimit, tooltip = "Show the mission's menu",
          data = mission.index, code = showMissionInfo, dialog = dialog)
      addButton(label = mission.baseReward, tooltip = "Show the mission's menu",
          data = mission.index, code = showMissionInfo, dialog = dialog)
      setButtonStyle(field = textNormal, color = theme.colors[tableTextColor])
      row.inc
      if row == gameSettings.listsLimit + 1:
        break
    restoreButtonStyle()
    addPagination(page = currentPage, row = row)
