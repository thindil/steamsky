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

## Provides code related to the information about the player's ship's crew
## members, like listing them, showing information, give orders, etc.

import contracts, nuklear/nuklear_sdl_renderer
import ../[config, game, messages, shipscrew, types]
import coreui, errordialog, setui, table, themes

type
  CrewSortOrders = enum
    nameAsc, nameDesc, orderAsc, orderDesc, skillAsc, skillDesc, healthAsc,
      healthDesc, fatigueAsc, fatigueDesc, thirstAsc, thirstDesc, hungerAsc,
      hungerDesc, moraleAsc, moraleDesc, none

const defaultCrewSortOrder*: CrewSortOrders = none

var
  showCrewOptions*: bool = false
    ## Show additonal options for managing the player's ship's crew
  skillIndex: Natural = 0
  crewSortOrder: CrewSortOrders = defaultCrewSortOrder

proc sortCrew(sortAsc, sortDesc: CrewSortOrders;
    dialog: var GameDialog) {.raises: [], tags: [RootEffect], contractual.} =
  ## Sort items on the trades list
  ##
  ## * sortAsc  - the sorting value for ascending sort
  ## * sortDesc - the sorting value for descending sort
  ## * dialog   - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  if crewSortOrder == sortAsc:
    crewSortOrder = sortDesc
  else:
    crewSortOrder = sortAsc

proc ordersForAll(order: CrewOrders; dialog: var GameDialog) {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Give the selected order to all crew members
  ##
  ## * order - the order to give
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  for i in playerShip.crew.low .. playerShip.crew.high:
    try:
      giveOrders(ship = playerShip, memberIndex = i, givenOrder = order)
    except CrewOrderError:
      addMessage(message = getCurrentExceptionMsg(), mType = orderMessage)
    except:
      dialog = setError(message = "Can't give orders.")

const
  headers: array[8, HeaderData[CrewSortOrders]] = [
    HeaderData[CrewSortOrders](label: "Name", sortAsc: nameAsc,
        sortDesc: nameDesc),
    HeaderData[CrewSortOrders](label: "Order", sortAsc: orderAsc,
        sortDesc: orderDesc),
    HeaderData[CrewSortOrders](label: "Skill", sortAsc: skillAsc,
        sortDesc: skillDesc),
    HeaderData[CrewSortOrders](label: "Health", sortAsc: healthAsc,
        sortDesc: healthDesc),
    HeaderData[CrewSortOrders](label: "Fatigue", sortAsc: fatigueAsc,
        sortDesc: fatigueDesc),
    HeaderData[CrewSortOrders](label: "Thirst", sortAsc: thirstAsc,
        sortDesc: thirstDesc),
    HeaderData[CrewSortOrders](label: "Hunger", sortAsc: hungerAsc,
        sortDesc: hungerDesc),
    HeaderData[CrewSortOrders](label: "Morale", sortAsc: moraleAsc,
        sortDesc: moraleDesc)]
  ratio: array[8, cfloat] = [300.cfloat, 200, 200, 200, 200, 200, 200, 200]

proc showCrewInfo*(dialog: var GameDialog) {.raises: [], tags: [RootEffect],
    contractual.} =
  ## Show the list of the player's ship's crew members
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  if showCrewOptions:
    var
      cols: Positive = 2
      ratio: seq[cfloat] = @[0.4.cfloat, 0.1]
    if needClean:
      cols.inc
      ratio.add(y = 0.1.cfloat)
    if needRepair:
      cols.inc
      ratio.add(y = 0.1.cfloat)
    setLayoutRowDynamic(height = 35, cols = cols, ratio = ratio)
    label(str = "Orders for all:")
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(), text = "Go rest everyone")
    imageButton(image = images[goRestIcon]):
      ordersForAll(order = rest, dialog = dialog)
    if needClean:
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(), text = "Clean the ship everyone")
      imageButton(image = images[cleanOrderIcon]):
        ordersForAll(order = clean, dialog = dialog)
    if needRepair:
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Repair the ship everyone")
      imageButton(image = images[repairOrderIcon]):
        ordersForAll(order = repair, dialog = dialog)
    setLayoutRowDynamic(height = 35, cols = 2, ratio = [0.4.cfloat, 0.6])
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Show the level of the selected skill for the crew members.If selected option 'Highest', show the highest skill of the crew members.")
    label(str = "Skill:")
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Show the level of the selected skill for the crew members.If selected option 'Highest', show the highest skill of the crew members.")
    let newSkill = comboList(items = crewSkillsList,
        selected = skillIndex, itemHeight = 25, x = 200, y = 150)
    if newSkill != skillIndex:
      skillIndex = newSkill
    setLayoutRowStatic(height = 35, cols = 2, width = 35)
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(), text = "Select all crew member")
    imageButton(image = images[selectAllIcon]):
      discard
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(), text = "Unselect all crew member")
    imageButton(image = images[unselectAllIcon]):
      discard
  # Show the list of crew members
  addHeader(headers = headers, ratio = ratio, tooltip = "items",
      code = sortCrew, dialog = dialog)
  var currentRow: Positive = 1
  let startRow: Positive = ((currentPage - 1) * gameSettings.listsLimit) + 1
  for index, mIndex in crewIndexes:
    if currentRow < startRow:
      currentRow.inc
      continue
