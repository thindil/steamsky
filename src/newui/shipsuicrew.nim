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

import std/[algorithm, sequtils, strutils, tables]
import contracts, nuklear/nuklear_sdl_renderer
import ../[config, crew, game, messages, shipscrew, shipscrew2, types]
import coreui, dialogs, errordialog, setui, table, themes

type
  CrewSortOrders = enum
    selectedAsc, selectedDesc, nameAsc, nameDesc, orderAsc, orderDesc, skillAsc,
      skillDesc, healthAsc, healthDesc, fatigueAsc, fatigueDesc, thirstAsc,
      thirstDesc, hungerAsc, hungerDesc, moraleAsc, moraleDesc, none
  LocalMemberData = object
    selected: bool
    name: string
    order: string
    skill: string
    health: SkillRange
    fatigue: int
    thirst: SkillRange
    hunger: SkillRange
    morale: SkillRange
    id: Natural

const defaultCrewSortOrder*: CrewSortOrders = none

var
  showCrewOptions*: bool = false
    ## Show additonal options for managing the player's ship's crew
  skillIndex: Natural = 0
  crewSortOrder: CrewSortOrders = defaultCrewSortOrder

proc sortMembers(x, y: LocalMemberData): int {.raises: [], tags: [],
    contractual.} =
  ## Compare two members and return which should go first, members on the sort
  ## order of the members
  ##
  ## * x - the first member to compare
  ## * y - the second member to compare
  ##
  ## Returns 1 if the first member should go first, -1 if the second member
  ## should go first.
  case crewSortOrder
  of selectedAsc:
    if x.selected < y.selected:
      return 1
    return -1
  of selectedDesc:
    if x.selected > y.selected:
      return 1
    return -1
  of nameAsc:
    if x.name < y.name:
      return 1
    return -1
  of nameDesc:
    if x.name > y.name:
      return 1
    return -1
  of orderAsc:
    if x.order < y.order:
      return 1
    return -1
  of orderDesc:
    if x.order > y.order:
      return 1
    return -1
  of skillAsc:
    if x.skill < y.skill:
      return 1
    return -1
  of skillDesc:
    if x.skill > y.skill:
      return 1
    return -1
  of healthAsc:
    if x.health < y.health:
      return 1
    return -1
  of healthDesc:
    if x.health > y.health:
      return 1
    return -1
  of fatigueAsc:
    if x.fatigue < y.fatigue:
      return 1
    return -1
  of fatigueDesc:
    if x.fatigue > y.fatigue:
      return 1
    return -1
  of thirstAsc:
    if x.thirst < y.thirst:
      return 1
    return -1
  of thirstDesc:
    if x.thirst > y.thirst:
      return 1
    return -1
  of hungerAsc:
    if x.hunger < y.hunger:
      return 1
    return -1
  of hungerDesc:
    if x.hunger > y.hunger:
      return 1
    return -1
  of moraleAsc:
    if x.morale < y.morale:
      return 1
    return -1
  of moraleDesc:
    if x.morale > y.morale:
      return 1
    return -1
  of none:
    return -1

proc getHighestSkill(memberIndex: Natural;
    dialog: var GameDialog): string {.raises: [], tags: [WriteIOEffect,
    TimeEffect, RootEffect], contractual.} =
  ## Get the name of the highest skill of the selected crew member
  ##
  ## * memberIndex - the crew index of the member which the highest skill will
  ##                 be get
  ## * dialog -      the current in-game dialog displayed on the screen
  ##
  ## Returns the name of the highest skill of the selected crew member
  var
    highestLevel: Positive = 1
    highestIndex: Positive = 1
  for skill in playerShip.crew[memberIndex].skills:
    if skill.level > highestLevel:
      highestLevel = skill.level
      highestIndex = skill.index
  try:
    return skillsList[highestIndex].name
  except KeyError:
    dialog = setError(message = "Can't thge the highest skill. Index: " & $highestIndex)
    return "Unknown"

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
  var localCrew: seq[LocalMemberData] = @[]
  for index, member in playerShip.crew:
    var selected: bool = false
    for data in crewDataList:
      if data.index == index:
        selected = data.checked
        break
    try:
      localCrew.add(y = LocalMemberData(selected: selected, name: member.name,
          order: $member.order, skill: (if skillIndex == 0: getHighestSkill(
          memberIndex = index, dialog = dialog) else: getSkillLevelName(
          skillLevel = getSkillLevel(
          member = member, skillIndex = skillIndex))), health: member.health,
          fatigue: member.tired - member.attributes[conditionIndex].level,
          thirst: member.thirst, hunger: member.hunger, morale: member.morale[
              1], id: index))
    except:
      dialog = setError(message = "Can't add local crew member.")
  localCrew.sort(cmp = sortMembers)
  crewDataList = @[]
  for member in localCrew:
    crewDataList.add(y = CrewData(index: member.id, checked: member.selected))

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

proc showMemberInfo(data: int; dialog: var GameDialog) {.raises: [], tags: [
    RootEffect], contractual.} =
  ## Show the selected member information
  ##
  ## * data   - the index of the selected item
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  crewIndex = crewDataList[data].index

proc setGiveOrder(data: int; dialog: var GameDialog) {.raises: [], tags: [
    RootEffect], contractual.} =
  ## Set the dialog to give an order for the selected crew member
  ##
  ## * data   - the index of the selected item
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  crewIndex = crewDataList[data].index
  dialog = giveOrderDialog

proc showGiveOrder*(dialog: var GameDialog) {.raises: [], tags: [
    RootEffect], contractual.} =
  ## Show the dialog to give an order for the selected crew member
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog.
  const
    width: float = 400
    height: float = 200

  let
    member: MemberData = playerShip.crew[crewIndex]
    windowName: string = "Change order for " & member.name
  updateDialog(width = width, height = height)
  window(name = windowName, x = dialogX, y = dialogY, w = width, h = height,
      flags = {windowBorder, windowTitle, windowNoScrollbar, windowMovable}):
    setLayoutRowDynamic(height = 30, cols = 2)
    label(str = "Current order:")
    try:
      colorLabel(str = getCurrentOrder(memberIndex = crewIndex),
          color = theme.colors[goldenColor])
    except:
      dialog = setError(message = "Can't get the current order.")
    setButtonStyle(field = textNormal, color = theme.colors[greenColor])
    imageLabelButton(image = images[giveOrderColoredIcon], text = "Assign",
        alignment = right):
      dialog = none
    restoreButtonStyle()
    addCloseButton(dialog = dialog, icon = cancelIcon, color = redColor,
        label = "Cancel", isPopup = false)

  windowSetFocus(name = windowName)

const
  headers: array[9, HeaderData[CrewSortOrders]] = [
    HeaderData[CrewSortOrders](label: "", sortAsc: selectedAsc,
        sortDesc: selectedDesc),
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
  ratio: array[9, cfloat] = [40.cfloat, 300, 200, 200, 200, 200, 200, 200, 200]

proc showCrewInfo*(dialog: var GameDialog) {.raises: [], tags: [RootEffect],
    contractual.} =
  ## Show the list of the player's ship's crew members
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  # Show options related to managing the crew
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
      addTooltip(bounds = getWidgetBounds(), text = "Go rest " &
        (if crewDataList.any(proc (x: CrewData): bool = x.checked):
          "selected crew members" else: "everyone"))
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
      for data in crewDataList.mitems:
        data.checked = true
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(), text = "Unselect all crew member")
    imageButton(image = images[unselectAllIcon]):
      for data in crewDataList.mitems:
        data.checked = false
  # Show the list of crew members
  addHeader(headers = headers, ratio = ratio, tooltip = "items",
      code = sortCrew, dialog = dialog)
  var currentRow: Positive = 1
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
  let startRow: Positive = ((currentPage - 1) * gameSettings.listsLimit) + 1
  var row: Positive = 1
  for index, data in crewDataList.mpairs:
    if currentRow < startRow:
      currentRow.inc
      continue
    addCheckButton(tooltip = "Select the crew member to give orders to them.",
        checked = data.checked)
    addButton(label = playerShip.crew[data.index].name,
        tooltip = "Show available crew member's options", data = data.index,
        code = showMemberInfo, dialog = dialog)
    addButton(label = ($playerShip.crew[data.index].order).capitalizeAscii,
        tooltip = "The current order for the selected crew member. Press the mouse button to change it.",
        data = data.index, code = setGiveOrder, dialog = dialog)
    if skillIndex == 0:
      addButton(label = getHighestSkill(memberIndex = data.index,
          dialog = dialog),
          tooltip = "The highest skill of the selected crew member",
          data = data.index, code = showMemberInfo, dialog = dialog)
    else:
      try:
        addButton(label = getSkillLevelName(skillLevel = getSkillLevel(
            member = playerShip.crew[data.index], skillIndex = findSkillIndex(
            skillName = crewSkillsList[skillIndex]))),
            tooltip = "The level of " & crewSkillsList[skillIndex] &
            " of the selected crew member", data = data.index,
            code = showMemberInfo, dialog = dialog)
      except KeyError:
        dialog = setError(message = "Can't get the level of the skill.")
    addProgressBar(tooltip = "The current health level of the selected crew member",
        value = playerShip.crew[data.index].health, maxValue = SkillRange.high,
        data = data.index, code = showMemberInfo, dialog = dialog)
    var tiredLevel: int = playerShip.crew[data.index].tired - playerShip.crew[
        data.index].attributes[conditionIndex].level
    if tiredLevel < 0:
      tiredLevel = 0
    addProgressBar(tooltip = "The current tired level of the selected crew member",
        value = tiredLevel, maxValue = SkillRange.high, data = data.index,
        code = showMemberInfo, dialog = dialog)
    addProgressBar(tooltip = "The current thirst level of the selected crew member",
        value = playerShip.crew[data.index].thirst, maxValue = SkillRange.high,
        data = data.index, code = showMemberInfo, dialog = dialog)
    addProgressBar(tooltip = "The current hunger level of the selected crew member",
        value = playerShip.crew[data.index].hunger, maxValue = SkillRange.high,
        data = data.index, code = showMemberInfo, dialog = dialog)
    addProgressBar(tooltip = "The current morale level of the selected crew member",
        value = playerShip.crew[data.index].morale[1],
            maxValue = SkillRange.high,
        data = data.index, code = showMemberInfo, dialog = dialog)
    row.inc
    if row == gameSettings.listsLimit + 1:
      break
  restoreButtonStyle()
  addPagination(page = currentPage, row = row)
