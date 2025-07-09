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

## Provides code related to various interactions in bases, like buying recipes,
## repair ship, healing wounded crew memebrs, etc.

import std/[algorithm, sequtils]
import contracts, nuklear/nuklear_sdl_renderer
import ../[basestrade, basesship2, config, game, types]
import coreui, errordialog, header, messagesui, setui, table, themes

type
  BaseSortOrders = enum
    nameAsc, nameDesc, costAsc, costDesc, timeAsc, timeDesc, none

const defaultBaseSortOrder: BaseSortOrders = none

var
  baseSortOrder: BaseSortOrders = defaultBaseSortOrder
  baseState: GameState = healWounded

proc sortItems(x, y: BaseItemData): int {.raises: [], tags: [], contractual.} =
  ## Check how to sort the selected items on the list
  ##
  ## * x - the first item to sort
  ## * y - the second item to sort
  ##
  ## Returns 1 if the x item should go first, otherwise -1
  case baseSortOrder
  of nameAsc:
    if x.name < y.name:
      return 1
    return -1
  of nameDesc:
    if x.name > y.name:
      return 1
    return -1
  of costAsc:
    if x.cost < y.cost:
      return 1
    return -1
  of costDesc:
    if x.cost > y.cost:
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
  of none:
    return -1

proc sortItems(sortAsc, sortDesc: BaseSortOrders;
    dialog: var GameDialog) {.raises: [], tags: [RootEffect], contractual.} =
  ## Sort items in the base lists
  ##
  ## * sortAsc  - the sorting value for ascending sort
  ## * sortDesc - the sorting value for descending sort
  ## * dialog   - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  if baseSortOrder == sortAsc:
    baseSortOrder = sortDesc
  else:
    baseSortOrder = sortAsc
  if baseState == healWounded:
    actionsList = setWoundedList(dialog = dialog)
  actionsList.sort(cmp = sortItems)

var actionId: int = -1

proc setActionMenu(data: int; dialog: var GameDialog) {.raises: [], tags: [
    RootEffect], contractual.} =
  ## Show the menu with the option to heal the selected wounded crew member
  ##
  ## * data   - the index of the selected crew member
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  actionId = data

proc showWoundedMenu(bounds: NimRect; dialog: var GameDialog;
    state: var GameState) {.raises: [], tags: [RootEffect], contractual.} =
  ## Show the menu for the selected wounded crew membre
  ##
  ## * bounds - the rectangle in which the player should click the mouse's
  ##            button to show the menu
  ## * dialog - the current in-game dialog displayed on the screen
  ## * state  - the current game's state
  ##
  ## Returns the modified parameters dialog and state. Dialog is modified if
  ## any error happened and state is modified when there is no other crew
  ## members to heal.
  contextualMenu(flags = {windowNoFlags}, x = 150, y = 150,
      triggerBounds = bounds, button = (
      if gameSettings.rightButton: Buttons.right else: Buttons.left)):
    setLayoutRowDynamic(25, 1)
    contextualItemLabel(label = "Buy healing", align = centered):
      try:
        healWounded(memberIndex = actionId - 1)
        actionsList = setWoundedList(dialog = dialog)
        if actionsList.len == 1:
          state = map
      except:
        dialog = setError(message = "Can't heal wounded.")
    contextualItemLabel(label = "Close", align = centered):
      discard

const
  headers: array[3, HeaderData[BaseSortOrders]] = [
    HeaderData[BaseSortOrders](label: "Action", sortAsc: nameAsc,
        sortDesc: nameDesc),
    HeaderData[BaseSortOrders](label: "Cost", sortAsc: costAsc,
        sortDesc: costDesc),
    HeaderData[BaseSortOrders](label: "Time", sortAsc: timeAsc,
        sortDesc: timeDesc)]
  ratio: array[3, cfloat] = [400.cfloat, 200, 200]

proc formatTime(time: Natural): string {.raises: [], tags: [], contractual.} =
  ## Format the amount of time needed for the action
  ##
  ## * time - the time to format
  ##
  ## Returns string with formatted time
  if time < 60:
    result = $time & " minute"
    if time > 1:
      result.add(y = "s")
  else:
    let hours: Positive = (time / 60).Positive
    result = $hours & " hour"
    if hours > 1:
      result.add(y = "s")
    if time mod 60 > 0:
      result.add(y = " and " & $(time mod 60) & " minute")
      if time mod 60 > 1:
        result.add(y = "s")

proc showWounded*(state: var GameState; dialog: var GameDialog) {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Show the UI with the list of wounded the player's ship's crew members
  ##
  ## * state - the current game's state
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameters state and dialog. The latter is modified if
  ## any error happened.
  if showHeader(dialog = dialog, close = CloseDestination.map, state = state):
    return
  baseState = state
  let tableHeight: float = windowHeight - gameSettings.messagesPosition.float - 20
  setLayoutRowDynamic(height = tableHeight, cols = 1)
  group(title = "HealGroup", flags = {windowNoFlags}):
    if dialog != none:
      windowDisable()
    # Show information about money owned by the player
    setLayoutRowStatic(height = 30, cols = moneyWidth.len, ratio = moneyWidth)
    for index, text in moneyText:
      if index mod 2 == 0:
        label(str = text)
      else:
        colorLabel(str = text, color = theme.colors[goldenColor])
    addHeader(headers = headers, ratio = ratio, tooltip = "actions",
      code = sortItems, dialog = dialog)
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
    for action in actionsList:
      addButton(label = action.name, tooltip = "Show available options",
          data = action.id, code = setActionMenu, dialog = dialog)
      addButton(label = $action.cost & " " & moneyName,
          tooltip = "Show available options", data = action.id,
          code = setActionMenu, dialog = dialog)
      addButton(label = action.time.formatTime,
          tooltip = "Show available options", data = action.id,
          code = setActionMenu, dialog = dialog)
    restoreButtonStyle()
  showLastMessages(theme = theme, dialog = dialog, height = windowHeight - tableHeight)
  showWoundedMenu(bounds = NimRect(x: 0, y: 135, w: windowWidth, h: (
      actionsList.len * 35).float), dialog = dialog, state = state)

proc showRepairMenu(bounds: NimRect; dialog: var GameDialog;
    state: var GameState) {.raises: [], tags: [RootEffect], contractual.} =
  ## Show the menu for the selected damaged player's ship's module
  ##
  ## * bounds - the rectangle in which the player should click the mouse's
  ##            button to show the menu
  ## * dialog - the current in-game dialog displayed on the screen
  ## * state  - the current game's state
  ##
  ## Returns the modified parameters dialog and state. Dialog is modified if
  ## any error happened and state is modified when there is no other ship's
  ## module to repair.
  contextualMenu(flags = {windowNoFlags}, x = 150, y = 150,
      triggerBounds = bounds, button = (
      if gameSettings.rightButton: Buttons.right else: Buttons.left)):
    setLayoutRowDynamic(25, 1)
    contextualItemLabel(label = "Buy repair", align = centered):
      try:
        repairShip(moduleIndex = actionId - 1)
        actionsList = setRepairsList(dialog = dialog)
        if actionsList.all(pred = proc (x: BaseItemData): bool = x.id < 1):
          state = map
      except:
        dialog = setError(message = "Can't repair the ship.")
    contextualItemLabel(label = "Close", align = centered):
      discard

proc showRepairs*(state: var GameState; dialog: var GameDialog) {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Show the UI with the list of damaged player's ship's modules
  ##
  ## * state - the current game's state
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameters state and dialog. The latter is modified if
  ## any error happened.
  if showHeader(dialog = dialog, close = CloseDestination.map, state = state):
    return
  baseState = state
  let tableHeight: float = windowHeight - gameSettings.messagesPosition.float - 20
  setLayoutRowDynamic(height = tableHeight, cols = 1)
  group(title = "RepairGroup", flags = {windowNoFlags}):
    if dialog != none:
      windowDisable()
    # Show information about money owned by the player
    setLayoutRowStatic(height = 30, cols = moneyWidth.len, ratio = moneyWidth)
    for index, text in moneyText:
      if index mod 2 == 0:
        label(str = text)
      else:
        colorLabel(str = text, color = theme.colors[goldenColor])
    addHeader(headers = headers, ratio = ratio, tooltip = "actions",
      code = sortItems, dialog = dialog)
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
    for action in actionsList:
      addButton(label = action.name, tooltip = "Show available options",
          data = action.id, code = setActionMenu, dialog = dialog)
      addButton(label = $action.cost & " " & moneyName,
          tooltip = "Show available options", data = action.id,
          code = setActionMenu, dialog = dialog)
      addButton(label = action.time.formatTime,
          tooltip = "Show available options", data = action.id,
          code = setActionMenu, dialog = dialog)
    restoreButtonStyle()
    restoreButtonStyle()
  showLastMessages(theme = theme, dialog = dialog, height = windowHeight - tableHeight)
  showRepairMenu(bounds = NimRect(x: 0, y: 135, w: windowWidth, h: (
      actionsList.len * 40).float), dialog = dialog, state = state)

proc showRecipes*(state: var GameState; dialog: var GameDialog) {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Show the UI with the list of crafting recipes to buy
  ##
  ## * state - the current game's state
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameters state and dialog. The latter is modified if
  ## any error happened.
  if showHeader(dialog = dialog, close = CloseDestination.map, state = state,
      options = true):
    return
  baseState = state
  let tableHeight: float = windowHeight - gameSettings.messagesPosition.float - 20
  setLayoutRowDynamic(height = tableHeight, cols = 1)
  group(title = "RepairGroup", flags = {windowNoFlags}):
    if dialog != none:
      windowDisable()
    # Show information about money owned by the player
    setLayoutRowStatic(height = 30, cols = moneyWidth.len, ratio = moneyWidth)
    for index, text in moneyText:
      if index mod 2 == 0:
        label(str = text)
      else:
        colorLabel(str = text, color = theme.colors[goldenColor])
    addHeader(headers = headers, ratio = ratio, tooltip = "actions",
      code = sortItems, dialog = dialog)
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
    # Table here
    restoreButtonStyle()
    restoreButtonStyle()
  showLastMessages(theme = theme, dialog = dialog, height = windowHeight - tableHeight)
