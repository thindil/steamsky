# Copyright 2025-2026 Bartek thindil Jasicki
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

## Provides code related to the game's header, like showing buttons and icons
## on it, the game's menu, etc.

import std/[colors, math, tables, strutils]
import contracts, nuklear/nuklear_sdl_renderer
import ../[config, game, maps, messages, shipscargo, shipsmovement, types]
import coreui, dialogs, errordialog, ordersmenu, setui, themes, waitmenu

proc showResourcesInfo(fuelAmount, foodAmount,
    drinksAmount: Natural) {.raises: [], tags: [RootEffect], contractual.} =
  ## Show information about food, drinks and fuel in the player's ship's cargo
  ##
  ## * fuelAmount   - the amount of fuel on the player's ship
  ## * foodAmount   - the amount of food on the player's ship
  ## * drinksAmount - the amount of drinks on the player's ship
  var
    color: Color = colBlack
    image: PImage = nil
    tooltipText: string = ""
  if fuelAmount > gameSettings.lowFuel:
    color = theme.colors[greenColor]
    image = images[fuelIcon]
    tooltipText = "The amount of fuel in the ship's cargo."
  elif fuelAmount > 0:
    color = theme.colors[goldenColor]
    image = images[lowFuelIcon]
    tooltipText = "Low level of fuel on ship. Only " & $fuelAmount & " left."
  else:
    color = theme.colors[redColor]
    image = images[noFuelIcon]
    tooltipText = "You can't travel anymore, because you don't have any fuel for ship."
  image(image = image, padding = Vec2(x: 5, y: 5), tooltip = tooltipText)
  colorLabel(str = $fuelAmount, color = color, tooltip = tooltipText)
  if foodAmount > gameSettings.lowFood:
    color = theme.colors[greenColor]
    image = images[foodIcon]
    tooltipText = "The amount of food in the ship's cargo."
  elif foodAmount > 0:
    color = theme.colors[goldenColor]
    image = images[lowFoodIcon]
    tooltipText = "Low level of food on ship. Only " & $foodAmount & " left."
  else:
    color = theme.colors[redColor]
    image = images[noFoodIcon]
    tooltipText = "You don't have any food in ship but your crew needs them to live."
  image(image = image, padding = Vec2(x: 5, y: 5), tooltip = tooltipText)
  colorLabel(str = $foodAmount, color = color, tooltip = tooltipText)
  if drinksAmount > gameSettings.lowFood:
    color = theme.colors[greenColor]
    image = images[drinksIcon]
    tooltipText = "The amount of drinks in the ship's cargo."
  elif drinksAmount > 0:
    color = theme.colors[goldenColor]
    image = images[lowDrinksIcon]
    tooltipText = "Low level of drinks on ship. Only " & $drinksAmount & " left."
  else:
    color = theme.colors[redColor]
    image = images[noDrinksIcon]
    tooltipText = "You don't have any drinks in ship but your crew needs them to live."
  image(image = image, padding = Vec2(x: 5, y: 5), tooltip = tooltipText)
  colorLabel(str = $drinksAmount, color = color, tooltip = tooltipText)

proc showNotifications(speed: float; havePilot, haveEngineer, haveTrader,
    haveUpgrader, haveCleaner, haveRepairman, haveGunner, needRepairs,
    needWorker, haveWorker, needCleaning: bool;
    faction: FactionData) {.raises: [], tags: [], contractual.} =
  ## Show various notifications icons, like lack of crew members on position,
  ## crafting something, etc
  ##
  ## * speed         - the current speed of the player's ship
  ## * havePilot     - do there is a pilot on duty
  ## * haveEngineer  - do there is an engineer on duty
  ## * haveTrader    - do there is a trader on duty
  ## * haveUpgrader  - do there is someone who upgrading the ship
  ## * haveCleaner   - do there is someone who cleans the ship
  ## * haveRepairman - do there is someone who repairs the ship
  ## * haveGunner    - do all guns have gunners
  ## * needRepairs   - do the ship need repair
  ## * needWorker    - do there are crafting orders set
  ## * haveWorker    - do all crafting orders have assigned worker
  ## * needCleaning  - do the ship is dirty
  ## * faction       - the player's faction
  var
    image: PImage = nil
    tooltipText: string = ""
  if speed < 0.5 and ((havePilot and haveEngineer) or "sentientships" in
      faction.flags):
    image(image = images[overloadedIcon], padding = Vec2(x: 5, y: 5),
        tooltip = "You can't fly with your ship, because it is overloaded.")
  if not havePilot:
    if "sentientships" in faction.flags:
      image = images[noPilotIcon]
      tooltipText = "No pilot assigned. Ship fly on it own."
    else:
      image = images[pilotIcon]
      tooltipText = "No pilot assigned. Ship can't move."
    image(image = image, padding = Vec2(x: 5, y: 5), tooltip = tooltipText)
  if not haveEngineer:
    if "sentientships" in faction.flags:
      image = images[noEngineerIcon]
      tooltipText = "No engineer assigned. Ship fly on it own."
    else:
      image = images[engineerIcon]
      tooltipText = "No engineer assigned. Ship can't move."
    image(image = image, padding = Vec2(x: 5, y: 5), tooltip = tooltipText)
  if not haveGunner:
    image(image = images[gunnerIcon], padding = Vec2(x: 5, y: 5),
        tooltip = "One or more guns don't have a gunner.")
  if not haveTrader:
    image(image = images[traderIcon], padding = Vec2(x: 5, y: 5),
        tooltip = "No trader assigned. You need one to talk/trade.")
  if needRepairs:
    if haveRepairman:
      image(image = images[repairIcon], padding = Vec2(x: 5, y: 5),
          tooltip = "The ship is being repaired.")
    else:
      image(image = images[noRepairIcon], padding = Vec2(x: 5, y: 5),
          tooltip = "The ship needs repairs but no one is working them.")
  if needWorker:
    if haveWorker:
      image(image = images[manufactureIcon], padding = Vec2(x: 5, y: 5),
          tooltip = "All crafting orders are being executed.")
    else:
      image(image = images[noManufactureIcon], padding = Vec2(x: 5, y: 5),
          tooltip = "You need to assign crew members to begin manufacturing.")
  if playerShip.upgradeModule > -1:
    if haveUpgrader:
      image(image = images[upgradeIcon], padding = Vec2(x: 5, y: 5),
          tooltip = "A ship module upgrade in progress.")
    else:
      image(image = images[noUpgradeIcon], padding = Vec2(x: 5, y: 5),
          tooltip = "A ship module upgrade is in progress but no one is working on it.")
  if needCleaning:
    if haveCleaner:
      image(image = images[cleanIcon], padding = Vec2(x: 5, y: 5),
          tooltip = "Ship is cleaned.")
    else:
      image(image = images[noCleanIcon], padding = Vec2(x: 5, y: 5),
          tooltip = "Ship is dirty but no one is cleaning it.")

type
  CloseDestination* = enum
    ## Destination for the close button in the header
    none, combat, map, previous

var expandedSection*: Natural = 0
  ## Expanded section in some screens, like ship info or knowledge screens

proc showDialogs(dialog: var GameDialog; state: var GameState;
    oldState: GameState): bool {.raises: [], tags: [RootEffect], contractual.} =
  ## Show various in-game dialogs
  ##
  ## * dialog   - the current in-game dialog displayed on the screen
  ## * state    - the current state of the game
  ## * oldState - the previous state of the game UI
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened or the game's menu is to show. Additionally, it returns true if
  ## the game's state changed, otherwise false.
  if playerShip.crew[0].health == 0 and dialog == none and state != gameStatistics:
    dialog = setQuestion(question = "You are dead. Would you like to see your game statistics?",
        qType = showDeadStats)
  # Draw dialogs
  showQuestion(dialog = dialog, state = state)
  if state != oldState:
    return true
  showShipOrders(dialog = dialog, state = state)
  if state != oldState:
    return true
  showMessage(dialog = dialog)
  showInfo(dialog = dialog)
  return false

proc showInfo(dialog: var GameDialog): bool {.raises: [], tags: [RootEffect],
    contractual.} =
  ## Show the game time and the player's ship's speed info
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened or the game's menu is to show. Additionally, it returns true if
  ## the game's state changed, otherwise false.
  if gameSettings.showNumbers:
    try:
      label(str = formattedTime() & " Speed: " & $((realSpeed(
          ship = playerShip) * 60) / 1_000) & " km/h", alignment = centered,
          tooltip = "Game time and current ship speed.")
    except:
      dialog = setError(message = "Can't get the ship's speed")
      return true
  else:
    label(str = formattedTime(), alignment = centered, tooltip = "Game time.")

proc closeScreen(close: CloseDestination; state: var GameState;
    dialog: var GameDialog) {.raises: [], tags: [RootEffect],
    contractual.} =
  ## Close the currently opened screen and back to the previous one
  ##
  ## * close   - the close button's destination. If none, don't show the button
  ## * state   - the current state of the game
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameters state and dialog. The later is modified
  ## only when an error happened.
  showOptions = false
  expandedSection = 0
  case close
  of combat:
    state = combat
  of map:
    state = map
  of previous:
    state = previousState
  of none:
    discard
  if previousState == options:
    updateOptions(dialog = dialog)

proc showShipInfo(dialog: var GameDialog; state: var GameState) {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Show the ship info screen
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ## * state   - the current state of the game
  ##
  ## Returns the modified parameters dialog and state.
  if state == shipInfo:
    state = previousState
    previousState = emptyState
  else:
    if previousState == emptyState:
      previousState = state
    setShipInfo(dialog = dialog)
    state = shipInfo
    mapPreview = false
  dialog = none

proc showCraftScreen(dialog: var GameDialog; state: var GameState) {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Show the crafting screen
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ## * state   - the current state of the game
  ##
  ## Returns the modified parameters dialog and state.
  if state == crafting:
    state = previousState
    previousState = emptyState
  else:
    if previousState == emptyState:
      previousState = state
    setCrafting(dialog = dialog)
    state = crafting
    mapPreview = false
  dialog = none

proc showLastMessagesScreen(dialog: var GameDialog;
    state: var GameState) {.raises: [], tags: [RootEffect], contractual.} =
  ## Show the last messages screen
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ## * state   - the current state of the game
  ##
  ## Returns the modified parameters dialog and state.
  if state == lastMessages:
    state = previousState
    previousState = emptyState
  else:
    if previousState == emptyState:
      previousState = state
    state = lastMessages
    mapPreview = false
  dialog = none

proc showKnowledgeScreen(dialog: var GameDialog;
    state: var GameState) {.raises: [], tags: [RootEffect], contractual.} =
  ## Show the knowledge screen
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ## * state   - the current state of the game
  ##
  ## Returns the modified parameters dialog and state.
  if state == knowledgeLists:
    state = previousState
    previousState = emptyState
  else:
    if previousState == emptyState:
      previousState = state
    setKnowledge(dialog = dialog)
    state = knowledgeLists
    mapPreview = false
  dialog = none

proc showStatsScreen(dialog: var GameDialog; state: var GameState) {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Show the game statistics screen
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ## * state   - the current state of the game
  ##
  ## Returns the modified parameters dialog and state.
  if state == gameStatistics:
    state = previousState
    previousState = emptyState
  else:
    if previousState == emptyState:
      previousState = state
    setStatistics(dialog = dialog)
    state = gameStatistics
    mapPreview = false
  dialog = none

proc showHelpScreen(dialog: var GameDialog; state: var GameState) {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Show the in-game help screen
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ## * state   - the current state of the game
  ##
  ## Returns the modified parameters dialog and state.
  if state == help:
    state = previousState
    previousState = emptyState
  else:
    if previousState == emptyState:
      previousState = state
    case state
    of crafting:
      setHelp(dialog = dialog, helpIndex = 6)
    of knowledgeLists:
      setHelp(dialog = dialog, helpIndex = 12)
    of trade:
      setHelp(dialog = dialog, helpIndex = 4)
    of recruits:
      setHelp(dialog = dialog, helpIndex = 11)
    of combat:
      setHelp(dialog = dialog, helpIndex = 5)
    else:
      setHelp(dialog = dialog)
    state = help
    mapPreview = false
  dialog = none

proc showOptionsScreen(dialog: var GameDialog; state: var GameState) {.raises: [
    ], tags: [RootEffect], contractual.} =
  ## Show the options screen
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ## * state   - the current state of the game
  ##
  ## Returns the modified parameters dialog and state.
  if state == options:
    state = previousState
    previousState = emptyState
  else:
    if previousState == emptyState:
      previousState = state
    setOptions()
    state = options
    mapPreview = false
  dialog = none

var key: string = ""

proc showHeader*(dialog: var GameDialog; close: CloseDestination = none;
    state: var GameState; options: bool = false): bool {.raises: [], tags: [
    RootEffect], contractual.} =
  ## Show the game's header
  ##
  ## * dialog  - the current in-game dialog displayed on the screen
  ## * close   - the close button's destination. If none, don't show the button
  ## * state   - the current state of the game
  ## * options - if true, show the button for more options. Default is false
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened or the game's menu is to show. Additionally, it returns true if
  ## the game's state changed, otherwise false.
  let
    oldState: GameState = state
    fuelAmount: Natural = try:
        getItemAmount(itemType = fuelType)
      except KeyError:
        dialog = setError(message = "Can't get fuel amount.")
        return true
    foodAmount: Natural = try:
        getItemsAmount(iType = "Food")
      except KeyError:
        dialog = setError(message = "Can't get food amount.")
        return true
    drinksAmount: Natural = try:
        getItemsAmount(iType = "Drinks")
      except KeyError:
        dialog = setError(message = "Can't get drinks amount.")
        return true
  var havePilot, haveEngineer, haveTrader, haveUpgrader, haveCleaner,
    haveRepairman: bool = false
  for member in playerShip.crew:
    case member.order
    of pilot:
      havePilot = true
    of engineer:
      haveEngineer = true
    of talk:
      haveTrader = true
    of upgrading:
      haveUpgrader = true
    of clean:
      haveCleaner = true
    of repair:
      haveRepairman = true
    else:
      discard
  if not haveTrader and (skyMap[playerShip.skyX][playerShip.skyY].baseIndex ==
      0 or (skyMap[playerShip.skyX][playerShip.skyY].eventIndex > -1 and
      eventsList[skyMap[playerShip.skyX][playerShip.skyY].eventIndex].eType !=
      friendlyShip)) or skyMap[playerShip.skyX][playerShip.skyY].eventIndex == 0:
    haveTrader = true
  var speed: float = 0.0
  let
    faction: FactionData = try:
        factionsList[playerShip.crew[0].faction]
      except KeyError:
        dialog = setError(message = "Can't get faction.")
        return true
  if (havePilot and haveEngineer) or "sentientships" in faction.flags:
    speed = try:
        (if playerShip.speed == docked: realSpeed(ship = playerShip,
            infoOnly = true).float / 1_000 else: realSpeed(
            ship = playerShip).float / 1_000.0)
      except ValueError:
        dialog = setError(message = "Can't count speed.")
        return true
  var
    haveGunner, haveWorker: bool = true
    needWorker, needCleaning, needRepairs: bool = false
  for module in playerShip.modules:
    try:
      case modulesList[module.protoIndex].mType
      of gun, harpoonGun:
        if module.owner[0] == -1:
          haveGunner = false
        elif playerShip.crew[module.owner[0]].order != gunner:
          haveGunner = false
      of alchemyLab .. greenhouse:
        if module.craftingIndex.len > 0:
          needWorker = true
          for owner in module.owner:
            if owner == -1:
              haveWorker = false
            elif playerShip.crew[owner].order != craft:
              haveWorker = false
            if not haveWorker:
              break
      of cabin:
        if module.cleanliness != module.quality:
          needCleaning = true
      else:
        discard
    except KeyError:
      dialog = setError(message = "Can't check modules.")
      return true
    if module.durability != module.maxDurability:
      needRepairs = true
  setRowTemplate(height = 35):
    rowTemplateStatic(width = 40)
    if close != none:
      rowTemplateStatic(width = 40)
    if options:
      rowTemplateStatic(width = 40)
    rowTemplateDynamic()
    rowTemplateStatic(width = 35)
    try:
      rowTemplateStatic(width = getTextWidth(text = $fuelAmount))
    except:
      dialog = setError(message = "Can't set fuel text width")
      return true
    rowTemplateStatic(width = 35)
    try:
      rowTemplateStatic(width = getTextWidth(text = $foodAmount))
    except:
      dialog = setError(message = "Can't set food text width")
      return true
    rowTemplateStatic(width = 35)
    try:
      rowTemplateStatic(width = getTextWidth(text = $drinksAmount))
    except:
      dialog = setError(message = "Can't set drinks text width")
      return true
    if speed < 0.5:
      rowTemplateStatic(width = 35)
    if not havePilot:
      rowTemplateStatic(width = 35)
    if not haveEngineer:
      rowTemplateStatic(width = 35)
    if not haveGunner:
      rowTemplateStatic(width = 35)
    if not haveTrader:
      rowTemplateStatic(width = 35)
    if needRepairs:
      rowTemplateStatic(width = 35)
    if needWorker:
      rowTemplateStatic(width = 35)
    if playerShip.upgradeModule > -1:
      rowTemplateStatic(width = 35)
    if needCleaning:
      rowTemplateStatic(width = 35)
  imageButton(image = images[menuIcon], tooltip = "The main game menu. Show info about the ship, its crew and allow to quit the game"):
    if dialog == none:
      setDialog(y = 20)
      dialog = gameMenuDialog
  if close != none:
    imageButton(image = images[exitIcon], tooltip = "Back to the " & $close & " screen"):
      closeScreen(close = close, state = state, dialog = dialog)
    if isKeyPressed(key = keyEscape) and dialog == none:
      closeScreen(close = close, state = state, dialog = dialog)
  if options:
    imageButton(image = images[moreOptionsIcon], tooltip = "Show more options"):
      showOptions = not showOptions
  if showInfo(dialog = dialog):
    return true
  showResourcesInfo(fuelAmount = fuelAmount, foodAmount = foodAmount,
      drinksAmount = drinksAmount)
  showNotifications(speed = speed, havePilot = havePilot,
      haveEngineer = haveEngineer, haveTrader = haveTrader,
      haveUpgrader = haveUpgrader, haveCleaner = haveCleaner,
      haveRepairman = haveRepairman, haveGunner = haveGunner,
      needRepairs = needRepairs, needWorker = needWorker,
      haveWorker = haveWorker, needCleaning = needCleaning, faction = faction)
  # Keyboard shortcuts
  const specialKeys: set[Keys] = {keyShift, keyCtrl, keyAlt}
  for sKey in specialKeys:
    if isKeyPressed(key = sKey):
      key = $sKey & "-"
      break
  var keyPressed: Keys = keyNone
  for key in keyScrollDown..keyF12:
    if isKeyPressed(key = key):
      keyPressed = key
      break
  if isKeyPressed(key = keyEscape):
    key = ""
    keyPressed = keyNone
    dialog = none
  if (getInputTextLen() > 0 or keyPressed != keyNone) and shortcutsEnabled:
    redraw = true
    if getInputTextLen() > 0:
      key &= getInputText().toLowerAscii
    elif keyPressed notin {keyEscape, keyTab, keyAlt}:
      key &= $keyPressed
    if key == menuAccelerators[1]:
      showShipInfo(dialog = dialog, state = state)
    elif key == menuAccelerators[4]:
      showLastMessagesScreen(dialog = dialog, state = state)
    elif key == menuAccelerators[5]:
      showKnowledgeScreen(dialog = dialog, state = state)
    elif key == menuAccelerators[7]:
      showStatsScreen(dialog = dialog, state = state)
    elif key == fullScreenAccel:
      gameSettings.fullScreen = not gameSettings.fullScreen
      nuklearSetWindowFullScreen(fullScreen = gameSettings.fullScreen)
    elif state in {shipInfo, knowledgeLists}:
      if key == generalAccelerators[0]:
        if expandedSection == 1:
          expandedSection = 0
        else:
          expandedSection = 1
      elif key == generalAccelerators[1]:
        if expandedSection == 2:
          expandedSection = 0
        else:
          expandedSection = 2
      elif key == generalAccelerators[2]:
        if expandedSection == 3:
          expandedSection = 0
        else:
          expandedSection = 3
      elif key == generalAccelerators[3]:
        if expandedSection == 4:
          expandedSection = 0
        else:
          expandedSection = 4
    elif playerShip.crew[0].health > 0:
      if key == menuAccelerators[8]:
        showHelpScreen(dialog = dialog, state = state)
      elif key == menuAccelerators[9]:
        showOptionsScreen(dialog = dialog, state = state)
      elif key == menuAccelerators[10]:
        dialog = setQuestion(question = "Are you sure want to quit?",
            qType = quitGame)
      elif key == menuAccelerators[11]:
        dialog = setQuestion(question = "Are you sure want to resign from game?",
            qType = resignGame)
      elif key == mapAccelerators[1]:
        if dialog == none:
          setDialog(y = 20)
          dialog = gameMenuDialog
      elif not inCombat:
        if key == menuAccelerators[2]:
          setDialog()
          dialog = ordersDialog
        elif key == menuAccelerators[3]:
          showCraftScreen(dialog = dialog, state = state)
        elif key == menuAccelerators[6]:
          setDialog()
          setWaitMenu()
          dialog = waitDialog
    if key != "Alt-":
      key = ""
  return showDialogs(dialog = dialog, state = state, oldState = oldState)

proc showGameMenu*(dialog: var GameDialog; state: var GameState) {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Show the main game's menu
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ## * state   - the current state of the game
  ##
  ## Returns the modified parameters dialog and state.
  const
    width: float = 200
    windowName: string = "Game Menu"
  var height: float = 455
  if inCombat:
    height = 390
  elif playerShip.crew[0].health == 0:
    height = 220
  updateDialog(width = width, height = height)
  window(name = windowName, x = dialogX, y = dialogY,
      w = width, h = height, flags = {windowBorder, windowTitle,
      windowNoScrollbar, windowMovable}):
    setLayoutRowDynamic(height = 30, cols = 1)
    labelButton(title = "Ship information"):
      showShipInfo(dialog = dialog, state = state)
    if playerShip.crew[0].health > 0 and not inCombat:
      labelButton(title = "Ship orders"):
        setDialog()
        dialog = ordersDialog
    if playerShip.crew[0].health > 0:
      labelButton(title = "Crafting"):
        showCraftScreen(dialog = dialog, state = state)
    labelButton(title = "Last messages"):
      showLastMessagesScreen(dialog = dialog, state = state)
    labelButton(title = "Knowledge lists"):
      showKnowledgeScreen(dialog = dialog, state = state)
    if playerShip.crew[0].health > 0 and not inCombat:
      labelButton(title = "Wait orders"):
        setDialog()
        setWaitMenu()
        dialog = waitDialog
    labelButton(title = "Game statistics"):
      showStatsScreen(dialog = dialog, state = state)
    if playerShip.crew[0].health > 0:
      labelButton(title = "Help"):
        showHelpScreen(dialog = dialog, state = state)
      labelButton(title = "Game options"):
        showOptionsScreen(dialog = dialog, state = state)
      labelButton(title = "Quit from game"):
        dialog = setQuestion(question = "Are you sure want to quit?",
            qType = quitGame)
      labelButton(title = "Resign from game"):
        dialog = setQuestion(question = "Are you sure want to resign from game?",
            qType = resignGame)
    labelButton(title = "Close"):
      dialog = none

  windowSetFocus(name = windowName)
