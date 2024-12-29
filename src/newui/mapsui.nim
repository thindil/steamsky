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
# along with Steam Sky.  if, see <http://www.gnu.org/licenses/>.

## Provides code related to the game's main map, like, creating the game's UI,
## etc.

import std/[colors, math, tables]
import contracts, nimalyzer, nuklear/nuklear_sdl_renderer
import ../[config, game, maps, messages, shipscargo, shipsmovement, types]
import coreui, errordialog, themes

const iconsAmount: Positive = 25

{.push ruleOff: "varDeclared".}
var mapImages: array[iconsAmount, PImage]
{.pop ruleOn: "varDeclared".}

proc createGameUi*(dialog: var GameDialog) {.raises: [], tags: [RootEffect],
    contractual.} =
  ## Create the game's UI and show the map to the player
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns parameter dialog, modified if any error happened.
  if mapImages[0] == nil:
    # Load images
    try:
      for index, fileName in themesList[gameSettings.interfaceTheme].icons[
          4..iconsAmount + 3]:
        mapImages[index] = nuklearLoadSVGImage(filePath = fileName,
            width = 0, height = 20 + gameSettings.interfaceFontSize)
    except:
      dialog = setError(message = "Can't set the game's images.")

proc showResourcesInfo(fuelAmount, foodAmount, drinksAmount: Natural;
    dialog: var GameDialog) {.raises: [], tags: [RootEffect], contractual.} =
  ## Show information about food, drinks and fuel in the player's ship's cargo
  ##
  ## * fuelAmount   - the amount of fuel on the player's ship
  ## * foodAmount   - the amount of food on the player's ship
  ## * drinksAmount - the amount of drinks on the player's ship
  ## * dialog       - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened or the game's menu is to show.
  ##
  let theme: ThemeData = try:
      themesList[gameSettings.interfaceTheme]
    except:
      dialog = setError(message = "Can't get the game's theme.")
      return
  var
    r, g, b: Natural = 0
    image: PImage = nil
    tooltipText: string = ""
  if fuelAmount > gameSettings.lowFuel:
    (r, g, b) = theme.colors[2].extractRGB
    image = mapImages[1]
    tooltipText = "The amount of fuel in the ship's cargo."
  elif fuelAmount > 0:
    (r, g, b) = theme.colors[27].extractRGB
    image = mapImages[3]
    tooltipText = "Low level of fuel on ship. Only " & $fuelAmount & " left."
  else:
    (r, g, b) = theme.colors[28].extractRGB
    image = mapImages[2]
    tooltipText = "You can't travel anymore, because you don't have any fuel for ship."
  if gameSettings.showTooltips:
    addTooltip(bounds = getWidgetBounds(), text = tooltipText)
  image(image = image, padding = NimVec2(x: 5, y: 5))
  if gameSettings.showTooltips:
    addTooltip(bounds = getWidgetBounds(), text = tooltipText)
  colorLabel(str = $fuelAmount, r = r, g = g, b = b)
  if foodAmount > gameSettings.lowFood:
    (r, g, b) = theme.colors[2].extractRGB
    image = mapImages[4]
    tooltipText = "The amount of food in the ship's cargo."
  elif foodAmount > 0:
    (r, g, b) = theme.colors[27].extractRGB
    image = mapImages[6]
    tooltipText = "Low level of food on ship. Only " & $foodAmount & " left."
  else:
    (r, g, b) = theme.colors[28].extractRGB
    image = mapImages[5]
    tooltipText = "You don't have any food in ship but your crew needs them to live."
  if gameSettings.showTooltips:
    addTooltip(bounds = getWidgetBounds(), text = tooltipText)
  image(image = image, padding = NimVec2(x: 5, y: 5))
  if gameSettings.showTooltips:
    addTooltip(bounds = getWidgetBounds(), text = tooltipText)
  colorLabel(str = $foodAmount, r = r, g = g, b = b)
  if drinksAmount > gameSettings.lowFood:
    (r, g, b) = theme.colors[2].extractRGB
    image = mapImages[7]
    tooltipText = "The amount of drinks in the ship's cargo."
  elif drinksAmount > 0:
    (r, g, b) = theme.colors[27].extractRGB
    image = mapImages[8]
    tooltipText = "Low level of drinks on ship. Only " & $drinksAmount & " left."
  else:
    (r, g, b) = theme.colors[28].extractRGB
    image = mapImages[9]
    tooltipText = "You don't have any drinks in ship but your crew needs them to live."
  if gameSettings.showTooltips:
    addTooltip(bounds = getWidgetBounds(), text = tooltipText)
  image(image = image, padding = NimVec2(x: 5, y: 5))
  if gameSettings.showTooltips:
    addTooltip(bounds = getWidgetBounds(), text = tooltipText)
  colorLabel(str = $drinksAmount, r = r, g = g, b = b)

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
  if speed < 0.5:
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "You can't fly with your ship, because it is overloaded.")
    image(image = mapImages[14], padding = NimVec2(x: 5, y: 5))
  if not havePilot:
    if "sentientships" in faction.flags:
      image = mapImages[11]
      tooltipText = "No pilot assigned. Ship fly on it own."
    else:
      image = mapImages[10]
      tooltipText = "No pilot assigned. Ship can't move."
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(), text = tooltipText)
    image(image = image, padding = NimVec2(x: 5, y: 5))
  if not haveEngineer:
    if "sentientships" in faction.flags:
      image = mapImages[13]
      tooltipText = "No engineer assigned. Ship fly on it own."
    else:
      image = mapImages[12]
      tooltipText = "No engineer assigned. Ship can't move."
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(), text = tooltipText)
    image(image = image, padding = NimVec2(x: 5, y: 5))
  if not haveGunner:
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "One or more guns don't have a gunner.")
    image(image = mapImages[15], padding = NimVec2(x: 5, y: 5))
  if not haveTrader:
    if skyMap[playerShip.skyX][playerShip.skyY].baseIndex > 0:
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "No trader assigned. You need one to talk/trade.")
      image(image = mapImages[22], padding = NimVec2(x: 5, y: 5))
    elif skyMap[playerShip.skyX][playerShip.skyY].eventIndex > -1 and
        eventsList[skyMap[playerShip.skyX][playerShip.skyY].eventIndex].eType == friendlyShip:
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "No trader assigned. You need one to talk/trade.")
      image(image = mapImages[22], padding = NimVec2(x: 5, y: 5))
  if needRepairs:
    if haveRepairman:
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "The ship is being repaired.")
      image(image = mapImages[16], padding = NimVec2(x: 5, y: 5))
    else:
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "The ship needs repairs but no one is working them.")
      image(image = mapImages[17], padding = NimVec2(x: 5, y: 5))
  if needWorker:
    if haveWorker:
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "All crafting orders are being executed.")
      image(image = mapImages[18], padding = NimVec2(x: 5, y: 5))
    else:
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "You need to assign crew members to begin manufacturing.")
      image(image = mapImages[19], padding = NimVec2(x: 5, y: 5))
  if playerShip.upgradeModule > -1:
    if haveUpgrader:
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "A ship module upgrade in progress.")
      image(image = mapImages[20], padding = NimVec2(x: 5, y: 5))
    else:
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "A ship module upgrade is in progress but no one is working on it.")
      image(image = mapImages[21], padding = NimVec2(x: 5, y: 5))
  if needCleaning:
    if haveCleaner:
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Ship is cleaned.")
      image(image = mapImages[23], padding = NimVec2(x: 5, y: 5))
    else:
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Ship is dirty but no one is cleaning it.")
      image(image = mapImages[24], padding = NimVec2(x: 5, y: 5))

var showQuestion: bool = true

proc showHeader(dialog: var GameDialog) {.raises: [], tags: [RootEffect],
    contractual.} =
  ## Show the game's header
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened or the game's menu is to show.
  let
    fuelAmount: Natural = try:
        getItemAmount(itemType = fuelType)
      except KeyError:
        dialog = setError(message = "Can't get fuel amount.")
        return
    foodAmount: Natural = try:
        getItemsAmount(iType = "Food")
      except KeyError:
        dialog = setError(message = "Can't get food amount.")
        return
    drinksAmount: Natural = try:
        getItemsAmount(iType = "Drinks")
      except KeyError:
        dialog = setError(message = "Can't get drinks amount.")
        return
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
  var speed: float = 0.0
  let
    faction: FactionData = try:
        factionsList[playerShip.crew[0].faction]
      except KeyError:
        dialog = setError(message = "Can't get faction.")
        return
  if (havePilot and haveEngineer) or "sentientships" in faction.flags:
    speed = try:
        (if playerShip.speed == docked: realSpeed(ship = playerShip,
            infoOnly = true).float / 1_000 else: realSpeed(
            ship = playerShip).float / 1_000.0)
      except ValueError:
        dialog = setError(message = "Can't count speed.")
        return
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
      return
    if module.durability != module.maxDurability:
      needRepairs = true
  setRowTemplate(height = 35):
    rowTemplateStatic(width = 40)
    rowTemplateDynamic()
    rowTemplateStatic(width = 30)
    try:
      rowTemplateStatic(width = getTextWidth(text = $fuelAmount))
    except:
      dialog = setError(message = "Can't set fuel text width")
      return
    rowTemplateStatic(width = 30)
    try:
      rowTemplateStatic(width = getTextWidth(text = $foodAmount))
    except:
      dialog = setError(message = "Can't set food text width")
      return
    rowTemplateStatic(width = 30)
    try:
      rowTemplateStatic(width = getTextWidth(text = $drinksAmount))
    except:
      dialog = setError(message = "Can't set drinks text width")
      return
    if speed < 0.5:
      rowTemplateStatic(width = 30)
    if not havePilot:
      rowTemplateStatic(width = 30)
    if not haveEngineer:
      rowTemplateStatic(width = 30)
    if not haveGunner:
      rowTemplateStatic(width = 30)
    if not haveTrader:
      rowTemplateStatic(width = 30)
    if needRepairs:
      rowTemplateStatic(width = 30)
    if needWorker:
      rowTemplateStatic(width = 30)
    if playerShip.upgradeModule > -1:
      rowTemplateStatic(width = 30)
    if needCleaning:
      rowTemplateStatic(width = 30)
  if gameSettings.showTooltips:
    addTooltip(bounds = getWidgetBounds(),
        text = "The main game menu. Show info about the ship, its crew and allow to quit the game")
  imageButton(image = mapImages[0]):
    discard
  if gameSettings.showNumbers:
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Game time and current ship speed.")
    try:
      label(str = formattedTime() & " Speed: " & $((realSpeed(
          ship = playerShip) * 60) / 1_000) & " km/h", alignment = centered)
    except:
      dialog = setError(message = "Can't get the ship's speed")
      return
  else:
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(), text = "Game time.")
    label(str = formattedTime(), alignment = centered)
  showResourcesInfo(fuelAmount = fuelAmount, foodAmount = foodAmount,
      drinksAmount = drinksAmount, dialog = dialog)
  if dialog != none:
    return
  showNotifications(speed = speed, havePilot = havePilot,
      haveEngineer = haveEngineer, haveTrader = haveTrader,
      haveUpgrader = haveUpgrader, haveCleaner = haveCleaner,
      haveRepairman = haveRepairman, haveGunner = haveGunner,
      needRepairs = needRepairs, needWorker = needWorker,
      haveWorker = haveWorker, needCleaning = needCleaning, faction = faction)
  if playerShip.crew[0].health == 0 and showQuestion:
    try:
      popup(pType = staticPopup, flags = {windowNoScrollbar}, title = "Question",
          x = windowWidth / 3, y = windowHeight / 3, w = 300, h = 115):
        setLayoutRowDynamic(height = 60, cols = 1)
        wrapLabel(str = "You are dead. Would you like to see your game statistics?")
        setLayoutRowDynamic(height = 30, cols = 2)
        labelButton(title = "Yes"):
          showQuestion = false
          closePopup()
        labelButton(title = "No"):
          showQuestion = false
          closePopup()
    except:
      dialog = setError(message = "Can't create popup. Reason: " &
          getCurrentExceptionMsg())

proc showMap*(state: var GameState; dialog: var GameDialog) {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Show the game's map
  ##
  ## * state - the current game's state
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameters state and dialog. The latter is modified if
  ## any error happened.
  showHeader(dialog = dialog)
  # draw map
  let
    rows: Natural = ((windowHeight.Natural - 35 - gameSettings.messagesPosition) / gameSettings.mapFontSize).floor.Natural
    colWidth: Positive = try:
        getTextWidth(text = " ").Positive
      except:
        dialog = setError(message = "Can't count map column's width.")
        return
    cols: Natural = (windowWidth.Natural / colWidth).floor.Positive
  echo rows, " ", cols
  state = map
