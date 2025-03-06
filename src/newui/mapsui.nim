# Copyright 2024-2025 Bartek thindil Jasicki
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

import std/[colors, math, tables, unicode]
import contracts, nimalyzer, nuklear/nuklear_sdl_renderer
import ../[basestypes, config, game, maps, messages, missions, shipscargo,
    shipsmovement, stories, types]
import coreui, dialogs, errordialog, messagesui, ordersmenu, themes, utilsui2

const iconsAmount: Positive = 36

{.push ruleOff: "varDeclared".}
var mapImages: array[iconsAmount, PImage]
{.pop ruleOn: "varDeclared".}
var
  centerX: MapXRange = 1
  centerY: MapYRange = 1

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
  centerX = playerShip.skyX
  centerY = playerShip.skyY

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
    color: Color = colBlack
    image: PImage = nil
    tooltipText: string = ""
  if fuelAmount > gameSettings.lowFuel:
    color = theme.colors[greenColor]
    image = mapImages[1]
    tooltipText = "The amount of fuel in the ship's cargo."
  elif fuelAmount > 0:
    color = theme.colors[goldenColor]
    image = mapImages[3]
    tooltipText = "Low level of fuel on ship. Only " & $fuelAmount & " left."
  else:
    color = theme.colors[redColor]
    image = mapImages[2]
    tooltipText = "You can't travel anymore, because you don't have any fuel for ship."
  if gameSettings.showTooltips:
    addTooltip(bounds = getWidgetBounds(), text = tooltipText)
  image(image = image, padding = NimVec2(x: 5, y: 5))
  if gameSettings.showTooltips:
    addTooltip(bounds = getWidgetBounds(), text = tooltipText)
  colorLabel(str = $fuelAmount, color = color)
  if foodAmount > gameSettings.lowFood:
    color = theme.colors[greenColor]
    image = mapImages[4]
    tooltipText = "The amount of food in the ship's cargo."
  elif foodAmount > 0:
    color = theme.colors[goldenColor]
    image = mapImages[6]
    tooltipText = "Low level of food on ship. Only " & $foodAmount & " left."
  else:
    color = theme.colors[redColor]
    image = mapImages[5]
    tooltipText = "You don't have any food in ship but your crew needs them to live."
  if gameSettings.showTooltips:
    addTooltip(bounds = getWidgetBounds(), text = tooltipText)
  image(image = image, padding = NimVec2(x: 5, y: 5))
  if gameSettings.showTooltips:
    addTooltip(bounds = getWidgetBounds(), text = tooltipText)
  colorLabel(str = $foodAmount, color = color)
  if drinksAmount > gameSettings.lowFood:
    color = theme.colors[greenColor]
    image = mapImages[7]
    tooltipText = "The amount of drinks in the ship's cargo."
  elif drinksAmount > 0:
    color = theme.colors[goldenColor]
    image = mapImages[8]
    tooltipText = "Low level of drinks on ship. Only " & $drinksAmount & " left."
  else:
    color = theme.colors[redColor]
    image = mapImages[9]
    tooltipText = "You don't have any drinks in ship but your crew needs them to live."
  if gameSettings.showTooltips:
    addTooltip(bounds = getWidgetBounds(), text = tooltipText)
  image(image = image, padding = NimVec2(x: 5, y: 5))
  if gameSettings.showTooltips:
    addTooltip(bounds = getWidgetBounds(), text = tooltipText)
  colorLabel(str = $drinksAmount, color = color)

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
    if dialog == none:
      setDialog(y = 20)
      dialog = gameMenuDialog
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
  showNotifications(speed = speed, havePilot = havePilot,
      haveEngineer = haveEngineer, haveTrader = haveTrader,
      haveUpgrader = haveUpgrader, haveCleaner = haveCleaner,
      haveRepairman = haveRepairman, haveGunner = haveGunner,
      needRepairs = needRepairs, needWorker = needWorker,
      haveWorker = haveWorker, needCleaning = needCleaning, faction = faction)
  if playerShip.crew[0].health == 0 and dialog == none:
    dialog = setQuestion(question = "You are dead. Would you like to see your game statistics?",
        qType = showDeadStats)

proc showMapInfo(x: MapXRange; y: MapYRange; theme: ThemeData) {.raises: [
    ValueError], tags: [WriteIOEffect, TimeEffect, RootEffect], contractual.} =
  ## Show the map cell info popup
  ##
  ## * x     - the X coordinate of the map cell which info will be show
  ## * y     - the Y coordinate of the map cell which info will be show
  ## * theme - the current game's theme
  nuklearSetDefaultFont(defaultFont = fonts[UIFont],
      fontSize = gameSettings.interfaceFontSize + 10)
  tooltip(x = (windowWidth - 240), y = 45, width = 230):
    layoutStatic(height = 25, cols = 4):
      row(width = 20):
        label(str = "X:")
      row(width = 80):
        colorLabel(str = $x, color = theme.mapColors[mapGoldenYellow])
      row(width = 20):
        label(str = "Y:")
      row(width = 80):
        colorLabel(str = $y, color = theme.mapColors[mapGoldenYellow])
    if playerShip.skyX != x or playerShip.skyY != y:
      let
        distance: Natural = countDistance(destinationX = x, destinationY = y)
        travelValues: TravelArray = travelInfo(distance = distance)
      layoutStatic(height = 25, cols = 2):
        row(width = 80):
          label(str = "Distance:")
        row(width = 100):
          colorLabel(str = $distance, color = theme.mapColors[mapGoldenYellow])
      if travelValues[1] > 0:
        layoutStatic(height = 25, cols = 2):
          row(width = 50):
            label(str = "ETA:")
          row(width = 180):
            var distanceText: string = ""
            minutesToDate(minutes = travelValues[1], infoText = distanceText)
            colorLabel(str = distanceText, color = theme.mapColors[mapGoldenYellow])
          row(width = 160):
            label(str = "Approx fuel usage:")
          row(width = 70):
            colorLabel(str = $travelValues[2], color = theme.mapColors[mapGoldenYellow])
    if skyMap[x][y].baseIndex > 0:
      let baseIndex: Positive = skyMap[x][y].baseIndex
      if skyBases[baseIndex].known:
        setLayoutRowDynamic(height = 25, cols = 1)
        colorLabel(str = "Base info:", color = theme.mapColors[mapPinkColor])
        layoutStatic(height = 25, cols = 2):
          row(width = 60):
            label(str = "Name:")
          row(width = 170):
            colorLabel(str = skyBases[baseIndex].name, color = theme.mapColors[mapGoldenYellow])
          if skyBases[baseIndex].visited.year > 0:
            row(width = 60):
              label(str = "Type:")
            row(width = 170):
              colorLabel(str = basesTypesList[skyBases[
                  baseIndex].baseType].name, color = basesTypesList[skyBases[
                  baseIndex].baseType].color)
            if skyBases[baseIndex].population > 0:
              row(width = 100):
                label(str = "Population:")
              row(width = 130):
                case skyBases[baseIndex].population
                of 1..149:
                  colorLabel(str = "small", color = theme.mapColors[mapGoldenYellow])
                of 150..299:
                  colorLabel(str = "medium", color = theme.mapColors[mapGoldenYellow])
                else:
                  colorLabel(str = "large", color = theme.mapColors[mapGoldenYellow])
            row(width = 60):
              label(str = "Size:")
            row(width = 170):
              colorLabel(str = $skyBases[baseIndex].size,
                  color = theme.mapColors[mapGoldenYellow])
            if skyBases[baseIndex].population > 0:
              row(width = 70):
                label(str = "Owner:")
              row(width = 160):
                colorLabel(str = factionsList[skyBases[baseIndex].owner].name,
                    color = theme.mapColors[mapGoldenYellow])
            else:
              row(width = 230):
                label(str = "Base is abandoned")
        setLayoutRowDynamic(height = 25, cols = 1)
        if skyBases[baseIndex].population > 0:
          case skyBases[baseIndex].reputation.level
          of -100 .. -75:
            colorLabel(str = "You are hated here", color = theme.mapColors[mapRedColor])
          of -74 .. -50:
            colorLabel(str = "You are outlawed here", color = theme.mapColors[mapRedColor])
          of -49 .. -25:
            colorLabel(str = "You are disliked here", color = theme.mapColors[mapRedColor])
          of -24 .. -1:
            colorLabel(str = "They are unfriendly to you",
                color = theme.mapColors[mapRedColor])
          of 0:
            colorLabel(str = "You are unknown here", color = theme.mapColors[mapGoldenYellow])
          of 1..25:
            colorLabel(str = "You are know here as visitor",
                color = theme.mapColors[mapGreenColor])
          of 26..50:
            colorLabel(str = "You are know here as trader",
                color = theme.mapColors[mapGreenColor])
          of 51..75:
            colorLabel(str = "You are know here as friend",
                color = theme.mapColors[mapGreenColor])
          of 76..100:
            colorLabel(str = "You are well known here", color = theme.mapColors[mapGreenColor])
        if baseIndex == playerShip.homeBase:
          colorLabel(str = "It is your home base", color = theme.mapColors[mapCyanColor])
  nuklearSetDefaultFont(defaultFont = fonts[FontsNames.mapFont],
      fontSize = gameSettings.mapFontSize + 10)

var
  moveX: MapXRange = 1
  moveY: MapYRange = 1
  rows, cols: Positive = 1

proc showMapMenu(bounds: NimRect) {.raises: [], tags: [RootEffect],
    contractual.} =
  ## Show the map's menu
  ##
  ## * bounds - the rectangle in which the player should click the mouse's
  ##            button to show the menu

  proc closeMapMenu() {.raises: [], tags: [], contractual.} =
    ## Close the menu, reset the position form
    moveX = 1
    moveY = 1

  nuklearSetDefaultFont(defaultFont = fonts[UIFont],
      fontSize = gameSettings.interfaceFontSize + 10)
  contextualMenu(flags = {windowNoFlags}, x = 535, y = 190,
      triggerBounds = bounds, button = left):
    setLayoutRowStatic(height = 35, cols = 6, ratio = [35.cfloat, 35, 35, 35,
        135, 230])
    imageButton(image = mapImages[25]):
      centerY = (if centerY - (rows / 3).int < 1: (rows /
          3).int else: centerY - (rows / 3).int)
      centerX = (if centerX - (cols / 3).int < 1: (cols /
          3).int else: centerX - (cols / 3).int)
    imageButton(image = mapImages[26]):
      centerY = (if centerY - (rows / 3).int < 1: (rows /
          3).int else: centerY - (rows / 3).int)
    imageButton(image = mapImages[27]):
      centerY = (if centerY - (rows / 3).int < 1: (rows /
          3).int else: centerY - (rows / 3).int)
      centerX = (if centerX + (cols / 3).int > 1_024: (cols /
          3).int else: centerX + (cols / 3).int)
    label(str = "X:")
    property(name = "#", min = MapXRange.low, val = moveX,
        max = MapXRange.high, step = 1, incPerPixel = 1)
    labelButton(title = "Center map on ship"):
      centerX = playerShip.skyX
      centerY = playerShip.skyY
      closeMapMenu()
    imageButton(image = mapImages[28]):
      centerX = (if centerX - (cols / 3).int < 1: (cols /
          3).int else: centerX - (cols / 3).int)
    label(str = "")
    imageButton(image = mapImages[29]):
      centerX = (if centerX + (cols / 3).int > 1_024: (cols /
          3).int else: centerX + (cols / 3).int)
    label(str = "Y:")
    property(name = "#", min = MapYRange.low, val = moveY,
        max = MapYRange.high, step = 1, incPerPixel = 1)
    labelButton(title = "Center map on home base"):
      centerX = skyBases[playerShip.homeBase].skyX
      centerY = skyBases[playerShip.homeBase].skyY
      closeMapMenu()
    setLayoutRowStatic(height = 35, cols = 5, ratio = [35.cfloat, 35, 35, 175, 230])
    imageButton(image = mapImages[30]):
      centerY = (if centerY + (rows / 3).int > 1_024: (rows /
          3).int else: centerY + (rows / 3).int)
      centerX = (if centerX - (cols / 3).int < 1: (cols /
          3).int else: centerX - (cols / 3).int)
    imageButton(image = mapImages[31]):
      centerY = (if centerY + (rows / 3).int > 1_024: (rows /
          3).int else: centerY + (rows / 3).int)
    imageButton(image = mapImages[32]):
      centerY = (if centerY + (rows / 3).int > 1_024: (rows /
          3).int else: centerY + (rows / 3).int)
      centerX = (if centerX + (cols / 3).int > 1_024: (cols /
          3).int else: centerX + (cols / 3).int)
    labelButton(title = "Move map"):
      centerX = moveX
      centerY = moveY
      closeMapMenu()
    contextualItemLabel(label = "Close", align = centered):
      closeMapMenu()
  nuklearSetDefaultFont(defaultFont = fonts[FontsNames.mapFont],
      fontSize = gameSettings.mapFontSize + 10)

const shipSpeeds: array[4, string] = ["Full stop", "Quarter speed",
    "Half speed", "Full speed"]

proc showButtons(dialog: var GameDialog) {.raises: [], tags: [], contractual.} =
  ## Show the buttons for manage the ship, like orders, movement or wait
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameters dialog.
  group(title = "ButtonsGroup", flags = {windowNoScrollbar}):
    if playerShip.speed == docked or playerShip.destinationX == 0:
      setLayoutRowDynamic(height = 30, cols = 1)
    else:
      setLayoutRowDynamic(height = 30, cols = 2, ratio = [0.75.cfloat, 0.25])
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Show available orders for your ship.")
    labelButton(title = "Ship orders"):
      setDialog()
      dialog = ordersDialog
    if playerShip.speed != docked and playerShip.destinationX > 0:
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Auto move your ship to its destination.")
      imageButton(image = mapImages[34]):
        discard
    setLayoutRowDynamic(height = 30, cols = 1)
    if playerShip.speed == docked:
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(), text = "Wait 1 minute.")
      imageButtonCentered(image = mapImages[33]):
        discard
    else:
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Set speed for your ship. The faster you move, the more fuel used. But faster movement has bigger chance to evade enemies.")
      playerShip.speed = (comboList(items = shipSpeeds,
          selected = playerShip.speed.ord - 1, itemHeight = 25, x = 200,
          y = 150) + 1).ShipSpeed
      setLayoutRowStatic(height = 30, cols = 3, ratio = [40.cfloat, 40, 40])
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Move ship up and left")
      imageButton(image = mapImages[25]):
        discard
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Move ship up")
      imageButton(image = mapImages[26]):
        discard
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Move ship up and right")
      imageButton(image = mapImages[27]):
        discard
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Move ship left")
      imageButton(image = mapImages[28]):
        discard
      if playerShip.destinationX == 0:
        if gameSettings.showTooltips:
          addTooltip(bounds = getWidgetBounds(),
              text = "Wait 1 minute")
        imageButton(image = mapImages[33]):
          discard
      else:
        if gameSettings.showTooltips:
          addTooltip(bounds = getWidgetBounds(),
              text = "Move ship one map field toward destination")
        imageButton(image = mapImages[35]):
          discard
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Move ship right")
      imageButton(image = mapImages[29]):
        discard
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Move ship down and left")
      imageButton(image = mapImages[30]):
        discard
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Move ship down")
      imageButton(image = mapImages[31]):
        discard
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Move ship down and right")
      imageButton(image = mapImages[32]):
        discard

proc showGameMenu(dialog: var GameDialog) {.raises: [], tags: [RootEffect],
    contractual.} =
  ## Show the main game's menu
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameters dialog.
  if dialog != gameMenuDialog:
    return
  try:
    const
      width: float = 200
      height: float = 455
    updateDialog(width = width, height = height)
    popup(pType = staticPopup, title = "Game Menu", x = dialogX, y = dialogY,
        w = width, h = height, flags = {windowBorder, windowTitle,
        windowNoScrollbar}):
      setLayoutRowDynamic(30, 1)
      labelButton(title = "Ship information"):
        closePopup()
      if playerShip.crew[0].health > 0:
        labelButton(title = "Ship orders"):
          closePopup()
          setDialog()
          dialog = ordersDialog
        labelButton(title = "Crafting"):
          closePopup()
      labelButton(title = "Last messages"):
        closePopup()
      labelButton(title = "Knowledge lists"):
        closePopup()
      if playerShip.crew[0].health > 0:
        labelButton(title = "Wait orders"):
          closePopup()
          setDialog()
          dialog = waitDialog
      labelButton(title = "Game statistics"):
        closePopup()
      if playerShip.crew[0].health > 0:
        labelButton(title = "Help"):
          closePopup()
        labelButton(title = "Game options"):
          closePopup()
        labelButton(title = "Quit from game"):
          closePopup()
          dialog = setQuestion(question = "Are you sure want to quit?",
              qType = quitGame)
        labelButton(title = "Resign from game"):
          dialog = setQuestion(question = "Are you sure want to resign from game?",
              qType = resignGame)
          closePopup()
      labelButton(title = "Close"):
        closePopup()
        dialog = none
  except:
    dialog = setError(message = "Can't show the game's menu")

var mapX, mapY: Natural = 0

proc showDestinationMenu(dialog: var GameDialog) {.raises: [], tags: [
    RootEffect], contractual.} =
  ## Show the menu for setting a destination for the player's ship
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog.
  if dialog != destinationDialog:
    return
  try:
    const width: float = 250
    let height: float = (if playerShip.speed == docked: 115 else: 185)

    proc closeDialog(dialog: var GameDialog) {.raises: [], tags: [],
        contractual.} =
      ## Close the destination menu dialog
      ## * dialog - the current in-game dialog displayed on the screen
      ##
      ## Returns the reseted parameter dialog.
      closePopup()
      dialog = none
      mapX = 0
      mapY = 0

    proc setDestination(dialog: var GameDialog) {.raises: [], tags: [],
        contractual.} =
      ## Set the new destination point for the player's ship
      ##
      ## * dialog - the current in-game dialog displayed on the screen
      ##
      ## Returns the reseted parameter dialog.
      playerShip.destinationX = mapX
      playerShip.destinationY = mapY
      closeDialog(dialog = dialog)

    updateDialog(width = width, height = height)
    popup(pType = staticPopup, title = "Set destination", x = dialogX,
        y = dialogY, w = width, h = height, flags = {windowBorder, windowTitle,
        windowNoScrollbar}):
      setLayoutRowDynamic(30, 1)
      labelButton(title = "Set destination"):
        setDestination(dialog = dialog)
      if playerShip.speed != docked:
        labelButton(title = "Set destination and move"):
          setDestination(dialog = dialog)
        labelButton(title = "Move to"):
          discard
      labelButton(title = "Close"):
        closeDialog(dialog = dialog)
  except:
    dialog = setError(message = "Can't show the destination's menu")

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
  # draw dialogs
  showQuestion(dialog = dialog, state = state)
  showShipOrders(dialog = dialog)
  showDestinationMenu(dialog = dialog)
  showMessage(dialog = dialog)
  # draw map
  nuklearSetDefaultFont(defaultFont = fonts[FontsNames.mapFont],
      fontSize = gameSettings.mapFontSize + 10)
  let
    theme: ThemeData = try:
        themesList[gameSettings.interfaceTheme]
      except:
        dialog = setError(message = "Can't get the game's theme.")
        return
    height: Positive = gameSettings.mapFontSize + 10
  rows = ((windowHeight - 35 - gameSettings.messagesPosition.float) /
        height.float).floor.Natural + 4
  let colWidth: Positive = try:
      getTextWidth(text = theme.mapIcons[1]).Positive + 4
    except:
      dialog = setError(message = "Can't count map column's width.")
      return
  cols = (windowWidth / colWidth.float).floor.Positive + 6
  let mapHeight: float = (((height - 2) * rows) + 5).float
  setLayoutRowDynamic(height = mapHeight, cols = 1)
  group(title = "MapGroup", flags = {windowNoScrollbar}):
    var
      startX: int = centerX - (cols / 2).int
      startY: int = centerY - (rows / 2).int
      endY: int = centerY + (rows / 2).floor.int
      endX: int = centerX + (cols / 2).floor.int
      storyX: int = 1
      storyY: int = 1
    if startY < 1:
      startY = 1
      endY = rows + 1
    if startX < 1:
      startX = 1
      endX = cols + 1
    if endY > 1_024:
      endY = 1_024
      startY = 1_024 - rows
    if endX > 1_024:
      endX = 1_024
      startX = 1_025 - cols
    saveButtonStyle()
    setButtonStyle(field = rounding, value = 0)
    setButtonStyle(field = border, value = 0)
    if currentStory.index.len > 0:
      (storyX, storyY) = try:
          getStoryLocation()
        except:
          dialog = setError(message = "Can't get the current story location.")
          return
      if storyX == playerShip.skyX and storyY == playerShip.skyY:
        storyX = 0
        storyY = 0
    layoutSpaceStatic(height = ((height - 2) * rows).float,
        widgetsCount = rows * cols):
      var curRow, col: int = -1
      for y in startY..endY:
        curRow.inc
        col = -1
        for x in startX..endX:
          col.inc
          row(x = (col * (colWidth - 2)).float, y = (curRow * (height -
              2)).float, w = colWidth.float, h = height.float):
            var
              mapChar: string = theme.mapIcons[1]
              mapColor: Color = theme.mapColors[mapUnvisitedColor]
            if x == playerShip.skyX and y == playerShip.skyY:
              skyMap[x][y].visited = true
              mapChar = theme.mapIcons[0]
              mapColor = theme.mapColors[mapDefaultColor]
            else:
              if x == playerShip.destinationX and y == playerShip.destinationY:
                mapChar = theme.mapIcons[2]
                mapColor = theme.mapColors[mapDefaultColor]
              elif currentStory.index.len > 0 and (x == storyX and y == storyY):
                mapChar = theme.mapIcons[3]
                mapColor = theme.mapColors[mapGreenColor]
              elif skyMap[x][y].missionIndex > -1:
                case acceptedMissions[skyMap[x][y].missionIndex].mType
                of deliver:
                  mapChar = theme.mapIcons[4]
                  mapColor = theme.mapColors[mapYellowColor]
                of destroy:
                  mapChar = theme.mapIcons[5]
                  mapColor = theme.mapColors[mapRedColor]
                of patrol:
                  mapChar = theme.mapIcons[6]
                  mapColor = theme.mapColors[mapLimeColor]
                of explore:
                  mapChar = theme.mapIcons[7]
                  mapColor = theme.mapColors[mapGreenColor]
                of passenger:
                  mapChar = theme.mapIcons[8]
                  mapColor = theme.mapColors[mapCyanColor]
              elif skyMap[x][y].eventIndex > -1:
                if skyMap[x][y].eventIndex > eventsList.high:
                  skyMap[x][y].eventIndex = -1
                else:
                  case eventsList[skyMap[x][y].eventIndex].eType
                  of enemyShip:
                    mapChar = theme.mapIcons[9]
                    mapColor = theme.mapColors[mapRedColor]
                  of attackOnBase:
                    mapChar = theme.mapIcons[10]
                    mapColor = theme.mapColors[mapRed2Color]
                  of enemyPatrol:
                    mapChar = theme.mapIcons[11]
                    mapColor = theme.mapColors[mapRed3Color]
                  of disease:
                    mapChar = theme.mapIcons[12]
                    mapColor = theme.mapColors[mapYellowColor]
                  of fullDocks:
                    mapChar = theme.mapIcons[13]
                    mapColor = theme.mapColors[mapCyanColor]
                  of doublePrice:
                    mapChar = theme.mapIcons[14]
                    mapColor = theme.mapColors[mapLimeColor]
                  of trader:
                    mapChar = theme.mapIcons[15]
                    mapColor = theme.mapColors[mapGreenColor]
                  of friendlyShip:
                    mapChar = theme.mapIcons[16]
                    mapColor = theme.mapColors[mapGreen2Color]
                  of EventsTypes.none, baseRecovery:
                    discard
              elif skyMap[x][y].baseIndex > 0:
                mapChar = theme.mapIcons[17]
                if skyBases[skyMap[x][y].baseIndex].known:
                  mapColor = theme.mapColors[mapDefaultColor]
                  if skyBases[skyMap[x][y].baseIndex].visited.year > 0:
                    mapChar = try:
                        factionsList[skyBases[skyMap[x][
                            y].baseIndex].owner].baseIcon.Rune.toUTF8
                      except:
                        dialog = setError(message = "Can't get the base icon.")
                        return
                    mapColor = try:
                        basesTypesList[skyBases[skyMap[x][
                            y].baseIndex].baseType].color
                      except:
                        dialog = setError(
                            message = "Can't get the color of the base.")
                        return
            try:
              let background: Color = (if skyMap[x][y].visited: theme.mapColors[
                  mapVisitedColor] else: theme.mapColors[mapUnvisitedColor])
              setButtonStyle(field = borderColor, color = background)
              setButtonStyle(field = normal, color = background)
              setButtonStyle(field = textBackground, color = background)
              setButtonStyle(field = hover, color = background + 0x303030.Color)
              setButtonStyle(field = textHover, color = mapColor +
                  0x303030.Color)
              setButtonStyle(field = textNormal, color = mapColor)
            except:
              dialog = setError(message = "Can't set map color")
              return
            if isMouseHovering(rect = getWidgetBounds()):
              try:
                showMapInfo(x = x, y = y, theme = theme)
              except:
                dialog = setError(message = "Can't show the map info")
                return
            labelButton(title = mapChar):
              if x == playerShip.skyX and y == playerShip.skyY:
                setDialog(y = windowHeight / 7)
                dialog = ordersDialog
              else:
                setDialog()
                dialog = destinationDialog
                mapX = x
                mapY = y
  restoreButtonStyle()
  # Draw the map's buttons
  setLayoutRowDynamic(height = 20, cols = 5)
  let bounds: NimRect = getWidgetBounds()
  if gameSettings.showTooltips:
    addTooltip(bounds = bounds, text = "Show the map movement menu.")
  showMapMenu(bounds = bounds)
  labelButton(title = "\uf85b"):
    discard
  if gameSettings.showTooltips:
    addTooltip(bounds = getWidgetBounds(),
        text = "Make the map smaller by one row.")
  labelButton(title = "\u25b2"):
    gameSettings.messagesPosition += height
  if gameSettings.showTooltips:
    addTooltip(bounds = getWidgetBounds(),
        text = "Make the map bigger by one row.")
  labelButton(title = "\u25bc"):
    gameSettings.messagesPosition -= height
  if gameSettings.showTooltips:
    addTooltip(bounds = getWidgetBounds(),
        text = "Zoom in the map.")
  labelButton(title = "+"):
    gameSettings.mapFontSize += 2
  if gameSettings.showTooltips:
    addTooltip(bounds = getWidgetBounds(),
        text = "Zoom out the map.")
  labelButton(title = "-"):
    gameSettings.mapFontSize -= 2
  nuklearSetDefaultFont(defaultFont = fonts[UIFont],
      fontSize = gameSettings.interfaceFontSize + 10)
  layoutDynamic(height = windowHeight - mapHeight - 75, cols = 2):
    # Draw last messages
    row(0.75):
      showLastMessages(theme = theme, dialog = dialog)
    row(0.25):
      showButtons(dialog = dialog)
  showGameMenu(dialog = dialog)
