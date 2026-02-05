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
import contracts, nuklear/nuklear_sdl_renderer
import ../[bases, basestypes, config, crew2, events2, game, game2, maps,
    messages, missions, missions2, shipscrew, shipscargo, shipsmovement,
    stories, types]
import coreui, dialogs, errordialog, header, messagesui, themes, utilsui2

var
  centerX*: MapXRange = 1
    ## The X coordinate of the center point of the map
  centerY*: MapYRange = 1
    ## The Y coordinate of the center point of the map

proc createGameUi*(dialog: var GameDialog) {.raises: [], tags: [RootEffect],
    contractual.} =
  ## Create the game's UI and show the map to the player
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns parameter dialog, modified if any error happened.
  if images[menuIcon] == nil:
    # Load images
    try:
      for index, fileName in themesList[gameSettings.interfaceTheme].icons[
          menuIcon..IconsNames.high]:
        images[(index + 4).IconsNames] = nuklearLoadSVGImage(
            filePath = fileName, width = 0, height = 20 +
            gameSettings.interfaceFontSize)
    except:
      dialog = setError(message = "Can't set the game's images.")
  centerX = playerShip.skyX
  centerY = playerShip.skyY
  mapPreview = false

proc showMapInfo(x: MapXRange; y: MapYRange; theme: ThemeData) {.raises: [
    ValueError], tags: [WriteIOEffect, TimeEffect, RootEffect], contractual.} =
  ## Show the map cell info popup
  ##
  ## * x      - the X coordinate of the map cell which info will be show
  ## * y      - the Y coordinate of the map cell which info will be show
  ## * theme  - the current game's theme
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
            if getBasePopulation(baseIndex = baseIndex) == empty:
              row(width = 230):
                label(str = "Base is abandoned")
            else:
              row(width = 100):
                label(str = "Population:")
              row(width = 130):
                colorLabel(str = $getBasePopulation(baseIndex = baseIndex),
                    color = theme.mapColors[mapGoldenYellow])
            row(width = 60):
              label(str = "Size:")
            row(width = 170):
              colorLabel(str = $skyBases[baseIndex].size,
                  color = theme.mapColors[mapGoldenYellow])
            if getBasePopulation(baseIndex = baseIndex) > empty:
              row(width = 70):
                label(str = "Owner:")
              row(width = 160):
                colorLabel(str = factionsList[skyBases[baseIndex].owner].name,
                    color = theme.mapColors[mapGoldenYellow])
        setLayoutRowDynamic(height = 25, cols = 1)
        if getBasePopulation(baseIndex = baseIndex) > empty and skyBases[
            baseIndex].visited.year > 0:
          case skyBases[baseIndex].reputation.level
          of -100.. -75:
            colorLabel(str = "You are hated here", color = theme.mapColors[mapRedColor])
          of -74.. -50:
            colorLabel(str = "You are outlawed here", color = theme.mapColors[mapRedColor])
          of -49.. -25:
            colorLabel(str = "You are disliked here", color = theme.mapColors[mapRedColor])
          of -24.. -1:
            colorLabel(str = "They are unfriendly to you",
                color = theme.mapColors[mapRedColor])
          of 0:
            label(str = "You are unknown here")
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
    if skyMap[x][y].missionIndex > -1:
      setLayoutRowDynamic(height = 25, cols = 1)
      let missionIndex: int = skyMap[x][y].missionIndex
      case acceptedMissions[missionIndex].mType
      of deliver:
        label(str = "Deliver " & itemsList[acceptedMissions[
            missionIndex].itemIndex].name)
      of destroy:
        label(str = "Destroy " & protoShipsList[acceptedMissions[
            missionIndex].shipIndex].name)
      of patrol:
        label(str = "Patrol area")
      of explore:
        label(str = "Explore area")
      of passenger:
        label(str = "Transport passenger")
    if mapPreview:
      for mission in skyBases[skyMap[playerShip.skyX][
          playerShip.skyY].baseIndex].missions:
        if mission.targetX == x and mission.targetY == y:
          setLayoutRowDynamic(height = 50, cols = 1)
          case mission.mType
          of deliver:
            wrapLabel(str = "Deliver " & itemsList[mission.itemIndex].name)
          of destroy:
            wrapLabel(str = "Destroy " & protoShipsList[mission.shipIndex].name)
          of patrol:
            label(str = "Patrol area")
          of explore:
            label(str = "Explore area")
          of passenger:
            label(str = "Transport passenger")
          break
    if currentStory.index.len > 0:
      var storyX, storyY: Natural = 1
      (storyX, storyY) = getStoryLocation()
      if storyX == playerShip.skyX and storyY == playerShip.skyY:
        storyX = 0
        storyY = 0
      var finishCondition: StepConditionType = any
      if y == storyX and y == storyY:
        finishCondition = (if currentStory.currentStep == 0: storiesList[
            currentStory.index].startingStep.finishCondition elif currentStory.currentStep >
            0: storiesList[currentStory.index].steps[
            currentStory.currentStep].finishCondition else: storiesList[
            currentStory.index].finalStep.finishCondition)
        if finishCondition in {askInBase, destroyShip, explore}:
          setLayoutRowDynamic(height = 25, cols = 1)
          label(str = "Story leads you here")
    if x == playerShip.skyX and y == playerShip.skyY:
      setLayoutRowDynamic(height = 25, cols = 1)
      colorLabel(str = "You are here", color = theme.mapColors[mapYellowColor])
    if skyMap[x][y].eventIndex > -1:
      setLayoutRowDynamic(height = 25, cols = 1)
      let eventIndex: Natural = skyMap[x][y].eventIndex
      label(str = "")
      case eventsList[eventIndex].eType
      of trader:
        colorLabel(str = protoShipsList[eventsList[eventIndex].shipIndex].name,
            color = theme.mapColors[mapGreenColor])
      of friendlyShip:
        colorLabel(str = protoShipsList[eventsList[eventIndex].shipIndex].name,
            color = theme.mapColors[mapGreen2Color])
      of enemyShip:
        colorLabel(str = protoShipsList[eventsList[eventIndex].shipIndex].name,
            color = theme.mapColors[mapRedColor])
      of fullDocks:
        colorLabel(str = "Full docks in base", color = theme.mapColors[mapCyanColor])
      of attackOnBase:
        colorLabel(str = "Base is under attack", color = theme.mapColors[mapRedColor])
      of disease:
        colorLabel(str = "Disease in base", color = theme.mapColors[mapYellowColor])
      of enemyPatrol:
        colorLabel(str = "Enemy patrol", color = theme.mapColors[mapRed3Color])
      of doublePrice:
        colorLabel(str = "Double price for " & itemsList[eventsList[
            eventIndex].itemIndex].name, color = theme.mapColors[mapLimeColor])
      of EventsTypes.none, baseRecovery:
        discard
  nuklearSetDefaultFont(defaultFont = fonts[FontsNames.mapFont],
      fontSize = gameSettings.mapFontSize + 10)

var
  moveX: MapXRange = 1
  moveY: MapYRange = 1
  rows, cols: Positive = 1

proc showMapMenu(bounds: Rect) {.raises: [], tags: [RootEffect],
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
    imageButton(image = images[arrowUpLeft]):
      centerY = (if centerY - (rows / 3).int < 1: (rows /
          3).int else: centerY - (rows / 3).int)
      centerX = (if centerX - (cols / 3).int < 1: (cols /
          3).int else: centerX - (cols / 3).int)
    imageButton(image = images[arrowUp]):
      centerY = (if centerY - (rows / 3).int < 1: (rows /
          3).int else: centerY - (rows / 3).int)
    imageButton(image = images[arrowUpRight]):
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
    imageButton(image = images[arrowLeft]):
      centerX = (if centerX - (cols / 3).int < 1: (cols /
          3).int else: centerX - (cols / 3).int)
    label(str = "")
    imageButton(image = images[arrowRight]):
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
    imageButton(image = images[arrowDownLeft]):
      centerY = (if centerY + (rows / 3).int > 1_024: (rows /
          3).int else: centerY + (rows / 3).int)
      centerX = (if centerX - (cols / 3).int < 1: (cols /
          3).int else: centerX - (cols / 3).int)
    imageButton(image = images[arrowDown]):
      centerY = (if centerY + (rows / 3).int > 1_024: (rows /
          3).int else: centerY + (rows / 3).int)
    imageButton(image = images[arrowDownRight]):
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

proc updateCoordinates(newX, newY: var int) {.raises: [], tags: [],
    contractual.} =
  ## Update the new coordinates after move the player's ship
  ##
  ## * newX - the difference on X axis for the player's ship position
  ## * newY - the difference on Y axis for the player's ship position
  ##
  ## Returns modified parameters newX and newY
  if playerShip.destinationX > playerShip.skyX:
    newX = 1
  elif playerShip.destinationX < playerShip.skyX:
    newX = -1
  if playerShip.destinationY > playerShip.skyY:
    newY = 1
  elif playerShip.destinationY < playerShip.skyY:
    newY = -1

proc moveShipOnMap(dialog: var GameDialog): Natural {.raises: [],
  tags: [RootEffect], contractual.} =
  ## Move the player's ship on the map
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameters dialog and the result's code of the
  ## movement.
  result = 0
  while true:
    var
      newX, newY: int = 0
      message: string = ""
    updateCoordinates(newX = newX, newY = newY)
    result = try:
        moveShip(x = newX, y = newY, message = message)
      except:
        dialog = setError(message = "Can't move the ship.")
        return
    if result == 0:
      break
    var startsCombat: bool = try:
        checkForEvent()
      except:
        dialog = setError(message = "Can't check for events.")
        return
    if startsCombat:
      result = 4
      break
    if result == 8:
      try:
        waitForRest()
      except:
        dialog = setError(message = "Can't wait for rest of the crew.")
        return
      try:
        if "sentientships" notin factionsList[playerShip.crew[
            0].faction].flags and (findMember(order = pilot) == -1 or
            findMember(order = engineer) == 0):
          try:
            waitForRest()
          except:
            dialog = setError(message = "Can't wait for rest of the crew.")
            return
      except:
        dialog = setError(message = "Can't check do faction has sentientships flag.")
        return
      result = 1
      startsCombat = try:
          checkForEvent()
        except:
          dialog = setError(message = "Can't check for events.")
          return
      if startsCombat:
        result = 4
        break
    if gameSettings.autoMoveStop != never and skyMap[playerShip.skyX][
        playerShip.skyY].eventIndex > -1:
      let eventIndex: int = skyMap[playerShip.skyX][playerShip.skyY].eventIndex
      case gameSettings.autoMoveStop
      of any:
        if eventsList[eventIndex].eType in {enemyShip, trader, friendlyShip, enemyPatrol}:
          result = 0
          break
      of friendly:
        if eventsList[eventIndex].eType in {trader, friendlyShip}:
          result = 0
          break
      of enemy:
        if eventsList[eventIndex].eType in {enemyShip, enemyPatrol}:
          result = 0
          break
      of never:
        discard
    if dialog == none:
      try:
        if getItemAmount(itemType = fuelType) <= gameSettings.lowFuel:
          dialog = setMessage(message = "Your fuel level is dangerously low.",
              title = "Low fuel level")
          result = 4
          break
        elif getItemsAmount(iType = "Food") <= gameSettings.lowFood:
          dialog = setMessage(message = "Your food level is dangerously low.",
              title = "Low food level")
          result = 4
          break
        elif getItemsAmount(iType = "Drinks") <= gameSettings.lowDrinks:
          dialog = setMessage(message = "Your drinks level is dangerously low.",
              title = "Low drinks level")
          result = 4
          break
      except:
        dialog = setError(message = "Can't check low level of items.")
        return
    if playerShip.destinationX == playerShip.skyX and
        playerShip.destinationY == playerShip.skyY:
      addMessage(message = "You reached your travel destination.",
          mType = orderMessage)
      playerShip.destinationX = 0
      playerShip.destinationY = 0
      if gameSettings.autoFinish:
        message = try:
            autoFinishMissions()
          except:
            dialog = setError(message = "Can't finish missions.")
            return
      result = 4
      break
    if result in 6..7:
      break

proc showButtons(dialog: var GameDialog) {.raises: [], tags: [RootEffect],
    contractual.} =
  ## Show the buttons for manage the ship, like orders, movement or wait
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameters dialog.
  group(title = "ButtonsGroup", flags = {windowNoScrollbar}):
    if dialog != none:
      windowDisable()
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
    var
      res: Natural = 0
      message: string = ""
      newX, newY: int = 0

    if playerShip.speed != docked and playerShip.destinationX > 0:
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Auto move your ship to its destination.")
      imageButton(image = images[moveToIcon]):
        res = moveShipOnMap(dialog = dialog)
    setLayoutRowDynamic(height = 30, cols = 1)
    if playerShip.speed == docked:
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
          text = if gameSettings.waitMinutes ==
              1: "Wait 1 minute." else: "Wait " & $gameSettings.waitMinutes & " minutes.")
      imageButtonCentered(image = images[waitIcon]):
        try:
          updateGame(minutes = gameSettings.waitMinutes)
          waitInPlace(minutes = gameSettings.waitMinutes)
        except:
          dialog = setError(message = "Can't update the game.")
    else:
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Set speed for your ship. The faster you move, the more fuel used. But faster movement has bigger chance to evade enemies.")
      playerShip.speed = (comboList(items = shipSpeeds,
          selected = playerShip.speed.ord - 1, itemHeight = 25, x = 200,
          y = 50) + 1).ShipSpeed
      setLayoutRowStatic(height = 30, cols = 3, ratio = [40.cfloat, 40, 40])
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Move ship up and left")
      imageButton(image = images[arrowUpLeft]):
        try:
          res = moveShip(x = -1, y = -1, message = message)
        except:
          dialog = setError(message = "Can't move the ship.")
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Move ship up")
      imageButton(image = images[arrowUp]):
        try:
          res = moveShip(x = 0, y = -1, message = message)
        except:
          dialog = setError(message = "Can't move the ship.")
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Move ship up and right")
      imageButton(image = images[arrowUpRight]):
        try:
          res = moveShip(x = 1, y = -1, message = message)
        except:
          dialog = setError(message = "Can't move the ship.")
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Move ship left")
      imageButton(image = images[arrowLeft]):
        try:
          res = moveShip(x = -1, y = 0, message = message)
        except:
          dialog = setError(message = "Can't move the ship.")
      if playerShip.destinationX == 0:
        if gameSettings.showTooltips:
          addTooltip(bounds = getWidgetBounds(),
            text = if gameSettings.waitMinutes ==
                1: "Wait 1 minute." else: "Wait " & $gameSettings.waitMinutes & " minutes.")
        imageButton(image = images[waitIcon]):
          res = 1
          try:
            updateGame(minutes = gameSettings.waitMinutes)
            waitInPlace(minutes = gameSettings.waitMinutes)
          except:
            dialog = setError(message = "Can't update the game.")
      else:
        if gameSettings.showTooltips:
          addTooltip(bounds = getWidgetBounds(),
              text = "Move ship one map field toward destination")
        imageButton(image = images[moveStepIcon]):
          updateCoordinates(newX = newX, newY = newY)
          res = try:
              moveShip(x = newX, y = newY, message = message)
            except:
              dialog = setError(message = "Can't move the ship.")
              return
          if playerShip.destinationX == playerShip.skyX and
              playerShip.destinationY == playerShip.skyY:
            addMessage(message = "You reached your travel destination.",
                mType = orderMessage)
            playerShip.destinationX = 0
            playerShip.destinationY = 0
            if gameSettings.autoFinish:
              message = try:
                  autoFinishMissions()
                except:
                  dialog = setError(message = "Can't finish missions.")
                  return
            res = 4
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Move ship right")
      imageButton(image = images[arrowRight]):
        try:
          res = moveShip(x = 1, y = 0, message = message)
        except:
          dialog = setError(message = "Can't move the ship.")
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Move ship down and left")
      imageButton(image = images[arrowDownLeft]):
        try:
          res = moveShip(x = -1, y = 1, message = message)
        except:
          dialog = setError(message = "Can't move the ship.")
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Move ship down")
      imageButton(image = images[arrowDown]):
        try:
          res = moveShip(x = 0, y = 1, message = message)
        except:
          dialog = setError(message = "Can't move the ship.")
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Move ship down and right")
      imageButton(image = images[arrowDownRight]):
        try:
          res = moveShip(x = 1, y = 1, message = message)
        except:
          dialog = setError(message = "Can't move the ship.")

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
    var height: float = 0
    if playerShip.speed == docked:
      height = 115
    else:
      if playerShip.destinationX > 0 and playerShip.destinationY > 0:
        height = 185
      else:
        height = 150

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
      setLayoutRowDynamic(height = 30, cols = 1)
      labelButton(title = "Set destination"):
        setDestination(dialog = dialog)
      if playerShip.speed != docked:
        labelButton(title = "Set destination and move"):
          setDestination(dialog = dialog)
          discard moveShipOnMap(dialog = dialog)
        if playerShip.destinationX > 0 and playerShip.destinationY > 0:
          labelButton(title = "Move to"):
            closeDialog(dialog = dialog)
            discard moveShipOnMap(dialog = dialog)
      labelButton(title = "Close"):
        closeDialog(dialog = dialog)
  except:
    dialog = setError(message = "Can't show the destination's menu")

proc showMission(mType: MissionsTypes): tuple[icon: string;
    color: Color] {.raises: [], tags: [], contractual.} =
  ## Show the mission info on the map, based on the missions' type
  ##
  ## * mType - the type of the mission
  ##
  ## Returns the tuple with icon and color tag for the selected mission
  case mType
  of deliver:
    result.icon = theme.mapIcons[deliverIcon]
    result.color = theme.mapColors[mapYellowColor]
  of destroy:
    result.icon = theme.mapIcons[destroyIcon]
    result.color = theme.mapColors[mapRedColor]
  of patrol:
    result.icon = theme.mapIcons[patrolIcon]
    result.color = theme.mapColors[mapLimeColor]
  of explore:
    result.icon = theme.mapIcons[exploreIcon]
    result.color = theme.mapColors[mapGreenColor]
  of passenger:
    result.icon = theme.mapIcons[passengerIcon]
    result.color = theme.mapColors[mapCyanColor]

proc showMap*(state: var GameState; dialog: var GameDialog) {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Show the game's map
  ##
  ## * state - the current game's state
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameters state and dialog. The latter is modified if
  ## any error happened.
  discard showHeader(dialog = dialog, state = state)
  if playerShip.speed != docked and mapPreview:
    mapPreview = false
  # draw dialogs
  showDestinationMenu(dialog = dialog)
  # draw map
  nuklearSetDefaultFont(defaultFont = fonts[FontsNames.mapFont],
      fontSize = gameSettings.mapFontSize + 10)
  let height: Positive = gameSettings.mapFontSize + 10
  rows = ((windowHeight - 35 - gameSettings.messagesPosition.float) /
        height.float).ceil.Natural
  let colWidth: Positive = try:
      getTextWidth(text = theme.mapIcons[emptyMapIcon]).Positive + 4
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
              mapChar: string = theme.mapIcons[emptyMapIcon]
              mapColor: Color = theme.mapColors[mapUnvisitedColor]
            if x == playerShip.skyX and y == playerShip.skyY:
              skyMap[x][y].visited = true
              mapChar = theme.mapIcons[playerShipIcon]
              mapColor = theme.mapColors[mapDefaultColor]
            else:
              if x == playerShip.destinationX and y == playerShip.destinationY:
                mapChar = theme.mapIcons[targetIcon]
                mapColor = theme.mapColors[mapDefaultColor]
              elif currentStory.index.len > 0 and (x == storyX and y == storyY):
                mapChar = theme.mapIcons[storyIcon]
                mapColor = theme.mapColors[mapGreenColor]
              elif skyMap[x][y].missionIndex > -1:
                (mapChar, mapColor) = showMission(mType = acceptedMissions[
                    skyMap[x][y].missionIndex].mType)
              elif skyMap[x][y].eventIndex > -1:
                if skyMap[x][y].eventIndex > eventsList.high:
                  skyMap[x][y].eventIndex = -1
                else:
                  case eventsList[skyMap[x][y].eventIndex].eType
                  of enemyShip:
                    mapChar = theme.mapIcons[enemyShipIcon]
                    mapColor = theme.mapColors[mapRedColor]
                  of attackOnBase:
                    mapChar = theme.mapIcons[attackOnBaseIcon]
                    mapColor = theme.mapColors[mapRed2Color]
                  of enemyPatrol:
                    mapChar = theme.mapIcons[enemyPatrolIcon]
                    mapColor = theme.mapColors[mapRed3Color]
                  of disease:
                    mapChar = theme.mapIcons[diseaseIcon]
                    mapColor = theme.mapColors[mapYellowColor]
                  of fullDocks:
                    mapChar = theme.mapIcons[fullDocksIcon]
                    mapColor = theme.mapColors[mapCyanColor]
                  of doublePrice:
                    mapChar = theme.mapIcons[doublePriceIcon]
                    mapColor = theme.mapColors[mapLimeColor]
                  of trader:
                    mapChar = theme.mapIcons[mapTraderIcon]
                    mapColor = theme.mapColors[mapGreenColor]
                  of friendlyShip:
                    mapChar = theme.mapIcons[friendlyShipIcon]
                    mapColor = theme.mapColors[mapGreen2Color]
                  of EventsTypes.none, baseRecovery:
                    discard
              elif skyMap[x][y].baseIndex > 0:
                mapChar = theme.mapIcons[notVisitedBaseIcon]
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
            if mapPreview:
              for mission in skyBases[skyMap[playerShip.skyX][
                  playerShip.skyY].baseIndex].missions:
                if mission.targetX == x and mission.targetY == y:
                  (mapChar, mapColor) = showMission(mType = mission.mType)
                  break
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
            if dialog != none:
              windowDisable()
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
  let bounds: Rect = getWidgetBounds()
  if gameSettings.showTooltips:
    addTooltip(bounds = bounds, text = "Show the map movement menu.")
  showMapMenu(bounds = bounds)
  labelButton(title = "\uf85b"):
    discard
  if gameSettings.showTooltips:
    addTooltip(bounds = getWidgetBounds(),
        text = "Make the map smaller by one row.")
  imageButtonCentered(image = images[contract2Icon]):
    gameSettings.messagesPosition += height
  if gameSettings.showTooltips:
    addTooltip(bounds = getWidgetBounds(),
        text = "Make the map bigger by one row.")
  imageButtonCentered(image = images[expand2Icon]):
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
    row(width = 0.75):
      showLastMessages(theme = theme, dialog = dialog, withButtons = false, height = 0)
    row(width = 0.25):
      showButtons(dialog = dialog)
