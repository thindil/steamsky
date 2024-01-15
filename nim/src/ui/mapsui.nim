# Copyright 2023-2024 Bartek thindil Jasicki
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

import std/[os, parsecfg, streams, strutils, tables]
import ../[config, game, maps, messages, missions, shipscargo, shipsmovement,
    statistics, stories, tk, types]
import coreui, dialogs, mapsuicommands, utilsui2, themes

var
  centerX*, centerY*: Positive  ## Coordinates of the center point on the map
  generalAccelerators*: array[4, string] = ["Alt-a", "Alt-b", "Alt-c", "Alt-d"]
    ## The list of keyboard shortcuts used in some places
  mapView = ""
  menuAccelerators*: array[1 .. 11, string] = ["s", "o", "r", "m", "k", "w",
      "g", "F1", "p", "q", "x"]
    ## The game menu keyboard shortcuts
  mapAccelerators*: array[1 .. 37, string] = ["e", "v", "plus", "minus",
      "KP_Home", "KP_Up", "KP_Prior", "KP_Left", "KP_Begin", "KP_Right",
      "KP_End", "KP_Down", "KP_Next", "KP_Divide", "Shift-Return", "Shift-h",
      "Shift-KP_Home", "Shift-KP_Up", "Shift-KP_Prior", "Shift-KP_Left",
      "Shift-KP_Right", "Shift-KP_End", "Shift-KP_Down", "Shift-KP_Next",
      "Control-KP_Home", "Control-KP_Up", "Control-KP_Prior", "Control-KP_Left",
      "Control-KP_Right", "Control-KP_End", "Control-KP_Down",
      "Control-KP_Next", "Control-Return", "Control-a", "Control-b",
      "Control-c", "Control-d"] ## The keyboard shortcuts used on the map
  fullScreenAccel* = "Control-f"

proc updateHeader*() {.sideEffect, raises: [], tags: [].} =
  ## Update in-game header with information about time, state of the crew
  ## members, etc.
  var label = gameHeader & ".time"
  tclEval(script = label & " configure -text {" & formattedTime() & "}")
  if gameSettings.showNumbers:
    try:
      discard tclEval(script = label & " configure -text {" & formattedTime() &
          " Speed: " & $((realSpeed(ship = playerShip) * 60) / 1_000) & " km/h}")
    except ValueError:
      tclEval(script = "bgerror {Can't show the speed of the ship. Reason: " &
          getCurrentExceptionMsg() & "}")
      return
    tclEval(script = "tooltip::tooltip " & label & " \"Game time and current ship speed.\"")
  label = gameHeader & ".nofuel"
  tclEval(script = "grid remove " & label)
  var itemAmount = try:
        getItemAmount(itemType = fuelType)
      except KeyError:
        tclEval(script = "bgerror {Can't get items amount. Reason: " &
            getCurrentExceptionMsg() & "}")
        return
  if itemAmount == 0:
    tclEval(script = label & " configure -image nofuelicon")
    tclEval(script = "tooltip::tooltip " & label & " \"You can't travel anymore, because you don't have any fuel for ship.\"")
    tclEval(script = "grid " & label)
  elif itemAmount <= gameSettings.lowFuel:
    tclEval(script = label & " configure -image lowfuelicon")
    tclEval(script = "tooltip::tooltip " & label &
        " \"Low level of fuel on ship. Only " & $itemAmount & " left.\"")
    tclEval(script = "grid " & label)
  label = gameHeader & ".nodrink"
  tclEval(script = "grid remove " & label)
  itemAmount = try:
      getItemsAmount(iType = "Drinks")
    except KeyError:
      tclEval(script = "bgerror {Can't get items amount. Reason: " &
          getCurrentExceptionMsg() & "}")
      return
  if itemAmount == 0:
    tclEval(script = label & " configure -image nodrinksicon")
    tclEval(script = "tooltip::tooltip " & label & " \"You don't have any drinks in ship but your crew needs them to live.\"")
    tclEval(script = "grid " & label)
  elif itemAmount <= gameSettings.lowDrinks:
    tclEval(script = label & " configure -image lowdrinksicon")
    tclEval(script = "tooltip::tooltip " & label &
        " \"Low level of drinks on ship. Only " & $itemAmount & " left.\"")
    tclEval(script = "grid " & label)
  label = gameHeader & ".nofood"
  tclEval(script = "grid remove " & label)
  itemAmount = try:
      getItemsAmount(iType = "Food")
    except KeyError:
      tclEval(script = "bgerror {Can't get items amount. Reason: " &
          getCurrentExceptionMsg() & "}")
      return
  if itemAmount == 0:
    tclEval(script = label & " configure -image nofoodicon")
    tclEval(script = "tooltip::tooltip " & label & " \"You don't have any food in ship but your crew needs them to live.\"")
    tclEval(script = "grid " & label)
  elif itemAmount <= gameSettings.lowFood:
    tclEval(script = label & " configure -image lowfoodicon")
    tclEval(script = "tooltip::tooltip " & label &
        " \"Low level of food on ship. Only " & $itemAmount & " left.\"")
    tclEval(script = "grid " & label)
  var havePilot, haveEngineer, haveTrader, haveUpgrader, haveCleaner,
    haveRepairman = false
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
  label = gameHeader & ".overloaded"
  tclEval(script = "grid remove " & label)
  let
    faction = try:
        factionsList[playerShip.crew[0].faction]
      except KeyError:
        tclEval(script = "bgerror {Can't get faction. Reason: " &
            getCurrentExceptionMsg() & "}")
        return
    frame = mainPaned & ".combat"
  if havePilot and (haveEngineer or "sentientships" in faction.flags) and (
      tclEval2(script = "winfo exists " & frame) == "0" or tclEval2(
      script = "winfo ismapped " & frame) == "0"):
    let speed = try:
          (if playerShip.speed != docked: realSpeed(
              ship = playerShip).float / 1_000.0 else: realSpeed(
                  ship = playerShip,
              infoOnly = true).float / 1_000)
        except ValueError:
          tclEval(script = "bgerror {Can't count speed. Reason: " &
              getCurrentExceptionMsg() & "}")
          return
    if speed < 0.5:
      tclEval(script = "tooltip::tooltip " & label & " \"You can't fly with your ship, because it is overloaded.\"")
      tclEval(script = "grid " & label)
  var
    haveGunner, haveWorker = true
    needWorker, needCleaning, needRepairs = false
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
      tclEval(script = "bgerror {Can't check modules. Reason: " &
          getCurrentExceptionMsg() & "}")
      return
    if module.durability != module.maxDurability:
      needRepairs = true
  label = gameHeader & ".pilot"
  if havePilot:
    tclEval(script = "grid remove " & label)
  else:
    if "sentientships" in faction.flags:
      tclEval(script = label & " configure -image nopiloticon")
      tclEval(script = "tooltip::tooltip " & label & " \"No pilot assigned. Ship fly on it own.\"")
    else:
      tclEval(script = label & " configure -image piloticon")
      tclEval(script = "tooltip::tooltip " & label & " \"No pilot assigned. Ship can't move.\"")
    tclEval(script = "grid " & label)
  label = gameHeader & ".engineer"
  if haveEngineer:
    tclEval(script = "grid remove " & label)
  else:
    if "sentientships" in faction.flags:
      tclEval(script = label & " configure -image noengineericon")
      tclEval(script = "tooltip::tooltip " & label & " \"No engineer assigned. Ship fly on it own.\"")
    else:
      tclEval(script = label & " configure -image engineericon")
      tclEval(script = "tooltip::tooltip " & label & " \"No engineer assigned. Ship can't move.\"")
    tclEval(script = "grid " & label)
  label = gameHeader & ".gunner"
  if haveGunner:
    tclEval(script = "grid remove " & label)
  else:
    tclEval(script = label & " configure -style Headerred.TLabel")
    tclEval(script = "tooltip::tooltip " & label & " \"One or more guns don't have a gunner.\"")
    tclEval(script = "grid " & label)
  label = gameHeader & ".repairs"
  if needRepairs:
    if haveRepairman:
      tclEval(script = label & " configure -image repairicon")
      tclEval(script = "tooltip::tooltip " & label & " \"The ship is being repaired.\"")
    else:
      tclEval(script = label & " configure -image norepairicon")
      tclEval(script = "tooltip::tooltip " & label & " \"The ship needs repairs but no one is working them.\"")
    tclEval(script = "grid " & label)
  else:
    tclEval(script = "grid remove " & label)
  label = gameHeader & ".crafting"
  if needWorker:
    if haveWorker:
      tclEval(script = label & " configure -image manufactureicon")
      tclEval(script = "tooltip::tooltip " & label & " \"All crafting orders are being executed.\"")
    else:
      tclEval(script = label & " configure -image nocrafticon")
      tclEval(script = "tooltip::tooltip " & label & " \"You need to assign crew members to begin manufacturing.\"")
    tclEval(script = "grid " & label)
  else:
    tclEval(script = "grid remove " & label)
  label = gameHeader & ".upgrade"
  if playerShip.upgradeModule > -1:
    if haveUpgrader:
      tclEval(script = label & " configure -image upgradeicon")
      tclEval(script = "tooltip::tooltip " & label & " \"A ship module upgrade in progress.\"")
    else:
      tclEval(script = label & " configure -image noupgradeticon")
      tclEval(script = "tooltip::tooltip " & label & " \"A ship module upgrade is in progress but no one is working on it.\"")
    tclEval(script = "grid " & label)
  else:
    tclEval(script = "grid remove " & label)
  label = gameHeader & ".talk"
  if haveTrader:
    tclEval(script = "grid remove " & label)
  elif skyMap[playerShip.skyX][playerShip.skyY].baseIndex > 0:
    tclEval(script = "tooltip::tooltip " & label & " \"No trader assigned. You need one to talk/trade.\"")
    tclEval(script = "grid " & label)
  elif skyMap[playerShip.skyX][playerShip.skyY].eventIndex > -1:
    if eventsList[skyMap[playerShip.skyX][playerShip.skyY].eventIndex].eType == friendlyShip:
      tclEval(script = "tooltip::tooltip " & label & " \"No trader assigned. You need one to talk/trade.\"")
      tclEval(script = "grid " & label)
    else:
      tclEval(script = "grid remove " & label)
  else:
    tclEval(script = "grid remove " & label)
  label = gameHeader & ".clean"
  if needCleaning:
    if haveCleaner:
      tclEval(script = label & " configure -image cleanicon")
      tclEval(script = "tooltip::tooltip " & label & " \"Ship is cleaned.\"")
    else:
      tclEval(script = label & " configure -image nocleanicon")
      tclEval(script = "tooltip::tooltip " & label & " \"Ship is dirty but no one is cleaning it.\"")
    tclEval(script = "grid " & label)
  else:
    tclEval(script = "grid remove " & label)
  if playerShip.crew[0].health == 0:
    showQuestion(question = "You are dead. Would you like to see your game statistics?",
        res = "showstats")

proc updateMoveButtons*() {.sideEffect, raises: [], tags: [].} =
  ## Update the player's ship movement buttons, depending on the state of the
  ## ship
  let
    moveButtonsNames = ["nw", "n", "ne", "w", "e", "sw", "s", "se"]
    frameName = mainPaned & ".controls.buttons"
    speedBox = frameName & ".box.speed"
  var button = frameName & ".box.moveto"
  if playerShip.speed == docked:
    tclEval(script = "grid remove " & speedBox)
    tclEval(script = "grid remove " & button)
    button = frameName & ".wait"
    tclEval(script = button & " configure -image waiticon")
    tclEval(script = "tooltip::tooltip " & button & " \"Wait " &
        $gameSettings.waitMinutes & " minute" & (if gameSettings.waitMinutes >
        1: "s" else: "") & ".\"")
    for buttonName in moveButtonsNames:
      button = frameName & "." & buttonName
      tclEval(script = button & " state disabled")
      tclEval(script = "tooltip::tooltip " & button & " \"You have to give order 'Undock' from\nMenu->Ship orders first to move ship.\"")
  else:
    tclEval(script = speedBox & " current " & $(playerShip.speed.ord - 1))
    tclEval(script = "grid " & speedBox)
    if playerShip.destinationX > 0 and playerShip.destinationY > 0:
      button = frameName & ".box.moveto"
      tclEval(script = "grid " & button)
      tclEval(script = "grid configure " & speedBox)
      button = frameName & ".wait"
      tclEval(script = button & " configure -image movestepicon")
      tclEval(script = "tooltip::tooltip " & button & " \"Move ship one map field toward destination.\"")
    else:
      button = frameName & ".box.moveto"
      tclEval(script = "grid remove " & button)
      tclEval(script = "grid configure " & speedBox)
      button = frameName & ".wait"
      tclEval(script = button & " configure -image waiticon")
      tclEval(script = "tooltip::tooltip " & button & " \"Wait 1 minute.\"")
    let moveButtonsTooltips = ["Move ship up and left", "Move ship up",
        "Move ship up and right", "Move ship left", "Move ship right",
        "Move ship down and left", "Move ship down", "Move ship down and right"]
    for index, name in moveButtonsNames:
      button = frameName & "." & name
      tclEval(script = button & " state !disabled")
      tclEval(script = "tooltip::tooltip " & button & " \"" &
          moveButtonsTooltips[index] & "\"")

proc finishStory*() {.raises: [], tags: [], exportc.} =
  ## Finish the current player's story. Give experience and ask about
  ## finishing the game
  gameStats.points = gameStats.points + (10_000 * currentStory.maxSteps)
  clearCurrentStory()
  try:
    showQuestion(question = storiesList[currentStory.index].endText &
        " Do you want to finish the game?", res = "retire")
  except KeyError:
    tclEval(script = "bgerror {Can't get the end text of the current story. Result: " &
        tclGetResult2() & "}")

proc showSkyMap*(clear: bool = false) {.sideEffect, raises: [], tags: [].} =
  ## Show the sky map, draw the map, update the header, etc
  ##
  ## * clear - if true, remove the old subwindow and replace it with the one
  ##           with the sky map
  tclSetVar(varName = "refreshmap", newValue = "1")
  if clear:
    showScreen(newScreenName = "mapframe")
  tclSetVar(varName = "gamestate", newValue = "general")
  updateHeader()
  if tclGetVar(varName = "refreshmap") == "1":
    tclEval(script = "DrawMap")
  updateMoveButtons()
  tclEval(script = "update")
  updateMessages()
  if playerShip.speed != docked:
    let speedBox = "$bframe.box.speed"
    tclEval(script = "bind " & speedBox & " <<ComboboxSelected>> {}")
    tclEval(script = speedBox & " current " & $(playerShip.speed.ord - 1))
    tclEval(script = "bind " & speedBox &
        " <<ComboboxSelected>> {SetShipSpeed [" & speedBox & " current]}")
  if currentStory.index.len > 0 and currentStory.showText:
    if currentStory.currentStep > -2:
      try:
        showInfo(text = getCurrentStoryText(), title = "Story")
      except KeyError:
        tclEval(script = "bgerror {Can't show the story text. Reason: " &
            getCurrentExceptionMsg() & "}")
    else:
      finishStory()
      if playerShip.crew[0].health == 0:
        showQuestion(question = "You are dead. Would you like to see your game statistics?",
            res = "showstats")
    currentStory.showText = true

proc drawMap*() =
  var preview = (if tclGetVar(varName = "mappreview").len > 0: true else: false)
  if preview and playerShip.speed != docked:
    tclUnsetVar(varName = "mappreview")
    preview = false
  tclEval(script = mapView & " configure -state normal")
  tclEval(script = mapView & " delete 1.0 end")
  let
    mapHeight: Positive = tclEval2(script = mapView & " cget -height").parseInt()
    mapWidth: Positive = tclEval2(script = mapView & " cget -widht").parseInt()
  var
    startX = centerX - (mapHeight / 2).int
    startY = centerY - (mapWidth / 2).int
    endY = centerY + (mapHeight / 2).int
    endX = centerX + (mapWidth / 2).int
    storyX = 1
    storyY = 1
  if currentStory.index.len > 0:
    (storyX, storyY) = getStoryLocation()
    if storyX == playerShip.skyX and storyY == playerShip.skyY:
      storyX = 0
      storyY = 0
  if playerShip.speed == docked and skyMap[playerShip.skyX][
      playerShip.skyY].baseIndex == 0:
    playerShip.speed = fullStop
  let currentTheme = themesList[gameSettings.interfaceTheme]
  for y in startY .. endY:
    for x in startX .. endX:
      var mapTag, mapChar = ""
      if x == playerShip.skyX and y == playerShip.skyY:
        mapChar = currentTheme.playerShipIcon
      else:
        mapChar = currentTheme.emptyMapIcon
        mapTag = (if skyMap[x][y].visited: "black" else: "unvisited gray")
        if x == playerShip.destinationX and y == playerShip.destinationY:
          mapChar = currentTheme.targetIcon
          mapTag = (if skyMap[x][y].visited: "" else: "unvisited")
        elif currentStory.index.len > 0 and (x == storyX and y == storyY):
          mapChar = currentTheme.storyIcon
          mapTag = "green"
        elif skyMap[x][y].missionIndex > -1:
          case acceptedMissions[skyMap[x][y].missionIndex].mType
          of deliver:
            mapChar = currentTheme.deliverIcon
            mapTag = "yellow"
          of destroy:
            mapChar = currentTheme.destroyIcon
            mapTag = "red"
          of patrol:
            mapChar = currentTheme.patrolIcon
            mapTag = "lime"
          of explore:
            mapChar = currentTheme.exploreIcon
            mapTag = "green"
          of passenger:
            mapChar = currentTheme.passengerIcon
            mapTag = "cyan"
          if not skyMap[x][y].visited:
            mapTag = mapTag & " unvisited"
        elif skyMap[x][y].eventIndex > -1:
          if skyMap[x][y].eventIndex > eventsList.high:
            skyMap[x][y].eventIndex = -1
          else:
            case eventsList[skyMap[x][y].eventIndex].eType
            of enemyShip:
              mapChar = currentTheme.enemyShipIcon
              mapTag = "red"
            of attackOnBase:
              mapChar = currentTheme.attackOnBaseIcon
              mapTag = "red2"
            of enemyPatrol:
              mapChar = currentTheme.enemyPatrolIcon
              mapTag = "red3"
            of disease:
              mapChar = currentTheme.diseaseIcon
              mapTag = "yellow"
            of fullDocks:
              mapChar = currentTheme.fullDocksIcon
              mapTag = "cyan"
            of doublePrice:
              mapChar = currentTheme.doublePriceIcon
              mapTag = "lime"
            of trader:
              mapChar = currentTheme.traderIcon
              mapTag = "green"
            of friendlyShip:
              mapChar = currentTheme.friendlyShipIcon
              mapTag = "green2"
            of EventsTypes.none, baseRecovery:
              discard
            if not skyMap[x][y].visited:
              mapTag = mapTag & " unvisited"
        elif skyMap[x][y].baseIndex > 0:
          mapChar = currentTheme.notVisitedBaseIcon
          if skyBases[skyMap[x][y].baseIndex].known:
            if skyBases[skyMap[x][y].baseIndex].visited.year > 0:
              mapChar = factionsList[skyBases[skyMap[x][y].baseIndex].owner].baseIcon.toHex(4).parseHexStr
              mapTag = skyBases[skyMap[x][y].baseIndex].baseType
            else:
              mapTag = "unvisited"
          else:
            mapTag = "unvisited gray"
      if preview:
        for mission in skyBases[skyMap[playerShip.skyX][playerShip.skyY].baseIndex].missions:
          if mission.targetX == x and mission.targetY == y:
            case mission.mType
            of deliver:
              mapChar = currentTheme.deliverIcon
              mapTag = "yellow"
            of destroy:
              mapChar = currentTheme.destroyIcon
              mapTag = "red"
            of patrol:
              mapChar = currentTheme.patrolIcon
              mapTag = "lime"
            of explore:
              mapChar = currentTheme.exploreIcon
              mapTag = "green"
            of passenger:
              mapChar = currentTheme.passengerIcon
              mapTag = "cyan"
            if not skyMap[x][y].visited:
              mapTag = mapTag & " unvisited"
            break
      tclEval(script = mapView & " insert end {" & mapChar & "} [list " & mapTag & "]")
    if y < endY:
      tclEval(script = mapView & " insert end {\n}")
  tclEval(script = mapView & " configure -state disable")

proc createGameUi*() =
  let
    gameFrame = ".gameframe"
    paned = gameFrame & ".paned"
  mapView = paned & ".mapframe.map"
  var newStart = false
  if tclEval2(script = "winfo exists " & mapView) == "0":
    newStart = true
    let fileName = saveDirectory & "keys.cfg"
    var configFile = newFileStream(fileName)
    if configFile != nil:
      var parser: CfgParser
      parser.open(configFile, fileName)
      while true:
        var entry = parser.next
        case entry.kind
          of cfgEof:
            break
          of cfgSectionStart, cfgOption:
            discard
          of cfgKeyValuePair:
            case entry.key
            of "ShipInfo":
              menuAccelerators[1] = entry.value
            of "Orders":
              menuAccelerators[2] = entry.value
            of "Crafting":
              menuAccelerators[3] = entry.value
            of "LastMessages":
              menuAccelerators[4] = entry.value
            of "Knowledge":
              menuAccelerators[5] = entry.value
            of "WaitOrders":
              menuAccelerators[6] = entry.value
            of "GameStats":
              menuAccelerators[7] = entry.value
            of "Help":
              menuAccelerators[8] = entry.value
            of "GameOptions":
              menuAccelerators[9] = entry.value
            of "Quit":
              menuAccelerators[10] = entry.value
            of "Resign":
              menuAccelerators[11] = entry.value
            of "GameMenu":
              mapAccelerators[1] = entry.value
            of "MapOptions":
              mapAccelerators[2] = entry.value
            of "ZoomInMap":
              mapAccelerators[3] = entry.value
            of "ZoomOutMap":
              mapAccelerators[4] = entry.value
            of "MoveUpLeft":
              mapAccelerators[5] = entry.value
            of "MoveUp":
              mapAccelerators[6] = entry.value
            of "MoveUpRight":
              mapAccelerators[7] = entry.value
            of "MoveLeft":
              mapAccelerators[8] = entry.value
            of "WaitInPlace":
              mapAccelerators[10] = entry.value
            of "MoveRight":
              mapAccelerators[9] = entry.value
            of "MoveDownLeft":
              mapAccelerators[11] = entry.value
            of "MoveDown":
              mapAccelerators[12] = entry.value
            of "MoveDownRight":
              mapAccelerators[13] = entry.value
            of "MoveTo":
              mapAccelerators[14] = entry.value
            of "CenterMap":
              mapAccelerators[15] = entry.value
            of "CenterMapOnHomeBase":
              mapAccelerators[16] = entry.value
            of "MoveMapUpLeft":
              mapAccelerators[17] = entry.value
            of "MoveMapUp":
              mapAccelerators[18] = entry.value
            of "MoveMapUpRight":
              mapAccelerators[19] = entry.value
            of "MoveMapLeft":
              mapAccelerators[20] = entry.value
            of "MoveMapRight":
              mapAccelerators[21] = entry.value
            of "MoveMapDownLeft":
              mapAccelerators[22] = entry.value
            of "MoveMapDown":
              mapAccelerators[23] = entry.value
            of "MoveMapDownRight":
              mapAccelerators[24] = entry.value
            of "MoveCursorUpLeft":
              mapAccelerators[25] = entry.value
            of "MoveCursorUp":
              mapAccelerators[26] = entry.value
            of "MoveCursorUpRight":
              mapAccelerators[27] = entry.value
            of "MoveCursorLeft":
              mapAccelerators[28] = entry.value
            of "MoveCursorRight":
              mapAccelerators[29] = entry.value
            of "MoveCursorDownLeft":
              mapAccelerators[30] = entry.value
            of "MoveCursorDown":
              mapAccelerators[31] = entry.value
            of "MoveCursorDownRight":
              mapAccelerators[32] = entry.value
            of "LeftClickMouse":
              mapAccelerators[33] = entry.value
            of "FullStop":
              mapAccelerators[34] = entry.value
            of "QuarterSpeed":
              mapAccelerators[35] = entry.value
            of "HalfSpeed":
              mapAccelerators[36] = entry.value
            of "FullSpeed":
              mapAccelerators[37] = entry.value
            of "FullScreen":
              fullScreenAccel = entry.value
          of cfgError:
            tclEval(script = "bgerror {Can't set keyboard shortcuts. Reason: " &
                entry.msg & "}")
      parser.close()
    else:
      if DirSep == '\\':
        mapAccelerators[5] = "Home"
        mapAccelerators[6] = "Up"
        mapAccelerators[7] = "Prior"
        mapAccelerators[8] = "Left"
        mapAccelerators[9] = "Clear"
        mapAccelerators[10] = "Right"
        mapAccelerators[11] = "End"
        mapAccelerators[12] = "Down"
        mapAccelerators[13] = "Next"
        mapAccelerators[14] = "slash"
        mapAccelerators[17] = "Shift-Home"
        mapAccelerators[18] = "Shift-Up"
        mapAccelerators[19] = "Shift-Prior"
        mapAccelerators[20] = "Shift-Left"
        mapAccelerators[21] = "Shift-Right"
        mapAccelerators[22] = "Shift-End"
        mapAccelerators[23] = "Shift-Down"
        mapAccelerators[24] = "Shift-Next"
        mapAccelerators[25] = "Control-Home"
        mapAccelerators[26] = "Control-Up"
        mapAccelerators[27] = "Control-Prior"
        mapAccelerators[28] = "Control-Left"
        mapAccelerators[29] = "Control-Right"
        mapAccelerators[30] = "Control-End"
        mapAccelerators[31] = "Control-Down"
        mapAccelerators[32] = "Control-Next"
    mapsuicommands.addCommands()

# Temporary code for interfacing with Ada

proc updateAdaHeader() {.raises: [], tags: [], exportc.} =
  try:
    updateHeader()
  except:
    discard

proc updateAdaMoveButtons() {.raises: [], tags: [], exportc.} =
  updateMoveButtons()

proc finishAdaStory() {.raises: [], tags: [], exportc.} =
  try:
    finishStory()
  except:
    discard

proc showAdaSkyMap(clear: cint) {.raises: [], tags: [], exportc.} =
  try:
    showSkyMap(clear == 1)
  except:
    discard

proc getAdaCenterPoint(x, y: var cint) {.raises: [], tags: [], exportc.} =
  x = centerX.cint
  y = centerY.cint

proc setAdaCenterPoint(x, y: cint) {.raises: [], tags: [], exportc.} =
  centerX = x.Positive
  centerY = y.Positive

proc getAdaGeneralAccelerator(index: cint): cstring {.raises: [], tags: [], exportc.} =
  return generalAccelerators[index - 1].cstring

proc setAdaGeneralAccelerator(index: cint; value: cstring) {.raises: [], tags: [], exportc.} =
  generalAccelerators[index - 1] = $value

proc getAdaMenuAccelerator(index: cint): cstring {.raises: [], tags: [], exportc.} =
  return menuAccelerators[index].cstring

proc setAdaMenuAccelerator(index: cint; value: cstring) {.raises: [], tags: [], exportc.} =
  menuAccelerators[index] = $value

proc getAdaMapAccelerator(index: cint): cstring {.raises: [], tags: [], exportc.} =
  return mapAccelerators[index].cstring

proc setAdaMapAccelerator(index: cint; value: cstring) {.raises: [], tags: [], exportc.} =
  mapAccelerators[index] = $value

proc getAdaFullScreenAccel(): cstring {.raises: [], tags: [], exportc.} =
  return fullScreenAccel.cstring

proc setAdaFullScreenAccel(value: cstring) {.raises: [], tags: [], exportc.} =
  fullScreenAccel = $value

proc drawAdaMap() {.raises: [], tags: [], exportc.} =
  try:
    drawMap()
  except:
    discard
