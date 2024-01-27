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

import std/[os, parsecfg, streams, strutils, tables, unicode]
import ../[basestypes, config, crew2, events2, game, game2, maps, messages,
    missions, missions2, shipscargo, shipscrew, shipsmovement, statistics,
    stories, tk, types]
import coreui, dialogs, mapsuicommands, ordersmenu, utilsui2, themes

var
  centerX*, centerY*: Positive  ## Coordinates of the center point on the map
  startX, startY: int           ## Coordinates of the top left point on the map
  generalAccelerators*: array[4, string] = ["Alt-a", "Alt-b", "Alt-c", "Alt-d"]
    ## The list of keyboard shortcuts used in some places
  mapView = ".gameframe.paned.mapframe.map"
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

proc drawMap*() {.sideEffect, raises: [], tags: [].} =
  ## Draw the map on the screen
  var preview = (if tclGetVar(varName = "mappreview").len > 0: true else: false)
  if preview and playerShip.speed != docked:
    tclUnsetVar(varName = "mappreview")
    preview = false
  tclEval(script = mapView & " configure -state normal")
  tclEval(script = mapView & " delete 1.0 end")
  let
    mapHeight: Positive = try:
        tclEval2(script = mapView & " cget -height").parseInt()
      except:
        tclEval(script = "bgerror {Can't get map height. Reason: " &
            getCurrentExceptionMsg() & "}")
        return
    mapWidth: Positive = try:
        tclEval2(script = mapView & " cget -width").parseInt()
      except:
        tclEval(script = "bgerror {Can't get map width. Reason: " &
            getCurrentExceptionMsg() & "}")
        return
  var
    startX = centerX - (mapWidth / 2).int
    startY = centerY - (mapHeight / 2).int
    endY = centerY + (mapHeight / 2).int
    endX = centerX + (mapWidth / 2).int
    storyX = 1
    storyY = 1
  if currentStory.index.len > 0:
    (storyX, storyY) = try:
        getStoryLocation()
      except:
        tclEval(script = "bgerror {Can't the current story location. Reason: " &
            getCurrentExceptionMsg() & "}")
        return
    if storyX == playerShip.skyX and storyY == playerShip.skyY:
      storyX = 0
      storyY = 0
  if playerShip.speed == docked and skyMap[playerShip.skyX][
      playerShip.skyY].baseIndex == 0:
    playerShip.speed = fullStop
  let currentTheme = try:
        themesList[gameSettings.interfaceTheme]
      except:
        tclEval(script = "bgerror {Can't the current game's theme. Reason: " &
            getCurrentExceptionMsg() & "}")
        return
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
              mapChar = try:
                  factionsList[skyBases[skyMap[x][
                      y].baseIndex].owner].baseIcon.Rune.toUTF8
                except:
                  tclEval(script = "bgerror {Can't the base icon. Reason: " &
                      getCurrentExceptionMsg() & "}")
                  return
              mapTag = skyBases[skyMap[x][y].baseIndex].baseType
            else:
              mapTag = "unvisited"
          else:
            mapTag = "unvisited gray"
      if preview:
        for mission in skyBases[skyMap[playerShip.skyX][
            playerShip.skyY].baseIndex].missions:
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
      tclEval(script = mapView & " insert end {" & mapChar & "} [list " &
          mapTag & "]")
    if y < endY:
      tclEval(script = mapView & " insert end {\n}")
  tclEval(script = mapView & " configure -state disable")

proc drawMapCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults {.sideEffect, raises: [], tags: [].} =
  ## Draw the sky map
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## DrawMap
  let mapView = mainPaned & ".mapframe.map"
  try:
    discard tclEval(script = mapView & " configure -width [expr [winfo width $mapview] / [font measure MapFont {" &
        themesList[gameSettings.interfaceTheme].emptyMapIcon & "}]]")
  except:
    tclEval(script = "bgerror {Can't set map width. Reason: " &
        getCurrentExceptionMsg() & "}")
    return tclOk
  tclEval(script = mapView & " configure -height [expr [winfo height $mapview] / [font metrics MapFont -linespace]]")
  if tclGetVar(varName = "refreshmap") == "1":
    drawMap()
  return tclOk

proc updateMapInfo*(x: Positive = playerShip.skyX;
    y: Positive = playerShip.skyY) {.sideEffect, raises: [], tags: [].} =
  ## Update frame with information about the map cell on which the player
  ## currently points.
  ##
  ## * x - the X coordinate of the map's cell
  ## * y - the Y coordinate of the map's cell
  let mapInfo = mainPaned & ".mapframe.info"
  tclEval(script = mapInfo & " configure -state normal")
  tclEval(script = mapInfo & " delete 1.0 end")
  var width = 1

  proc insertText(newText: string; tagName: string = "") =
    if newText.len > width:
      width = newText.len
    if width > 21:
      width = 21
    tclEval(script = mapInfo & " insert end {" & newText & "}" & (
        if tagName.len == 0: "" else: " [list " & tagName & "]"))

  insertText(newText = "X:")
  insertText(newText = " " & $x, tagName = "yellow2")
  insertText(newText = " Y:")
  insertText(newText = " " & $y, tagName = "yellow2")
  if playerShip.skyX != x or playerShip.skyY != y:
    let
      distance = countDistance(destinationX = x, destinationY = y)
      travelValues = travelInfo(distance = distance)
    insertText(newText = "\nDistance: ")
    insertText(newText = $distance, tagName = "yellow2")
    if travelValues[1] > 0:
      insertText(newText = "\nETA:")
      var distanceText = ""
      minutesToDate(minutes = travelValues[1], infoText = distanceText)
      insertText(newText = distanceText, tagName = "yellow2")
      insertText(newText = "\nApprox fuel usage: ")
      insertText(newText = $travelValues[2], tagName = "yellow2")
  if skyMap[x][y].baseIndex > 0:
    let baseIndex = skyMap[x][y].baseIndex
    if skyBases[baseIndex].known:
      insertText(newText = "\nBase info:", tagName = "pink underline")
      insertText(newText = "\nName: ")
      insertText(newText = skyBases[baseIndex].name, tagName = "yellow2")
    if skyBases[baseIndex].visited.year > 0:
      try:
        discard tclEval(script = mapInfo &
            " tag configure basetype -foreground #" & basesTypesList[skyBases[
            baseIndex].baseType].color)
      except:
        tclEval(script = "bgerror {Can't get the color of the base's type. Reason: " &
            getCurrentExceptionMsg() & "}")
        return
      insertText(newText = "\nType: ")
      try:
        insertText(newText = basesTypesList[skyBases[baseIndex].baseType].name,
            tagName = "basetype")
      except:
        tclEval(script = "bgerror {Can't get the name of the base's type. Reason: " &
            getCurrentExceptionMsg() & "}")
        return
      if skyBases[baseIndex].population > 0:
        insertText(newText = "\nPopulation: ")
      if skyBases[baseIndex].population > 0 and skyBases[baseIndex].population < 150:
        insertText(newText = "small", tagName = "yellow2")
      elif skyBases[baseIndex].population > 149 and skyBases[
          baseIndex].population < 300:
        insertText(newText = "medium", tagName = "yellow2")
      elif skyBases[baseIndex].population > 299:
        insertText(newText = "large", tagName = "yellow2")
      insertText(newText = "\nSize: ")
      insertText(newText = $skyBases[baseIndex].size & "\n",
          tagName = "yellow2")
      if skyBases[baseIndex].population > 0:
        insertText(newText = "Owner: ")
        try:
          insertText(newText = factionsList[skyBases[baseIndex].owner].name,
              tagName = "yellow2")
        except:
          tclEval(script = "bgerror {Can't get the name of the owner's faction. Reason: " &
              getCurrentExceptionMsg() & "}")
          return
      else:
        insertText(newText = "Base is abandoned")
      if skyBases[baseIndex].population > 0:
        var
          baseInfoText = "\n"
          color = ""
        case skyBases[baseIndex].reputation.level
        of -100 .. -75:
          baseInfoText = baseInfoText & "You are hated here"
          color = "red"
        of -74 .. -50:
          baseInfoText = baseInfoText & "You are outlawed here"
          color = "red"
        of -49 .. -25:
          baseInfoText = baseInfoText & "You are disliked here"
          color = "red"
        of -24 .. -1:
          baseInfoText = baseInfoText & "They are unfriendly to you"
          color = "red"
        of 0:
          baseInfoText = baseInfoText & "You are unknown here"
        of 1 .. 25:
          baseInfoText = baseInfoText & "You are know here as visitor"
          color = "green"
        of 26 .. 50:
          baseInfoText = baseInfoText & "You are know here as trader"
          color = "green"
        of 51 .. 75:
          baseInfoText = baseInfoText & "You are know here as friend"
          color = "green"
        of 76 .. 100:
          baseInfoText = baseInfoText & "You are well known here"
          color = "green"
        insertText(newText = baseInfoText, tagName = color)
      if baseIndex == playerShip.homeBase:
        insertText(newText = "\nIt is your home base", tagName = "cyan")
  if skyMap[x][y].missionIndex > -1:
    var missionInfoText = "\n"
    if skyMap[x][y].baseIndex > 0 or skyMap[x][y].eventIndex > -1:
      missionInfoText = missionInfoText & "\n"
    let missionIndex = skyMap[x][y].missionIndex
    case acceptedMissions[missionIndex].mType
    of deliver:
      try:
        missionInfoText = missionInfoText & "Deliver " & itemsList[
            acceptedMissions[missionIndex].itemIndex].name
      except:
        tclEval(script = "bgerror {Can't get the name of the item to deliver. Reason: " &
            getCurrentExceptionMsg() & "}")
        return
    of destroy:
      try:
        missionInfoText = missionInfoText & "Destroy " & protoShipsList[
            acceptedMissions[missionIndex].shipIndex].name
      except:
        tclEval(script = "bgerror {Can't get the name of the ship to destroy. Reason: " &
            getCurrentExceptionMsg() & "}")
        return
    of patrol:
      missionInfoText = missionInfoText & "Patrol area"
    of explore:
      missionInfoText = missionInfoText & "Explore area"
    of passenger:
      missionInfoText = missionInfoText & "Transport passenger"
    insertText(newText = missionInfoText)
  if currentStory.index.len > 0:
    var storyX, storyY: Natural = 1
    try:
      (storyX, storyY) = getStoryLocation()
    except:
      tclEval(script = "bgerror {Can't get the location of the current story. Reason: " &
          getCurrentExceptionMsg() & "}")
      return
    if storyX == playerShip.skyX and storyY == playerShip.skyY:
      storyX = 0
      storyY = 0
    var finishCondition: StepConditionType = any
    if y == storyX and y == storyY:
      try:
        finishCondition = (if currentStory.currentStep == 0: storiesList[
            currentStory.index].startingStep.finishCondition elif currentStory.currentStep >
            0: storiesList[currentStory.index].steps[
            currentStory.currentStep].finishCondition else: storiesList[
            currentStory.index].finalStep.finishCondition)
        if finishCondition in {askInBase, destroyShip, explore}:
          insertText(newText = "\nStory leads you here")
      except:
        tclEval(script = "bgerror {Can't get the finish condition of the current story. Reason: " &
            getCurrentExceptionMsg() & "}")
        return
  if x == playerShip.skyX and y == playerShip.skyY:
    insertText(newText = "\nYou are here", tagName = "yellow")
  if skyMap[x][y].eventIndex > -1:
    let eventIndex = skyMap[x][y].eventIndex
    var eventInfoText = ""
    if eventsList[eventIndex].eType notin {baseRecovery, EventsTypes.none}:
      eventInfoText = "\n\n"
    var color = ""
    case eventsList[eventIndex].eType
    of trader:
      try:
        eventInfoText = eventInfoText & protoShipsList[eventsList[
            eventIndex].shipIndex].name
      except:
        tclEval(script = "bgerror {Can't get the name of the ship for the event. Reason: " &
            getCurrentExceptionMsg() & "}")
        return
      color = "green"
    of friendlyShip:
      try:
        eventInfoText = eventInfoText & protoShipsList[eventsList[
            eventIndex].shipIndex].name
      except:
        tclEval(script = "bgerror {Can't get the name of the ship for the event. Reason: " &
            getCurrentExceptionMsg() & "}")
        return
      color = "green2"
    of enemyShip:
      try:
        eventInfoText = eventInfoText & protoShipsList[eventsList[
            eventIndex].shipIndex].name
      except:
        tclEval(script = "bgerror {Can't get the name of the ship for the event. Reason: " &
            getCurrentExceptionMsg() & "}")
        return
      color = "red"
    of fullDocks:
      eventInfoText = eventInfoText & "Full docks in base"
      color = "cyan"
    of attackOnBase:
      eventInfoText = eventInfoText & "Base is under attack"
      color = "red"
    of disease:
      eventInfoText = eventInfoText & "Disease in base"
      color = "yellow"
    of enemyPatrol:
      eventInfoText = eventInfoText & "Enemy patrol"
      color = "red3"
    of doublePrice:
      try:
        eventInfoText = eventInfoText & itemsList[eventsList[
            eventIndex].itemIndex].name
      except:
        tclEval(script = "bgerror {Can't get the name of the item for the event. Reason: " &
            getCurrentExceptionMsg() & "}")
        return
      color = "lime"
    of EventsTypes.none, baseRecovery:
      discard
    insertText(newText = eventInfoText, tagName = color)
  tclEval(script = mapInfo & " configure -state disabled -width " & $width &
      " -height " & tclEval2(script = mapInfo & " count -displaylines 0.0 end"))

var mapX, mapY = 0

proc updateMapInfoCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults {.sideEffect, raises: [], tags: [].} =
  ## Update the information about the selected map's cell
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## UpdateMapInfo x y
  ## X and Y are coordinates of the map cell which info will be show
  let
    mapView = mainPaned & ".mapframe.map"
    mapIndex = tclEval2(script = mapView & " index @" & $argv[1] & "," & $argv[2])
  try:
    if startY + (mapIndex[0 .. mapIndex.find(".") - 1]).parseInt - 1 < 1:
      return tclOk
    mapY = startY + (mapIndex[0 .. mapIndex.find(".") - 1]).parseInt - 1
    if mapY > 1_024:
      return tclOk
  except:
    tclEval(script = "bgerror {Can't set map Y coordinate. Reason: " &
        getCurrentExceptionMsg() & "}")
    return tclOk
  try:
    if startX + (mapIndex[mapIndex.find(".") + 1 .. ^1]).parseInt < 1:
      return tclOk
    mapX = startX + (mapIndex[mapIndex.find(".") + 1 .. ^1]).parseInt
    if mapX > 1_024:
      return tclOk
  except:
    tclEval(script = "bgerror {Can't set map X coordinate. Reason: " &
        getCurrentExceptionMsg() & "}")
    return tclOk
  updateMapInfo(x = mapX, y = mapY)
  return tclOk

proc showDestinationMenuCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults {.sideEffect, raises: [], tags: [].} =
  ## Create and show the destination menu dialog
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ShowDestinationMenu x y
  ## X and Y are the map coordinates for which the destination menu will be show
  if mapX == 0 or mapY == 0 and updateMapInfoCommand(clientData = clientData,
      interp = interp, argc = argc, argv = argv) != tclOk:
    return tclError
  let destinationDialog = createDialog(name = ".gameframe.destinationmenu",
      title = "Set destination", parentName = ".gameframe")
  if playerShip.skyX == mapX and playerShip.skyY == mapY:
    tclEval(script = "CloseDialog " & destinationDialog)
    return showOrdersCommand(clientData = clientData, interp = interp,
        argc = argc, argv = argv)
  var button = destinationDialog & ".set"
  tclEval(script = "ttk::button " & button &
      " -text {Set destination} -command {SetDestination;CloseDialog " &
      destinationDialog & "}")
  tclEval(script = "grid " & button & " -sticky we -padx 5")
  let dialogCloseButton = destinationDialog & ".button"
  tclEval(script = "ttk::button " & dialogCloseButton &
      " -text Close -command {CloseDialog " & destinationDialog & "}")
  tclEval(script = "bind " & button & " <Escape> {" & dialogCloseButton & " invoke;break}")
  if playerShip.speed != docked:
    tclEval(script = "bind " & button & " <Tab> {focus " & destinationDialog & ".setandmove;break}")
    button = destinationDialog & ".setandmove"
    tclEval(script = "ttk::button " & button &
        " -text {Set destination and move} -command {SetDestination;MoveShip moveto;CloseDialog " &
        destinationDialog & "}")
    tclEval(script = "grid " & button & " -sticky we -padx 5")
    tclEval(script = "bind " & button & " <Escape> {" & dialogCloseButton & " invoke;break}")
    if playerShip.destinationX > 0 and playerShip.destinationY > 0:
      tclEval(script = "bind " & button & " <Tab> {focus " & destinationDialog & ".move;break}")
      button = destinationDialog & ".move"
      tclEval(script = "ttk::button " & button &
          " -text {Move to} -command {MoveShip moveto;CloseDialog " &
          destinationDialog & "}")
      tclEval(script = "grid " & button & " -sticky we -padx 5")
      tclEval(script = "bind " & button & " <Escape> {" & dialogCloseButton & " invoke;break}")
      tclEval(script = "bind " & button & " <Tab> {focus " & destinationDialog & ".button;break}")
  tclEval(script = "grid " & dialogCloseButton & " -sticky we -padx 5 -pady {0 5}")
  tclEval(script = "bind " & dialogCloseButton & " <Tab> {focus " &
      destinationDialog & ".set;break}")
  tclEval(script = "bind " & dialogCloseButton & " <Escape> {" &
      dialogCloseButton & " invoke;break}")
  showDialog(dialog = destinationDialog, parentFrame = ".gameframe")
  return tclOk

proc setShipDestinationCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults {.sideEffect, raises: [], tags: [].} =
  ## Set the current map cell as the destination for the player's ship
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## SetDestination
  playerShip.destinationX = mapX
  playerShip.destinationY = mapY
  addMessage(message = "You set the travel destination for your ship.",
      mType = orderMessage)
  if gameSettings.autoCenter:
    centerX = playerShip.skyX
    centerY = playerShip.skyY
  drawMap()
  updateMoveButtons()
  return tclOk

proc moveMapCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults {.sideEffect, raises: [], tags: [].} =
  ## Move the map in the selected direction
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## MoveMap direction
  ## Direction in which the map will be moved
  let mapView = mainPaned & ".mapframe.map"
  if tclEval2(script = "winfo ismapped " & mapView) == "0":
    return tclOk
  let
    mapHeight = try:
        tclEval2(script = mapView & " cget -height").parseInt
      except:
        tclEval(script = "bgerror {Can't get the map's height. Reason: " &
            getCurrentExceptionMsg() & "}")
        return tclOk
    mapWidth = try:
        tclEval2(script = mapView & " cget -width").parseInt
      except:
        tclEval(script = "bgerror {Can't get the map's width. Reason: " &
            getCurrentExceptionMsg() & "}")
        return tclOk
    dialogName = ".gameframe.movemapdialog"
  if argv[1] == "centeronship":
    centerX = playerShip.skyX
    centerY = playerShip.skyY
  elif argv[1] == "movemapto":
    var spinBox = dialogName & ".x"
    centerX = try:
        tclEval2(script = spinBox & " get").parseInt
      except:
        tclEval(script = "bgerror {Can't set center X. Reason: " &
            getCurrentExceptionMsg() & "}")
        return tclOk
    spinBox = dialogName & ".y"
    centerY = try:
        tclEval2(script = spinBox & " get").parseInt
      except:
        tclEval(script = "bgerror {Can't set center Y. Reason: " &
            getCurrentExceptionMsg() & "}")
        return tclOk
  elif argv[1] == "n":
    centerY = (if centerY - (mapHeight / 3).int < 1: (mapHeight /
        3).int else: centerY - (mapHeight / 3).int)
  elif argv[1] == "s":
    centerY = (if centerY + (mapHeight / 3).int > 1_024: (mapHeight /
        3).int else: centerY + (mapHeight / 3).int)
  elif argv[1] == "w":
    centerX = (if centerX - (mapWidth / 3).int < 1: (mapWidth /
        3).int else: centerX - (mapWidth / 3).int)
  elif argv[1] == "e":
    centerX = (if centerX + (mapWidth / 3).int > 1_024: (mapWidth /
        3).int else: centerX + (mapWidth / 3).int)
  elif argv[1] == "nw":
    centerY = (if centerY - (mapHeight / 3).int < 1: (mapHeight /
        3).int else: centerY - (mapHeight / 3).int)
    centerX = (if centerX - (mapWidth / 3).int < 1: (mapWidth /
        3).int else: centerX - (mapWidth / 3).int)
  elif argv[1] == "ne":
    centerY = (if centerY - (mapHeight / 3).int < 1: (mapHeight /
        3).int else: centerY - (mapHeight / 3).int)
    centerX = (if centerX + (mapWidth / 3).int > 1_024: (mapWidth /
        3).int else: centerX + (mapWidth / 3).int)
  elif argv[1] == "sw":
    centerY = (if centerY + (mapHeight / 3).int > 1_024: (mapHeight /
        3).int else: centerY + (mapHeight / 3).int)
    centerX = (if centerX - (mapWidth / 3).int < 1: (mapWidth /
        3).int else: centerX - (mapWidth / 3).int)
  elif argv[1] == "se":
    centerY = (if centerY + (mapHeight / 3).int > 1_024: (mapHeight /
        3).int else: centerY + (mapHeight / 3).int)
    centerX = (if centerX + (mapWidth / 3).int > 1_024: (mapWidth /
        3).int else: centerX + (mapWidth / 3).int)
  elif argv[1] == "centeronhome":
    centerX = skyBases[playerShip.homeBase].skyX
    centerY = skyBases[playerShip.homeBase].skyY
  drawMap()
  return tclOk

proc zoomMapCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults {.sideEffect, raises: [], tags: [].} =
  ## Zoom in or our the sky map
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ZoomMap
  gameSettings.mapFontSize = (if argv[1] == "raise": gameSettings.mapFontSize +
      1 else: gameSettings.mapFontSize - 1)
  if gameSettings.mapFontSize < 3:
    gameSettings.mapFontSize = 3
  elif gameSettings.mapFontSize > 50:
    gameSettings.mapFontSize = 50
  tclEval(script = "font configure MapFont -size " & $gameSettings.mapFontSize)
  tclSetVar(varName = "refreshmap", newValue = "1")
  return drawMapCommand(clientData = clientData, interp = interp, argc = argc, argv = argv)

proc moveShipCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults =
  var
    res = 0
    message = ""
    newX, newY = 0
    startsCombat = false

  proc updateCoordinates() =
    if playerShip.destinationX > playerShip.skyX:
      newX = 1
    elif playerShip.destinationX < playerShip.skyX:
      newX = -1
    if playerShip.destinationY > playerShip.skyY:
      newY = 1
    elif playerShip.destinationY < playerShip.skyY:
      newY = -1

  if argv[1] == "n":
    res = moveShip(x = 0, y = -1, message = message)
  elif argv[1] == "s":
    res = moveShip(x = 0, y = 1, message = message)
  elif argv[1] == "e":
    res = moveShip(x = 1, y = 0, message = message)
  elif argv[1] == "w":
    res = moveShip(x = -1, y = 0, message = message)
  elif argv[1] == "sw":
    res = moveShip(x = -1, y = 1, message = message)
  elif argv[1] == "se":
    res = moveShip(x = 1, y = 1, message = message)
  elif argv[1] == "nw":
    res = moveShip(x = -1, y = -1, message = message)
  elif argv[1] == "ne":
    res = moveShip(x = 1, y = -1, message = message)
  elif argv[1] == "waitormove":
    if playerShip.destinationX == 0 and playerShip.destinationY == 0:
      res = 1
      updateGame(minutes = gameSettings.waitMinutes)
      waitInPlace(minutes = gameSettings.waitMinutes)
    else:
      updateCoordinates()
      res = moveShip(x = newX, y = newY, message = message)
      if playerShip.destinationX == playerShip.skyX and
          playerShip.destinationY == playerShip.skyY:
        addMessage(message = "You reached your travel destination.",
            mType = orderMessage)
        playerShip.destinationX = 0
        playerShip.destinationY = 0
        if gameSettings.autoFinish:
          message = autoFinishMissions()
        res = 4
  elif argv[1] == "moveto":
    while true:
      newX = 0
      newY = 0
      updateCoordinates()
      res = moveShip(x = newX, y = newY, message = message)
      if res == 0:
        break
      startsCombat = checkForEvent()
      if startsCombat:
        res = 4
        break
      if res == 8:
        waitForRest()
        if "sentientships" notin factionsList[playerShip.crew[
            0].faction].flags and (findMember(order = pilot) == -1 or
            findMember(order = engineer) == 0):
          waitForRest()
        res = 1
        startsCombat = checkForEvent()
        if startsCombat:
          res = 4
          break
      if gameSettings.autoMoveStop != never and skyMap[playerShip.skyX][
          playerShip.skyY].eventIndex > -1:
        let eventIndex = skyMap[playerShip.skyX][playerShip.skyY].eventIndex
        case gameSettings.autoMoveStop
        of any:
          if eventsList[eventIndex].eType in {enemyShip, trader, friendlyShip, enemyPatrol}:
            res = 0
            break
        of friendly:
          if eventsList[eventIndex].eType in {trader, friendlyShip}:
            res = 0
            break
        of enemy:
          if eventsList[eventIndex].eType in {enemyShip, enemyPatrol}:
            res = 0
            break
        of never:
          discard
      let messageDialog = ".message"
      if tclEval2(script = "winfo exists " & messageDialog) == "0":
        if getItemAmount(itemType = fuelType) <= gameSettings.lowFuel:
          showMessage(text = "Your fuel level is dangerously low.", title = "Low fuel level")
          res = 4
          break
        elif getItemsAmount(iType = "Food") <= gameSettings.lowFood:
          showMessage(text = "Your food level is dangerously low.", title = "Low food level")
          res = 4
          break
        elif getItemsAmount(iType = "Drinks") <= gameSettings.lowDrinks:
          showMessage(text = "Your drinks level is dangerously low.", title = "Low drinks level")
          res = 4
          break
      if playerShip.destinationX == playerShip.skyX and playerShip.destinationY == playerShip.skyY:
        addMessage("You reached your travel destination.", mType = orderMessage)
        playerShip.destinationX = 0
        playerShip.destinationY = 0
        if gameSettings.autoFinish:
          message = autoFinishMissions()
        res = 4
        break
      if res in 6 .. 7:
        break
  case res
  # Ship moved, check for events
  of 1:
    startsCombat = checkForEvent()
    if not startsCombat and gameSettings.autoFinish:
      message = autoFinishMissions()
  # Ship moved, but pilot needs rest, confirm
  of 6:
    showQuestion(question = "You don't have pilot on duty. Do you want to wait until your pilot rest?", res = "nopilot")
    return tclOk
  # Ship moved, but engineer needs rest, confirm
  of 7:
    showQuestion(question = "You don't have engineer on duty. Do you want to wait until your pilot rest?", res = "nopilot")
    return tclOk
  else:
    discard
  return tclOk

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
    addCommand("DrawMap", drawMapCommand)
    addCommand("UpdateMapInfo", updateMapInfoCommand)
    addCommand("ShowDestinationMenu", showDestinationMenuCommand)
    addCommand("SetDestination", setShipDestinationCommand)
    addCommand("MoveMap", moveMapCommand)
    addCommand("ZoomMap", zoomMapCommand)
    addCommand("MoveShip", moveShipCommand)

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

proc updateAdaMapInfo(x, y: cint) {.raises: [], tags: [], exportc.} =
  try:
    updateMapInfo(x = x.Positive, y = y.Positive)
  except:
    discard

proc getAdaStartPoint(x, y: var cint) {.raises: [], tags: [], exportc.} =
  x = startX.cint
  y = startY.cint

proc setAdaStartPoint(x, y: cint) {.raises: [], tags: [], exportc.} =
  startX = x.Positive
  startY = y.Positive
