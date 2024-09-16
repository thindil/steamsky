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
import ../[basestypes, config, game, log, maps, missions, statistics, stories, tk, types]
import coreui, dialogs, errordialog, themes, updateheader, utilsui2

var
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
      "Control-c", "Control-d"]         ## The keyboard shortcuts used on the map
  fullScreenAccel* = "Control-f"        ## Keyboard shortcut for toggle full screen
  defaultFontSizes*: array[3, Positive] ## The default sizes of fonts

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
    tclEval(script = "grid configure " & button & " -columnspan 3 -column 0 -row 1")
    for buttonName in moveButtonsNames:
      button = frameName & "." & buttonName
      tclEval(script = "grid remove " & button)
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
    tclEval(script = "grid configure " & button & " -columnspan 1 -column 1 -row 2")
    for index, name in moveButtonsNames:
      button = frameName & "." & name
      tclEval(script = "grid " & button)
  button = frameName & ".box.orders"
  if skyMap[playerShip.skyX][playerShip.skyY].eventIndex == -1 and skyMap[
      playerShip.skyX][playerShip.skyY].baseIndex == 0:
    tclEval(script = "grid remove " & button)
  else:
    tclEval(script = "grid " & button)

proc finishStory*() {.raises: [], tags: [WriteIOEffect, TimeEffect], exportc.} =
  ## Finish the current player's story. Give experience and ask about
  ## finishing the game
  gameStats.points = gameStats.points + (10_000 * currentStory.maxSteps)
  clearCurrentStory()
  try:
    showQuestion(question = storiesList[currentStory.index].endText &
        " Do you want to finish the game?", res = "retire")
  except KeyError:
    showError(message = "Can't get the end text of the current story. Result: " &
        tclGetResult2())

proc showSkyMap*(clear: bool = false) {.sideEffect, raises: [], tags: [WriteIOEffect, TimeEffect].} =
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
        showError(message = "Can't show the story text.")
    else:
      finishStory()
      if playerShip.crew[0].health == 0:
        showQuestion(question = "You are dead. Would you like to see your game statistics?",
            res = "showstats")
    currentStory.showText = true

proc drawMap*() {.sideEffect, raises: [], tags: [WriteIOEffect, TimeEffect].} =
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
        showError(message = "Can't get map height.")
        return
    mapWidth: Positive = try:
        tclEval2(script = mapView & " cget -width").parseInt()
      except:
        showError(message = "Can't get map width.")
        return
  startX = centerX - (mapWidth / 2).int
  startY = centerY - (mapHeight / 2).int
  var
    endY = centerY + (mapHeight / 2).int
    endX = centerX + (mapWidth / 2).int
    storyX = 1
    storyY = 1
  if startY < 1:
    startY = 1
    endY = mapHeight + 1
  if startX < 1:
    startX = 1
    endX = mapWidth + 1
  if endY > 1_024:
    endY = 1_024
    startY = 1_024 - mapHeight
  if endX > 1_024:
    endX = 1_024
    startX = 1_025 - mapWidth
  if currentStory.index.len > 0:
    (storyX, storyY) = try:
        getStoryLocation()
      except:
        showError(message = "Can't get the current story location.")
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
        showError(message = "Can't get the curernt game's theme.")
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
                  showError(message = "Can't get the base icon.")
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

proc updateMapInfo*(x: Positive = playerShip.skyX;
    y: Positive = playerShip.skyY) {.sideEffect, raises: [], tags: [WriteIOEffect, TimeEffect].} =
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
        showError(message = "Can't get the color of the base's type.")
        return
      insertText(newText = "\nType: ")
      try:
        insertText(newText = basesTypesList[skyBases[baseIndex].baseType].name,
            tagName = "basetype")
      except:
        showError(message = "Can't get the name of the base's type.")
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
          showError(message = "Can't get the name of the owner's faction.")
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
        showError(message = "Can't get the name of the item to deliver.")
        return
    of destroy:
      try:
        missionInfoText = missionInfoText & "Destroy " & protoShipsList[
            acceptedMissions[missionIndex].shipIndex].name
      except:
        showError(message = "Can't get the name of the ship to destroy.")
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
      showError(message = "Can't get the location of the current story.")
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
        showError(message = "Can't get the finish condition of the current story.")
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
        showError(message = "Can't get the name of the trader's ship for the event.")
        return
      color = "green"
    of friendlyShip:
      try:
        eventInfoText = eventInfoText & protoShipsList[eventsList[
            eventIndex].shipIndex].name
      except:
        showError(message = "Can't get the name of the friendly ship for the event.")
        return
      color = "green2"
    of enemyShip:
      try:
        eventInfoText = eventInfoText & protoShipsList[eventsList[
            eventIndex].shipIndex].name
      except:
        showError(message = "Can't get the name of the enemy's ship for the event.")
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
        eventInfoText = eventInfoText & "Double price for " & itemsList[
            eventsList[eventIndex].itemIndex].name
      except:
        showError(message = "Can't get the name of the item for the event.")
        return
      color = "lime"
    of EventsTypes.none, baseRecovery:
      discard
    insertText(newText = eventInfoText, tagName = color)
  tclEval(script = mapInfo & " configure -state disabled -width " & $width &
      " -height " & tclEval2(script = mapInfo & " count -displaylines 0.0 end"))

proc setKeys*() {.sideEffect, raises: [], tags: [].} =
  ## Set the keyboard shortcuts for the map
  const tclCommandsArray: array[37, string] = [
    "{if {[winfo class [focus]] != {TEntry} && [tk busy status " & gameHeader &
      "] == 0} {ShowGameMenu}}", "{" & mainPaned &
      ".mapframe.buttons.wait invoke}", "{ZoomMap raise}", "{ZoomMap lower}",
      "{InvokeButton $bframe.nw}", "{InvokeButton $bframe.n}",
      "{InvokeButton $bframe.ne}", "{InvokeButton $bframe.w}",
      "{InvokeButton $bframe.wait}", "{InvokeButton $bframe.e}",
      "{InvokeButton $bframe.sw}", "{InvokeButton $bframe.s}",
      "{InvokeButton $bframe.se}", "{InvokeButton $bframe.box.moveto}",
      "{MoveMap centeronship}", "{MoveMap centeronhome}", "{MoveMap nw}",
      "{MoveMap n}", "{MoveMap ne}", "{MoveMap w}", "{MoveMap e}",
      "{MoveMap sw}", "{MoveMap s}", "{MoveMap se}", "{MoveCursor nw %x %y}",
      "{MoveCursor n %x %y}", "{MoveCursor ne %x %y}", "{MoveCursor w %x %y}",
      "{MoveCursor e %x %y}", "{MoveCursor sw %x %y}", "{MoveCursor s %x %y}",
      "{MoveCursor se %x %y}", "{MoveCursor click %x %y}", "{" & mainPaned &
      ".controls.buttons.box.speed current 0}", "{" & mainPaned &
      ".controls.buttons.box.speed current 1}", "{" & mainPaned &
      ".controls.buttons.box.speed current 2}", "{" & mainPaned & ".controls.buttons.box.speed current 3}"]
  for index, command in tclCommandsArray:
    var
      pos = mapAccelerators[index + 1].rfind(sub = '-')
      keyName = ""
    if pos > -1:
      keyName = mapAccelerators[index + 1][0 .. pos] & "KeyPress-" &
          mapAccelerators[index + 1][pos + 1 .. ^1]
    else:
      keyName = "KeyPress-" & mapAccelerators[index + 1]
    tclEval(script = "bind . <" & keyName & "> " & command)
  var
    pos = fullScreenAccel.rfind(sub = '-')
    keyName = ""
  if pos > -1:
    keyName = fullScreenAccel[0 .. pos] & "KeyPress-" & fullScreenAccel[pos +
        1 .. ^1]
  else:
    keyName = "KeyPress-" & fullScreenAccel
  tclEval(script = "bind . <" & keyName & "> {ToggleFullScreen}")

import basesui, baseslootui, basesrecruitui, basesschoolui, basesshipyardui,
    craftsui, debugui, gameoptions, helpui, knowledge, mapsuicommands,
    messagesui, missionsui, ordersmenu, shipsui, statisticsui, tradesui, waitmenu

proc createGameUi*() {.sideEffect, raises: [], tags: [WriteIOEffect, TimeEffect, ReadIOEffect, RootEffect], exportc.} =
  ## Create the game UI and show sky map to the player
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
      try:
        parser.open(configFile, fileName)
      except:
        showError(message = "Can't open the shortcut's configuration file.")
        return
      while true:
        var entry = try:
            parser.next
          except:
            showError(message = "Can't get next shortcut setting.")
            return
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
            showError(message = "Can't set keyboard shortcuts. Message: " & entry.msg)
      try:
        parser.close()
      except:
        showError(message = "Can't close the shortcuts' configuration file.")
        return
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
#    mapsuicommands.addCommands()
    tclEvalFile(fileName = dataDirectory & "ui" & DirSep & "game.tcl")
    setTheme()
#    ordersmenu.addCommands()
#    waitmenu.addCommands()
#    helpui.addCommands()
#    shipsui.addCommands()
#    craftsui.addCommands()
#    messagesui.addCommands()
#    gameoptions.addCommands()
#    tradesui.addCommands()
#    basesschoolui.addCommands()
#    basesrecruitui.addCommands()
#    basesui.addCommands()
#    basesshipyardui.addCommands()
#    baseslootui.addCommands()
#    knowledge.addCommands()
#    missionsui.addCommands()
#    statisticsui.addCommands()
    let messagesFrame = paned & ".controls.messages"
    tclEval(script = "bind " & messagesFrame & " <Configure> {ResizeLastMessages}")
    tclEval(script = "bind " & mapView & " <Configure> {DrawMap}")
    tclEval(script = "bind " & mapView & " <Motion> {UpdateMapInfo %x %y}")
    tclEval(script = "bind " & mapView & " <Button-" & (
        if gameSettings.rightButton: "3" else: "1") & "> {ShowDestinationMenu %X %Y}")
    tclEval(script = "bind " & mapView & " <MouseWheel> {if {%D > 0} {ZoomMap raise} else {ZoomMap lower}}")
    tclEval(script = "bind " & mapView & " <Button-4> {ZoomMap raise}")
    tclEval(script = "bind " & mapView & " <Button-5> {ZoomMap lower}")
    setKeys()
#    if debugMode == menu:
#      showDebugUi()
  else:
    tclEval(script = "pack " & gameFrame & " -fill both -expand true")
  tclSetVar(varName = "refreshmap", newValue = "1")
  tclEval(script = "wm title . {Steam Sky}")
  if gameSettings.fullScreen:
    tclEval(script = "wm attributes . -fullscreen 1")
  for accel in menuAccelerators:
    let pos = accel.rfind(sub = '-')
    tclEval(script = "bind . <" & accel[0..pos] & "KeyPress-" &
      accel[pos + 1..^1] & "> {InvokeMenu " & accel & "}")
  if not tclEval2(script = "grid slaves .").contains(sub = ".gameframe.header"):
    let header = gameFrame & ".header"
    tclEval(script = "grid " & header)
  updateHeader()
  centerX = playerShip.skyX
  centerY = playerShip.skyY
  for baseType in basesTypesList.values:
    tclEval(script = mapView & " tag configure " & baseType.name &
        " -foreground #" & baseType.color)
  let panedPosition = (if gameSettings.windowHeight -
      gameSettings.messagesPosition <
      0: gameSettings.windowHeight else: gameSettings.windowHeight -
      gameSettings.messagesPosition)
  tclEval(script = paned & " sashpos 0 " & $panedPosition)
  if not tclEval2(script = "grid slaves .").contains(sub = ".gameframe.paned"):
    tclEval(script = "grid " & paned)
  tclEval(script = "update")
  let button = paned & ".mapframe.buttons.hide"
  tclEval(script = button & " invoke")
  tclEval(script = "bind . <Escape> {InvokeButton " & closeButton & "}")
  updateMessages()
  if not newStart:
    tclEval(script = "DrawMap")
  updateMoveButtons()
  updateMapInfo()
  if not gameSettings.showLastMessages:
    let messagesFrame = paned & ".controls.messages"
    tclEval(script = "grid remove " & messagesFrame)
  tclSetVar(varName = "shipname", newValue = playerShip.name)
  tclSetVar(varName = "gamestate", newValue = "general")
  if tclEval2(script = "winfo ismapped " & closeButton) == "1":
    showSkyMap(clear = true)
    tclEval(script = "grid remove " & closeButton)

# Temporary code for interfacing with Ada

proc updateAdaHeader() {.raises: [], tags: [WriteIOEffect, TimeEffect], exportc.} =
  try:
    updateHeader()
  except:
    discard

proc updateAdaMoveButtons() {.raises: [], tags: [], exportc.} =
  updateMoveButtons()

proc finishAdaStory() {.raises: [], tags: [WriteIOEffect, TimeEffect], exportc.} =
  try:
    finishStory()
  except:
    discard

proc showAdaSkyMap(clear: cint) {.raises: [], tags: [WriteIOEffect, TimeEffect], exportc.} =
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

proc drawAdaMap() {.raises: [], tags: [WriteIOEffect, TimeEffect], exportc.} =
  try:
    drawMap()
  except:
    discard

proc updateAdaMapInfo(x, y: cint) {.raises: [], tags: [WriteIOEffect, TimeEffect], exportc.} =
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

proc getAdaFontSizes(map, inter, help: cint) {.raises: [], tags: [], exportc.} =
  defaultFontSizes = [map.Positive, inter, help]

proc setAdaKeys() {.raises: [], tags: [], exportc.} =
  setKeys()
