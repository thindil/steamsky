# Copyright 2023-2026 Bartek thindil Jasicki
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

## Provides code related to showing the main game's map, like drawing the map,
## updating movement buttons, etc.

import std/[colors, os, parsecfg, streams, strutils, tables, unicode]
import contracts
import ../[bases, basestypes, config, game, log, maps, missions, statistics,
    stories, tk, types]
import coreui, dialogs, errordialog, themes, updateheader, utilsui2

var
  mapView: string = ".gameframe.paned.mapframe.map"
  menuAccelerators*: array[1..11, string] = ["s", "o", "r", "m", "k", "w",
      "g", "F1", "p", "q", "x"]
    ## The game menu keyboard shortcuts
  mapAccelerators*: array[1..37, string] = ["e", "v", "plus", "minus",
      "KP_Home", "KP_Up", "KP_Prior", "KP_Left", "KP_Begin", "KP_Right",
      "KP_End", "KP_Down", "KP_Next", "KP_Divide", "Shift-Return", "Shift-h",
      "Shift-KP_Home", "Shift-KP_Up", "Shift-KP_Prior", "Shift-KP_Left",
      "Shift-KP_Right", "Shift-KP_End", "Shift-KP_Down", "Shift-KP_Next",
      "Control-KP_Home", "Control-KP_Up", "Control-KP_Prior", "Control-KP_Left",
      "Control-KP_Right", "Control-KP_End", "Control-KP_Down",
      "Control-KP_Next", "Control-Return", "Control-a", "Control-b",
      "Control-c", "Control-d"] ## The keyboard shortcuts used on the map
  fullScreenAccel*: string = "Control-f" ## Keyboard shortcut for toggle full screen
  defaultFontSizes*: array[3, Positive] = [10, 10, 10] ## The default sizes of fonts

proc updateMoveButtons*() {.raises: [], tags: [], contractual.} =
  ## Update the player's ship movement buttons, depending on the state of the
  ## ship
  const
    moveButtonsNames: array[8, string] = ["nw", "n", "ne", "w", "e", "sw", "s", "se"]
    frameName: string = mainPaned & ".controls.buttons"
    speedBox: string = frameName & ".box.speed"
  var button: string = frameName & ".box.moveto"
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
      tclEval(script = "tooltip::tooltip " & button & " \"" & (
          if gameSettings.waitMinutes == 1: "Wait 1 minute." else: "Wait " &
          $gameSettings.waitMinutes & " minutes.") & "\"")
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

proc finishStory*() {.raises: [], tags: [WriteIOEffect, TimeEffect, RootEffect],
    contractual.} =
  ## Finish the current player's story. Give experience and ask about
  ## finishing the game
  gameStats.points += (10_000 * currentStory.maxSteps)
  clearCurrentStory()
  try:
    showQuestion(question = storiesList[currentStory.index].endText &
        " Do you want to finish the game?", res = "retire")
  except KeyError:
    showError(message = "Can't get the end text of the current story. Result: " &
        tclGetResult2())

proc showSkyMap*(clear: bool = false) {.raises: [], tags: [WriteIOEffect,
    TimeEffect, RootEffect], contractual.} =
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
    const speedBox: string = "$bframe.box.speed"
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

proc showMission(currentTheme: ThemeRecord; mType: MissionsTypes): tuple[icon,
    tag: string] {.raises: [], tags: [], contractual.} =
  ## Show the mission info on the map, based on the missions' type
  ##
  ## * mType - the type of the mission
  ##
  ## Returns the tuple with icon and text tag for the selected mission
  case mType
  of deliver:
    result.icon = currentTheme.deliverIcon
    result.tag = "yellow"
  of destroy:
    result.icon = currentTheme.destroyIcon
    result.tag = "red"
  of patrol:
    result.icon = currentTheme.patrolIcon
    result.tag = "lime"
  of explore:
    result.icon = currentTheme.exploreIcon
    result.tag = "green"
  of passenger:
    result.icon = currentTheme.passengerIcon
    result.tag = "cyan"

var preview: bool = false

proc drawMap*() {.raises: [], tags: [WriteIOEffect, TimeEffect, RootEffect],
    contractual.} =
  ## Draw the map on the screen
  preview = (if tclGetVar(varName = "mappreview").len > 0: true else: false)
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
    endY: int = centerY + (mapHeight / 2).int
    endX: int = centerX + (mapWidth / 2).int
    storyX: int = 1
    storyY: int = 1
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
  let currentTheme: ThemeRecord = try:
        themesList[gameSettings.interfaceTheme]
      except:
        showError(message = "Can't get the curernt game's theme.")
        return
  for y in startY..endY:
    for x in startX..endX:
      var mapTag, mapChar: string = ""
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
          (mapChar, mapTag) = showMission(currentTheme = currentTheme,
              mType = acceptedMissions[skyMap[x][y].missionIndex].mType)
          if not skyMap[x][y].visited:
            mapTag &= " unvisited"
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
              mapTag &= " unvisited"
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
            (mapChar, mapTag) = showMission(currentTheme = currentTheme,
                mType = mission.mType)
            if not skyMap[x][y].visited:
              mapTag &= " unvisited"
            break
      tclEval(script = mapView & " insert end {" & mapChar & "} [list " &
          mapTag & "]")
    if y < endY:
      tclEval(script = mapView & " insert end {\n}")
  tclEval(script = mapView & " configure -state disable")

proc updateMapInfo*(x: Positive = playerShip.skyX;
    y: Positive = playerShip.skyY) {.raises: [], tags: [WriteIOEffect,
        TimeEffect, RootEffect], contractual.} =
  ## Update frame with information about the map cell on which the player
  ## currently points.
  ##
  ## * x - the X coordinate of the map's cell
  ## * y - the Y coordinate of the map's cell
  const mapInfo: string = mainPaned & ".mapframe.info"
  tclEval(script = mapInfo & " configure -state normal")
  tclEval(script = mapInfo & " delete 1.0 end")
  var width: int = 1

  proc insertText(newText: string; tagName: string = "") {.raises: [], tags: [],
      contractual.} =
    ## Insert a text into the map info
    ##
    ## newText - the text to insert
    ## tagName - the text's tag to add to the text in the info widget
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
      distance: Natural = countDistance(destinationX = x, destinationY = y)
      travelValues: TravelArray = travelInfo(distance = distance)
    insertText(newText = "\nDistance: ")
    insertText(newText = $distance, tagName = "yellow2")
    if travelValues[1] > 0:
      insertText(newText = "\nETA:")
      var distanceText: string = ""
      minutesToDate(minutes = travelValues[1], infoText = distanceText)
      insertText(newText = distanceText, tagName = "yellow2")
      insertText(newText = "\nApprox fuel usage: ")
      insertText(newText = $travelValues[2], tagName = "yellow2")
  if skyMap[x][y].baseIndex > 0:
    let baseIndex: Positive = skyMap[x][y].baseIndex
    if skyBases[baseIndex].known:
      insertText(newText = "\nBase info:", tagName = "pink underline")
      insertText(newText = "\nName: ")
      insertText(newText = skyBases[baseIndex].name, tagName = "yellow2")
    if skyBases[baseIndex].visited.year > 0:
      try:
        discard tclEval(script = mapInfo &
            " tag configure basetype -foreground " & $basesTypesList[skyBases[
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
      let population: BasePopulation = getBasePopulation(baseIndex = baseIndex)
      if population > empty:
        insertText(newText = "\nPopulation: ")
        insertText(newText = $population, tagName = "yellow2")
      insertText(newText = "\nSize: ")
      insertText(newText = $skyBases[baseIndex].size & "\n",
          tagName = "yellow2")
      if population > empty:
        insertText(newText = "Owner: ")
        try:
          insertText(newText = factionsList[skyBases[baseIndex].owner].name,
              tagName = "yellow2")
        except:
          showError(message = "Can't get the name of the owner's faction.")
          return
      else:
        insertText(newText = "Base is abandoned")
      if population > empty:
        var
          baseInfoText: string = "\n"
          color: string = ""
        case skyBases[baseIndex].reputation.level
        of -100.. -75:
          baseInfoText &= "You are hated here"
          color = "red"
        of -74.. -50:
          baseInfoText &= "You are outlawed here"
          color = "red"
        of -49.. -25:
          baseInfoText &= "You are disliked here"
          color = "red"
        of -24.. -1:
          baseInfoText &= "They are unfriendly to you"
          color = "red"
        of 0:
          baseInfoText &= "You are unknown here"
        of 1..25:
          baseInfoText &= "You are know here as visitor"
          color = "green"
        of 26..50:
          baseInfoText &= "You are know here as trader"
          color = "green"
        of 51..75:
          baseInfoText &= "You are know here as friend"
          color = "green"
        of 76..100:
          baseInfoText &= "You are well known here"
          color = "green"
        insertText(newText = baseInfoText, tagName = color)
      if baseIndex == playerShip.homeBase:
        insertText(newText = "\nIt is your home base", tagName = "cyan")
  if skyMap[x][y].missionIndex > -1:
    var missionInfoText: string = "\n"
    if skyMap[x][y].baseIndex > 0 or skyMap[x][y].eventIndex > -1:
      missionInfoText &= "\n"
    let missionIndex: int = skyMap[x][y].missionIndex
    case acceptedMissions[missionIndex].mType
    of deliver:
      try:
        missionInfoText &= "Deliver " & itemsList[
            acceptedMissions[missionIndex].itemIndex].name
      except:
        showError(message = "Can't get the name of the item to deliver.")
        return
    of destroy:
      try:
        missionInfoText &= "Destroy " & protoShipsList[
            acceptedMissions[missionIndex].shipIndex].name
      except:
        showError(message = "Can't get the name of the ship to destroy.")
        return
    of patrol:
      missionInfoText &= "Patrol area"
    of explore:
      missionInfoText &= "Explore area"
    of passenger:
      missionInfoText &= "Transport passenger"
    insertText(newText = missionInfoText)
  if preview:
    for mission in skyBases[skyMap[playerShip.skyX][
        playerShip.skyY].baseIndex].missions:
      if mission.targetX == x and mission.targetY == y:
        var missionInfoText: string = "\n"
        if skyMap[x][y].baseIndex > 0 or skyMap[x][y].eventIndex > -1:
          missionInfoText &= "\n"
        case mission.mType
        of deliver:
          try:
            missionInfoText &= "Deliver " & itemsList[mission.itemIndex].name
          except:
            showError(message = "Can't get the name of the item to deliver.")
            return
        of destroy:
          try:
            missionInfoText &= "Destroy " & protoShipsList[
                mission.shipIndex].name
          except:
            showError(message = "Can't get the name of the ship to destroy.")
            return
        of patrol:
          missionInfoText &= "Patrol area"
        of explore:
          missionInfoText &= "Explore area"
        of passenger:
          missionInfoText &= "Transport passenger"
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
    let eventIndex: Natural = skyMap[x][y].eventIndex
    var eventInfoText: string = ""
    if eventsList[eventIndex].eType notin {baseRecovery, EventsTypes.none}:
      eventInfoText = "\n\n"
    var color: string = ""
    case eventsList[eventIndex].eType
    of trader:
      try:
        eventInfoText &= protoShipsList[eventsList[
            eventIndex].shipIndex].name
      except:
        showError(message = "Can't get the name of the trader's ship for the event.")
        return
      color = "green"
    of friendlyShip:
      try:
        eventInfoText &= protoShipsList[eventsList[
            eventIndex].shipIndex].name
      except:
        showError(message = "Can't get the name of the friendly ship for the event.")
        return
      color = "green2"
    of enemyShip:
      try:
        eventInfoText &= protoShipsList[eventsList[
            eventIndex].shipIndex].name
      except:
        showError(message = "Can't get the name of the enemy's ship for the event.")
        return
      color = "red"
    of fullDocks:
      eventInfoText &= "Full docks in base"
      color = "cyan"
    of attackOnBase:
      eventInfoText &= "Base is under attack"
      color = "red"
    of disease:
      eventInfoText &= "Disease in base"
      color = "yellow"
    of enemyPatrol:
      eventInfoText &= "Enemy patrol"
      color = "red3"
    of doublePrice:
      try:
        eventInfoText &= "Double price for " & itemsList[
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

proc setKeys*() {.raises: [], tags: [], contractual.} =
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
      pos: int = mapAccelerators[index + 1].rfind(sub = '-')
      keyName: string = ""
    if pos > -1:
      keyName = mapAccelerators[index + 1][0..pos] & "KeyPress-" &
          mapAccelerators[index + 1][pos + 1 .. ^1]
    else:
      keyName = "KeyPress-" & mapAccelerators[index + 1]
    tclEval(script = "bind . <" & keyName & "> " & command)
  var
    pos: int = fullScreenAccel.rfind(sub = '-')
    keyName: string = ""
  if pos > -1:
    keyName = fullScreenAccel[0..pos] & "KeyPress-" & fullScreenAccel[pos +
        1 .. ^1]
  else:
    keyName = "KeyPress-" & fullScreenAccel
  tclEval(script = "bind . <" & keyName & "> {ToggleFullScreen}")

import basesui, baseslootui, basesrecruitui, basesschoolui, basesshipyardui,
    craftsui, debugui, gameoptions, helpui, knowledge, mapsuicommands,
    messagesui, missionsui, ordersmenu, shipsui, statisticsui, tradesui, waitmenu

proc createGameUi*() {.raises: [], tags: [WriteIOEffect, TimeEffect, RootEffect,
    ReadIOEffect, RootEffect], contractual.} =
  ## Create the game UI and show sky map to the player
  const
    gameFrame: string = ".gameframe"
    paned: string = gameFrame & ".paned"
  mapView = paned & ".mapframe.map"
  var newStart: bool = false
  if tclEval2(script = "winfo exists " & mapView) == "0":
    newStart = true
    let fileName: string = saveDirectory.string & "keys.cfg"
    var configFile: FileStream = newFileStream(filename = fileName)
    if configFile == nil:
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
    else:
      var parser: CfgParser = CfgParser()
      try:
        parser.open(input = configFile, filename = fileName)
      except:
        showError(message = "Can't open the shortcut's configuration file.")
        return
      while true:
        var entry: CfgEvent = try:
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
    mapsuicommands.addCommands()
    tclEval(script = """
      pack [ttk::frame .gameframe -style Main.TFrame] -fill both -expand true
      # Game header
      ttk::frame .gameframe.header
      grid [ttk::button .gameframe.header.menubutton -style Small.TButton \
         -command ShowGameMenu] -sticky w
      tooltip::tooltip .gameframe.header.menubutton \
         "The main game menu. Show info about the ships,\nits crew and allow to quit the game"
      ttk::button .gameframe.header.closebutton -style Small.TButton \
         -command {ShowSkyMap}
      tooltip::tooltip .gameframe.header.closebutton {Back to the game map [Escape key]}
      ttk::button .gameframe.header.morebutton -style Small.TButton \
         -command {ShowMore}
      tooltip::tooltip .gameframe.header.morebutton {Show more options}
      grid [ttk::label .gameframe.header.time -text {1600-03-01}] -row 0 -column 3
      tooltip::tooltip .gameframe.header.time {The game time}
      grid columnconfigure .gameframe.header .gameframe.header.time -weight 1
      grid [ttk::label .gameframe.header.fuel] -row 0 -column 4 -padx 3
      grid [ttk::label .gameframe.header.food] -row 0 -column 5 -padx 3
      grid [ttk::label .gameframe.header.drinks] -row 0 -column 6 -padx 3
      grid [ttk::label .gameframe.header.overloaded] -row 0 -column 7 -padx 3
      grid [ttk::label .gameframe.header.pilot] -row 0 -column 8 -padx 3
      grid [ttk::label .gameframe.header.engineer] -row 0 -column 9 -padx 3
      grid [ttk::label .gameframe.header.gunner] -row 0 -column 10 -padx 3
      grid [ttk::label .gameframe.header.talk] -row 0 -column 11 -padx 3
      grid [ttk::label .gameframe.header.repairs] -row 0 -column 12 -padx 3
      grid [ttk::label .gameframe.header.upgrade] -row 0 -column 13 -padx 3
      grid [ttk::label .gameframe.header.clean] -row 0 -column 14 -padx 3
      grid [ttk::label .gameframe.header.crafting] -row 0 -column 15 -padx 3
      grid .gameframe.header -sticky we -padx 5 -pady {5 0}
      ttk::panedwindow .gameframe.paned
      # Game map
      .gameframe.paned add [ttk::frame .gameframe.paned.mapframe]
      set mapview [text .gameframe.paned.mapframe.map \
         -bg [set ttk::theme::[ttk::style theme use]::colors(-black)] -wrap none \
         -fg white -font MapFont -cursor crosshair -bd 0]
      grid $mapview -sticky nwes
      $mapview tag configure unvisited -background [ttk::style lookup Map -unvisited]
      $mapview tag configure yellow -foreground [ttk::style lookup Map -yellow]
      $mapview tag configure green -foreground [ttk::style lookup Map -green]
      $mapview tag configure red -foreground [ttk::style lookup Map -red]
      $mapview tag configure cyan -foreground [ttk::style lookup Map -cyan]
      $mapview tag configure lime -foreground [ttk::style lookup Map -lime]
      $mapview tag configure red2 -foreground [ttk::style lookup Map -red2]
      $mapview tag configure red3 -foreground [ttk::style lookup Map -red3]
      $mapview tag configure green2 -foreground [ttk::style lookup Map -green2]
      $mapview tag configure gray -foreground [ttk::style lookup Map -gray]
      $mapview tag configure black -foreground [ttk::style lookup Map -black]
      proc ValidateSpinbox {widget value button} {
         if {$value == ""} {
            if {$button != ""} {
               $button configure -state disabled
            }
            return true
         }
         if {$button != ""} {
            $button configure -state normal
         }
         set newvalue [regsub -all {[^0-9]} $value {}]
         set minvalue [$widget cget -from]
         if {$newvalue == ""} {
            $widget set $minvalue
            return false
         }
         if {$newvalue < $minvalue} {
            $widget set $minvalue
            return true
         }
         set maxvalue [$widget cget -to]
         if {$newvalue > $maxvalue} {
            $widget set $maxvalue
            return true
         }
         $widget set $newvalue
         $widget icursor end
         return true
      }
      # Move map buttons
      set mframe [ttk::frame .gameframe.paned.mapframe.buttons]
      grid [ttk::button $mframe.show -style Toolbutton -command ShowMapButtons] \
         -columnspan 5 -sticky we
      tooltip::tooltip $mframe.show {Show the map manipulation buttons}
      grid [ttk::button $mframe.left -style Map.Toolbutton \
         -command {MoveMapButtons left}] -rowspan 3 -row 1 -column 0 -sticky ns
      tooltip::tooltip $mframe.left {Move map buttons to the left corner}
      grid [ttk::button $mframe.nw -style Map.Toolbutton -command {MoveMap nw}] \
         -row 1 -column 1
      tooltip::tooltip $mframe.nw {Move map up and left}
      grid [ttk::button $mframe.n -style Map.Toolbutton -command {MoveMap n}] \
         -column 2 -row 1
      tooltip::tooltip $mframe.n {Move map up}
      grid [ttk::button $mframe.ne -style Map.Toolbutton -command {MoveMap ne}] \
         -column 3 -row 1
      tooltip::tooltip $mframe.ne {Move map up and right}
      grid [ttk::button $mframe.right -style Map.Toolbutton \
         -command {MoveMapButtons right}] -rowspan 3 -row 1 -column 4 -sticky ns
      tooltip::tooltip $mframe.right {Move map buttons to the right corner}
      grid [ttk::button $mframe.w -style Map.Toolbutton -command {MoveMap w}] \
         -row 2 -column 1
      tooltip::tooltip $mframe.w {Move map left}
      grid [ttk::button $mframe.wait -style Map.Toolbutton -command {
         if {[winfo ismapped .gameframe.paned.mapframe] == "0"} {
            return
         }
         if {[winfo exists .gameframe.movemapdialog]} {
            CloseDialog .gameframe.movemapdialog
            return
         }
         tk busy .gameframe.header
         tk busy .gameframe.paned
         ttk::frame .gameframe.movemapdialog -style Dialog.TFrame
         grid [ttk::label .gameframe.movemapdialog.header -text {Move map} \
            -style Header.TLabel] -sticky we -columnspan 2
         grid [ttk::label .gameframe.movemapdialog.xlabel -text X: -takefocus 0] \
            -pady {5 0}
         grid [ttk::spinbox .gameframe.movemapdialog.x -from 1 -to 1024 \
            -validate key \
            -validatecommand {ValidateSpinbox %W %P .gameframe.movemapdialog.moveto} \
            -width 5] -row 1 -column 1 -pady {5 0}
         .gameframe.movemapdialog.x set 1
         grid [ttk::label .gameframe.movemapdialog.ylabel -text Y: -takefocus 0] \
            -row 2
         grid [ttk::spinbox .gameframe.movemapdialog.y -from 1 -to 1024 \
            -validate key \
            -validatecommand {ValidateSpinbox %W %P .gameframe.movemapdialog.moveto} \
            -width 5] -row 2 -column 1
         .gameframe.movemapdialog.y set 1
         grid [ttk::button .gameframe.movemapdialog.moveto \
            -text {Move map to selected location} -command {MoveMap movemapto} \
            -underline 0] -row 3 -columnspan 2 -sticky we -padx 5
         grid [ttk::button .gameframe.movemapdialog.centeronship \
            -text {Center map on ship} -command {MoveMap centeronship} -underline 0] \
            -row 4 -columnspan 2 -sticky we -padx 5
         grid [ttk::button .gameframe.movemapdialog.centeronhome \
            -text {Center map on home base} -command {MoveMap centeronhome} \
            -underline 1] -row 5 -columnspan 2 -sticky we -padx 5
         grid [ttk::button .gameframe.movemapdialog.close -text {Close} \
            -command {CloseDialog .gameframe.movemapdialog}] -row 6 -columnspan 2 \
            -sticky we -padx 5 -pady {0 5}
         place .gameframe.movemapdialog -in .gameframe -relx 0.3 -rely 0.25
         focus .gameframe.movemapdialog.close
         foreach widget [winfo children .gameframe.movemapdialog] {
            bind $widget <Alt-m> {.gameframe.movemapdialog.moveto invoke;break}
            bind $widget <Alt-c> {.gameframe.movemapdialog.centeronship invoke;break}
            bind $widget <Alt-e> {.gameframe.movemapdialog.centeronhome invoke;break}
            bind $widget <Escape> {.gameframe.movemapdialog.close invoke;break}
         }
         bind .gameframe.movemapdialog.close <Tab> \
            {focus .gameframe.movemapdialog.x;break}
      }] -column 2 -row 2
      tooltip::tooltip $mframe.wait {Show more the map's options}
      grid [ttk::button $mframe.e -style Map.Toolbutton -command {MoveMap e}] \
         -column 3 -row 2
      tooltip::tooltip $mframe.e {Move map right}
      grid [ttk::button $mframe.sw -style Map.Toolbutton -command {MoveMap sw}] \
         -row 3 -column 1
      tooltip::tooltip $mframe.sw {Move map down and left}
      grid [ttk::button $mframe.s -style Map.Toolbutton -command {MoveMap s}] \
         -column 2 -row 3
      tooltip::tooltip $mframe.s {Move map down}
      grid [ttk::button $mframe.se -style Map.Toolbutton -command {MoveMap se}] \
         -column 3 -row 3
      tooltip::tooltip $mframe.se {Move map down and right}
      grid [ttk::button $mframe.hide -style Map.Toolbutton -command HideMapButtons] \
         -columnspan 5 -row 4 -sticky we
      tooltip::tooltip $mframe.hide {Hide the map manipulation buttons}
      grid $mframe -row 0 -column 0 -sticky se
      # Map info frame
      set mapinfo [text .gameframe.paned.mapframe.info -wrap word -height 10 \
         -width 20 -background [ttk::style lookup MapInfo -background] \
         -relief ridge -borderwidth 3 -padx 5]
      $mapinfo tag configure yellow -foreground [ttk::style lookup Map -yellow]
      $mapinfo tag configure green -foreground [ttk::style lookup Map -green]
      $mapinfo tag configure red -foreground [ttk::style lookup Map -red]
      $mapinfo tag configure cyan -foreground [ttk::style lookup Map -cyan]
      $mapinfo tag configure lime -foreground [ttk::style lookup Map -lime]
      $mapinfo tag configure red2 -foreground [ttk::style lookup Map -red2]
      $mapinfo tag configure red3 -foreground [ttk::style lookup Map -red3]
      $mapinfo tag configure green2 -foreground [ttk::style lookup Map -green2]
      $mapinfo tag configure pink -foreground [ttk::style lookup Map -pink]
      $mapinfo tag configure yellow2 -foreground [ttk::style lookup Map -goldenyellow]
      $mapinfo tag configure underline -font UnderlineFont
      grid $mapinfo -column 0 -row 0 -sticky ne
      bind .gameframe.paned.mapframe.info <Enter> MoveMapInfo
      grid rowconfigure .gameframe.paned.mapframe 0 -weight 1
      grid columnconfigure .gameframe.paned.mapframe 0 -weight 1
      # Last messages
      .gameframe.paned add [ttk::frame .gameframe.paned.controls]
      grid [ttk::frame .gameframe.paned.controls.messages -style LastMessages.TFrame] \
         -sticky we
      pack [ttk::scrollbar .gameframe.paned.controls.messages.scroll -orient vertical \
         -command [list .gameframe.paned.controls.messages.view yview]] -side right \
         -fill y -padx {0 5} -pady 5
      set messagesview [text .gameframe.paned.controls.messages.view -wrap word \
         -yscrollcommand [list .gameframe.paned.controls.messages.scroll set]]
      $messagesview tag configure yellow -foreground \
         [ttk::style lookup Messages -yellow]
      $messagesview tag configure green -foreground \
         [ttk::style lookup Messages -green]
      $messagesview tag configure red -foreground \
         [ttk::style lookup Messages -red]
      $messagesview tag configure cyan -foreground \
         [ttk::style lookup Messages -cyan]
      $messagesview tag configure blue -foreground \
         [ttk::style lookup Messages -blue]
      $messagesview tag configure gray -foreground \
         [ttk::style lookup Messages -gray]
      pack $messagesview -side top -fill both -padx 5 -pady 5
      tooltip::tooltip $messagesview \
         "The last game messages. You can see more of them\nIn Menu->Last messages screen"
      ::autoscroll::autoscroll .gameframe.paned.controls.messages.scroll
      bind .gameframe.paned.controls <Configure> {
         $messagesview configure -height [expr \
            [winfo height .gameframe.paned.controls] / [font metrics InterfaceFont \
            -linespace]]
      }
      # Movement buttons
      set bframe [ttk::frame .gameframe.paned.controls.buttons]
      grid $bframe -row 0 -column 1 -sticky nw
      grid [ttk::frame $bframe.box] -columnspan 3 -sticky we
      grid [ttk::button $bframe.box.orders -command {ShowOrders} -text {Ship Orders}]
      tooltip::tooltip $bframe.box.orders "Show available orders for your ship."
      grid [ttk::combobox $bframe.box.speed -state readonly -values [list {Full stop} \
         {Quarted speed} {Half speed} {Full speed}] -width 10] -sticky we
      tooltip::tooltip $bframe.box.speed \
         "Set speed for your ship. The faster you move,\nthe more fuel used. But faster movement has\nbigger chance to evade enemies."
      grid [ttk::button $bframe.box.moveto -command {MoveShip moveto} \
         -style Move.TButton] -row 0 -column 1
      tooltip::tooltip $bframe.box.moveto "Auto move your ship to its destination"
      grid [ttk::button $bframe.nw -command {MoveShip nw} -style Move.TButton] \
         -row 1 -sticky we
      tooltip::tooltip $bframe.nw "Move ship up and left"
      grid [ttk::button $bframe.n -command {MoveShip n} -style Move.TButton] \
         -column 1 -row 1 -sticky we
      tooltip::tooltip $bframe.n "Move ship up"
      grid [ttk::button $bframe.ne -command {MoveShip ne} -style Move.TButton] \
         -column 2 -row 1 -sticky we
      tooltip::tooltip $bframe.ne "Move ship up and right"
      grid [ttk::button $bframe.w -command {MoveShip w} -style Move.TButton] -row 2 \
         -sticky we
      tooltip::tooltip $bframe.w "Move ship left"
      grid [ttk::button $bframe.wait -command {MoveShip waitormove} \
         -style Move.TButton] -column 1 -row 2 -sticky we
      grid [ttk::button $bframe.e -command {MoveShip e} -style Move.TButton] \
         -column 2 -row 2 -sticky we
      tooltip::tooltip $bframe.e "Move ship right"
      grid [ttk::button $bframe.sw -command {MoveShip sw} -style Move.TButton] \
         -row 3 -sticky we
      tooltip::tooltip $bframe.sw "Move ship down and left"
      grid [ttk::button $bframe.s -command {MoveShip s} -style Move.TButton] \
         -column 1 -row 3 -sticky we
      tooltip::tooltip $bframe.s "Move ship down"
      grid [ttk::button $bframe.se -command {MoveShip se} -style Move.TButton] \
         -column 2 -row 3 -sticky we
      tooltip::tooltip $bframe.se "Move ship down and right"
      grid columnconfigure .gameframe.paned.controls \
         .gameframe.paned.controls.messages -weight 1
      grid .gameframe.paned -sticky nwes -padx 5 -pady {0 5}
      grid columnconfigure .gameframe .gameframe.paned -weight 1
      grid rowconfigure .gameframe .gameframe.paned -weight 1
      update
    """)
    setTheme()
    ordersmenu.addCommands()
    waitmenu.addCommands()
    helpui.addCommands()
    shipsui.addCommands()
    craftsui.addCommands()
    messagesui.addCommands()
    gameoptions.addCommands()
    tradesui.addCommands()
    basesschoolui.addCommands()
    basesrecruitui.addCommands()
    basesui.addCommands()
    basesshipyardui.addCommands()
    baseslootui.addCommands()
    knowledge.addCommands()
    missionsui.addCommands()
    statisticsui.addCommands()
    const messagesFrame: string = paned & ".controls.messages"
    tclEval(script = "bind " & messagesFrame & " <Configure> {ResizeLastMessages}")
    tclEval(script = "bind " & mapView & " <Configure> {DrawMap}")
    tclEval(script = "bind " & mapView & " <Motion> {UpdateMapInfo %x %y}")
    tclEval(script = "bind " & mapView & " <Button-" & (
        if gameSettings.rightButton: "3" else: "1") & "> {ShowDestinationMenu %X %Y;break}")
    tclEval(script = "bind " & mapView & " <MouseWheel> {if {%D > 0} {ZoomMap raise} else {ZoomMap lower}}")
    tclEval(script = "bind " & mapView & " <Button-4> {ZoomMap raise}")
    tclEval(script = "bind " & mapView & " <Button-5> {ZoomMap lower}")
    setKeys()
    if debugMode == menu:
      showDebugUi()
  else:
    tclEval(script = "pack " & gameFrame & " -fill both -expand true")
  tclSetVar(varName = "refreshmap", newValue = "1")
  tclEval(script = "wm title . {Steam Sky}")
  if gameSettings.fullScreen:
    tclEval(script = "wm attributes . -fullscreen 1")
  for accel in menuAccelerators:
    let pos: int = accel.rfind(sub = '-')
    tclEval(script = "bind . <" & accel[0..pos] & "KeyPress-" &
      accel[pos + 1..^1] & "> {InvokeMenu " & accel & "}")
  if not tclEval2(script = "grid slaves .").contains(sub = ".gameframe.header"):
    let header: string = gameFrame & ".header"
    tclEval(script = "grid " & header)
  updateHeader()
  centerX = playerShip.skyX
  centerY = playerShip.skyY
  for index, baseType in basesTypesList:
    tclEval(script = mapView & " tag configure " & index & " -foreground " &
        $baseType.color)
  let panedPosition: int = (if gameSettings.windowHeight -
      gameSettings.messagesPosition <
      0: gameSettings.windowHeight else: gameSettings.windowHeight -
      gameSettings.messagesPosition)
  tclEval(script = paned & " sashpos 0 " & $panedPosition)
  if not tclEval2(script = "grid slaves .").contains(sub = ".gameframe.paned"):
    tclEval(script = "grid " & paned)
  tclEval(script = "update")
  const button: string = paned & ".mapframe.buttons.hide"
  tclEval(script = button & " invoke")
  tclEval(script = "bind . <Escape> {InvokeButton " & closeButton & "}")
  updateMessages()
  if not newStart:
    tclEval(script = "DrawMap")
  updateMoveButtons()
  updateMapInfo()
  if not gameSettings.showLastMessages:
    const messagesFrame: string = paned & ".controls.messages"
    tclEval(script = "grid remove " & messagesFrame)
  tclSetVar(varName = "shipname", newValue = playerShip.name)
  tclSetVar(varName = "gamestate", newValue = "general")
  if tclEval2(script = "winfo ismapped " & closeButton) == "1":
    showSkyMap(clear = true)
    tclEval(script = "grid remove " & closeButton)
