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

## Provides code related to the game's main menu, like showing the
## menu, and selecting its various sections

import std/[algorithm, colors, math, os, sequtils, strutils, tables, times]
import contracts, nuklear/nuklear_sdl_renderer, nimalyzer
import ../[basestypes, config, game, game2, gamesaveload, goals,
    halloffame, shipscrew, ships2, utils]
import coreui, dialogs, errordialog, goalsui, mapsui, themes

var
  menuImages: array[4, PImage] = [nil, nil, nil, nil]
  showLoadButton, showHoFButton: bool = false
  fileContent: string = ""
  fileName: string = ""
  fileLines: Positive = 1
  playerFactions, playerCareers, playerBases: seq[string] = @[]
  currentFaction, currentCareer, currentBase: int = 0
  newFaction, newCareer, newBase: Natural = 0
  showGender: bool = true

proc setMainMenu*(dialog: var GameDialog) {.raises: [], tags: [
    ReadDirEffect, WriteIOEffect, TimeEffect, RootEffect], contractual.} =
  ## Set the main menu, load logo if needed and set the menu's buttons
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns parameter dialog, modified if any error happened.
  if menuImages[0] == nil:
    # Load images
    try:
      theme = themesList[gameSettings.interfaceTheme]
      menuImages[0] = nuklearLoadSVGImage(filePath = theme.icons[logoImage],
          width = 0, height = 110)
      for index, fileName in theme.icons[1..3]:
        menuImages[index + 1] = nuklearLoadSVGImage(filePath = fileName,
            width = 0, height = 10 + gameSettings.interfaceFontSize)
    except:
      dialog = setError(message = "Can't set the game's images.")
    # Set the list of available factions
    for index, faction in factionsList:
      playerFactions.add(y = faction.name)
      if index == newGameSettings.playerFaction:
        showGender = "nogender" notin faction.flags
        currentFaction = playerFactions.high
        var i: Natural = 0
        for index, career in faction.careers:
          playerCareers.add(y = career.name)
          if index == newGameSettings.playerCareer:
            currentCareer = i
          i.inc
        playerCareers.add(y = "Random")
        playerBases.add(y = "Any")
        i = 1
        for baseType in faction.basesTypes.keys:
          try:
            playerBases.add(y = basesTypesList[baseType].name)
          except:
            dialog = setError(message = "Can't add a base type.")
            break
          if baseType == newGameSettings.startingBase:
            currentBase = i
          i.inc
    playerFactions.add(y = "Random")
  showLoadButton = walkFiles(pattern = saveDirectory & "*.sav").toSeq.len > 0
  showHoFButton = fileExists(filename = saveDirectory & "halloffame.dat")

proc showMainMenu*(state: var GameState; dialog: var GameDialog) {.raises: [],
    tags: [], contractual.} =
  ## Show the game's main menu and set the game's state
  ##
  ## * state - the current game's state
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter state and dialog. The latter is modified if
  ## any error happened.
  layoutSpaceStatic(height = 90, widgetsCount = 1):
    row(x = 50, y = 0, w = 500, h = 90):
      image(image = menuImages[0])
  setLayoutRowDynamic(height = 40, cols = 1)
  label(str = gameVersion & " development", alignment = centered)
  layoutSpaceStatic(height = 240, widgetsCount = 6):
    const
      x: float = 225
      w: float = 150
      h: float = 40
    row(x = x, y = 0, w = w, h = h):
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Set and start a new game")
      labelButton(title = "New game"):
        state = newGame
        dialog = none
        return
    var y: float = h;
    if showLoadButton:
      row(x = x, y = y, w = w, h = h):
        y += h
        if gameSettings.showTooltips:
          addTooltip(bounds = getWidgetBounds(),
              text = "Load one of the previously saved games")
        labelButton(title = "Load game"):
          state = loadGame
          return
    if showHoFButton:
      row(x = x, y = y, w = w, h = h):
        y += h
        if gameSettings.showTooltips:
          addTooltip(bounds = getWidgetBounds(),
              text = "Show your previous the bests scores in the game")
        labelButton(title = "Hall of Fame"):
          state = hallOfFame
          return
    row(x = x, y = y, w = w, h = h):
      y += h
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "The list of changes to the game")
      labelButton(title = "News"):
        state = news
        return
    row(x = x, y = y, w = w, h = h):
      y += h
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "General information about the game")
      labelButton(title = "About"):
        state = about
        return
    row(x = x, y = y, w = w, h = h):
      y += h
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(), text = "Quit from the game")
      labelButton(title = "Quit"):
        state = quitGame
        return

proc showNews*(state: var GameState; dialog: var GameDialog) {.raises: [],
    tags: [ReadDirEffect, ReadIOEffect, WriteIOEffect, TimeEffect, RootEffect],
        contractual.} =
  ## Show the game's latest changes
  ##
  ## * state  - the current game's state
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter state and dialog. The latter is modified if
  ## any error happened.
  if fileContent.len == 0 and dialog == none:
    if fileExists(filename = docDirectory & "CHANGELOG.md"):
      try:
        var index: Natural = 0
        fileLines = 1
        for line in lines(filename = docDirectory & "CHANGELOG.md"):
          index.inc
          if index < 6:
            continue
          if state == news and line.len > 1 and line[0..2] == "## ":
            break
          fileContent.add(y = line & "\n")
          var needLines: float = ceil(x = getTextWidth(text = line) /
              menuWidth.float)
          if needLines < 1.0:
            needLines = 1.0
          fileLines += needLines.int
        fileLines *= 25
      except:
        dialog = setError(message = "Can't read ChangeLog file.")
  setLayoutRowDynamic(height = (menuHeight - 50).float, cols = 1)
  if fileContent.len > 0:
    group(title = "NewsGroup", flags = {windowNoFlags}):
      setLayoutRowDynamic(height = fileLines.float, cols = 1)
      wrapLabel(str = fileContent)
  else:
    wrapLabel(str = "Can't find file to load. Did 'CHANGELOG.md' file is in '" &
        docDirectory & "' directory?")
  layoutSpaceStatic(height = 50, widgetsCount = 2):
    if state == news:
      row(x = (menuWidth - 310).float, y = 0, w = 155, h = 40):
        if gameSettings.showTooltips:
          addTooltip(bounds = getWidgetBounds(),
              text = "Show all changes to the game since previous big stable version")
        labelButton(title = "Show all changes"):
          state = allNews
          fileContent = ""
          return
    else:
      row(x = (menuWidth - 405).float, y = 0, w = 250, h = 40):
        if gameSettings.showTooltips:
          addTooltip(bounds = getWidgetBounds(),
              text = "Show only changes to the game since previous release")
        labelButton(title = "Show only newest changes"):
          state = news
          fileContent = ""
          return
    row(x = (menuWidth - 150).float, y = 0, w = 140, h = 40):
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(), text = "Back to the main menu")
      labelButton(title = "Back to menu"):
        state = mainMenu
        fileContent = ""
        return
  if isKeyPressed(key = keyEscape):
    state = mainMenu
    fileContent = ""

proc showAbout*(state: var GameState; dialog: var GameDialog) {.raises: [],
    tags: [ReadIOEffect, RootEffect], contractual.} =
  ## Show the general information about the game
  ##
  ## * state - the current game's state
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter state and dialog. The latter is modified if
  ## any error happened.
  setLayoutRowDynamic(height = 30, cols = 1)
  label(str = "Roguelike in the sky with a steampunk theme",
      alignment = centered)
  saveButtonStyle()
  setButtonStyle(field = borderColor, a = 0)
  layoutSpaceStatic(height = 80, widgetsCount = 4):
    row(x = 255, y = 0, w = 100, h = 30):
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Visit the game website: https://thindil.itch.io/steam-sky")
      labelButton(title = "Website"):
        openLink(link = "https://thindil.itch.io/steam-sky")
    row(x = 270, y = 0, w = 85, h = 30):
      label(str = "______")
    row(x = 145, y = 40, w = 330, h = 30):
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Send a mail to the game creator")
      labelButton(title = "(c)2016-2025 Bartek thindil Jasicki"):
        openLink(link = "mailto:thindil@laeran.pl.eu.org")
    row(x = 160, y = 40, w = 315, h = 30):
      label(str = "__________________________")
  restoreButtonStyle()
  layoutSpaceStatic(height = 40, widgetsCount = 3):
    row(x = 75, y = 0, w = 150, h = 30):
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Guide how to help with creating the game, report bugs, etc.")
      labelButton(title = "Get involved"):
        fileName = "CONTRIBUTING.md"
        state = showFile
        dialog = none
        return
    row(x = 230, y = 0, w = 150, h = 30):
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Guide how to modify the game")
      labelButton(title = "Modify game"):
        fileName = "MODDING.md"
        state = showFile
        return
    row(x = 385, y = 0, w = 150, h = 30):
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Some technical information about the game")
      labelButton(title = "README"):
        fileName = "README.md"
        state = showFile
        return
  setLayoutRowDynamic(height = 175, cols = 1)
  wrapLabel(str = "Steam Sky is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.\nSteam Sky is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.")
  layoutSpaceStatic(height = 50, widgetsCount = 2):
    row(x = (menuWidth - 310).float, y = 0, w = 155, h = 40):
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Show full legal text of GNU GPLv3 license")
      labelButton(title = "Show full license"):
        fileName = "COPYING"
        state = showFile
        return
    row(x = (menuWidth - 150).float, y = 0, w = 140, h = 40):
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(), text = "Back to the main menu")
      labelButton(title = "Back to menu"):
        state = mainMenu
        return
  if isKeyPressed(key = keyEscape):
    state = mainMenu
    return
  showLinkError()

proc showFile*(state: var GameState; dialog: var GameDialog) {.raises: [],
    tags: [ReadIOEffect, RootEffect], contractual.} =
  ## Show the selected file content
  ##
  ## * state - the current game's state
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter state and dialog. The latter is modified if
  ## any error happened.
  if fileContent.len == 0 and dialog == none:
    if fileExists(filename = docDirectory & fileName):
      try:
        fileLines = 1
        if fileName == "CONTRIBUTING.md":
          fileLines = 6
        for line in lines(filename = docDirectory & fileName):
          fileContent.add(y = line & "\n")
          var needLines: float = ceil(x = getTextWidth(text = line) /
              menuWidth.float)
          if needLines < 1.0:
            needLines = 1.0
          fileLines += needLines.int
        fileLines *= 25
      except:
        dialog = setError(message = "Can't read '" & fileName & "' file.")
  setLayoutRowDynamic(height = (menuHeight - 50).float, cols = 1)
  if fileContent.len > 0:
    group(title = "FileGroup", flags = {windowNoFlags}):
      setLayoutRowDynamic(height = fileLines.float, cols = 1)
      wrapLabel(str = fileContent)
  else:
    wrapLabel(str = "Can't find file to load. Did '" & fileName &
        "' file is in '" & docDirectory & "' directory?")
  layoutSpaceStatic(height = 50, widgetsCount = 1):
    row(x = (menuWidth - 150).float, y = 0, w = 140, h = 40):
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(), text = "Back to the main menu")
      labelButton(title = "Back to menu"):
        state = mainMenu
        fileContent = ""
        return
  if isKeyPressed(key = keyEscape):
    state = mainMenu
    fileContent = ""

proc showHallOfFame*(state: var GameState; dialog: var GameDialog) {.raises: [],
    tags: [ReadIOEffect, RootEffect], contractual.} =
  ## Show the game's hall of fame
  ##
  ## * state - the current game's state
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter state and dialog. The latter is modified if
  ## any error happened.
  dialog = none
  setLayoutRowDynamic(height = (menuHeight - 50).float, cols = 1)
  group(title = "HofGroup", flags = {windowNoFlags}):
    setLayoutRowDynamic(height = 25, cols = 4)
    colorLabel(str = "Position", color = colYellow, align = centered)
    colorLabel(str = "Name", color = colYellow, align = centered)
    colorLabel(str = "Points", color = colYellow, align = centered)
    colorLabel(str = "Died from", color = colYellow, align = centered)
    for index, entry in hallOfFameArray:
      if entry.points == 0:
        break
      label(str = $index, alignment = centered)
      label(str = entry.name, alignment = centered)
      label(str = $entry.points, alignment = centered)
      label(str = entry.deathReason, alignment = centered)
  layoutSpaceStatic(height = 50, widgetsCount = 1):
    row(x = (menuWidth - 150).float, y = 0, w = 140, h = 40):
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(), text = "Back to the main menu")
      labelButton(title = "Back to menu"):
        state = mainMenu
  if isKeyPressed(key = keyEscape):
    state = mainMenu

type
  SortingOrder = enum
    playerAsc, playerDesc, shipAsc, shipDesc, timeAsc, timeDesc
  SaveData = object
    playerName, shipName, saveTime, path: string

var
  sortOrder: SortingOrder = timeDesc
  saveClicked: string = ""
  saves: seq[SaveData] = @[]

proc showLoadMenu(dialog: var GameDialog; bounds: NimRect) {.raises: [], tags: [
    RootEffect], contractual.} =
  ## Show the menu for the selected saved game
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ## * bounds - the rectangle in which the player should click the mouse's
  ##            button to show the menu
  ##
  ## Returns the parameter dialog. It is modified only when the player start
  ## loading the game.
  contextualMenu(flags = {windowNoFlags}, x = 150, y = 150,
      triggerBounds = bounds, button = (
      if gameSettings.rightButton: Buttons.right else: Buttons.left)):
    setLayoutRowDynamic(height = 25, cols = 1)
    contextualItemLabel(label = "Load game", align = centered):
      dialog = loading
    contextualItemLabel(label = "Delete game", align = centered):
      dialog = setQuestion(question = "Are you sure you want delete this savegame?",
          data = saveClicked, qType = deleteSave)
    contextualItemLabel(label = "Close", align = centered):
      discard

proc showLoadGame*(state: var GameState; dialog: var GameDialog) {.raises: [],
    tags: [ReadIOEffect, RootEffect], contractual.} =
  ## Show the list of saved games
  ##
  ## * state - the current game's state
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter state and dialog. The latter is modified if
  ## any error happened.
  setLayoutRowDynamic(height = (menuHeight - 50).float, cols = 1)
  group(title = "LoadGroup", flags = {windowNoFlags}):
    setLayoutRowDynamic(height = 30, cols = 3)
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Press mouse button to sort the saved games.")
    labelButton(title = "Player name"):
      if sortOrder == playerAsc:
        sortOrder = playerDesc
      else:
        sortOrder = playerAsc
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Press mouse button to sort the saved games.")
    labelButton(title = "Ship name"):
      if sortOrder == shipAsc:
        sortOrder = shipDesc
      else:
        sortOrder = shipAsc
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Press mouse button to sort the saved games.")
    labelButton(title = "Last saved"):
      if sortOrder == timeAsc:
        sortOrder = timeDesc
      else:
        sortOrder = timeAsc
    if answered:
      saves = @[]
      answered = false
    if saves.len == 0:
      for file in walkFiles(pattern = saveDirectory & "*.sav"):
        let
          (_, name, _) = splitFile(path = file)
          parts = name.split(sep = '_')
        try:
          saves.add(y = SaveData(playerName: parts[0], shipName: parts[1],
              saveTime: file.getLastModificationTime.format(
              f = "yyyy-MM-dd hh:mm:ss"), path: file))
        except:
          dialog = setError(message = "Can't add information about the save file.")
    if saves.len == 0:
      showLoadButton = false
      state = mainMenu
      return

    proc sortSaves(x, y: SaveData): int {.raises: [], tags: [], contractual.} =
      ## Check how to sort the selected saves on the list
      ##
      ## * x - the first save to sort
      ## * y - the second save to sort
      ##
      ## Returns 1 if the x save should go first, otherwise -1
      case sortOrder
      of playerAsc:
        if x.playerName < y.playerName:
          return 1
        return -1
      of playerDesc:
        if x.playerName > y.playerName:
          return 1
        return -1
      of shipAsc:
        if x.shipName < y.shipName:
          return 1
        return -1
      of shipDesc:
        if x.shipName > y.shipName:
          return 1
        return -1
      of timeAsc:
        if x.saveTime < y.saveTime:
          return 1
        return -1
      of timeDesc:
        if x.saveTime > y.saveTime:
          return 1
        return -1

    saves.sort(cmp = sortSaves)
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
    layoutSpaceStatic(height = (saves.len * 30).float, widgetsCount = (
        saves.len * 3)):
      for index, save in saves:
        let
          y: float = (index * 30).float
        row(x = 0, y = y, w = 190, h = 30):
          labelButton(title = save.playerName):
            saveClicked = save.path
        row(x = 190, y = y, w = 190, h = 30):
          labelButton(title = save.shipName):
            saveClicked = save.path
        row(x = 380, y = y, w = 190, h = 30):
          labelButton(title = save.saveTime):
            saveClicked = save.path
  restoreButtonStyle()
  let bounds: NimRect = NimRect(x: 0, y: 35, w: 580, h: (saves.len * 35).float)
  if gameSettings.showTooltips:
    addTooltip(bounds = bounds, text = "Press mouse " & (
        if gameSettings.rightButton: "right" else: "left") & " button to show available option")
  showLoadMenu(dialog = dialog, bounds = bounds)
  layoutSpaceStatic(height = 50, widgetsCount = 1):
    row(x = (menuWidth - 150).float, y = 0, w = 140, h = 40):
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(), text = "Back to the main menu")
      labelButton(title = "Back to menu"):
        state = mainMenu
        saveClicked = ""
  showQuestion(dialog = dialog, state = state)
  if isKeyPressed(key = keyEscape):
    if dialog == none:
      state = mainMenu
      saveClicked = ""
    else:
      dialog = none

proc setGame(dialog: var GameDialog) {.raises: [], tags: [RootEffect],
    contractual.} =
  ## Set the size of the main window and show the map
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  nuklearResizeWin(width = gameSettings.windowWidth,
      height = gameSettings.windowHeight)
  nuklearSetWindowPos(x = windowCentered, y = windowCentered)
  windowWidth = gameSettings.windowWidth.float
  windowHeight = gameSettings.windowHeight.float
  nuklearSetWindowResizable()
  createGameUi(dialog = dialog)

proc loadGame*(state: var GameState; dialog: var GameDialog) {.raises: [],
    tags: [WriteIOEffect, ReadIOEffect, TimeEffect, RootEffect], contractual.} =
  ## Start loading the selected saved game
  ##
  ## * state - the current game's state
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter state and dialog. The latter is modified if
  ## any error happened.
  try:
    saveName = saveClicked
    loadGame()
  except:
    state = loadGame
    dialog = setError(message = "Can't load this game.")
    return
  dialog = none
  setGame(dialog = dialog)
  if dialog == none:
    state = map

const playerTooltips: array[12, string] = ["Enter character name.",
    "Select a random name for the character, based on the character gender",
    "Enter ship name.",
    "Select a random name for the character, based on the character gender",
    "Select starting goal for your character. You can change it later in game.",
    "Select your faction from a list. Factions have the biggest impact on game. They determine the amount of bases and some playing styles. More information about each faction can be found after selecting it. You can't change this later.",
    "Select your career from a list. Careers have some impact on gameplay (each have bonuses to gaining experience in some fields plus they determine your starting ship and crew). More info about each career can be found after selecting it. You can't change career later.",
    "Select type of base in which you will start the game. This may have some impact on game difficulty.",
    "General player character settings. Select field which you want to set to see more information about.",
    "Faction, career and base type will be randomly selected for you during creating new game. Not recommended for new player.",
    "Career will be randomly selected for you during creating new game. Not recommended for new player.", "Start the game in randomly selected base type."]

var
  playerName: string = newGameSettings.playerName
  shipName: string = newGameSettings.shipName
  currentTab: cint = 0
  playerGender: cint = 2
  infoText: string = playerTooltips[8]

proc setInfoText(dialog: var GameDialog) {.raises: [], tags: [RootEffect],
    contractual.} =
  ## Set the info text based on the selected player's faction, career or base
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  if currentFaction == -1:
    currentFaction = newFaction
    if currentFaction == playerFactions.high:
      infoText = playerTooltips[5] & "\n\n" & playerTooltips[9]
      return
    for faction in factionsList.values:
      if faction.name == playerFactions[newFaction]:
        infoText = playerTooltips[5] & "\n\n" & faction.description
        return
  if currentCareer == -1:
    currentCareer = newCareer
    if currentCareer == playerCareers.high:
      infoText = playerTooltips[5] & "\n\n" & playerTooltips[10]
      return
    for faction in factionsList.values:
      if faction.name == playerFactions[newFaction]:
        for career in faction.careers.values:
          if career.name == playerCareers[newCareer]:
            infoText = playerTooltips[6] & "\n\n" & career.description
            return
  if currentBase == -1:
    currentBase = newBase
    if currentBase == 0:
      infoText = playerTooltips[5] & "\n\n" & playerTooltips[11]
      return
    for faction in factionsList.values:
      if faction.name == playerFactions[newFaction]:
        for baseType in faction.basesTypes.keys:
          try:
            if basesTypesList[baseType].name == playerBases[newBase]:
              infoText = playerTooltips[7] & "\n\n" & basesTypesList[
                  baseType].description
              return
          except:
            dialog = setError(message = "Can't get base type.")
            return

proc randomName(forPlayer: bool) {.raises: [], tags: [], contractual.} =
  ## Generate a random name for the player's character or their ship
  ##
  ## * forPlayer - if true, generate a random name for the player's character
  var factionIndex: string = ""
  for index, faction in factionsList:
    if faction.name == playerFactions[newFaction]:
      factionIndex = index
      break
  if forPlayer:
    let gender: char = (if playerGender == 2: 'M' else: 'F')
    playerName = generateMemberName(gender = gender,
        factionIndex = factionIndex)
  else:
    shipName = generateShipName(factionIndex = factionIndex)

proc newGamePlayer(dialog: var GameDialog) {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Show the player's settings for starting a new game
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  {.ruleOff: "varDeclared".}
  var
    bounds: array[8, NimRect]
  {.ruleOn: "varDeclared".}
  group(title = "groupSetting", flags = {windowNoFlags}):
    # Character's name
    setLayoutRowDynamic(height = 35, cols = 3, ratio = [0.4.cfloat, 0.5, 0.1])
    label(str = "Character name:")
    bounds[0] = getWidgetBounds()
    if mouseClicked(id = left, rect = bounds[0]):
      infoText = playerTooltips[0]
    editString(text = playerName, maxLen = 64)
    bounds[1] = getWidgetBounds()
    saveButtonStyle()
    setButtonStyle(field = padding, value = NimVec2(x: 0.0, y: 0.0))
    imageButton(image = menuImages[1]):
      randomName(forPlayer = true)
    restoreButtonStyle()
    if showGender:
      # Character's gender
      setLayoutRowDynamic(height = 35, cols = 3, ratio = [0.4.cfloat, 0.1, 0.1])
      label(str = "Character gender:")
      const genders: array[2..3, string] = [2: "Male", 3: "Female"]
      for i in 2..3:
        saveButtonStyle()
        setButtonStyle(field = padding, value = NimVec2(x: 0.0, y: 0.0))
        if playerGender == i:
          setButtonStyle2(source = active, destination = normal)
          if gameSettings.showTooltips:
            addTooltip(bounds = getWidgetBounds(), text = genders[i])
          imageButton(image = menuImages[i]):
            playerGender = i.cint
        else:
          if gameSettings.showTooltips:
            addTooltip(bounds = getWidgetBounds(), text = genders[i])
          imageButton(image = menuImages[i]):
            playerGender = i.cint
        restoreButtonStyle()
    # Player's ship's name
    setLayoutRowDynamic(height = 35, cols = 3, ratio = [0.4.cfloat, 0.5, 0.1])
    label(str = "Ship name:")
    bounds[2] = getWidgetBounds()
    if mouseClicked(id = left, rect = bounds[2]):
      infoText = playerTooltips[2]
    editString(text = shipName, maxLen = 64)
    bounds[3] = getWidgetBounds()
    saveButtonStyle()
    setButtonStyle(field = padding, value = NimVec2(x: 0.0, y: 0.0))
    imageButton(image = menuImages[1]):
      randomName(forPlayer = false)
    restoreButtonStyle()
    # Character's goal
    setLayoutRowDynamic(height = 35, cols = 2, ratio = [0.4.cfloat, 0.6])
    label(str = "Character goal:")
    bounds[4] = getWidgetBounds()
    if mouseClicked(id = left, rect = bounds[4]):
      infoText = playerTooltips[4]
    labelButton(title = selectedGoal):
      dialog = newGoalDialog
      setSelectedGoal()
    # Character's faction
    setLayoutRowDynamic(height = 35, cols = 2, ratio = [0.4.cfloat, 0.6])
    label(str = "Character faction:")
    bounds[5] = getWidgetBounds()
    newFaction = comboList(items = playerFactions,
        selected = currentFaction, itemHeight = 25, x = 200, y = 150)
    if newFaction != currentFaction or mouseClicked(id = left,
        rect = bounds[5]):
      currentFaction = -1
      playerCareers = @[]
      currentCareer = 0
      playerBases = @[]
      currentBase = 0
      if newFaction < playerFactions.high:
        for faction in factionsList.values:
          if faction.name == playerFactions[newFaction]:
            showGender = "nogender" notin faction.flags
            for career in faction.careers.values:
              playerCareers.add(y = career.name)
            playerCareers.add(y = "Random")
            playerBases.add(y = "Any")
            for baseType in faction.basesTypes.keys:
              try:
                playerBases.add(y = basesTypesList[baseType].name)
              except:
                dialog = setError(message = "Can't add a base type.")
                break
            break
    # Character's career
    if playerCareers.len > 0:
      label(str = "Character career:")
      bounds[6] = getWidgetBounds()
      newCareer = comboList(items = playerCareers,
          selected = currentCareer, itemHeight = 25, x = 200, y = 125)
      if newCareer != currentCareer or mouseClicked(id = left,
          rect = bounds[6]):
        currentCareer = -1
    # Starting base
    if playerBases.len > 0:
      label(str = "Starting base type:")
      bounds[7] = getWidgetBounds()
      newBase = comboList(items = playerBases, selected = currentBase,
          itemHeight = 25, x = 200, y = 60)
      if newBase != currentBase or mouseClicked(id = left, rect = bounds[
          7]):
        currentBase = -1
    setInfoText(dialog = dialog)
    if gameSettings.showTooltips:
      for index, bound in bounds:
        addTooltip(bounds = bound, text = playerTooltips[index])

var
  currentLevel: Natural = 2
  diffSettings: array[8, Positive] = [100, 100, 100, 100, 100, 100, 100, 100]
  randomSettings: bool = false
  points: Natural = 100

proc setPoints() {.raises: [], tags: [], contractual.} =
  ## Count the bonus for gained points with the selected game's difficulty
  var newPoints: int = 0
  for index, difficulty in diffSettings:
    var value: int = difficulty
    if index in {1, 3, 4, 5}:
      if value < 100:
        value = 100 + ((100 - value) * 4)
      elif value > 100:
        value = 100 - value
    newPoints += value
  newPoints = ((newPoints.float) / 8.0).int
  if newPoints < 1:
    newPoints = 1
  points = newPoints

const diffTooltips: array[12, string] = ["Select game difficulty preset level.",
    "Percentage of damage done by enemy ships in combat. Lowering it makes the  game easier but lowers the amount of score gained as well.",
    "Percentage of damage done by the player's ship in combat. Raising it makes the game easier but lowers the amount of score gained as well.",
    "Percentage of damage done by enemies in melee combat. Lowering it makes the game easier but lowers the amount of score gained as well.",
    "Percentage of damage done by player's crew (and player character) in melee combat. Raising it makes the game easier but lowers the amount of score gained as well.",
    "Percentage of experience gained by player and their crew from actions. Raising it makes the game easier but lowers the amount of score gained as well.",
    "Percentage of reputation in bases gained or lost by player in sky bases due to player actions. Raising it makes the game easier but lowers the amount of score gained as well.",
    "Percentage of the standard material cost and time needed for upgrading ship modules. Lowering it makes the game easier but lowers the amount of score gained as well.",
    "Percentage of the standard prices for services in bases (docking, repairing ship,\nrecruiting new crew members, etc). Lowering it makes the game easier but lowers the amount of score gained as well.",
    "Select random values for all settings.",
    "If you select this option, all difficulty settings will be randomized during start new game. Not recommended for new players.", "Set difficulty of new game. Each value can be between 1 and 500. Each change has an impact not only on the game's difficulty but also on amount of points gained in the game. Select a field to get more information about it."]

proc newGameDifficulty() {.raises: [], tags: [RootEffect], contractual.} =
  ## Show the difficulty settings for starting a new game
  {.ruleOff: "varDeclared".}
  var
    bounds: array[11, NimRect]
  {.ruleOn: "varDeclared".}
  group(title = "groupSetting", flags = {windowNoFlags}):
    # Difficulty level
    setLayoutRowDynamic(height = 35, cols = 2, ratio = [0.5.cfloat, 0.5])
    label(str = "Difficulty level:")
    bounds[0] = getWidgetBounds()
    var newLevel: Natural = comboList(items = ["Very Easy", "Easy", "Normal",
        "Hard", "Very Hard", "Custom"], selected = currentLevel,
        itemHeight = 25, x = 200, y = 180)
    if newLevel != currentLevel:
      currentLevel = newLevel
      case currentLevel
      of 0:
        diffSettings = [10, 450, 10, 450, 450, 450, 10, 10]
      of 1:
        diffSettings = [50, 250, 50, 250, 250, 250, 50, 50]
      of 2:
        diffSettings = [100, 100, 100, 100, 100, 100, 100, 100]
      of 3:
        diffSettings = [250, 50, 250, 50, 50, 50, 250, 250]
      of 4:
        diffSettings = [450, 10, 450, 10, 10, 10, 450, 450]
      else:
        discard
      setPoints()
    const diffLabels: array[8, string] = ["Enemy ship damage:",
        "Player ship damage:", "Enemy damage in melee combat:",
        "Player crew damage in melee combat:", "Experience gained:",
        "Reputation gained:", "Upgrade cost:", "Prices in bases:"]
    for index, diffLabel in diffLabels:
      if index in {2, 3}:
        setLayoutRowDynamic(height = 50, cols = 2, ratio = [0.5.cfloat, 0.5])
        wrapLabel(str = diffLabel)
      else:
        setLayoutRowDynamic(height = 35, cols = 2, ratio = [0.5.cfloat, 0.5])
        label(str = diffLabel)
      bounds[index + 1] = getWidgetBounds()
      let newValue: int = property2(name = "#", min = 1, val = diffSettings[
          index], max = 500, step = 1, incPerPixel = 1)
      if newValue != diffSettings[index]:
        diffSettings[index] = newValue
        currentLevel = 5
        setPoints()
    # Randomize settings
    setLayoutRowDynamic(height = 35, cols = 1)
    bounds[9] = getWidgetBounds()
    labelButton(title = "Random"):
      for diffSetting in diffSettings.mitems:
        diffSetting = getRandom(min = 1, max = 500)
      currentLevel = 5
      setPoints()
    # Randomize the settings on the game's start
    setLayoutRowDynamic(height = 50, cols = 2, ratio = [0.9.cfloat, 0.1])
    label(str = "Randomize difficulty on game start:")
    bounds[10] = getWidgetBounds()
    checkbox(label = "", checked = randomSettings)
    # Total gained points
    setLayoutRowDynamic(height = 35, cols = 2, ratio = [0.7.cfloat, 0.3])
    label(str = "Total gained points:")
    label(str = $points & "%")
    for index, bound in bounds:
      if mouseClicked(id = left, rect = bound):
        infoText = diffTooltips[index]
    if gameSettings.showTooltips:
      for index, bound in bounds:
        addTooltip(bounds = bound, text = diffTooltips[index])

proc startGame(dialog: var GameDialog) {.raises: [], tags: [RootEffect],
    contractual.} =
  ## Start the new game
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  newGameSettings.playerGender = (if playerGender == 3: 'F' else: 'M')
  if selectedGoal == "Random":
    clearCurrentGoal()
    currentGoal = try:
        goalsList[getRandom(min = 1, max = goalsList.len)]
      except:
        try:
          goalsList[getRandom(min = 1, max = goalsList.len)]
        except:
          dialog = setError(message = "Can't set the current goal.")
          return
  newGameSettings.playerName = playerName
  newGameSettings.shipName = shipName
  if currentFaction == playerFactions.high:
    newGameSettings.playerFaction = "random"
  else:
    block setFaction:
      for index, faction in factionsList:
        if faction.name == playerFactions[currentFaction]:
          newGameSettings.playerFaction = index
          for key, career in faction.careers:
            if career.name == playerCareers[currentCareer]:
              newGameSettings.playerCareer = key
              break setFaction
  if currentCareer == playerCareers.high:
    newGameSettings.playerCareer = "random"
  newGameSettings.startingBase = "Any"
  for index, baseType in basesTypesList:
    if baseType.name == playerBases[currentBase]:
      newGameSettings.startingBase = index
      break
  newGameSettings.difficultyLevel = currentLevel.DifficultyType
  newGameSettings.enemyDamageBonus = diffSettings[0].float / 100.0
  newGameSettings.playerDamageBonus = diffSettings[1].float / 100.0
  newGameSettings.enemyMeleeDamageBonus = diffSettings[2].float / 100.0
  newGameSettings.playerMeleeDamageBonus = diffSettings[3].float / 100.0
  newGameSettings.experienceBonus = diffSettings[4].float / 100.0
  newGameSettings.reputationBonus = diffSettings[5].float / 100.0
  newGameSettings.upgradeCostBonus = diffSettings[6].float / 100.0
  newGameSettings.pricesBonus = diffSettings[7].float / 100.0
  try:
    newGame()
  except:
    dialog = setError(message = "Can't start the new game.")

proc newGame*(state: var GameState; dialog: var GameDialog) {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Start the new game settings
  ##
  ## * state  - the current game's state
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter state and dialog. The latter is modified if
  ## any error happened.
  var editActive: bool = windowEditActive(name = "Main") or
      windowPropertyActive(name = "Main")
  changeStyle(field = spacing, x = 0, y = 0):
    changeStyle(field = buttonRounding, value = 0):
      layoutSpaceStatic(height = 30, widgetsCount = 2):
        var x: float = 200
        const
          tabs: array[2, string] = ["Player", "Difficulty"]
          tabTooltips: array[2, string] = ["Show settings for your character.", "Show settings for the game difficulty."]
        for index, tab in tabs:
          try:
            let
              textWidth: float = getTextWidth(text = tab)
              widgetWidth: float = textWidth + 15 * getButtonStyle(
                  field = padding).x;
            row(x = x, y = 0, w = widgetWidth, h = 30):
              if currentTab == index:
                changeStyle(src = active, dest = normal):
                  if gameSettings.showTooltips:
                    addTooltip(bounds = getWidgetBounds(), text = tabTooltips[index])
                  labelButton(title = tab):
                    discard
              else:
                if gameSettings.showTooltips:
                  addTooltip(bounds = getWidgetBounds(), text = tabTooltips[index])
                labelButton(title = tab):
                  currentTab = index.cint
                  infoText = (if index == 0: playerTooltips[
                      8] else: diffTooltips[^1])
            x += widgetWidth
          except:
            dialog = setError(message = "Can't set the tabs buttons.")
  layoutSpaceStatic(height = (menuHeight - 90).float, widgetsCount = (
      if currentTab == 0: 17 else: 20)):
    row(x = 0, y = 0, w = (menuWidth.float * 0.65), h = (menuHeight - 90).float):
      # Player settings
      if currentTab == 0:
        newGamePlayer(dialog = dialog)
      # Difficulty settings
      else:
        newGameDifficulty()
    let infoWidth: float = (menuWidth.float * 0.35)
    row(x = (menuWidth.float * 0.65), y = 0, w = infoWidth, h = (menuHeight - 90).float):
      fileLines = 3
      for line in infoText.split(sep = "\n\n"):
        var needLines: float = try:
            ceil(x = getTextWidth(text = line) / (infoWidth - 35.0))
          except:
            dialog = setError(message = "Can't count the line height.")
            return
        if needLines < 1.0:
          needLines = 1.0
        fileLines += needLines.int
      fileLines *= 25
      group(title = "Info", flags = {windowBorder, windowTitle}):
        setLayoutRowDynamic(height = fileLines.float, cols = 1)
        wrapLabel(str = infoText)
  layoutSpaceStatic(height = 50, widgetsCount = 2):
    row(x = 140, y = 0, w = 155, h = 40):
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Start the game")
      labelButton(title = "Start game"):
        startGame(dialog = dialog)
        if dialog == none:
          setGame(dialog = dialog)
          if dialog == none:
            state = map
    row(x = 300.float, y = 0, w = 140, h = 40):
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(), text = "Back to the main menu")
      labelButton(title = "Back to menu"):
        state = mainMenu
        return
  if isKeyPressed(key = keyEscape) and not editActive:
    if dialog == none:
      state = mainMenu
    else:
      dialog = none

proc backToMainMenu*(state: var GameState; dialog: var GameDialog) {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Return to the game's main menu and set the game's state
  ##
  ## * state - the current game's state
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter state and dialog. The latter is modified if
  ## any error happened.
  setMainMenu(dialog = dialog)
  showMainMenu(state = state, dialog = dialog)
  state = mainMenu
