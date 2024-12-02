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

## Provides code related to the game's main menu, like showing the
## menu, and selecting its various sections

import std/[algorithm, math, os, sequtils, strutils, tables, times]
import contracts, nuklear/nuklear_sdl_renderer, nimalyzer
import ../[basestypes, config, game, gamesaveload, halloffame, shipscrew, ships2]
import coreui, dialogs, errordialog, goalsui


{.push ruleOff: "varDeclared".}
var
  menuImages: array[4, PImage]
{.push ruleOn: "varDeclared".}
var
  showLoadButton, showHoFButton: bool = false
  fileContent: string = ""
  fileName: string = ""
  fileLines: Positive = 1
  playerFactions, playerCareers, playerBases: seq[string] = @[]
  currentFaction, currentCareer, currentBase: int = 0
  newFaction, newCareer, newBase: Natural = 0
  menuWidth*: Positive = 600  ## The width of the game's main window
  menuHeight*: Positive = 400 ## The height of the game's main window

proc setMainMenu*(dialog: var GameDialog) {.raises: [], tags: [
    ReadDirEffect, WriteIOEffect, TimeEffect, RootEffect], contractual.} =
  ## Set the main menu, load logo if needed and set the menu's buttons
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns parameter dialog, modified if any error happened.
  if menuImages[0] == nil:
    # Load images
    const fileNames: array[1..3, string] = ["random", "male", "female"]
    try:
      menuImages[0] = nuklearLoadSVGImage(filePath = dataDirectory & "ui" &
          DirSep & "images" & DirSep & "logo.svg", width = 0, height = 110)
      for index, fileName in fileNames:
        menuImages[index] = nuklearLoadSVGImage(filePath = dataDirectory &
            "ui" & DirSep & "images" & DirSep & "ui" & DirSep & fileName &
            ".svg", width = 0,
            height = 10 + gameSettings.interfaceFontSize)
    except:
      dialog = setError(message = "Can't set the game's images.")
    # Set the list of available factions
    for index, faction in factionsList:
      playerFactions.add(y = faction.name)
      if index == newGameSettings.playerFaction:
        currentFaction = playerFactions.high
        var i: Natural = 0
        for index, career in faction.careers:
          playerCareers.add(y = career.name)
          if index == newGameSettings.playerCareer:
            currentCareer = i
          i.inc
        i = 0
        for baseType in faction.basesTypes.keys:
          try:
            playerBases.add(y = basesTypesList[baseType].name)
          except:
            dialog = setError(message = "Can't add a base type.")
            break
          if baseType == newGameSettings.startingBase:
            currentBase = i
            break
          i.inc
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
      labelButton(title = "(c)2016-2024 Bartek thindil Jasicki"):
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
    colorLabel(str = "Position", r = 255, g = 255, b = 0, align = centered)
    colorLabel(str = "Name", r = 255, g = 255, b = 0, align = centered)
    colorLabel(str = "Points", r = 255, g = 255, b = 0, align = centered)
    colorLabel(str = "Died from", r = 255, g = 255, b = 0, align = centered)
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

type
  SortingOrder = enum
    playerAsc, playerDesc, shipAsc, shipDesc, timeAsc, timeDesc
  SaveData = object
    playerName, shipName, saveTime, path: string

var
  sortOrder: SortingOrder = timeDesc
  saveClicked: string = ""
  saves: seq[SaveData] = @[]

proc showLoadMenu*(dialog: var GameDialog) {.raises: [], tags: [RootEffect],
    contractual.} =
  ## Show the menu for the selected saved game
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the parameters state and dialog. It is modified only
  ## when the player closed the dialog.
  window(name = "Actions", x = (menuWidth / 3).float, y = (menuHeight /
      3).float, w = 150, h = 150, flags = {windowBorder, windowMoveable,
      windowTitle, windowNoScrollbar}):
    setLayoutRowDynamic(height = 30, cols = 1)
    labelButton(title = "Load game"):
      dialog = loading
    labelButton(title = "Delete game"):
      setQuestion(question = "Are you sure you want delete this savegame?",
          data = saveClicked, qType = deleteSave, dialog = dialog)
      dialog = questionDialog
    labelButton(title = "Close"):
      dialog = none

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
    setButtonStyle(field = normal, r = 18, g = 13, b = 13)
    setButtonStyle(field = rounding, value = 0)
    setButtonStyle(field = border, value = 0)
    layoutSpaceStatic(height = (saves.len * 30).float, widgetsCount = (
        saves.len * 3)):
      for index, save in saves:
        row(x = 0, y = (index * 30).float, w = 190, h = 30):
          if gameSettings.showTooltips:
            addTooltip(bounds = getWidgetBounds(), text = "Press mouse " & (
                if gameSettings.rightButton: "right" else: "left") & " button to show available option")
          labelButton(title = save.playerName):
            dialog = loadMenu
            saveClicked = save.path
        row(x = 190, y = (index * 30).float, w = 190, h = 30):
          if gameSettings.showTooltips:
            addTooltip(bounds = getWidgetBounds(), text = "Press mouse " & (
                if gameSettings.rightButton: "right" else: "left") & " button to show available option")
          labelButton(title = save.shipName):
            dialog = loadMenu
            saveClicked = save.path
        row(x = 380, y = (index * 30).float, w = 190, h = 30):
          if gameSettings.showTooltips:
            addTooltip(bounds = getWidgetBounds(), text = "Press mouse " & (
                if gameSettings.rightButton: "right" else: "left") & " button to show available option")
          labelButton(title = save.saveTime):
            dialog = loadMenu
            saveClicked = save.path
    restoreButtonStyle()
  layoutSpaceStatic(height = 50, widgetsCount = 1):
    row(x = (menuWidth - 150).float, y = 0, w = 140, h = 40):
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(), text = "Back to the main menu")
      labelButton(title = "Back to menu"):
        state = mainMenu
        saveClicked = ""

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
  state = map
  dialog = none

const playerTooltips: array[9, string] = [
    "Enter character name.", "Select a random name for the character, based on the character gender",
    "Enter ship name.",
    "Select a random name for the character, based on the character gender",
    "Select starting goal for your character. You can change it later in game.",
    "Select your faction from a list. Factions have the biggest impact on game. They determine the amount of bases and some playing styles. More information about each faction can be found after selecting it. You can't change this later.",
    "Select your career from a list. Careers have some impact on gameplay (each have bonuses to gaining experience in some fields plus they determine your starting ship and crew). More info about each career can be found after selecting it. You can't change career later.",
    "Select type of base in which you will start the game. This may have some impact on game difficulty.",
  "General player character settings. Select field which you want to set to see more information about."]

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
    for faction in factionsList.values:
      if faction.name == playerFactions[newFaction]:
        infoText = playerTooltips[5] & "\n\n" & faction.description
        return
  if currentCareer == -1:
    currentCareer = newCareer
    for faction in factionsList.values:
      if faction.name == playerFactions[newFaction]:
        for career in faction.careers.values:
          if career.name == playerCareers[newCareer]:
            infoText = playerTooltips[6] & "\n\n" & career.description
            return
  if currentBase == -1:
    currentBase = newBase
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
  # Character's name
  setLayoutRowDynamic(height = 35, cols = 3, ratio = [0.4.cfloat, 0.5, 0.1])
  label(str = "Character name:")
  bounds[0] = getWidgetBounds()
  editString(text = playerName, maxLen = 64)
  bounds[1] = getWidgetBounds()
  saveButtonStyle()
  setButtonStyle(field = padding, value = NimVec2(x: 0.0, y: 0.0))
  imageButton(image = menuImages[1]):
    randomName(forPlayer = true)
  restoreButtonStyle()
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
    for faction in factionsList.values:
      if faction.name == playerFactions[newFaction]:
        playerCareers = @[]
        currentCareer = 0
        for career in faction.careers.values:
          playerCareers.add(y = career.name)
        playerBases = @[]
        currentBase = 0
        for baseType in faction.basesTypes.keys:
          try:
            playerBases.add(y = basesTypesList[baseType].name)
          except:
            dialog = setError(message = "Can't add a base type.")
            break
        break
  # Character's career
  label(str = "Character career:")
  bounds[6] = getWidgetBounds()
  newCareer = comboList(items = playerCareers,
      selected = currentCareer, itemHeight = 25, x = 200, y = 125)
  if newCareer != currentCareer or mouseClicked(id = left,
      rect = bounds[6]):
    currentCareer = -1
  # Starting base
  label(str = "Starting base type:")
  bounds[7] = getWidgetBounds()
  newBase = comboList(items = playerBases, selected = currentBase,
      itemHeight = 25, x = 200, y = 90)
  if newBase != currentBase or mouseClicked(id = left, rect = bounds[
      7]):
    currentBase = -1
  setInfoText(dialog = dialog)
  if gameSettings.showTooltips:
    for index, bound in bounds:
      addTooltip(bounds = bound, text = playerTooltips[index])

var
  currentLevel: Natural = 2
  enemyDamage, playerDamage, enemyMelee, playerMelee, expBonus, repBonus: Positive = 100

proc newGameDifficulty() {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Show the difficulty settings for starting a new game
  {.ruleOff: "varDeclared".}
  var
    bounds: array[8, NimRect]
  {.ruleOn: "varDeclared".}
  # Difficulty level
  setLayoutRowDynamic(height = 35, cols = 2, ratio = [0.5.cfloat, 0.5])
  label(str = "Difficulty level:")
  bounds[0] = getWidgetBounds()
  var newLevel: Natural = comboList(items = ["Very Easy", "Easy", "Normal", "Hard",
      "Very Hard", "Custom"], selected = currentLevel, itemHeight = 25, x = 200, y = 90)
  if newLevel != currentLevel:
    currentLevel = newLevel
  # Enemy ship damage
  setLayoutRowDynamic(height = 35, cols = 2, ratio = [0.5.cfloat, 0.5])
  label(str = "Enemy ship damage:")
  bounds[1] = getWidgetBounds()
  property(name = "#", min = 1, val = enemyDamage, max = 500, step = 1,
      incPerPixel = 1)
  # Player's ship damage
  setLayoutRowDynamic(height = 35, cols = 2, ratio = [0.5.cfloat, 0.5])
  label(str = "Player ship damage:")
  bounds[2] = getWidgetBounds()
  property(name = "#", min = 1, val = playerDamage, max = 500, step = 1,
      incPerPixel = 1)
  # Enemy damage in melee combat
  setLayoutRowDynamic(height = 50, cols = 2, ratio = [0.5.cfloat, 0.5])
  wrapLabel(str = "Enemy damage in melee combat:")
  bounds[3] = getWidgetBounds()
  property(name = "#", min = 1, val = enemyMelee, max = 500, step = 1,
      incPerPixel = 1)
  # Player's crew damage in melee combat
  setLayoutRowDynamic(height = 50, cols = 2, ratio = [0.5.cfloat, 0.5])
  wrapLabel(str = "Player crew damage in melee combat:")
  bounds[4] = getWidgetBounds()
  property(name = "#", min = 1, val = playerMelee, max = 500, step = 1,
      incPerPixel = 1)
  # Experience
  setLayoutRowDynamic(height = 35, cols = 2, ratio = [0.5.cfloat, 0.5])
  label(str = "Experience gained:")
  bounds[5] = getWidgetBounds()
  property(name = "#", min = 1, val = expBonus, max = 500, step = 1,
      incPerPixel = 1)
  # Reputation
  setLayoutRowDynamic(height = 35, cols = 2, ratio = [0.5.cfloat, 0.5])
  label(str = "Reputation gained:")
  bounds[6] = getWidgetBounds()
  property(name = "#", min = 1, val = repBonus, max = 500, step = 1,
      incPerPixel = 1)

proc newGame*(state: var GameState; dialog: var GameDialog) {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Start the new game settings
  ##
  ## * state  - the current game's state
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter state and dialog. The latter is modified if
  ## any error happened.
  stylePushVec2(field = spacing, x = 0, y = 0)
  stylePushFloat(field = rounding, value = 0)
  layoutSpaceStatic(height = 30, widgetsCount = 2):
    var x: float = 200
    const tabs: array[2, string] = ["Player", "Difficulty"]
    for index, tab in tabs:
      try:
        let
          textWidth: float = getTextWidth(text = tab)
          widgetWidth: float = textWidth + 15 * getButtonStyle(
              field = padding).x;
        row(x = x, y = 0, w = widgetWidth, h = 30):
          if currentTab == index:
            saveButtonStyle()
            setButtonStyle2(source = active, destination = normal)
            if gameSettings.showTooltips:
              addTooltip(bounds = getWidgetBounds(),
                  text = "Show settings for your character.")
            labelButton(title = tab):
              currentTab = index.cint
            restoreButtonStyle()
          else:
            if gameSettings.showTooltips:
              addTooltip(bounds = getWidgetBounds(),
                  text = "Show settings for the game difficulty.")
            labelButton(title = tab):
              currentTab = index.cint
        x += widgetWidth
      except:
        dialog = setError(message = "Can't set the tabs buttons.")
  stylePopFloat()
  stylePopVec2()
  layoutSpaceStatic(height = (menuHeight - 90).float, widgetsCount = 17):
    row(x = 0, y = 0, w = (menuWidth.float * 0.65), h = (menuHeight - 90).float):
      group(title = "groupSetting", flags = {windowNoScrollbar}):
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
        echo "button pressed"
    row(x = 300.float, y = 0, w = 140, h = 40):
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(), text = "Back to the main menu")
      labelButton(title = "Back to menu"):
        state = mainMenu
        return
