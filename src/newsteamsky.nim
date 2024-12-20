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
# along with Steam Sky.  If not, see <http://www.gnu.org/licenses/>.

## The main game module. Read the game configuration, command line arguments,
## initialize graphic and start the game.

import std/[os, parseopt, strutils, times]
import contracts, newui/nuklear/nuklear_sdl_renderer
import config, halloffame, game, game2, log
import newui/[coreui, dialogs, errordialog, goalsui, mainmenu, mapsui, themes]

proc steamsky() {.raises: [], tags: [ReadIOEffect, RootEffect], contractual.} =
  ## The main procedure of the game.

  try:
    setCurrentDir(newDir = getAppDir())
  except:
    echo "Can't set the current directory for the game."
    return
  # Get the command line params if any
  var gameParams: OptParser = initOptParser()
  for kind, key, val in gameParams.getopt():
    case key
    of "savedir":
      saveDirectory = val & DirSep
      normalizePath(path = saveDirectory)
    of "modsdir":
      modsDirectory = val & DirSep
      normalizePath(path = modsDirectory)
    of "datadir":
      dataDirectory = val & DirSep
      normalizePath(path = dataDirectory)
      if not dirExists(dir = dataDirectory):
        echo "Directory " & dataDirectory & " does not exists. You must use an existing directory as Data directory"
        return
    of "docdir":
      docDirectory = val & DirSep
      normalizePath(path = docDirectory)
      if not dirExists(dir = docDirectory):
        echo "Directory " & docDirectory & " does not exists. You must use an existing directory as Documentation directory"
        return
    of "themesdir":
      themesDirectory = val & DirSep
      normalizePath(path = themesDirectory)
    of "debug":
      try:
        debugMode = parseEnum[DebugLevels](s = val)
      except:
        echo "Invalid debug value: " & val
        return
  # Create the game directories
  try:
    createDir(dir = saveDirectory)
    createDir(dir = modsDirectory)
    createDir(dir = themesDirectory)
  except:
    echo "Can't create directories"
    return

  # Start logging
  startLogging()

  # Load the game configuration.
  loadConfig()

  # Load the hall of fame
  try:
    loadHallOfFame()
  except:
    echo "Can't load hall of fame"
    return

  const windowName: string = "Steam Sky"

  # Initialize SDL and create the main window of the game
  nuklearInit(windowWidth = menuWidth, windowHeight = menuHeight,
      name = windowName, iconPath = dataDirectory & "ui" & DirSep & "images" &
      DirSep & "icon.png")
  # Load the game's fonts
  fonts.add(y = nuklearLoadFont(font = FontData(path: dataDirectory & "ui" &
      DirSep & "fonts" & DirSep & "Amarante-Regular.ttf",
      size: gameSettings.interfaceFontSize + 10)))
  nuklearSetDefaultFont(defaultFont = fonts[0],
      fontSize = gameSettings.interfaceFontSize + 10)
  # Load the game's theme
  loadThemes()
  var
    state: GameState = mainMenu
    dialog: GameDialog = none
  try:
    discard loadGameData()
  except:
    dialog = setError(message = "Can't load the game's data.")

  # Set the main menu
  setMainMenu(dialog = dialog)
  if dialog != none:
    echo "Can't set the main menu. More details in error.log"
    return

  # Set the goals UI
  setGoalsUi(dialog = dialog)
  if dialog != none:
    echo "Can't set the goals UI. More details in error.log"
    return

  # The main game loop
  setTooltips(tDelay = 1_000, fDelay = dtime)
  const showGame: array[GameState.mainMenu..GameState.map, proc (
      state: var GameState; dialog: var GameDialog){.nimcall, raises: [].}] = [
    GameState.mainMenu: showMainMenu, news: showNews, allNews: showNews,
      about: showAbout, showFile: mainMenu.showFile, hallOfFame: showHallOfFame,
      loadGame: showLoadGame, loadingGame: mainMenu.loadGame,
      newGame: mainMenu.newGame, map: showMap]
  const showDialog: array[GameDialog.errorDialog..GameDialog.newGoalDialog, proc (
      dialog: var GameDialog){.nimcall, raises: [].}] = [
    GameDialog.errorDialog: showError, loadMenu: showLoadMenu,
      questionDialog: showQuestion, newGoalDialog: showGoals]
  windowWidth = menuWidth.float
  windowHeight = menuHeight.float
  while true:
    let started: float = cpuTime()
    # Input
    case nuklearInput()
    of quitEvent:
      break
    of sizeChangedEvent:
      (windowWidth, windowHeight) = nuklearGetWindowSize()
    of noEvent:
      discard

    # Reset the UI tooltips if enabled
    if gameSettings.showTooltips:
      resetTooltips()

    # The main window
    window(name = "Main", x = 0, y = 0, w = windowWidth,
        h = windowHeight, flags = {windowNoScrollbar}):
      if state in GameState.mainMenu..GameState.map:
        # Show the proper window
        showGame[state](state = state, dialog = dialog)
      # Add the tooltips, if enabled
      if gameSettings.showTooltips:
        showTooltips()

    # Quit from the game
    if state == quitGame:
      break

    # Dialogs if needed
    case dialog
    of GameDialog.errorDialog..newGoalDialog:
      # Show the dialog
      showDialog[dialog](dialog = dialog)
    of loading:
      # Start loading the game
      state = loadingGame
      dialog = none
    of none:
      # No dialog to show
      discard

    # Draw
    nuklearDraw()

    # Timing
    let dt: float = cpuTime() - started
    if (dt < dtime):
      sleep(milsecs = (dtime - dt).int)

  nuklearClose()

# Run the game
steamsky()
