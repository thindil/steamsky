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
# along with Steam Sky.  If not, see <http://www.gnu.org/licenses/>.

## The main game module. Read the game configuration, command line arguments,
## initialize graphic and start the game.

import std/[os, parseopt, strutils, tables, times]
import contracts, newui/nuklear/nuklear_sdl_renderer
import config, halloffame, game, game2, log
import newui/[baseslootui, basesschoolui, basesrecruitui, basesshipyardui,
    basesui, combatui, coreui, craftsui, dialogs, errordialog, goalsui, header,
    mainmenu, mapsui, missionsui, shipsui, shipsuicrew, shipsuicrewinventory,
    shipsuimodules, themes, tradesui, waitmenu]

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
    quit "Can't load hall of fame"

  const windowName: string = "Steam Sky"

  # Initialize SDL and create the main window of the game
  nuklearInit(windowWidth = menuWidth, windowHeight = menuHeight,
      name = windowName, iconPath = dataDirectory & "ui" & DirSep & "images" &
      DirSep & "icon.png")
  # Load the game's theme
  loadThemes()
  # Load the game's fonts
  try:
    fonts[UIFont] = nuklearLoadFont(font = FontData(path: themesList[
        gameSettings.interfaceTheme].fonts[UIFont],
        size: gameSettings.interfaceFontSize + 10))
    fonts[FontsNames.mapFont] = nuklearLoadFont(font = FontData(
        path: themesList[gameSettings.interfaceTheme].fonts[FontsNames.mapFont],
        size: gameSettings.mapFontSize + 10), glyphsRanges = [0x0020.nk_rune,
        0x00ff, 0x2000, 0xffff, 0])
  except:
    quit "Can't load the game's fonts."
  nuklearSetDefaultFont(defaultFont = fonts[UIFont],
      fontSize = gameSettings.interfaceFontSize + 10)
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
  setTooltips(tDelay = 1_000, fDelay = 200)
  const
    showGame: array[GameState.mainMenu..GameState.crafting, proc (
        state: var GameState; dialog: var GameDialog){.nimcall, raises: [
            ].}] = [
      GameState.mainMenu: showMainMenu, news: showNews, allNews: showNews,
        about: showAbout, showFile: mainMenu.showFile,
        hallOfFame: showHallOfFame, loadGame: showLoadGame,
        loadingGame: mainMenu.loadGame, newGame: mainMenu.newGame, map: showMap,
        endGame: backToMainMenu, combat: showCombat, boarding: showBoarding,
        trade: showTrade, school: showSchool, recruits: showRecruits,
        healWounded: showWounded, repairShip: showRepairs,
        buyRecipes: showRecipes, shipyard: showShipyard,
        baseMissions: showMissions, loot: showLoot, shipInfo: showShipInfo,
        crafting: showCrafting]
    showDialog: array[GameDialog.errorDialog..GameDialog.assignSkillDialog,
        proc(dialog: var GameDialog){.nimcall, raises: [].}] = [
      GameDialog.errorDialog: showError, waitDialog: showWaitMenu,
        newGoalDialog: showGoals, boardingDialog: showPartyMenu,
        defendingDialog: showPartyMenu, recruitDialog: showRecruitInfo,
        negotiateDialog: showNegotiate,
        moduleDialog: basesshipyardui.showModuleInfo,
        missionDialog: showMissionInfo, acceptMissionDialog: showAcceptMission,
        renameDialog: showRenameDialog, giveOrderDialog: showGiveOrder,
        memberDialog: showMemberInfo, renameMemberDialog: showRenameDialog,
        inventoryDialog: showMemberInventory,
        moduleInfoDialog: shipsuimodules.showModuleInfo,
        renameModuleDialog: showRenameDialog,
        assignCrewDialog: showAssignCrewDialog,
        assignAmmoDialog: showAssignAmmoDialog,
        assignSkillDialog: showAssignSkillDialog]
  windowWidth = menuWidth.float
  windowHeight = menuHeight.float
  var
    redrawTime: float = 1_000.0
  while true:
    let started: float = cpuTime()
    if redraw:
      # Reset the UI tooltips if enabled
      if gameSettings.showTooltips:
        resetTooltips()

      # Show dialogs if needed
      if dialog in GameDialog.errorDialog..GameDialog.assignSkillDialog:
        showDialog[dialog](dialog = dialog)
      elif dialog in buyDialog..dropCargoDialog:
        updateData = showManipulateItem(dialog = dialog)
      elif dialog == gameMenuDialog:
        showGameMenu(dialog = dialog, state = state)

      # The main window
      window(name = "Main", x = 0, y = 0, w = windowWidth,
          h = windowHeight, flags = {windowNoScrollbar}):
        # Disable the window if there is any dialog to show
        if dialog != none:
          windowDisable()
        let
          oldState: GameState = state
          oldDialog: GameDialog = dialog
        if state in GameState.mainMenu..GameState.crafting:
          # Show the proper window
          showGame[state](state = state, dialog = dialog)
        # Add the tooltips, if enabled
        if gameSettings.showTooltips:
          showTooltips()
        # Set the UI to redraw if state or dialog changed
        if oldState != state or oldDialog != dialog:
          redraw = true

      # Start loading the game
      if dialog == loading:
        state = loadingGame
        dialog = none

      # Quit from the game
      if state == quitGame:
        break

      # Draw
      nuklearDraw()

    # Input
    case nuklearInput()
    of quitEvent:
      break
    of sizeChangedEvent:
      (windowWidth, windowHeight) = nuklearGetWindowSize()
      redraw = true
    of noEvent:
      redraw = state == loadingGame
    else:
      redraw = true

    # Timing
    let dt: float = cpuTime() - started
    if (dt < dtime):
      sleep(milsecs = (dtime - dt).int)
    updateTimer(timeDiff = dtime)

    # Force to redraw every 1 sec
    if not redraw:
      redrawTime -= dtime
      if redrawTime <= 0.0:
        redraw = true
        redrawTime = 1_000.0

  nuklearClose()

# Run the game
steamsky()
