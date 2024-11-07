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

import std/[os, sequtils]
import contracts, nuklear/nuklear_sdl_renderer
import ../[config, game]
import coreui, errordialog

var
  logo: PImage = nil
  showLoadButton, showHoFButton: bool = false

proc setMainMenu*(dialog: var GameDialog) {.raises: [], tags: [
    ReadDirEffect, WriteIOEffect, TimeEffect, RootEffect], contractual.} =
  ## Set the main menu, load logo if needed and set the menu's buttons
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns parameter dialog, modified if any error happened.
  if logo == nil:
    try:
      logo = nuklearLoadSVGImage(filePath = dataDirectory & "ui" & DirSep &
          "images" & DirSep & "logo.svg", width = 0, height = 110)
    except:
      dialog = setError(message = "Can't set the game's logo.")
  showLoadButton = walkFiles(pattern = saveDirectory & "*.sav").toSeq.len > 0
  showHoFButton = fileExists(filename = saveDirectory & "halloffame.dat")

proc showMainMenu*(state: var GameState) {.raises: [], tags: [], contractual.} =
  ## Show the game's main menu and set the game's state
  ##
  ## * state - the current game's state
  ##
  ## Returns the modified parameter state.
  if gameSettings.showTooltips:
    resetTooltips()
  layoutSpaceStatic(height = 90, widgetsCount = 1):
    row(x = 50, y = 0, w = 500, h = 90):
      image(image = logo)
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
        echo "button pressed"
    var y: float = h;
    if showLoadButton:
      row(x = x, y = y, w = w, h = h):
        y += h
        if gameSettings.showTooltips:
          addTooltip(bounds = getWidgetBounds(),
              text = "Load one of the previously saved games")
        labelButton(title = "Load game"):
          echo "button pressed"
    if showHoFButton:
      row(x = x, y = y, w = w, h = h):
        y += h
        if gameSettings.showTooltips:
          addTooltip(bounds = getWidgetBounds(),
              text = "Show your previous the bests scores in the game")
        labelButton(title = "Hall of Fame"):
          echo "button pressed"
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
  if gameSettings.showTooltips:
    showTooltips()

proc showNews*(state: var GameState; dialog: var GameDialog) {.raises: [], tags: [ReadDirEffect,
    ReadIOEffect, WriteIOEffect, TimeEffect, RootEffect], contractual.} =
  ## Show the game's latest changes
  ## * state  - the current game's state
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter state and dialog. The latter is modified if
  ## any error happened.
  if gameSettings.showTooltips:
    resetTooltips()
  setLayoutRowDynamic(height = (windowHeight - 50).float, cols = 1)
  if fileExists(filename = docDirectory & "CHANGELOG.md"):
    if dialog == none:
      group(title = "NewsGroup", flags = {}):
        try:
          setLayoutRowDynamic(height = 25, cols = 1)
          var index: Natural = 0
          for line in lines(filename = docDirectory & "CHANGELOG.md"):
            index.inc
            if index < 6:
              continue
            if state == news and line.len > 1 and line[0..2] == "## ":
              break
            wrapLabel(str = line)
        except:
          dialog = setError(message = "Can't show ChangeLog file.")
  else:
    wrapLabel(str = "Can't find file to load. Did 'CHANGELOG.md' file is in '" &
        docDirectory & "' directory?")
  layoutSpaceStatic(height = 50, widgetsCount = 2):
    if state == news:
      row(x = (windowWidth - 310).float, y = 0, w = 155, h = 40):
        if gameSettings.showTooltips:
          addTooltip(bounds = getWidgetBounds(),
              text = "Show all changes to the game since previous big stable version")
        labelButton(title = "Show all changes"):
          state = allNews
          return
    else:
      row(x = (windowWidth - 405).float, y = 0, w = 250, h = 40):
        if gameSettings.showTooltips:
          addTooltip(bounds = getWidgetBounds(),
              text = "Show only changes to the game since previous relese")
        labelButton(title = "Show only newest changes"):
          state = news
          return
    row(x = (windowWidth - 150).float, y = 0, w = 140, h = 40):
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(), text = "Back to the main menu")
      labelButton(title = "Back to menu"):
        state = mainMenu
        return
  if gameSettings.showTooltips:
    showTooltips()

proc showAbout*(state: var GameState) {.raises: [], tags: [], contractual.} =
  ## Show the general information about the game
  ## * state - the current game's state
  ##
  ## Returns the modified parameter state.
  setLayoutRowDynamic(height = 30, cols = 1)
  label(str = "Roguelike in the sky with a steampunk theme",
      alignment = centered)
  saveButtonStyle()
  setButtonStyle(field = borderColor, r = 45, g = 45, b = 45)
  labelButton(title = "Website"):
    echo "button pressed"
  restoreButtonStyle()
  state = about
