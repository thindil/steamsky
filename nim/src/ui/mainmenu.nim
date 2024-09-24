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

import std/[os, strutils, tables]
import ../[config, events, game, game2, tk]
import dialogs2, errordialog, goalsui, mainmenucommands, showmainmenu, table,
    themes, utilsui, utilsui2

proc startGame() {.sideEffect, raises: [], tags: [WriteIOEffect, TimeEffect,
    ReadIOEffect, RootEffect], exportc.} =
  ##  Start the game
  let mainWindow = "."
  var x: int = try:
      ((tclEval2(script = "winfo vrootwidth " & mainWindow).parseInt -
        gameSettings.windowWidth) / 2).int
    except:
      showError(message = "Can't get window X position")
      return
  if x < 0:
    x = 0
  var y: int = try:
      ((tclEval2(script = "winfo vrootheight " & mainWindow).parseInt -
        gameSettings.windowHeight) / 2).int
    except:
      showError(message = "Can't get window Y position")
      return
  if y < 0:
    y = 0
  tclEval(script = "wm geometry . " & $gameSettings.windowWidth & "x" &
      $gameSettings.windowHeight & "+" & $x & "+" & $y)
  try:
    generateTraders()
  except:
    showError(message = "Can't generate traders")
  #createGameUi()

var dataError: string

proc createMainMenu*() =
  let
    uiDirectory = dataDirectory & "ui" & DirSep
    iconPath = uiDirectory & "images" & DirSep & "icon.png"
    mainWindow = "."
  if not dirExists(iconPath):
    tclEval(script = "wm withdraw " & mainWindow)
    tclEval(script = "tk_messageBox -message {Couldn't not find the game data files and the game have to stop. Are you sure that directory \"" &
        dataDirectory & "\" is the proper place where the game data files exists?} -icon error -type ok")
    tclEval(script = "exit 1")
    return
  mainmenucommands.addCommands()
  dialogs2.addCommands()
  utilsui.addCommands()
  goalsui.addCommands()
  table.addCommands()
  let icon = tclEval2(script = "image create photo logo -file {" & iconPath & "}")
  tclEval(script = "wm iconphoto . -default " & icon)
  tclEvalFile(fileName = themesList[gameSettings.interfaceTheme].fileName)
  tclEval(script = "ttk::style theme use " & gameSettings.interfaceTheme)
  loadThemeImages()
  tclEvalFile(fileName = uiDirectory & "mainmenu.tcl")
  if not gameSettings.showTooltips:
    tclEval(script = "tooltip::tooltip disable")
  setFonts(newSize = gameSettings.mapFontSize, fontType = mapFont)
  setFonts(newSize = gameSettings.helpFontSize, fontType = helpFont)
  setFonts(newSize = gameSettings.interfaceFontSize, fontType = interfaceFont)
  let versionLabel = ".mainmenu.version"
  tclEval(script = versionLabel & " configure -text {" & gameVersion & " development}")
  dataError = loadGameData()
  if dataError.len > 0:
    showMainMenu()
    return
  let playerFrameName = ".newgamemenu.canvas.player"
  var textEntry = playerFrameName & ".playername"
  tclEval(script = textEntry & " delete 0 end")
  tclEval(script = textEntry & " insert 0 {" & newGameSettings.playerName & "}")
  tclSetVar(varName = "playergender", newValue = $newGameSettings.playerGender)
  textEntry = playerFrameName & ".shipname"
  tclEval(script = textEntry & " delete 0 end")
  tclEval(script = textEntry & " insert 0 {" & newGameSettings.shipName & "}")
  var values = ""
  for faction in factionsList.values:
    if faction.careers.len > 0:
      values = values & " {" & faction.name & "}"
  values.add(y = " Random")
  var comboBox = playerFrameName & ".faction"
  tclEval(script = comboBox & " configure -values [list" & values & "]")
  if newGameSettings.playerFaction == "random":
    tclEval(script = comboBox & " set Random")
  else:
    tclEval(script = comboBox & " set {" & factionsList[newGameSettings.playerFaction].name & "}")
  tclEval(script = "SetFaction")
  comboBox = playerFrameName & ".career"
  if newGameSettings.playerCareer == "random":
    tclEval(script = comboBox & " set Random")
