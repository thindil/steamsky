# Copyright 2024-2026 Bartek thindil Jasicki
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

## Provides code related to showing the main menu of the game. Split
## from mainmenu module to avoid circular dependencies.

import std/[os, sequtils, strutils]
import contracts
import ../[config, game, tk]
import errordialog, dialogs

var dataError*: string = "" ## The additional information for an error when it happened.

proc showMainMenu*() {.raises: [], tags: [WriteIOEffect, TimeEffect, RootEffect,
    ReadDirEffect, WriteDirEffect], contractual.} =
  ## Show the main menu to the player
  const mainWindow: string = "."
  var
    x: int = try:
        ((tclEval2(script = "winfo vrootwidth " & mainWindow).parseInt - 600) / 2).int
      except:
        showError(message = "Can't count X coord")
        return
    y: int = try:
        ((tclEval2(script = "winfo vrootheight " & mainWindow).parseInt - 400) / 2).int
      except:
        showError(message = "Can't count Y coord")
        return
  if x < 0:
    x = 0
  if y < 0:
    y = 0
  if gameSettings.fullScreen:
    tclEval(script = "wm attributes " & mainWindow & " -fullscreen 0")
  if tclGetVar(varName = "tlc_platform(os)") == "Linux":
    tclEval(script = "wm attributes " & mainWindow & " -zoomed 0")
  else:
    tclEval(script = "wm state " & mainWindow & " normal")
  tclEval(script = "wm title " & mainWindow & " {Steam Sky - Main Menu}")
  tclEval(script = "wm geometry " & mainWindow & " 600x400+" & $x & "+" & $y)
  const gameFrame: string = ".gameframe"
  if tclEval2(script = "winfo exists " & gameFrame) == "1":
    tclEval(script = "pack forget " & gameFrame)
  const mainMenuFrame: string = ".mainmenu"
  tclEval(script = "pack " & mainMenuFrame & " -fill both -expand true")
  var button: string = ".mainmenu.loadgame"
  try:
    if walkFiles(pattern = saveDirectory.string & "*.sav").toSeq.len > 0:
      tclEval(script = "pack " & button & " -after .mainmenu.newgame")
    else:
      tclEval(script = "pack forget " & button)
      button = ".mainmenu.newgame"
      tclEval(script = "focus " & button)
  except:
    showMessage(text = "Can't check if saved games exists. Error: " &
        getCurrentExceptionMsg(), parentFrame = ".",
        title = "Can't check saves")
  button = ".mainmenu.halloffame"
  if fileExists(fileName = saveDirectory.string & "halloffame.dat"):
    tclEval(script = "pack " & button & " -before .mainmenu.news")
  else:
    tclEval(script = "pack forget " & button)
  if dataError.len > 0:
    button = ".mainmenu.newgame"
    tclEval(script = "pack forget " & button)
    button = ".mainmenu.loadgame"
    tclEval(script = "pack forget " & button)
    showMessage(text = "Can't load game data files. Error: " & dataError,
        parentFrame = ".", title = "The game data error")
    return
  try:
    writeFile(fileName = saveDirectory.string & "test.txt", content = "")
    removeFile(file = saveDirectory.string & "test.txt")
  except IOError, OSError:
    button = ".mainmenu.newgame"
    tclEval(script = "pack forget " & button)
    button = ".mainmenu.loadgame"
    tclEval(script = "pack forget " & button)
    when defined(windows):
      showMessage(text = "You don't have permissions to write to directory \"" &
          saveDirectory &
          "\" which is set as directory for saved games. Please run the game as Administrator or select different directory.",
          parentFrame = ".", title = "Can't save the game")
    else:
      showMessage(text = "You don't have permissions to write to directory " &
          saveDirectory.string &
          "\" which is set as directory for saved games. Please select different directory.",
          parentFrame = ".", title = "Can't save the game")
