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

import std/[os, sequtils, strutils]
import ../[config, game, tk]
import errordialog, dialogs

var dataError*: string

proc showMainMenu*() =
  let mainWindow = "."
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
  let gameFrame = ".gameframe"
  if tclEval2(script = "winfo exists " & gameFrame) == "1":
    tclEval(script = "pack forget " & gameFrame)
  let mainMenuFrame = ".mainmenu"
  tclEval(script = "pack " & mainMenuFrame & " -fill both -expand true")
  var button = ".mainmenu.loadgame"
  if walkFiles(pattern = saveDirectory & "*.sav").toSeq.len > 0:
    tclEval(script = "pack " & button & " -after .mainmenu.newgame")
  else:
    tclEval(script = "pack forget " & button)
    button = ".mainmenu.newgame"
    tclEval(script = "focus " & button)
  button = ".mainmenu.halloffame"
  if fileExists(fileName = saveDirectory & "halloffame.dat"):
    tclEval(script = "pack " & button & " -before .mainmenu.news")
  else:
    tclEval(script = "pack forget " & button)
  if dataError.len > 0:
    button = ".mainmenu.newgame"
    tclEval(script = "pack forget " & button)
    button = ".mainmenu.loadgame"
    tclEval(script = "pack forget " & button)
    showMessage(text = "Can't load game data files. Error: " & dataError, parentFrame = ".", title = "The game data error")
    return
  try:
    writeFile(fileName = saveDirectory & "test.txt", content = "test")
    removeFile(file = saveDirectory & "test.txt")
  except IOError, OSError:
    button = ".mainmenu.newgame"
    tclEval(script = "pack forget " & button)
    button = ".mainmenu.loadgame"
    tclEval(script = "pack forget " & button)
