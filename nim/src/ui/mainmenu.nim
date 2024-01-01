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

import std/[os, strutils]
import ../[config, game, tk]

var mainMenuFrame = ""

proc createMainMenu*() =
  let
    uiDirectory = dataDirectory & "ui" & DirSep
    iconPath = uiDirectory & "images" & DirSep & "icon.png"
    mainWindow = "."
  if not dirExists(iconPath):
    tclEval(script = "wm withdraw " & mainWindow)
    tclEval(script = "tk_messageBox -message {Couldn't not find the game data files and the game have to stop. Are you sure that directory \"" & dataDirectory & "\" is the proper place where the game data files exists?} -icon error -type ok")
    tclEval(script = "exit 1")
    return
  let icon = tclEval2(script = "image create photo logo -file {" & iconPath & "}")

proc showMainMenu*() =
  let mainWindow = "."
  var
    x: int = ((tclEval2(script = "winfo vrootwidth " & mainWindow).parseInt - 600) / 2).int
    y: int = ((tclEval2(script = "winfo vrootheight " & mainWindow).parseInt - 400) / 2).int
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
