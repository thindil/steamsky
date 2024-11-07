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

## Provides code related to showing information about error which happended
## in the game.

import std/[logging, strutils, times]
import contracts, nuklear/nuklear_sdl_renderer
import ../[game, log]
import coreui

var debugInfo: string = ""

proc setError*(message: string; e: ref Exception = getCurrentException(
    )): GameDialog {.raises: [], tags: [TimeEffect, WriteIOEffect, RootEffect],
        contractual.} =
  ## Set the information about the error which occured in the game
  ##
  ## * message - the message to show in the error dialog
  ## * e       - the exception which happened. Default value is the current exception
  ##
  ## This procedure always returns errorDialog
  debugInfo = $now() & '\n' & gameVersion & '\n' & message
  if e != nil:
    debugInfo.add(y = " Reason: " & getCurrentExceptionMsg())
    when defined(debug):
      debugInfo.add(y = "\nStack trace:\n" & e.getStackTrace)
  logMessage(message = debugInfo, messageLevel = lvlError)
  try:
    let errorLog: File = open(fileName = saveDirectory & "error.log",
        mode = fmAppend)
    errorLog.write(s = debugInfo & '\n' & repeat(c = '-', count = 80) & '\n')
    errorLog.close
  except:
    debugInfo.add(y = "Can't save error to file. Reason: " &
        getCurrentExceptionMsg())
  result = errorDialog

proc showError*(dialog: var GameDialog) {.raises: [], tags: [], contractual.} =
  ## Show the error dialog with information about the current in-game error
  ##
  ## * dialog - the current in-game dialog to show
  ##
  ## Returns parameter dialog
  window(name = "Error", x = (windowWidth / 6), y = 20, w = (
      windowWidth.float / 1.5), h = (windowHeight.float / 1.1), flags = {windowBorder,
      windowMoveable, windowTitle, windowNoScrollbar}):
    setLayoutRowDynamic(height = 25, cols = 1)
    for line in debugInfo.split(sep = '\n'):
      wrapLabel(str = line)
  dialog = errorDialog
