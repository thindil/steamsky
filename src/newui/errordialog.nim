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

import std/[logging, strutils, os, osproc, times]
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

proc showError*(dialog: var GameDialog) {.raises: [], tags: [ReadIOEffect,
    RootEffect], contractual.} =
  ## Show the error dialog with information about the current in-game error
  ##
  ## * dialog - the current in-game dialog to show
  ##
  ## Returns parameter dialog
  window(name = "Error!Error!Error!", x = 40, y = 20, w = (
      windowWidth.float / 1.1), h = (windowHeight.float / 1.1), flags = {windowBorder,
      windowMoveable, windowTitle, windowNoScrollbar, windowMinimizable,
      windowCloseable}):
    setLayoutRowDynamic(height = 75, cols = 1)
    wrapLabel(str = "Oops, something bad happened and the game has encountered an error. Please, remember what you were doing before the error and report this problem at:")
    setLayoutRowDynamic(height = 25, cols = 1)
    labelButton(title = "https://www.laeran.pl.eu.org/repositories/steamsky/ticket"):
      let command: string = try:
            findExe(exe = (if hostOs == "windows": "start" elif hostOs ==
              "macosx": "open" else: "xdg-open"))
          except:
            ""
      if command.len == 0:
        echo "Can't open the link. Reason: no program to open it."
      else:
        try:
          discard execCmd(command = command & " https://www.laeran.pl.eu.org/repositories/steamsky/ticket")
        except:
          echo "Can't open the link"
    setLayoutRowDynamic(height = (30 * debugInfo.countLines).float, cols = 1)
    wrapLabel(str = debugInfo)
  dialog = errorDialog
