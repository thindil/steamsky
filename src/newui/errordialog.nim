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

## Provides code related to showing information about error which happended
## in the game.

import std/[logging, strutils, os, osproc, times]
import contracts, nuklear/nuklear_sdl_renderer
import ../[game, log]
import coreui

var
  debugInfo, message: string = ""

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
    let errorLog: File = open(fileName = saveDirectory.string & "error.log",
        mode = fmAppend)
    errorLog.write(s = debugInfo & '\n' & repeat(c = '-', count = 80) & '\n')
    errorLog.close
  except:
    debugInfo.add(y = "Can't save error to file. Reason: " &
        getCurrentExceptionMsg())
  result = errorDialog

proc openLink*(link: string) {.raises: [], tags: [ReadIOEffect, RootEffect],
    contractual.} =
  ## Open the selected URL in a default system browser. Show message if the
  ## link can't be opened
  ##
  ## * link - the URL to open
  require:
    link.len > 0
  body:
    let command: string = try:
          findExe(exe = (if hostOS == "windows": "start" elif hostOS ==
            "macosx": "open" else: "xdg-open"))
        except:
          ""
    if command.len == 0:
      message = "Reason: no program to open it."
    else:
      try:
        discard execCmd(command = command & " " & link)
      except:
        message = "Can't open the link"

proc showLinkError*() {.raises: [], tags: [RootEffect], contractual.} =
  ## Show an error related to opening a link if any
  if message.len == 0:
    return
  try:
    popup(pType = staticPopup, title = "Can't open the link", flags = {
        windowBorder, windowTitle, windowNoScrollbar}, x = 120, y = 80, w = 350, h = 120):
      setLayoutRowDynamic(height = 25, cols = 1)
      label(str = message)
      labelButton(title = "Close"):
        message = ""
        closePopup()
  except:
    echo "Can't create the message popup: ", getCurrentExceptionMsg()

proc showError*(dialog: var GameDialog) {.raises: [], tags: [ReadIOEffect,
    RootEffect], contractual.} =
  ## Show the error dialog with information about the current in-game error
  ##
  ## * dialog - the current in-game dialog to show
  ##
  ## Returns parameter dialog
  ##
  try:
    const
      width: float = 540
      height: float = 360
    updateDialog(width = width, height = height)
    window(name = "Error!Error!Error!", x = 40, y = 20, w = width, h = height,
        flags = {windowBorder, windowTitle, windowMinimizable, windowMovable}):
      setLayoutRowDynamic(height = 75, cols = 1)
      wrapLabel(str = "Oops, something bad happened and the game has encountered an error. Please, remember what you were doing before the error and report this problem at:")
      setLayoutRowDynamic(height = 25, cols = 1)
      var url: string = "https://www.laeran.pl.eu.org/repositories/steamsky/ticket"
      labelButton(title = url):
        openLink(link = url)
      setLayoutRowDynamic(height = 25, cols = 1)
      label(str = "or if you prefer, on one of the game community options:")
      url = "https://thindil.itch.io/steam-sky"
      labelButton(title = url):
        openLink(link = url)
      label(str = "and attach (if possible) file with saved game or 'error.log'.")
      labelButton(title = "Open directory with saved games"):
        openLink(link = saveDirectory.string)
      treeTab(title = "Technical details", state = minimized, index = 1):
        setLayoutRowDynamic(height = (30 * debugInfo.countLines).float, cols = 1)
        wrapLabel(str = debugInfo)
      setLayoutRowDynamic(height = 25, cols = 1)
      labelButton(title = "Close"):
        dialog = none
      showLinkError()

    windowSetFocus(name = "Error!Error!Error!")
  except:
    discard
