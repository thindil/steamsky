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
import contracts
import ../[game, log]

proc showError*(message: string; e: ref Exception = getCurrentException()) {.raises: [],
    tags: [TimeEffect, WriteIOEffect, RootEffect], contractual.} =
  ## Show the error dialog with the message containing technical details about the issue
  ##
  ## * message - the message to show in the error dialog
  ## * e       - the exception which happened. Default value is the current exception
  var debugInfo: string = $now() & '\n' & gameVersion & '\n' & message
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
