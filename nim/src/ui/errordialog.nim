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

import contracts
import ../[game, tk]

proc showError*(message: string; e: ref Exception = getCurrentException()): TclResults {.discardable,
    sideEffect, raises: [], tags: [WriteIOEffect], contractual.} =
  ## Show the error dialog with the message containing technical details about the issue
  ##
  ## * message - the message to show in the error dialog
  ## * e       - the exception which happened. Default value is the current exception
  ##
  ## This procedure always returns tclOk
  var debugInfo: string = message
  if e != nil:
    debugInfo.add(y = " Reason: " & getCurrentExceptionMsg())
    when defined(debug):
      debugInfo.add(y = "\nStack trace:\n" & e.getStackTrace)
  debugInfo.add(y = "\nLast Tcl error: " & tclGetVar(varName = "errorInfo"))
  try:
    writeFile(fileName = saveDirectory & "error.log", content = debugInfo)
  except:
    debugInfo.add(y = "Can't save error to file. Reason: " &
        getCurrentExceptionMsg())
  tclEval(script = "bgerror {" & debugInfo & "}")
  return tclOk
