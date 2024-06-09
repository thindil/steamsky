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

import ../tk
import dialogs

proc closeDialogCommand*(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [], exportc.} =
  ## Close the selected dialog
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## CloseDialog dialogname
  ## Dialogname is name of the dialog to close
  if timerId.len > 0:
    tclEval(script = "after cancel " & timerId)
    timerId = ""
  var
    frame = ".gameframe.header"
    dialog = $argv[1]
  if argc == 3:
    frame = $argv[2]
    tclEval(script = "tk busy forget " & frame)
    if argv[2] == ".memberdialog":
      frame = frame & ".buttons.button"
      if tclEval2(script = "winfo exists " & frame) == "0":
        frame = ".memberdialog.button"
    tclEval(script = "focus " & frame)
    tclEval(script = "destroy " & dialog)
    return tclOk
  if tclEval2(script = "tk busy status " & frame) == "1":
    tclEval(script = "tk busy forget " & frame)
    frame = ".gameframe.paned"
    tclEval(script = "tk busy forget " & frame)
  tclEval(script = "destroy " & dialog)
  return tclOk

proc addCommands*() =
  # addCommand("CloseDialog", closeDialogCommand)
  discard

# Temporary code for interfacing with Ada

proc addAdaDialogsCommands() {.raises: [], tags: [RootEffect], exportc.} =
  try:
    addCommands()
  except:
    echo getCurrentExceptionMsg()

