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

import std/strutils
import ../tk
import dialogs, errordialog

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

proc updateDialogCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [
    WriteIOEffect, TimeEffect], exportc.} =
  ## Update countdown timer on the selected dialog. If timer reach 0, close
  ## dialog
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## UpdateDialog dialogname
  ## Dialogname is name of the dialog to update
  let messageButton = $argv[1] & ".button"
  if tclEval2(script = "winfo exists " & messageButton) == "0":
    return closeDialogCommand(clientData = clientData, interp = interp,
        argc = argc, argv = argv)
  let
    text = tclEval2(script = messageButton & " cget -text")
    seconds = try:
        text[0..^1].parseInt - 1
      except:
        return showError(message = "Can't get amount of seconds.")
  if seconds == 0:
    return closeDialogCommand(clientData = clientData, interp = interp,
        argc = argc, argv = argv)
  tclEval(script = messageButton & " configure -text {Close " & $seconds & "}")
  timerId = tclEval2(script = "after 1000 {UpdateDialog " & $argv[1] & (
      if argc == 3: " " & $argv[2] else: "") & "}")
  return tclOk

proc getStringCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.exportc.} =
  let
    stringDialog = createDialog(name = ".getstring", title = $argv[3],
        titleWidth = 275, columns = 2)
    stringLabel = stringDialog & ".text"
  tclEval(script = "ttk::label " & stringLabel & " -text {" & $argv[1] & "} -wraplength 300")
  tclEval(script = "grid " & stringLabel & " -padx 5 -pady {5 0} -columnspan 2")
  let stringEntry = stringDialog & ".entry"
  tclEval(script = "ttk::entry " & stringEntry & " -validate key -validatecommand {set value %P;if {$value == {} || [string length $value] > 64} {.getstring.okbutton state disabled; return 1} else {.getstring.okbutton state !disabled; return 1}}")
  tclEval(script = "grid " & stringEntry & " -sticky we -padx 5 -columnspan 2")
  let okButton = stringDialog & ".okbutton"
  tclEval(script = "ttk::button " & okButton & " -text {" & $argv[4] &
      "} -command {SetTextVariable " & $argv[2] & ";CloseDialog " &
      stringDialog & "} -image edit2icon -style Dialoggreen.TButton")
  let cancelButton = stringDialog & ".closebutton"
  tclEval(script = "ttk::button " & cancelButton &
      " -text Cancel -command {CloseDialog " & stringDialog & "} -image cancelicon -style Dialogred.TButton")
  return tclOk

proc addCommands*() =
  # addCommand("CloseDialog", closeDialogCommand)
  # addCommand("UpdateDialog", updateDialogCommand)
  # addCommand("GetString", getStringCommand)
  discard

# Temporary code for interfacing with Ada

proc addAdaDialogsCommands() {.raises: [], tags: [RootEffect], exportc.} =
  try:
    addCommands()
  except:
    echo getCurrentExceptionMsg()

