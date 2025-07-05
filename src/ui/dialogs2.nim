# Copyright 2024-2025 Bartek thindil Jasicki
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

## Provides various procedures related to the in-game dialog's windows like
## closing a dialog, updating it, etc. Split from the utilsui module to avoid
## circular dependencies.

import std/strutils
import contracts, nimalyzer
import ../tk
import dialogs, errordialog

proc closeDialogCommand*(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [], cdecl, contractual,
    ruleOff: "params".} =
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
    argv: cstringArray): TclResults {.raises: [], tags: [
    WriteIOEffect, TimeEffect, RootEffect], cdecl, contractual.} =
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
        text[6..^1].parseInt - 1
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
    argv: cstringArray): TclResults {.raises: [], tags: [], cdecl,
        contractual.} =
  ## Get string value from the player, like new ship or module name
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## GetString caption closeaction title okbutton
  ## Caption is the text showed above entry field in the dialog, variable
  ## is the variable which will be set, title is the title of the dialog and
  ## okbutton is the text which will be displayed on the confirmation
  ## button
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
  tclEval(script = "grid " & okButton & " -row 3 -pady 5 -padx 5")
  tclEval(script = okButton & " state disabled")
  let cancelButton = stringDialog & ".closebutton"
  tclEval(script = "ttk::button " & cancelButton &
      " -text Cancel -command {CloseDialog " & stringDialog & "} -image cancelicon -style Dialogred.TButton")
  tclEval(script = "grid " & cancelButton & " -row 3 -column 1 -pady 5 -padx 5")
  tclEval(script = "bind " & cancelButton & " <Tab> {focus .getstring.entry;break}")
  tclEval(script = "bind " & cancelButton & " <Escape> {" & cancelButton & " invoke;break}")
  tclEval(script = "bind " & okButton & " <Escape> {" & cancelButton & " invoke;break}")
  tclEval(script = "bind " & stringEntry & " <Escape> {" & cancelButton & " invoke;break}")
  tclEval(script = "bind " & stringEntry & " <Return> {" & okButton & " invoke;break}")
  tclEval(script = "focus " & stringEntry)
  showDialog(dialog = stringDialog)
  return tclOk

var mouseXPosition, mouseYPosition = 0

proc setMousePositionCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [], cdecl,
        contractual.} =
  ## Set the mouse position
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## SetMousePosition x y
  ## X and Y are current position of the mouse
  mouseXPosition = try:
      ($argv[2]).parseInt
    except:
      0
  mouseYPosition = try:
      ($argv[3]).parseInt
    except:
      0
  let dialogHeader = $argv[1]
  if mouseXPosition > 0 and mouseYPosition > 0:
    tclEval(script = dialogHeader & " configure -cursor fleur")
  else:
    tclEval(script = dialogHeader & " configure -cursor hand1")
  return tclOk

proc moveDialogCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [
        WriteIOEffect, TimeEffect, RootEffect], cdecl, contractual.} =
  ## Move the selected dialog around
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## MoveDialog dialogname x y
  ## Dialogname is name of the dialog to move, x and y are the current
  ## position of the mouse to count where to move the dialog
  if mouseXPosition == 0 and mouseYPosition == 0:
    return tclOk
  let
    currentXMouse = try:
        ($argv[2]).parseInt
      except:
        return showError(message = "Can't get current mouse X.")
    currentYMouse = try:
        ($argv[3]).parseInt
      except:
        return showError(message = "Can't get current mouse Y.")
    dialog = $argv[1]
    dialogX = try:
        tclEval2(script = "winfo x " & dialog).parseInt
      except:
        return showError(message = "Can't get dialog X.")
    dialogY = try:
        tclEval2(script = "winfo y " & dialog).parseInt
      except:
        return showError(message = "Can't get dialog X.")
  if mouseXPosition > currentXMouse and dialogX < 5:
    return tclOk
  if mouseYPosition > currentYMouse and dialogY < 5:
    return tclOk
  let
    dialogWidth = try:
        tclEval2(script = "winfo width " & dialog).parseInt
      except:
        return showError(message = "Can't get dialog width.")
    mainWindowWidth = try:
        tclEval2(script = "winfo width .").parseInt
      except:
        return showError(message = "Can't get main window width.")
  if mouseXPosition < currentXMouse and dialogX + dialogWidth > mainWindowWidth:
    return tclOk
  let
    dialogHeight = try:
        tclEval2(script = "winfo height " & dialog).parseInt
      except:
        return showError(message = "Can't get dialog height.")
    mainWindowHeight = try:
        tclEval2(script = "winfo height .").parseInt
      except:
        return showError(message = "Can't get main window height.")
  if mouseYPosition < currentYMouse and dialogY + dialogHeight + 5 > mainWindowHeight:
    return tclOk

  proc getCoordinate(name: string): int {.raises: [], tags: [
      WriteIOEffect, TimeEffect, RootEffect], contractual.} =
    ## Get the x or y coordinate of the dialog
    ##
    ## * name - the name of axis in which coordinate will be looked for
    ##
    ## Returns the selected coordinate (x or y)
    let value = tclEval2(script = "lindex [place configure " & dialog & " -" &
        name & "] 4")
    if value.len == 0:
      return 0
    try:
      return value.parseInt
    except:
      showError(message = "Can't get coordinate.")
      return 0

  let
    newX = getCoordinate(name = "x") - (mouseXPosition - currentXMouse)
    newY = getCoordinate(name = "y") - (mouseYPosition - currentYMouse)
  tclEval(script = "place configure " & dialog & " -x " & $newX & " -y " & $newY)
  mouseXPosition = currentXMouse
  mouseYPosition = currentYMouse
  return tclOk

proc addCommands*() {.raises: [], tags: [WriteIOEffect, TimeEffect, RootEffect],
    contractual.} =
  ## Adds Tcl commands related to dialogs
  try:
    addCommand(name = "CloseDialog", nimProc = closeDialogCommand)
    addCommand(name = "UpdateDialog", nimProc = updateDialogCommand)
    addCommand(name = "GetString", nimProc = getStringCommand)
    addCommand(name = "SetMousePosition", nimProc = setMousePositionCommand)
    addCommand(name = "MoveDialog", nimProc = moveDialogCommand)
  except:
    showError(message = "Can't add a Tcl command.")
