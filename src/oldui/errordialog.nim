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

## Provides code related to showing information about in-game error, like
## saving the error data in the error.log file and present them to the
## player.

import std/[logging, strutils, times]
import contracts
import ../[config, game, log, tk]
import coreui

proc createDialog(name, title: string; titleWidth: Positive = 275;
    columns: Positive = 1;
    parentName: string = ".gameframe"): string {.raises: [], tags: [],
    contractual.} =
  ## Create a new dialog with the selected title
  ##
  ## * name       - the Tk path of the new dialog
  ## * title      - the title of the new dialog
  ## * titleWidth - the width in pixels of the new dialog
  ## * columns    - the amount of columns for elements in the new dialog
  ## * parentName - the Tk path of the parent widget
  ##
  ## Returns the full Tk path of the new dialog
  require:
    name.len > 0
  body:
    if parentName == ".gameframe":
      tclEval(script = "tk busy " & gameHeader)
      tclEval(script = "tk busy " & mainPaned)
    else:
      tclEval(script = "tk busy " & parentName)
    tclEval(script = "update")
    result = name
    tclEval(script = "ttk::frame " & result & " -style Dialog.TFrame")
    let dialogHeader: string = result & ".header"
    tclEval(script = "ttk::label " & dialogHeader & " -text {" & title &
        "} -wraplength " & $titleWidth & " -style Header.TLabel -cursor hand1")
    tclEval(script = "grid " & dialogHeader &
        " -sticky we -padx 2 -pady {2 0}" & (if columns > 1: " -columnspan " &
            $columns else: ""))
    tclEval(script = "bind " & dialogHeader & " <ButtonPress-" & (
        if gameSettings.rightButton: "3" else: "1") & "> {SetMousePosition " &
        dialogHeader & " %X %Y}")
    tclEval(script = "bind " & dialogHeader & " <Motion> {MoveDialog " &
        result & " %X %Y}")
    tclEval(script = "bind " & dialogHeader & " <ButtonRelease-" & (
        if gameSettings.rightButton: "3" else: "1") & "> {SetMousePosition " &
        dialogHeader & " 0 0}")

proc addCloseButton(name, text, command: string; columnSpan: Positive = 1;
    row: Natural = 0; column: Natural = 0; icon: string = "exiticon";
    color: string = "") {.raises: [], tags: [], contractual.} =
  ## Add a close button to the selected dialog and set keyboard bindings for it
  ##
  ## * name       - the Tk path (name) for the button
  ## * text       - the text to display on the button
  ## * command    - the Tcl command to run when the button was clicked
  ## * columnSpan - the amount of columns to merge when placing the button
  ## * row        - the row in which the button will be placed
  ## * column     - the column in which the button will be placed
  ## * icon       - the Tcl name of the image which will be displayed on the
  ##                button intead of the text or close to the text
  ## * color      - the color of the text on the button. Depends on the
  ##                current game's theme
  require:
    name.len > 0
  body:
    let button: string = name
    tclEval(script = "ttk::button " & button & " -command {" & command &
        "} -image {" & icon & "} -style Dialog" & color & ".TButton -text {" &
        text & "}")
    tclEval(script = "tooltip::tooltip " & button & " \"Close the dialog \\[Escape key\\]\"")
    tclEval(script = "grid " & button & " -pady 5" & (if columnSpan >
        1: " -columnspan " & $columnSpan else: "") & " -row " & $row & (
        if column > 0: " -column " & $column else: ""))
    tclEval(script = "focus " & button)
    tclEval(script = "bind " & button & " <Tab> {break}")
    tclEval(script = "bind " & button & " <Escape> {" & button & " invoke;break}")

proc showDialog(dialog: string; parentFrame: string = ".gameframe";
    relativeX: float = 0.3; relativeY: float = 0.3) {.raises: [], tags: [],
    contractual.} =
  ## Show the selected dialog to the player
  ##
  ## * dialog      - the Tk path (name) of the dialog to show
  ## * parentFrame - the Tk path (name) of the parent frame for the dialog
  ## * relativeX   - the relative X coordinate of the dialog inside its parent
  ##                 frame. 0.0 is the left border
  ## * relativeY   - the relative Y coordinate of the dialog inside its parent
  ##                 frame. 0.0 is the top border
  require:
    dialog.len > 0
  body:
    tclEval(script = "place " & dialog & " -in " & parentFrame & " -relx " &
        $relativeX & " -rely " & $relativeY)
    tclEval(script = "raise " & dialog)

proc showError*(message: string; e: ref Exception = getCurrentException(
    )): TclResults {.discardable, raises: [], tags: [WriteIOEffect, TimeEffect,
        RootEffect], contractual.} =
  ## Show the error dialog with the message containing technical details about the issue
  ##
  ## * message - the message to show in the error dialog
  ## * e       - the exception which happened. Default value is the current exception
  ##
  ## This procedure always returns tclOk
  var debugInfo: string = $now() & '\n' & gameVersion & '\n' & message
  if e != nil:
    debugInfo.add(y = " Reason: " & getCurrentExceptionMsg())
    when defined(debug):
      debugInfo.add(y = "\nStack trace:\n" & e.getStackTrace)
  debugInfo.add(y = "\nLast Tcl error: " & tclGetVar(varName = "errorInfo"))
  logMessage(message = debugInfo, messageLevel = lvlError)
  try:
    let errorLog: File = open(fileName = saveDirectory.string & "error.log",
        mode = fmAppend)
    errorLog.write(s = debugInfo & '\n' & repeat(c = '-', count = 80) & '\n')
    errorLog.close
  except:
    debugInfo.add(y = "Can't save error to file. Reason: " &
        getCurrentExceptionMsg())
  let
    parentName: string = (if tclEval2(script = "winfo exists .gameframe") ==
        "1": ".gameframe" else: ".")
    errorDialog: string = createDialog(name = ".errordialog",
        title = "Error!Error!Error!", parentName = parentName)
  var errorLabel: string = errorDialog & ".general"
  tclEval(script = "ttk::label " & errorLabel & " -wraplength 650 -text {Oops, something bad happened and the game has encountered an error. Please, remember what you were doing before the error and report this problem at:}")
  tclEval(script = "grid " & errorLabel & " -padx 5 -sticky w")
  var errorButton: string = errorDialog & ".link"
  tclEval(script = "ttk::button " & errorButton & " -text {https://www.laeran.pl.eu.org/repositories/steamsky/ticket} -command {OpenLink https://www.laeran.pl.eu.org/repositories/steamsky/ticket} -style Toolbutton")
  tclEval(script = "grid " & errorButton)
  errorLabel = errorDialog & ".general2"
  tclEval(script = "ttk::label " & errorLabel & " -wraplength 650 -text {or if you prefer, on one of the game community options:}")
  tclEval(script = "grid " & errorLabel & " -padx 5 -sticky w")
  errorButton = errorDialog & ".social"
  tclEval(script = "ttk::button " & errorButton & " -text {https://thindil.itch.io/steam-sky} -command {OpenLink https://thindil.itch.io/steam-sky} -style Toolbutton")
  tclEval(script = "grid " & errorButton)
  errorLabel = errorDialog & ".general3"
  tclEval(script = "ttk::label " & errorLabel & " -wraplength 650 -text {and attach (if possible) file with saved game or 'error.log'.}")
  tclEval(script = "grid " & errorLabel & " -padx 5 -sticky w")
  var errorFrame: string = errorDialog & ".frame"
  tclEval(script = "ttk::frame " & errorFrame)
  errorButton = errorFrame & ".showdirectory"
  tclEval(script = "ttk::button " & errorButton &
      " -text {Open directory with saved games} -command {OpenLink {" &
      saveDirectory.string & "}}")
  tclEval(script = "grid " & errorButton)
  addCloseButton(name = errorFrame & ".close", text = "Close",
      command = "CloseDialog " & errorDialog & (if parentName ==
      ".": " ." else: ""), row = 0, column = 1)
  tclEval(script = "grid " & errorFrame)
  errorFrame = errorDialog & ".technical"
  tclEval(script = "ttk::labelframe " & errorFrame & " -text {Technical details:}")
  errorLabel = errorFrame & ".technical"
  let yScroll: string = errorFrame & ".yscroll"
  tclEval(script = "ttk::scrollbar " & yScroll &
      " -orient vertical -command [list " & errorLabel & " yview]")
  tclEval(script = "text " & errorLabel &
      " -wrap char -width 55 -height 10 -yscrollcommand [list " & yScroll & " set]")
  tclEval(script = errorLabel & " configure -state enabled")
  tclEval(script = errorLabel & " insert end {" & debugInfo & "}")
  tclEval(script = errorLabel & " configure -state disabled")
  tclEval(script = "grid " & errorLabel & " -padx {5 0} -pady {0 5} -sticky w")
  tclEval(script = "grid " & yScroll & " -sticky ns -pady 5 -padx {0 5} -pady {0 5} -row 0 -column 1")
  tclEval(script = "grid " & errorFrame & " -pady {0 5}")
  showDialog(dialog = errorDialog, parentFrame = parentName, relativeX = 0.1,
      relativeY = 0.05)
  return tclOk
