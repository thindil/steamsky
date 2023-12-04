# Copyright 2023 Bartek thindil Jasicki
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

import ../[config, tk]
import coreui

var timerId: string = "" ## Id of the timer for auto close command

proc createDialog*(name, title: string; titleWidth: Positive = 275;
    columns: Positive = 1;
    parentName: string = ".gameframe"): string {.sideEffect, raises: [], tags: [].} =
  ## Create a new dialog with the selected title
  ##
  ## * name       - the Tk path of the new dialog
  ## * title      - the title of the new dialog
  ## * titleWidth - the width in pixels of the new dialog
  ## * columns    - the amount of columns for elements in the new dialog
  ## * parentName - the Tk path of the parent widget
  ##
  ## Returns the full Tk path of the new dialog
  if parentName == ".gameframe":
    tclEval(script = "tk busy " & gameHeader)
    tclEval(script = "tk busy " & mainPaned)
  else:
    tclEval(script = "tk busy " & parentName)
  if timerId.len > 0:
    tclEval(script = "after cancel " & timerId)
    timerId = ""
  tclEval(script = "update")
  result = name
  tclEval(script = "ttk::frame " & result & " -style Dialog.TFrame")
  let dialogHeader = result & ".header"
  tclEval(script = "ttk::label " & dialogHeader & " -text {" & title &
      "} -wraplength " & $titleWidth & " -style Header.TLabel -cursor hand1")
  tclEval(script = "grid " & dialogHeader & " -sticky we -padx 2 -pady {2 0}" &
      (if columns > 1: " -columnspan " & $columns else: ""))
  tclEval(script = "bind " & dialogHeader & " <ButtonPress-" & (
      if gameSettings.rightButton: "3" else: "1") & "> {SetMousePosition " &
      dialogHeader & " %X %Y}")
  tclEval(script = "bind " & dialogHeader & " <Motion> {MoveDialog " & result & " %X %Y}")
  tclEval(script = "bind " & dialogHeader & " <ButtonRelease-" & (
      if gameSettings.rightButton: "3" else: "1") & "> {SetMousePosition " &
      dialogHeader & " 0 0}")

proc addCloseButton*(name, text, command: string; columnSpan: Positive = 1;
    row: Natural = 0; column: Natural = 0; icon: string = "exiticon";
    color: string = "") {.sideEffect, raises: [], tags: [].} =
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
  let button = name
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

proc showDialog*(dialog: string; parentFrame: string = ".gameframe";
    withTimer: bool = false; relativeX: float = 0.3;
    relativeY: float = 0.3) {.sideEffect, raises: [], tags: [].} =
  ## Show the selected dialog to the player
  ##
  ## * dialog      - the Tk path (name) of the dialog to show
  ## * parentFrame - the Tk path (name) of the parent frame for the dialog
  ## * withTimer   - if true, add the close timer for the dialog
  ## * relativeX   - the relative X coordinate of the dialog inside its parent
  ##                 frame. 0.0 is the left border
  ## * relativeY   - the relative Y coordinate of the dialog inside its parent
  ##                 frame. 0.0 is the top border
  tclEval(script = "place " & dialog & " -in " & parentFrame & " -relx " &
      $relativeX & " -rely " & $relativeY)
  tclEval(script = "raise " & dialog)
  if withTimer:
    timerId = tclEval2(script = "after 1000 UpdateDialog " & dialog & (
        if parentFrame == ".gameframe": "" else: " " & parentFrame))

proc showMessage*(text: string; parentFrame: string = ".gameframe";
    title: string) =
  let
    messageDialog = createDialog(name = (if parentFrame ==
        ".": "" else: parentFrame) & ".message", title = title,
        parentName = parentFrame)
    messageLabel = messageDialog & ".text"
  tclEval(script = "ttk::label " & messageLabel & " -text {" & text & "} -wraplength 300")
  tclEval(script = "grid " & messageLabel & " -sticky we -padx 5 -pady 5")
  addCloseButton(name = messageDialog & ".button", text = "Close" &
      $gameSettings.autoCloseMessagesTime, command = "CloseDialog " &
      messageDialog & (if parentFrame == ".gameframe": "" else: " " &
      parentFrame), row = 2)

# Temporary code for interfacing with Ada

proc createAdaDialog(name, title: cstring; titleWidth, columns: cint;
    parentName: cstring; timerName: var cstring): cstring {.raises: [], tags: [], exportc.} =
  timerId = $timerName
  result = createDialog($name, $title, titleWidth.Positive, columns.Positive,
      $parentName).cstring
  timerName = timerId.cstring

proc addAdaCloseButton(name, text, command: cstring; columnSpan, row,
    column: cint; icon, color: cstring) {.raises: [], tags: [], exportc.} =
  addCloseButton($name, $text, $command, columnSpan.Positive, row.Natural,
      column.Natural, $icon, $color)

proc showAdaDialog(dialog, parentFrame: cstring; withTimer: cint; relativeX,
    relativeY: cfloat): cstring {.raises: [], tags: [], exportc.} =
  showDialog($dialog, $parentFrame, withTimer == 1, relativeX.float,
      relativeY.float)
  return timerId.cstring
