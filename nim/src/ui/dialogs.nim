# Copyright 2023-2024 Bartek thindil Jasicki
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
import ../[config, game, tk]
import coreui

type ButtonSettings* = object
  text*, command*, icon*, tooltip*, color*: string

const emptyButtonSettings* = ButtonSettings(text: "", command: "", icon: "",
    tooltip: "", color: "")

var timerId*: string = "" ## Id of the timer for auto close command

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
    title: string) {.sideEffect, raises: [], tags: [].} =
  ## Show the dialog with the selected message to the player
  ##
  ## * text        - the text to of the message to show
  ## * parentFrame - the Tk path (name) of the parent frame of the message
  ## * title       - the title of the dialog with the message
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
  showDialog(dialog = messageDialog, parentFrame = parentFrame,
      withTimer = true)

proc showQuestion*(question, res: string; inGame: bool = true) {.sideEffect,
    raises: [], tags: {}.} =
  ## Show the dialog with the selected question to the player
  ##
  ## * question - the question to show to the player
  ## * res      - the Tcl value set for the Ok button
  ## * inGame   - if true, the dialog will be show in the game, otherwise in
  ##              the main screen (like delete save game, etc.)
  let
    questionDialog = createDialog(name = ".questiondialog", title = (if res ==
        "showstats": "Question" else: "Confirmation"), titleWidth = 275,
        columns = 2, parentName = (if inGame: ".gameframe" else: "."))
    label = questionDialog & ".question"
  tclEval(script = "ttk::label " & label & " -text {" & question & "} -wraplength 370 -takefocus 0")
  tclEval(script = "grid " & label & " -columnspan 2 -padx 5 -pady {5 0}")
  var button = questionDialog & ".yesbutton"
  tclEval(script = "ttk::button " & button &
      " -text Yes -command {.questiondialog.nobutton invoke; ProcessQuestion " &
      res & "}")
  tclEval(script = "grid " & button & " -column 0 -row 2 -pady {0 5} -padx 5")
  tclEval(script = "bind " & button & " <Escape> {" & questionDialog & ".nobutton invoke;break}")
  button = questionDialog & ".nobutton"
  tclEval(script = "ttk::button " & button &
      " -text No -command {CloseDialog " & questionDialog & (
      if inGame: "" else: " .") & "}")
  tclEval(script = "grid " & button & " -column 1 -row 2 -pady {0 5} -padx 5")
  tclEval(script = "focus " & button)
  if inGame:
    showDialog(dialog = questionDialog)
  else:
    showDialog(dialog = questionDialog, parentFrame = ".", relativeX = 0.2)
  tclEval(script = "bind " & button & " <Tab> {focus .questiondialog.yesbutton;break}")
  tclEval(script = "bind " & button & " <Escape> {" & button & " invoke;break}")
  if res == "showstats":
    tclEval(script = button & " configure -command {CloseDialog " &
        questionDialog & "; ProcessQuestion mainmenu}")
    button = questionDialog & ".yesbutton"
    tclEval(script = button & " configure -command {CloseDialog " &
        questionDialog & "; ProcessQuestion showstats}")

proc showInfo*(text: string; parentName: string = ".gameframe"; title: string;
    button1: ButtonSettings = emptyButtonSettings;
    button2: ButtonSettings = emptyButtonSettings) {.sideEffect, raises: [],
        tags: [].} =
  ## Show the dialog with the selected text to the player
  ##
  ## * text       - the text to show in the dialog. Can use special tags for colors,
  ##                like `{gold}{/gold}`
  ## * parentName - the name of the Tk parent frame
  ## * title      - the title of the dialog
  ## * button1    - the settings for the first optional button. If empty,
  ##                the button will not shown
  ## * button2    - the settings for the second optional button. If empty,
  ##                the button will not shown
  let
    infoDialog = createDialog(name = ".info", title = title, titleWidth = 275,
        columns = 3, parentName = parentName)
    infoLabel = infoDialog & ".text"
  tclEval(script = "text " & infoLabel & " -width 30 -height 25 -wrap word")
  tclEval(script = infoLabel & " tag configure gold -foreground " & tclGetVar(
      varName = "ttk::theme::" & gameSettings.interfaceTheme &
      "::colors(-goldenyellow)"))
  tclEval(script = infoLabel & " tag configure green -foreground " & tclGetVar(
      varName = "ttk::theme::" & gameSettings.interfaceTheme &
      "::colors(-green)"))
  tclEval(script = infoLabel & " tag configure red -foreground " & tclGetVar(
      varName = "ttk::theme::" & gameSettings.interfaceTheme &
      "::colors(-red)"))
  var
    startIndex = 0
    tagIndex = text.find('{')
  while true:
    if tagIndex == -1:
      tagIndex = text.len
    tclEval(script = infoLabel & " insert end {" & text[startIndex .. tagIndex -
        1] & "}")
    if tagIndex == text.len:
      break
    startIndex = tagIndex
    tagIndex = text.find('}', startIndex)
    let tagName = text[startIndex + 1 .. tagIndex - 1]
    startIndex = tagIndex + 1
    tagIndex = text.find("{/" & tagName & "}", startIndex)
    tclEval(script = infoLabel & " insert end {" & text[startIndex .. tagIndex -
        1] & "} [list " & tagName & "]")
    startIndex = tagIndex + tagName.len + 3
    tagIndex = text.find('{', startIndex)
  try:
    discard tclEval(script = infoLabel & " configure -state disabled -height " &
        $(tclEval2(script = infoLabel & " index end").parseFloat + 1.0))
  except ValueError:
    showError(message = "Can't show the info. Tcl result: " & tclGetResult2())
    return
  tclEval(script = "grid " & infoLabel & " -sticky we -padx 5 -pady {5 0}")
  let
    buttonsFrame = infoDialog & ".buttons"
    closeCommand = "CloseDialog " & infoDialog & (if parentName ==
        ".gameframe": "" else: " " & parentName)
  tclEval(script = "ttk::frame " & buttonsFrame)
  var button = ""
  if button1.text.len > 0 and button1.command.len > 0:
    button = buttonsFrame & ".button1"
    tclEval(script = "ttk::button " & button & " -text {" & button1.text & "}" &
        (if button1.icon.len > 0: " -image {" & button1.icon & "}" else: "") &
        " -command {" & closeCommand & ";" & button1.command &
        "} -style Dialog" & button1.color & ".TButton")
    tclEval(script = "tooltip::tooltip " & button & " \"" & button1.tooltip & "\"")
    tclEval(script = "grid " & button & " -padx 5")
    tclEval(script = "bind " & button & " <Tab> {focus " & buttonsFrame & ".button;break}")
    tclEval(script = "bind " & button & " <Escape> {" & buttonsFrame & ".button invoke;break}")
  addCloseButton(name = buttonsFrame & ".button", text = "Close",
      command = closeCommand, column = (if button1.text.len > 0: 1 else: 0),
      icon = "exiticon")
  button = buttonsFrame & ".button"
  if button2.text.len > 0 and button2.command.len > 1:
    tclEval(script = "bind " & button & " <Tab> {focus " & buttonsFrame & ".button2;break}")
    button = buttonsFrame & ".button2"
    tclEval(script = "ttk::button " & button & " -text {" & button2.text & "}" &
        (if button2.icon.len > 0: " -image {" & button2.icon & "}" else: "") &
        " -command {" & closeCommand & ";" & button2.command &
        "} -style Dialog" & button2.color & ".TButton")
    tclEval(script = "tooltip::tooltip " & button & " \"" & button2.tooltip & "\"")
    tclEval(script = "grid " & button & " -row 0 -column 2 -padx 5")
    if button1.text.len > 0:
      tclEval(script = "bind " & button & " <Tab> {focus " & buttonsFrame & ".button1;break}")
    else:
      tclEval(script = "bind " & button & " <Tab> {focus " & buttonsFrame & ".button;break}")
    tclEval(script = "bind " & button & " <Escape> {" & buttonsFrame & ".button invoke;break}")
  elif button1.text.len > 0 and button1.command.len > 0:
    tclEval(script = "bind " & button & " <Tab> {focus " & buttonsFrame & ".button1;break}")
  tclEval(script = "grid " & buttonsFrame & " -padx 5 -pady 5")
  showDialog(dialog = infoDialog)

proc showManipulateItem*(title, command, action: string; itemIndex: Natural;
    maxAmount: Natural = 0; cost: Natural = 0) =
  let itemDialog = createDialog(name = ".itemdialog", title = title,
      titleWidth = 275, columns = 2)
  var button = itemDialog & ".dropbutton"
  tclEval(script = "ttk::button " & button & " -command {" & command &
      "} -style Dialoggreen.TButton" & (if action ==
      "drop": " -image drop2icon" elif action ==
      "take": " -image give2icon" elif action ==
      "buy": " -image buyicon" elif action ==
      "sell": " -image sellicon" else: "") & " -text {" &
      action.capitalizeAscii & "}")
  if action == "drop":
    tclEval(script = "tooltip::tooltip " & button & " \"Drop the item from the ship's cargo\"")
  elif action == "take":
    tclEval(script = "tooltip::tooltip " & button & " \"Take the item from the base\"")
  elif action == "buy":
    tclEval(script = "tooltip::tooltip " & button & " \"Buy the selected amount of the item\"")
  elif action == "sell":
    tclEval(script = "tooltip::tooltip " & button & " \"Sell the selected amount of the item\"")
  let amountBox = itemDialog & ".amount"
  if maxAmount == 0:
    tclEval(script = "ttk::spinbox " & amountBox & " -width 10 -from 1 -to " &
        $playerShip.cargo[itemIndex].amount &
        " -validate key -validatecommand {CheckAmount " & amountBox & " " & $(
        itemIndex + 1) & " %P " & action & (if cost > 0: " " & $cost else: "") &
        " " & button & "} -command {ValidateAmount " & amountBox & " " & $(
        itemIndex + 1) & " " & action & (if cost > 0: " " & $cost else: "") &
        " " & button & "}")
  else:
    tclEval(script = "ttk::spinbox " & amountBox & " -width 10 -from 1 -to " &
        $maxAmount & " -validate key -validatecommand {CheckAmount " &
        amountBox & " " & $(itemIndex + 1) & " %P " & action & (if cost >
        0: " " & $cost else: "") & " " & button &
        "} -command {ValidateAmount " & amountBox & " " & $(itemIndex + 1) &
        " " & action & (if cost > 0: " " & $cost else: "") & " " & button & "}")
  let maxButton = itemDialog & ".amountlbl"
  if maxAmount == 0:
    tclEval(script = "ttk::button " & maxButton & " -text {Amount (max: " & $playerShip.cargo[itemIndex].amount & "):} -command {" & amountBox " set " & $playerShip.cargo.[itemIndex].amount & ";" & amountBox & " validate}")

# Temporary code for interfacing with Ada

type AdaButtonSettings* = object
  text*, command*, icon*, tooltip*, color*: cstring

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

proc showAdaMessage(text, parentFrame, title: cstring): cstring {.raises: [],
    tags: [], exportc.} =
  showMessage($text, $parentFrame, $title)
  return timerId.cstring

proc showAdaQuestion(question, res: cstring; inGame: cint) {.exportc, raises: [
    ], tags: [].} =
  showQuestion($question, $res, inGame == 1)

proc showAdaInfo(text, parentName, title: cstring; button1,
    button2: AdaButtonSettings) {.exportc, raises: [], tags: [].} =
  let
    nimButton1 = ButtonSettings(text: $button1.text, command: $button1.command,
        icon: $button1.icon, tooltip: $button1.tooltip, color: $button1.color)
    nimButton2 = ButtonSettings(text: $button2.text, command: $button2.command,
        icon: $button2.icon, tooltip: $button2.tooltip, color: $button2.color)
  try:
    showInfo($text, $parentName, $title, nimButton1, nimButton2)
  except:
    discard
