# Copyright 2023-2025 Bartek thindil Jasicki
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
import contracts, nimalyzer
import ../[config, tk]
import errordialog

type
  TableWidget* = object
    ## Contains data about a table widget
    ##
    ## * canvas       - the Tcl path to the canvas widget used as a base widget
    ## * columnsWidth - the width of all columns of the table
    ## * row          - the number of rows in the table
    ## * rowHeight    - the height of a row in the table
    ## * scrollbar    - the Tcl path to the vertical scrollbar associated with
    ##                  the table
    canvas*: string
    columnsWidth*: seq[Positive]
    row*: Positive = 1
    rowHeight*: Positive = 1
    scrollbar*: string
  HeadersList* = seq[string] ## The list of titles for a table's headers

proc createTable*(parent: string; headers: HeadersList; scrollbar: string = ".";
    command: string = ""; tooltipText: string = ""): TableWidget {.raises: [],
    tags: [WriteIOEffect, TimeEffect, RootEffect], contractual.} =
  ## Create a new table and columns' headers for it
  ##
  ## * parent      - the Tcl path to the parent widget for the table
  ## * headers     - the list of text to show on the table's colomns' headers
  ## * scrollbar   - the Tcl path to the vertical scrollbar associated with the
  ##                 table
  ## * command     - the Tcl command to execute when the player click in the table
  ## * tooltipText - the text to show when the player hover mouse above the table
  ##
  ## Returns the newly created TableWidget with information about the table
  result = TableWidget(canvas: parent & ".table")
  if scrollbar == ".":
    result.scrollbar = parent & ".scrolly"
    tclEval(script = "ttk::scrollbar " & result.scrollbar &
        " -orient vertical -command [list " & result.canvas & " yview]")
    tclEval(script = "ttk::scrollbar " & parent &
        ".scrollx -orient horizontal -command [list " & result.canvas & " xview]")
    tclEval(script = "canvas " & result.canvas & " -yscrollcommand [list " &
        result.scrollbar & " set] -xscrollcommand [list " & parent & ".scrollx set]")
    tclEval(script = "pack " & result.scrollbar & " -side right -fill y")
    tclEval(script = "pack " & result.canvas & " -side top -fill both -padx {5 0}")
    tclEval(script = "pack " & parent & ".scrollx -side bottom -fill x")
    tclEval(script = "::autoscroll::autoscroll " & parent & ".scrollx")
    tclEval(script = "::autoscroll::autoscroll " & result.scrollbar)
  else:
    result.scrollbar = scrollbar
    tclEval(script = "canvas " & result.canvas)
    tclEval(script = "grid " & result.canvas & " -sticky nwes -padx {5 0}")
    tclEval(script = "grid columnconfigure " & parent & " " & result.canvas & " -weight 1")
    tclEval(script = "grid rowconfigure " & parent & " " & result.canvas & " -weight 1")
  var x = 0
  let headerColor = tclEval2(script = "ttk::style lookup Table -headerforecolor")
  let headerBackColor = tclEval2(script = "ttk::style lookup Table -headerbackcolor")
  let borderStyle = tclEval2(script = "ttk::style lookup Table -headerbordercolor")
  for index, header in headers:
    let headerId = tclEval2(script = result.canvas & " create text " & $x &
        " 2 -anchor nw -text {" & header &
        "} -font InterfaceFont -justify center -fill " & headerColor &
        " -tags [list header" & $(index + 1) & "]")
    if command.len > 0:
      tclEval(script = result.canvas & " bind " & headerId & " <Enter> {" &
          result.canvas & " configure -cursor hand1}")
      tclEval(script = result.canvas & " bind " & headerId & " <Leave> {" &
          result.canvas & " configure -cursor left_ptr}")
      tclEval(script = result.canvas & " bind " & headerId & " <Button-1> {" &
          command & " %x}")
    if tooltipText.len > 0:
      tclEval(script = "tooltip::tooltip " & result.canvas & " -item " &
          headerId & " \"" & tooltipText & "\"")
    let
      tclResult = tclEval2(script = result.canvas & " bbox header" & $(index + 1))
      coords = tclResult.split
    var oldX = x - 5
    try:
      x = coords[2].parseInt + 5
      result.columnsWidth.add(x - coords[0].parseInt)
      if index == 0:
        result.rowHeight = coords[3].parseInt + 5
    except ValueError:
      showError(message = "Can't get coordinates for the table. Result: " & tclResult)
      return
    let backgroundId = tclEval2(script = result.canvas & " create rectangle " &
        $oldX & " 0 " & $(x - 2) & " " & $(result.rowHeight - 3) & " -fill " &
            headerBackColor &
        " -outline " & $borderStyle & " -width 2 -tags [list headerback" &
        $(index + 1) & "]")
    tclEval(script = result.canvas & " lower headerback" & $(index + 1))
    if command.len > 0:
      tclEval(script = result.canvas & " bind " & backgroundId & " <Enter> {" &
          result.canvas & " configure -cursor hand1}")
      tclEval(script = result.canvas & " bind " & backgroundId & " <Leave> {" &
          result.canvas & " configure -cursor left_ptr}")
      tclEval(script = result.canvas & " bind " & backgroundId &
          " <Button-1> {" & command & " %x}")
    if tooltipText.len > 0:
      tclEval(script = "tooltip::tooltip " & result.canvas & " -item " &
          backgroundId & " \"" & tooltipText & "\"")
  tclEval(script = "SetScrollbarBindings " & result.canvas & " " &
      result.scrollbar)
  tclEval(script = "bind " & result.canvas & " <Up> {UpdateCurrentRow " &
      result.canvas & " lower}")
  tclEval(script = "bind " & result.canvas & " <Down> {UpdateCurrentRow " &
      result.canvas & " raise}")
  tclEval(script = "bind " & result.canvas &
      " <Key-space> {ExecuteCurrentRow " & result.canvas & "}")
  tclEval(script = "bind " & result.canvas & " <FocusOut> {HideCurrentRow " &
      result.canvas & "}")
  tclEval(script = "bind " & result.canvas & " <Leave> {HideCurrentRow " &
      result.canvas & "}")

proc clearTable*(table: var TableWidget) {.raises: [], tags: [], contractual.} =
  ## Clear the data from the selected table
  ##
  ## * table - the TableWidget which data will be cleared
  ##
  ## Returns the modified parameter table
  let buttonsFrame = table.canvas & ".buttonframe"
  if tclEval2(script = "winfo exists " & buttonsFrame) == "1":
    tclEval(script = "destroy " & buttonsFrame & ".previous")
    tclEval(script = "destroy " & buttonsFrame & ".next")
    tclEval(script = "destroy " & buttonsFrame)
  for row in 1 .. table.row:
    for column in 1 .. table.columnsWidth.len:
      tclEval(script = table.canvas & " delete row" & $row & "col" & $column)
      tclEval(script = table.canvas & " delete row" & $row)
      tclEval(script = table.canvas & " delete progressbar" & $row & "back" & $column)
      tclEval(script = table.canvas & " delete progressbar" & $row & "bar" & $column)
  table.row = 1

proc addBindings(canvas, itemId, row, command, color: string) {.raises: [],
    tags: [], contractual.} =
  ## Add Tcl events to the selected element of the TableWidget
  ##
  ## * canvas  - Tk canvas in which the events will be added
  ## * itemId  - the Id of the item to which the events will be added
  ## * row     - the number of the row in which the events will be added
  ## * command - the Tcl command which will be executed on mouse button event
  tclEval(script = canvas & " bind " & itemId & " <Enter> {" & canvas &
      " itemconfigure row$currentrow -fill " & color & ";" & canvas &
      " itemconfigure row" & row & " -fill " & tclEval2(
      script = "ttk::style lookup " & gameSettings.interfaceTheme &
      " -selectbackground") & (if command.len > 0: ";" & canvas &
      " configure -cursor hand1" else: "") & ";set currentrow " & row & "}")
  tclEval(script = canvas & " bind " & itemId & " <Leave> {" & canvas & " configure -cursor left_ptr}")
  if command.len > 0:
    tclEval(script = canvas & " bind " & itemId & " <Button-" & (
        if gameSettings.rightButton: "3" else: "1") & "> {" & command & "}")

proc addBackground(table: TableWidget; newRow: bool;
    command: string): string {.raises: [], tags: [], contractual.} =
  ## Add the proper color to the item in the table and return the name of the
  ## used color
  ##
  ## * table   - the TableWinget in which the background will be set
  ## * newRow  - if true, add the background, otherwise just return its color
  ## * command - the Tcl command which will be executed when the background is
  ##             clicked by the player
  ##
  ## Returns the name of the color of the background of the item
  result = (if table.row mod 2 > 0: tclEval2(
      script = "ttk::style lookup Table -rowcolor") else: tclEval2(
      script = "ttk::style lookup " & gameSettings.interfaceTheme &
      " -background"))
  if not newRow:
    return
  let itemId = tclEval2(script = table.canvas & " create rectangle 0 " & $(
      table.row * table.rowHeight) & " 10 " & $((table.row * table.rowHeight) +
      table.rowHeight) & " -fill " & result & " -width 0 -tags [list row" & $(
      table.row) & "]")
  tclEval(script = table.canvas & " lower " & itemId)
  addBindings(canvas = table.canvas, itemId = "row" & $table.row,
      row = $table.row, command = command, color = result)

proc addButton*(table: var TableWidget; text, tooltip, command: string;
    column: Positive; newRow: bool = false; color: string = "") {.raises: [],
        tags: [WriteIOEffect, TimeEffect, RootEffect], contractual.} =
  ## Add a button item to the selected TableWidget
  ##
  ## * table   - the TableWidget to which the button will be added
  ## * text    - the text to display on the button
  ## * tooltip - the tooltip text to display when the player hover the mouse
  ##             over the button
  ## * command - the Tcl command to execute when the player press the button
  ## * column  - the column in which the button will be placed
  ## * newRow  - if true, add a new row to the table after adding the button
  ## * color   - the color of the text on the button. If empty, use the default
  ##             color of the current theme
  ##
  ## Returns the modified parameter table
  var x = 5
  for i in 1 .. column - 1:
    x = x + table.columnsWidth[i - 1]
  let
    textColor = (if color.len > 0: color else: tclEval2(
        script = "ttk::style lookup " & gameSettings.interfaceTheme &
        " -foreground"))
    itemId = tclEval2(script = table.canvas & " create text " & $x & " " & $((
        table.row * table.rowHeight) + 2) & " -anchor nw -text {" & text &
        "} -font InterfaceFont -fill " & textColor & " -tags [list row" &
        $table.row & "col" & $column & "]")
  if tooltip.len > 0:
    tclEval(script = "tooltip::tooltip " & table.canvas & " -item " &
        itemId & " \"" & tooltip & "\"")
  let backgroundColor = addBackground(table, newRow, command)
  addBindings(table.canvas, itemId, $table.row, command, backgroundColor)
  let
    tclResult = tclEval2(script = table.canvas & " bbox " & itemId)
    coords = tclResult.split
  try:
    x = (coords[2].parseInt + 10) - coords[0].parseInt
  except ValueError:
    showError(message = "Can't add a button to the table. Result: " & tclResult)
    return
  if x > table.columnsWidth[column - 1]:
    table.columnsWidth[column - 1] = x
  if newRow:
    table.row.inc

proc updateTable*(table: TableWidget; grabFocus: bool = true) {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Update the size and coordinates of all elements in the selected TableWidget
  ##
  ## * table     - the TableWidget in which the elements will be resized and
  ##               moved
  ## * grabFocus - if true, set the keyboard focus on the table after updating
  var tag = "headerback1"
  tclEval(script = table.canvas & " coords " & tag & " 0 0 " & $(
      table.columnsWidth[0] + 10) & " " & $(table.rowHeight - 3))
  var
    newX = table.columnsWidth[0] + 20
    newY = 2
  for column in 2 .. table.columnsWidth.len:
    tag = "header" & $column
    tclEval(script = table.canvas & " coords " & tag & " " & $newX & " " & $newY)
    tag = "headerback" & $column
    tclEval(script = table.canvas & " coords " & tag & " " & $(newX - 10) &
        " 0 " & $(newX + table.columnsWidth[column - 1] + 10) & " " & $(
            table.rowHeight - 3))
    for row in 1 .. table.row:
      newY = newY + table.rowHeight
      tag = "row" & $row & "col" & $column
      tclEval(script = table.canvas & " moveto " & tag & " " & $newX & " " & $newY)
      tag = "progressbar" & $row & "back" & $column
      tclEval(script = table.canvas & " moveto " & tag & " " & $newX & " " & $(
          newY + 5))
      tag = "progressbar" & $row & "bar" & $column
      tclEval(script = table.canvas & " moveto " & tag & " " & $(newX + 2) &
          " " & $(newY + 7))
    newX = newX + table.columnsWidth[column - 1] + 20
    newY = 2
  let
    tclResult = tclEval2(script = table.canvas & " bbox all")
    coords = tclResult.split
  if table.scrollbar.len == 0 or tclEval2(script = "winfo parent " &
      table.canvas) != tclEval2(script = "winfo parent " & table.scrollbar):
    tclEval(script = table.canvas & " configure -height [expr " & coords[3] &
        " - " & coords[1] & "] -width [expr " & coords[2] & " - " & coords[0] & " + 5]")
  newY = table.rowHeight
  for row in 1 .. table.row:
    newY = newY + table.rowHeight
    tag = "row" & $row
    try:
      discard tclEval(script = table.canvas & " coords " & tag & " 0 " & $(
          newY - table.rowHeight) & " " & $(coords[2].parseInt - 1) & " " & $newY)
    except ValueError, Exception:
      showError(message = "Can't update the table. Result: " & tclResult)
      return
  tclEval(script = "set currentrow 1")
  tclEval(script = table.canvas & " bind <FocusIn> {set maxrows " & $table.row &
      ";if {$currentrow > $maxrows} {set currentrow 1};" & table.canvas &
      " itemconfigure row$currentrow -fill [ttk::style lookup " &
      gameSettings.interfaceTheme & " -selectbackground]}")
  if grabFocus:
    tclEval(script = "focus " & table.canvas)

proc addProgressbar*(table: var TableWidget; value: Natural; maxValue: Positive;
    tooltip, command: string; column: Positive; newRow: bool = false;
    invertColors: bool = false) {.raises: [], tags: [WriteIOEffect,
        TimeEffect, RootEffect], contractual.} =
  ## Add a progressbar item to the selected TableWidget
  ##
  ## * table         - the TableWidget to which the progressbar will be added
  ## * value         - the current value for the progressbar
  ## * maxValue      - the max value for the progessbar
  ## * tooltip       - the tootip text for the progressbar
  ## * command       - the Tcl command to execute when the player press the
  ##                   progressbar
  ## * column        - the column in which place the progressbar
  ## * newRow        - if true, add a new row after adding the progressbar
  ## * invenrtColors - if true, invert colors of the progressbar
  ##
  ## Returns modified parameter table
  var x = 0
  for i in 1 .. column - 1:
    x = x + table.columnsWidth[i - 1]
  var itemId = tclEval2(script = table.canvas & " create rectangle " & $x &
      " " & $((table.row * table.rowHeight) + 5) & " " & $(x + 102) & " " & $((
      table.row * table.rowHeight) + (table.rowHeight - 10)) & " -fill " &
      tclEval2(script = "ttk::style lookup TProgressbar -troughcolor") &
          " -outline " & tclEval2(
          script = "ttk::style lookup TProgressbar -bordercolor") &
          " -tags [list progressbar" & $table.row & "back" & $column & "]")
  let backgroundColor = addBackground(table = table, newRow = newRow,
      command = command)
  addBindings(canvas = table.canvas, itemId = itemId, row = $table.row,
      command = command, color = backgroundColor)
  if tooltip.len > 0:
    tclEval(script = "tooltip::tooltip " & table.canvas & " -item " &
        itemId & " \"" & tooltip & "\"")
  let
    tclResult = tclEval2(script = table.canvas & " bbox " & itemId)
    coords = tclResult.split
  try:
    x = (coords[2].parseInt + 10) - coords[0].parseInt
  except ValueError:
    showError(message = "Can't add a progressbar to the table. Result: " & tclResult)
    return
  if x > table.columnsWidth[column - 1]:
    table.columnsWidth[column - 1] = x
  var color = ""
  let length: Natural = (100.0 + ((value.float - maxValue.float) /
      maxValue.float * 100.0)).Natural
  if invertColors:
    color = if length < 25:
        tclEval2(script = "ttk::style lookup green.Horizontal.TProgressbar -background")
      elif length > 24 and length < 75:
        tclEval2(script = "ttk::style lookup yellow.Horizontal.TProgressbar -background")
      else:
        tclEval2(script = "ttk::style lookup TProgressbar -background")
  else:
    color = if length > 74:
        tclEval2(script = "ttk::style lookup green.Horizontal.TProgressbar -background")
      elif length > 24:
        tclEval2(script = "ttk::style lookup yellow.Horizontal.TProgressbar -background")
      elif length > 0:
        tclEval2(script = "ttk::style lookup TProgressbar -background")
      else:
        tclEval2(script = "ttk::style lookup TProgressbar -troughcolor")
  itemId = tclEval2(script = table.canvas & " create rectangle " & $(x + 2) &
      " " & $((table.row * table.rowHeight) + 7) & " " & $(x + length) & " " &
      $((table.row * table.rowHeight) + (table.rowHeight - 12)) & " -fill " &
      color & " -tags [list progressbar" & $table.row & "bar" & $column & "]")
  addBindings(canvas = table.canvas, itemId = itemId, row = $table.row,
      command = command, color = backgroundColor)
  if tooltip.len > 0:
    tclEval(script = "tooltip::tooltip " & table.canvas & " -item " &
        itemId & " \"" & tooltip & "\"")
  if newRow:
    table.row.inc

proc addPagination*(table: TableWidget; previousCommand: string = "";
    nextCommand: string = "") {.raises: [], tags: [], contractual.} =
  ## Add pagination buttons to the bottom of the selected TableWidget
  ##
  ## * table           - the TableWidget to which the buttons will be added
  ## * previousCommand - the Tcl command executed when the previous page button
  ##                     is pressed
  ## * nextCommand     - the Tcl command executed when the next page button is
  ##                     pressed
  let buttonsFrame = table.canvas & ".buttonframe"
  tclEval(script = "ttk::frame " & buttonsFrame)
  var button: string
  if previousCommand.len > 0:
    button = buttonsFrame & ".previous"
    tclEval(script = "ttk::button " & button & " -text Previous -command {" &
        previousCommand & "}")
    tclEval(script = "grid " & button & " -sticky w")
    tclEval(script = "tooltip::tooltip " & button & " \"Previous page\"")
  if nextCommand.len > 0:
    button = buttonsFrame & ".next"
    tclEval(script = "ttk::button " & button & " -text Next -command {" &
        nextCommand & "}")
    tclEval(script = "grid " & button & " -sticky e -row 0 -column 1")
    tclEval(script = "tooltip::tooltip " & button & " \"Next page\"")
  tclEval(script = "update")
  tclEval(script = table.canvas & " create window 0 " & $(table.row *
      table.rowHeight) & " -anchor nw -window " & buttonsFrame)

proc addCheckButton*(table: var TableWidget; tooltip, command: string;
    checked: bool; column: Positive; newRow: bool = false;
    emptyUnchecked: bool = false) {.raises: [], tags: [WriteIOEffect,
    TimeEffect, RootEffect], contractual.} =
  ## Add checkbutton item to the selected TableWidget
  ##
  ## * table          - the TableWidget to which the checkbutton will be added
  ## * tooltip        - the tooltip text for the checkbutton
  ## * command        - the Tcl command which will be executed when checkbutton
  ##                    was clicked
  ## * checked        - if true, the checkbutton is checked
  ## * column         - the column in which the checkbutton will be placed
  ## * newRow         - if true, add a new row to the table after adding the
  ##                    checkbutton
  ## * emptyUnchecked - if true, show empty unchecked checkbox instead of red
  ##                    cross
  ##
  ## Returns modified parameter table
  var x = 5
  for i in 1 .. column - 1:
    x = x + table.columnsWidth[i - 1]
  let
    imageName = "${ttk::theme::" & tclEval2(script = "ttk::style theme use") &
      "::Images(checkbox-" & (if checked: "checked" else: (
      if emptyUnchecked: "unchecked-empty" else: "unchecked")) & ")}"
    itemId = tclEval2(script = table.canvas & " create image " & $x & " " & $((
        table.row * table.rowHeight) + 2) & " -anchor nw -image " & imageName &
        " -tags [list row" & $table.row & "col" & $column & "]")
  if tooltip.len > 0:
    tclEval(script = "tooltip::tooltip " & table.canvas & " -item " &
        itemId & " \"" & tooltip & "\"")
  let
    tclResult = tclEval2(script = table.canvas & " bbox " & itemId)
    coords = tclResult.split
  try:
    x = (coords[2].parseInt + 10) - coords[0].parseInt
  except ValueError:
    showError(message = "Can't add a checkbutton to the table. Result: " & tclResult)
    return
  if x > table.columnsWidth[column - 1]:
    table.columnsWidth[column - 1] = x
  if command.len > 0:

    proc addBackground(table: TableWidget; newRow: bool;
        command: string): string =
      result = (if table.row mod 2 > 0: tclEval2(
          script = "ttk::style lookup Table -rowcolor") else: tclEval2(
          script = "ttk::style lookup " & gameSettings.interfaceTheme &
          " -background"))
      if not newRow:
        return
      let itemId = tclEval2(script = table.canvas & " create rectangle 0 " & $(
          table.row * table.rowHeight) & " 10 " & $((table.row *
          table.rowHeight) + table.rowHeight) & " -fill " & result &
          " -width 0 -tags [list row" & $table.row & "]")
      tclEval(script = table.canvas & " lower " & itemId)
      addBindings(canvas = table.canvas, itemId = "row" & $table.row,
          row = $table.row, command = command, color = result)

    let backgroundColor = addBackground(table = table, newRow = newRow,
        command = command)
    addBindings(canvas = table.canvas, itemId = itemId, row = $table.row,
        command = command, color = backgroundColor)
  if newRow:
    table.row.inc

proc getColumnNumber*(table: TableWidget;
    xPosition: Natural): Positive {.raises: [], tags: [], contractual.} =
  ## Get the number of the column in the selected TableWidget according to
  ## the position in X axis
  ##
  ## * table     - the TableWidget in which the column will be checked
  ## * xPosition - the position in X axis which will be looking for
  ##
  ## The number of column, starts from 1, for the selected position in X axis
  var position = xPosition
  for index, width in table.columnsWidth:
    if position < width + 20:
      return index + 1
    position = position - width - 20
  return 1

proc updateHeadersCommand*(table: TableWidget; command: string) {.raises: [],
    tags: [], contractual.} =
  ## Update the selected TableWidget headers' Tcl command executed when the
  ## player clicks on a header
  ##
  ## * table   - the TableWidget in which headers will be updated
  ## * command - the Tcl command to assign to the headers. If empty, the
  ##             current command will be resetted.
  if command.len > 0:
    for i in table.columnsWidth.low .. table.columnsWidth.high:
      tclEval(script = table.canvas & " bind header" & $(i + 1) & " <Enter> {" &
          table.canvas & " configure -cursor hand1}")
      tclEval(script = table.canvas & " bind header" & $(i + 1) & " <Leave> {" &
          table.canvas & " configure -cursor left_ptr}")
      tclEval(script = table.canvas & " bind header" & $(i + 1) &
          " <Button-1> {" & command & " %x}")
      tclEval(script = table.canvas & " bind headerback" & $(i + 1) &
          " <Enter> {" & table.canvas & " configure -cursor hand1}")
      tclEval(script = table.canvas & " bind headerback" & $(i + 1) &
          " <Leave> {" & table.canvas & " configure -cursor left_ptr}")
      tclEval(script = table.canvas & " bind headerback" & $(i + 1) &
          " <Button-1> {" & command & " %x}")
  else:
    for i in table.columnsWidth.low .. table.columnsWidth.high:
      tclEval(script = table.canvas & " bind header" & $(i + 1) & " <Enter> {}")
      tclEval(script = table.canvas & " bind header" & $(i + 1) & " <Leave> {}")
      tclEval(script = table.canvas & " bind header" & $(i + 1) & " <Button-1> {}")
      tclEval(script = table.canvas & " bind headerback" & $(i + 1) & " <Enter> {}")
      tclEval(script = table.canvas & " bind headerback" & $(i + 1) & " <Leave> {}")
      tclEval(script = table.canvas & " bind headerback" & $(i + 1) & " <Button-1> {}")

proc updateCurrentRowCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [], cdecl, contractual,
    ruleOff: "params".} =
  ## Update the Tcl variable currentrow and show the currently selected row in
  ## the table
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## Returns tclOk if everything was set correctly, otherwise tclError
  ##
  ## Tcl:
  ## UpdateCurrentRow canvas action
  ## Canvas is the name of Table Tk_Canvas in which the current row will
  ## be updated, action is the name of action which will be taken. Can be
  ## raise or lower
  try:
    var currentRow: Natural = tclGetVar("currentrow").parseInt
    let maxRows: Natural = tclGetVar("maxrows").parseInt
    if argv[2] == "lower":
      currentRow.dec
      if currentRow == 0:
        currentRow = 1
    else:
      currentRow.inc
      if currentRow > maxRows:
        currentRow = maxRows
    let
      canvas = $argv[1]
      color = (if currentRow mod 2 > 0: tclEval2(
        script = "ttk::style lookup Table -rowcolor") else: tclEval2(
        script = "ttk::style lookup " & gameSettings.interfaceTheme &
        " -background"))
    tclEval(script = canvas & " row$currentrow -fill " & color)
    tclEval(script = canvas & " itemconfigure row" & $currentRow & " -fill " &
        tclEval2(script = "ttk::style lookup " & gameSettings.interfaceTheme &
        " -selectbackground"))
    tclSetVar("currentRow", $currentRow)
    return tclOk
  except:
    return tclError

proc executeCurrentRowCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [], cdecl,
        contractual.} =
  ## Excecut the Tcl command associated with the current row in the selected
  ## TableWidget
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## Returns tclOk if the command was executed correctly, otherwise tclError
  ##
  ## Tcl
  ## ExecuteCurrentRow canvas
  ## Canvas is the name of Table Tk_Canvas in which the Tcl command related
  ## to the current row will be executed
  let canvas = $argv[1]
  return tclEval(script = canvas & " bind row$currentrow <Button-1" & (
      if gameSettings.rightButton: "3" else: "1") & ">")

proc hideCurrentRowCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [], cdecl,
        contractual.} =
  ## Set the normal background for the current row in the selected TableWidget
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## Returns tclOk if the background was set correctly, otherwise tclError
  ##
  ## Tcl:
  ## HideCurrentRow canvas
  ## Canvas is the name of Table Tk_Canvas in which the selected row
  ## background will be recolored
  try:
    let
      canvas = $argv[1]
      color = (if tclGetVar("currentrow").parseInt mod 2 > 0: tclEval2(
          script = "ttk::style lookup Table -rowcolor") else: tclEval2(
          script = "ttk::style lookup " & gameSettings.interfaceTheme &
          " -background"))
    return tclEval(script = canvas & " itemconfigure row$currentrow -fill " & color)
  except:
    return tclError

proc isChecked*(table: TableWidget; row, column: Natural): bool {.raises: [],
    tags: [], contractual.} =
  ## Check if the selected checkbutton in the TableWidget is checked or not
  ##
  ## * table  - the TableWidget in which the checkbox will be checked
  ## * row    - the row in which the checkbox is
  ## * column - the column in which the checkbox is
  ##
  ## Returns true if the checkbox is checked, otherwise false.
  if tclEval2(script = table.canvas & " itemcget row" & $row & "col" & $column &
      " -image") == "checkbox-checked":
    return true
  return false

proc toggleCheckedButton*(table: TableWidget; row,
    column: Natural) {.raises: [], tags: [], contractual.} =
  ## Change the state of the selected checkbutton in the selected TableWidget
  ##
  ## * table  - the TableWidget in which the checkbox will be toggled
  ## * row    - the row in which the checkbox is
  ## * column - the column in which the checkbox is
  if isChecked(table, row, column):
    tclEval(script = table.canvas & " itemconfigure row" & $row & "col" &
        $column & " -image checkbox-unchecked-empty")
  else:
    tclEval(script = table.canvas & " itemconfigure row" & $row & "col" &
        $column & " -image checkbox-checked")

proc addCommands*() {.raises: [], tags: [WriteIOEffect, TimeEffect,
    RootEffect], contractual.} =
  ## Add Tcl commands related to the TableWidget
  try:
    addCommand("UpdateCurrentRow", updateCurrentRowCommand)
    addCommand("ExecuteCurrentRow", executeCurrentRowCommand)
    addCommand("HideCurrentRow", hideCurrentRowCommand)
  except:
    showError(message = "Can't add a Tcl command.")
