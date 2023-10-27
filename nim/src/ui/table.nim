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

import std/strutils
import ../tk

type
  TableWidget* = object
    canvas*: string
    columnsWidth*: seq[Positive]
    row*: Positive = 1
    rowHeight*: Positive = 1
    scrollbar*: string
  HeadersList* = seq[string]

proc createTable*(parent: string; headers: HeadersList; scrollbar: string = ".";
    command: string = ""; tooltipText: string = ""): TableWidget =
  let interp = getInterp()
  result = TableWidget(canvas: parent & ".table")
  if scrollbar == ".":
    result.scrollbar = parent & ".scrolly"
    interp.tclEval("ttk::scrollbar " & result.scrollbar &
        " -orient vertical -command [list " & result.canvas & " yview]")
    interp.tclEval("ttk::scrollbar " & parent &
        ".scrollx -orient vertical -command [list " & result.canvas & " xview]")
    interp.tclEval("canvas " & result.canvas & " -yscrollcommand [list " &
        result.scrollbar & " set] -xscrollcommand [list " & parent & ".scrollx set]")
    interp.tclEval("pack " & result.scrollbar & " -side right -fill y")
    interp.tclEval("pack " & result.canvas & " -side top -fill both -padx {5 0}")
    interp.tclEval("pack " & parent & ".scrollx -side bottom -fill x")
    interp.tclEval("::autoscroll::autoscroll " & parent & ".scrollx")
    interp.tclEval("::autoscroll::autoscroll " & result.scrollbar)
  else:
    result.scrollbar = scrollbar
    interp.tclEval("grid " & result.canvas & " -sticky nwes -padx {5 0}")
    interp.tclEval("grid columnconfigure " & parent & " " & result.canvas & " -weight 1")
    interp.tclEval("grid rowconfigure " & parent & " " & result.canvas & " -weight 1")
  var x = 0
  for index, header in headers:
    interp.tclEval("ttk::style lookup Table -headerforecolor")
    var fillStyle = $(interp.tclGetResult)
    interp.tclEval(result.canvas & " create text " & $x &
        " 2 -anchor nw -text {" & header &
        "} -font InterfaceFont -justify center -fill " & fillStyle &
        " -tags [list header" & $index & "]")
    let headerId = $(interp.tclGetResult)
    if command.len > 0:
      interp.tclEval(result.canvas & " bind " & headerId & " <Enter> {" &
          result.canvas & " configure -cursor hand1}")
      interp.tclEval(result.canvas & " bind " & headerId & " <Leave> {" &
          result.canvas & " configure -cursor left_ptr}")
      interp.tclEval(result.canvas & " bind " & headerId & " <Button-1> {" &
          command & " %x}")
    if tooltipText.len > 0:
      interp.tclEval("tooltip::tooltip " & result.canvas & " -item " &
          headerId & " \"" & tooltipText & "\"")
    interp.tclEval(result.canvas & " bbox header" & $index)
    let
      tclResult = $(interp.tclGetResult)
      coords = tclResult.split
    var oldX = x - 5
    x = coords[2].parseInt + 5
    result.columnsWidth.add(x - coords[0].parseInt)
    if index == 0:
      result.rowHeight = coords[3].parseInt + 5
    interp.tclEval("ttk::style lookup Table -headerbackcolor")
    fillStyle = $(interp.tclGetResult)
    interp.tclEval("ttk::style lookup Table -headerbordercolor")
    var borderStyle = $(interp.tclGetResult)
    interp.tclEval(result.canvas & " create rectangle " & $oldX & " 0 " & $(x -
        2) & " " & $(result.rowHeight - 3) & " -fill " & fillStyle &
        " -outline " & $borderStyle & " -width 2 -tags [list headerback" &
        $index & "]")
    let backgroundId = $(interp.tclGetResult)
    interp.tclEval(result.canvas & " lower headerback" & $index)
    if command.len > 0:
      interp.tclEval(result.canvas & " bind " & backgroundId & " <Enter> {" &
          result.canvas & " configure -cursor hand1}")
      interp.tclEval(result.canvas & " bind " & backgroundId & " <Leave> {" &
          result.canvas & " configure -cursor left_ptr}")
      interp.tclEval(result.canvas & " bind " & backgroundId & " <Button-1> {" &
          command & " %x}")
    if tooltipText.len > 0:
      interp.tclEval("tooltip::tooltip " & result.canvas & " -item " &
          backgroundId & " \"" & tooltipText & "\"")
  interp.tclEval("SetScrollBarBindings " & result.canvas & " " &
      result.scrollbar)
  interp.tclEval("bind " & result.canvas & " <Up> {UpdateCurrentRow" &
      result.canvas & " lower}")
  interp.tclEval("bind " & result.canvas & " <Down> {UpdateCurrentRow" &
      result.canvas & " raise}")
  interp.tclEval("bind " & result.canvas & " <Key-space> {ExecuteCurrentRow" &
      result.canvas & "}")
  interp.tclEval("bind " & result.canvas & " <FocusOut> {HideCurrentRow" &
      result.canvas & "}")
  interp.tclEval("bind " & result.canvas & " <Leave> {HideCurrentRow" &
      result.canvas & "}")

# Temporary code for interfacing with Ada

proc createAdaTable(parent: cstring; headers: array[10, cstring]; scrollbar,
    command, tooltipText: cstring; adaCanvas, adaScrollbar: var cstring;
    height: var cint; adaWidth: var array[10, cint]) {.raises: [], tags: [], exportc.} =
  var nimHeaders: HeadersList = @[]
  for header in headers:
    if header.len > 0:
      nimHeaders.add($header)
  try:
    let newTable = createTable(parent = $parent, headers = nimHeaders,
        scrollbar = $scrollbar, command = $command, tooltipText = $tooltipText)
    adaCanvas = newTable.canvas.cstring
    for width in adaWidth.mitems:
      width = 0
    for index, width in newTable.columnsWidth:
      adaWidth[index] = width.cint
    height = newTable.rowHeight.cint
    adaScrollbar = newTable.scrollbar.cstring
  except:
    discard
