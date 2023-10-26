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
  result = TableWidget()
  if scrollbar == ".":
    result.scrollbar = parent & ".scrolly"
    result.canvas = parent & ".table"
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

# Temporary code for interfacing with Ada

proc createAdaTable(parent: cstring; headers: array[10, cstring]; scrollbar,
    command, tooltipText: cstring) {.raises: [], tags: [], exportc.} =
  var nimHeaders: HeadersList = @[]
  for header in headers:
    if header.len > 0:
      nimHeaders.add($header)
  discard createTable(parent = $parent, headers = nimHeaders,
      scrollbar = $scrollbar, command = $command, tooltipText = $tooltipText)
