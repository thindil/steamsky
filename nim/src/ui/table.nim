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
    interp.tclEval("ttk::scrollbar " & parent &
        ".scrolly -orient vertical -command [list " & parent & ".table yview]")
    interp.tclEval("ttk::scrollbar " & parent &
        ".scrollx -orient vertical -command [list " & parent & ".table xview]")
    result.canvas = parent & ".table"
    interp.tclEval("canvas " & result.canvas & " -yscrollcommand [list " &
        parent & ".scrolly set] -xscrollcommand [list " & parent & ".scrollx set]")

# Temporary code for interfacing with Ada

proc createAdaTable(parent: cstring; headers: array[10, cstring]; scrollbar,
    command, tooltipText: cstring) {.raises: [], tags: [], exportc.} =
  discard
