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

import std/os
import ../[game, tk]
import coreui, mapsui, table

var
  baseTable: TableWidget

proc showBaseUiCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.exportc.} =
  var baseFrame = mainPaned & ".baseframe"
  let baseCanvas = baseFrame & ".canvas"
  if tclEval2(script = "winfo exists " & baseCanvas) == "0":
    tclEvalFile(fileName = dataDirectory & "ui" & DirSep & "base.tcl")
    tclEval(script = "bind " & baseFrame & " <Configure> {ResizeCanvas %W.canvas %h}")
  elif tclEval2(script = "winfo ismapped " & baseCanvas) == "1" and argc == 1:
    tclEval(script = "grid remove " & closeButton)
    showSkyMap(clear = true)
    return tclOk
  baseFrame = baseCanvas & ".base"
  if tclEval2(script = "winfo exists " & baseFrame & ".table") == "1":
    tclEval(script = "destroy " & baseTable.canvas)
  return tclOk

proc addCommands*() {.sideEffect, raises: [], tags: [].} =
  ## Adds Tcl commands related to the trades UI
  try:
    discard
#    addCommand("ShowBaseUi", showBaseUiCommand)
  except:
    showError(message = "Can't add a Tcl command.")
