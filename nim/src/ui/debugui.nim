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

proc refreshCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.exportc.} =
  let frameName = ".debugdialog.main"
  var spinBox = frameName & ".ship.x"
  tclEval(script = spinBox & " set " & $playerShip.skyX)
  spinBox = frameName & ".ship.y"
  tclEval(script = spinBox & " set " & $playerShip.skyY)
  var valuesList = ""
  for module in playerShip.modules:
    valuesList.add(y = " {" & module.name & "}")
  var comboBox = frameName & ".ship.module"
  tclEval(script = comboBox & " configure -values [list" & valuesList & "]")
  tclEval(script = comboBox & " current 0")
  return tclOk

proc showDebugUi*() =
  tclEvalFile(fileName = dataDirectory & DirSep & "debug.tcl")
#    addCommand("Refresh", refreshCommand)
