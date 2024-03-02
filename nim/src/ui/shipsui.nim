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
import coreui

proc showShipInfoCommand*(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults {.sideEffect, raises: [], tags: [
        WriteIOEffect, RootEffect].} =
  var
    shipInfoFrame = mainPaned & ".shipinfoframe"
    button = mainPaned & ".shipinfoframe.general.canvas.frame.rename"
  if tclEval2(script = "winfo exists " & shipInfoFrame) == "0":
    tclEvalFile(dataDirectory & "ui" & DirSep & "shipinfo.tcl")
    tclEval(script = button & " configure -image editicon")
    button = mainPaned & ".shipinfoframe.general.canvas.frame.showhome"
    tclEval(script = button & " configure -image showicon")
    button = mainPaned & ".shipinfoframe.general.canvas.frame.cancelupgrade"
    tclEval(script = button & " configure -image cancelicon")
    button = mainPaned & ".shipinfoframe.general.canvas.frame.canceldestination"
    tclEval(script = button & " configure -image cancelicon")
  elif tclEval2(script = "winfo exists " & shipInfoFrame) == "1" and argc == 1:
    tclEval(script = "InvokeButton " & closeButton)
    tclEval(script = "grid remove " & closeButton)
    for i in 0 .. 3:
      tclEval(script = "bind . <" & generalAccelerators[i] & "> {}")
    return tclOk
  let shipCanvas = shipInfoFrame & ".general.canvas"
  tclEval(script = "bind . <" & generalAccelerators[0] & "> {InvokeButton " &
      shipCanvas & ".frame.maxmin}")
  tclEval(script = "bind . <" & generalAccelerators[2] & "> {InvokeButton " &
      shipInfoFrame & ".modules.canvas.frame.maxmin}")
  tclEval(script = "bind . <" & generalAccelerators[1] & "> {InvokeButton " &
      shipInfoFrame & ".crew.canvas.frame.maxmin}")
  tclEval(script = "bind . <" & generalAccelerators[3] & "> {InvokeButton " &
      shipInfoFrame & ".cargo.canvas.frame.maxmin}")
  tclEval(script = "grid " & closeButton & " -row 0 -column 1")
  shipInfoFrame = mainPaned & ".shipinfoframe.general.canvas.frame"
  var label = shipInfoFrame & ".name"
  tclEval(script = label & " configure -text {Name: " & playerShip.name & "}")
  label = shipInfoFrame & ".upgradelabel"
  let upgradeProgress = shipInfoFrame & ".upgrade"
  var cancelButton = shipInfoFrame & ".cancelupgrade"
  if playerShip.upgradeModule == -1:
    tclEval(script = "grid remove " & label)
    tclEval(script = "grid remove " & upgradeProgress)
    tclEval(script = "grid remove " & cancelButton)
  return tclOk

proc addCommands*() {.sideEffect, raises: [], tags: [].} =
  ## Adds Tcl commands related to the wait menu
  try:
    addCommand("ShowShipInfo", showShipInfoCommand)
  except:
    tclEval(script = "bgerror {Can't add a Tcl command. Reason: " &
        getCurrentExceptionMsg() & "}")

# Temporary code for interfacing with Ada

proc addAdaShipsCommands() {.raises: [], tags: [RootEffect], exportc.} =
  try:
    addCommands()
  except:
    echo getCurrentExceptionMsg()
