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

import std/strutils
import ../tk
import coreui

const buttonNames: array[1 .. 13, string] = ["show", "nw", "n", "ne", "w",
    "wait", "e", "sw", "s", "se", "hide", "left", "right"]

proc hideMapButtonsCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults {.sideEffect, raises: [], tags: [].} =
  ## Hide buttons used to move the map
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## HideMapButtons
  for i in 2 .. 13:
    let buttonName = mainPaned & ".mapframe.buttons." & buttonNames[i]
    tclEval(script = "grid remove " & buttonName)
  let buttonName = mainPaned & ".mapframe.buttons.show"
  tclEval(script = "grid " & buttonName)
  return tclOk

proc showMapButtonsCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults =
  let buttonsBox = mainPaned & ".mapframe.buttons"
  for i in 2 .. 11:
    let buttonName = buttonsBox & "." & buttonNames[i]
    tclEval(script = "grid " & buttonName)
  var buttonName = buttonsBox & ".show"
  tclEval(script = "grid remove " & buttonName)
  buttonName = (if tclEval2(script = "grid info " & buttonsBox).contains(
      "-sticky es"): buttonsBox & ".left" else: buttonsBox & ".right")
  tclEval(script = "grid " & buttonName)
  return tclOk

proc addCommands*() =
  addCommand("HideMapButtons", hideMapButtonsCommand)
  addCommand("ShowMapButtons", showMapButtonsCommand)

# Temporary code for interfacing with Ada

proc addAdaMapsCommands() {.raises: [], tags: [RootEffect], exportc.} =
  try:
    addCommands()
  except:
    echo getCurrentExceptionMsg()

