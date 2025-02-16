# Copyright 2024-2025 Bartek thindil Jasicki
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

## Provides code related to the information about the player's ship, like
## minimizing/maximizin its sections, setting the ship's name, etc.

import ../[game, tk]
import contracts, nimalyzer
import coreui, errordialog, shipsuicargo, shipsuicrew, shipsuimodules, showshipinfo

proc setShipNameCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [], cdecl,
        contractual, ruleOff: "params".} =
  ## Change name of the player's ship
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## SetShipName shipname
  ## Shipname is the new name for the player's ship
  if argc == 1:
    return tclOk
  let nameEntry = mainPaned & ".shipinfoframe.general.canvas.frame.name"
  playerShip.name = $argv[1]
  tclEval(script = nameEntry & " configure -text {Name: " & $argv[1] & "}")
  return tclOk

proc shipMaxMinCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [], cdecl,
        contractual.} =
  ## Maximize or minimize the selected section of the player's ship info
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ShipMaxMin framename
  ## Framename is name of the frame to maximize or minimize
  type FrameInfo = object
    name: string
    column: range[0 .. 1]
    row: range[0 .. 1]
  let
    frames: array[1 .. 4, FrameInfo] = [FrameInfo(name: "general", column: 0,
        row: 0), FrameInfo(name: "modules", column: 0, row: 1), FrameInfo(
        name: "crew", column: 1, row: 0), FrameInfo(name: "cargo", column: 1, row: 1)]
    shipFrame = mainPaned & ".shipinfoframe"
    button = shipFrame & "." & $argv[1] & ".canvas.frame.maxmin.maxmin"
  if argv[2] == "show":
    for frameInfo in frames:
      let frame = shipFrame & "." & frameInfo.name
      if frameInfo.name == $argv[1]:
        tclEval(script = "grid configure " & frame & " -columnspan 2 -rowspan 2 -row 0 -column 0")
      else:
        tclEval(script = "grid remove " & frame)
    tclEval(script = button & " configure -image contracticon -command {ShipMaxMin " &
        $argv[1] & " hide}")
  else:
    for frameInfo in frames:
      let frame = shipFrame & "." & frameInfo.name
      if frameInfo.name == $argv[1]:
        tclEval(script = "grid configure " & frame &
            " -columnspan 1 -rowspan 1 -row " & $frameInfo.row & " -column " &
            $frameInfo.column)
      else:
        tclEval(script = "grid " & frame)
    tclEval(script = button & " configure -image expandicon -command {ShipMaxMin " &
        $argv[1] & " show}")
  return tclOk

proc shipMoreCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [], cdecl,
        contractual.} =
  ## Maximize or minimize the selected part in the player's ship info
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ShipMore framename show/hide
  ## Framename is name of the frame in which the part will be shown or hidden.
  ## If the second argument is set to show, show the part, otherwise hide it.
  let
    shipFrame = mainPaned & ".shipinfoframe"
    button = shipFrame & "." & $argv[1] & ".canvas.frame.maxmin.more"
  if argv[1] == "crew":
    if argv[2] == "show":
      tclEval(script = "grid " & shipFrame & ".crew.canvas.frame.ordersbuttons -sticky w -row 1")
      tclEval(script = "grid " & shipFrame & ".crew.canvas.frame.selectskill -sticky w -row 2")
    else:
      tclEval(script = "grid remove " & shipFrame & ".crew.canvas.frame.ordersbuttons")
      tclEval(script = "grid remove " & shipFrame & ".crew.canvas.frame.selectskill")
  elif argv[1] == "cargo":
    if argv[2] == "show":
      tclEval(script = "grid " & shipFrame & ".cargo.canvas.frame.selecttype -sticky w -row 2")
    else:
      tclEval(script = "grid remove " & shipFrame & ".cargo.canvas.frame.selecttype")
  if argv[2] == "show":
    tclEval(script = button & " configure -command {ShipMore " &
        $argv[1] & " hide}")
  else:
    tclEval(script = button & " configure -command {ShipMore " &
        $argv[1] & " show}")
  return tclOk

proc addCommands*() {.raises: [], tags: [WriteIOEffect,
    TimeEffect, RootEffect], contractual.} =
  ## Adds Tcl commands related to the wait menu
  try:
    shipsuimodules.addCommands()
    shipsuicrew.addCommands()
    shipsuicargo.addCommands()
    addCommand(name = "ShowShipInfo", nimProc = showShipInfoCommand)
    addCommand(name = "SetShipName", nimProc = setShipNameCommand)
    addCommand(name = "ShipMaxMin", nimProc = shipMaxMinCommand)
    addCommand(name = "ShipMore", nimProc = shipMoreCommand)
  except:
    showError(message = "Can't add a Tcl command.")
