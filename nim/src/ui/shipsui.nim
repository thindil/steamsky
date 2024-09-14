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

import ../[game, tk]
import coreui, errordialog, shipsuicargo, shipsuicrew, shipsuimodules

proc setShipNameCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [], exportc.} =
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
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [], exportc.} =
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
    button = shipFrame & "." & $argv[1] & ".canvas.frame.maxmin"
  if argv[2] == "show":
    for frameInfo in frames:
      let frame = shipFrame & "." & frameInfo.name
      if frameInfo.name == $argv[1]:
        tclEval(script = "grid configure " & frame & " -columnspan 2 -rowspan 2 -row 0 -column 0")
      else:
        tclEval(script = "grid remove " & frame)
    tclEval(script = button & " configure -image movemapdownicon -command {ShipMaxMin " &
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
    tclEval(script = button & " configure -image movemapupicon -command {ShipMaxMin " &
        $argv[1] & " show}")
  return tclOk

proc addCommands*() {.sideEffect, raises: [], tags: [].} =
  ## Adds Tcl commands related to the wait menu
  try:
    shipsuimodules.addCommands()
    shipsuicrew.addCommands()
    shipsuicargo.addCommands()
#    addCommand("ShowShipInfo", showShipInfoCommand)
#    addCommand("SetShipName", setShipNameCommand)
#    addCommand("ShipMaxMin", shipMaxMinCommand)
  except:
    showError(message = "Can't add a Tcl command.")

# Temporary code for interfacing with Ada

proc addAdaShipsCommands() {.raises: [], tags: [RootEffect], exportc.} =
  try:
    addCommands()
  except:
    echo getCurrentExceptionMsg()
