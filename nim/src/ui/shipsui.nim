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

import std/[os, tables]
import ../[config, game, maps, ships, tk]
import coreui, shipsuicargo, shipsuicrew, utilsui2, shipsuimodules, shipsuimodules2

proc showShipInfoCommand*(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults {.sideEffect, raises: [], tags: [
        RootEffect], exportc.} =
  ## Show information about the player's ship
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ShowShipInfo
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
    button = mainPaned & ".shipinfoframe.general.canvas.frame.cancelpriority"
    tclEval(script = button & " configure -image cancelicon")
    button = mainPaned & ".shipinfoframe.general.canvas.frame.canceldestination"
    tclEval(script = button & " configure -image cancelicon")
  elif tclEval2(script = "winfo ismapped " & shipInfoFrame) == "1" and argc == 1:
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
  # Show or hide upgrade module info
  if playerShip.upgradeModule == -1:
    tclEval(script = "grid remove " & label)
    label = shipInfoFrame & ".upgradelbl"
    tclEval(script = "grid remove " & label)
    tclEval(script = "grid remove " & upgradeProgress)
    tclEval(script = "grid remove " & cancelButton)
  else:
    var
      upgradeInfo = playerShip.modules[
          playerShip.upgradeModule].name & " "
      maxUpgrade = 0
    case playerShip.modules[playerShip.upgradeModule].upgradeAction
    of durability:
      upgradeInfo.add("(durability)")
      maxUpgrade = try:
          modulesList[playerShip.modules[
              playerShip.upgradeModule].protoIndex].durability
        except:
          return showError(message = "Can't set max upgrade info.")
    of maxValue:
      try:
        case modulesList[playerShip.modules[
            playerShip.upgradeModule].protoIndex].mType
        of engine:
          upgradeInfo.add("(power)")
          maxUpgrade = (modulesList[playerShip.modules[
              playerShip.upgradeModule].protoIndex].maxValue / 20).int
        of cabin:
          upgradeInfo.add("(quality)")
          maxUpgrade = modulesList[playerShip.modules[
              playerShip.upgradeModule].protoIndex].maxValue
        of gun, batteringRam:
          upgradeInfo.add("(quality)")
          maxUpgrade = modulesList[playerShip.modules[
              playerShip.upgradeModule].protoIndex].maxValue * 2
        of hull:
          upgradeInfo.add("(enlarge)")
          maxUpgrade = modulesList[playerShip.modules[
              playerShip.upgradeModule].protoIndex].maxValue * 40
        of harpoonGun:
          upgradeInfo.add("(strength)")
          maxUpgrade = modulesList[playerShip.modules[
              playerShip.upgradeModule].protoIndex].maxValue * 10
        else:
          discard
      except:
        return showError(message = "Can't set upgrade info.")
    of value:
      try:
        case modulesList[playerShip.modules[
            playerShip.upgradeModule].protoIndex].mType
        of engine:
          upgradeInfo.add("(fuel usage)")
          maxUpgrade = modulesList[playerShip.modules[
              playerShip.upgradeModule].protoIndex].value * 20
        else:
          discard
      except:
        return showError(message = "Can't set upgrade fuel usage info.")
    else:
      discard
    maxUpgrade = (maxUpgrade.float * newGameSettings.upgradeCostBonus).int
    if maxUpgrade == 0:
      maxUpgrade = 1
    let
      upgradePercent = 1.0 - (playerShip.modules[
          playerShip.upgradeModule].upgradeProgress.float / maxUpgrade.float)
      progressBarStyle = if upgradePercent > 0.74:
          " -style green.Horizontal.TProgressbar"
        elif upgradePercent > 0.24:
          " -style yellow.Horizontal.TProgressbar"
        else:
          " -style Horizontal.TProgressbar"
    tclEval(script = upgradeProgress & " configure -value " & $upgradePercent & progressBarStyle)
    tclEval(script = "grid " & label)
    label = shipInfoFrame & ".upgradelbl"
    tclEval(script = "grid " & label)
    tclEval(script = label & " configure -text {" & upgradeInfo & "}")
    tclEval(script = "grid " & upgradeProgress)
    tclEval(script = "grid " & cancelButton)
  # Show or hide repair priority info
  label = shipInfoFrame & ".repairlabel"
  cancelButton = shipInfoFrame & ".cancelpriority"
  if playerShip.repairModule == -1:
    tclEval(script = "grid remove " & label)
    label = shipInfoFrame & ".repairlbl"
    tclEval(script = "grid remove " & label)
    tclEval(script = "grid remove " & cancelButton)
  else:
    tclEval(script = label & " configure -text {Repair first: }")
    tclEval(script = "grid " & label)
    label = shipInfoFrame & ".repairlbl"
    tclEval(script = label & " configure -text {" &
        playerShip.modules[playerShip.repairModule].name & "}")
    tclEval(script = "grid " & label)
    tclEval(script = "grid " & cancelButton)
  # Show or hide destination info
  label = shipInfoFrame & ".destinationlabel"
  cancelButton = shipInfoFrame & ".canceldestination"
  if playerShip.destinationX == 0 and playerShip.destinationY == 0:
    tclEval(script = "grid remove " & label)
    label = shipInfoFrame & ".destinationlbl"
    tclEval(script = "grid remove " & label)
    tclEval(script = "grid remove " & cancelButton)
  else:
    if skyMap[playerShip.destinationX][playerShip.destinationY].baseIndex > 0:
      tclEval(script = label & " configure -text {Destination: }")
      let label2 = shipInfoFrame & ".destinationlbl"
      tclEval(script = label2 & " configure -text {" & skyBases[
          skyMap[playerShip.destinationX][
          playerShip.destinationY].baseIndex].name & "}")
    else:
      tclEval(script = label & " configure -text {Destination: }")
      let label2 = shipInfoFrame & ".destinationlbl"
      tclEval(script = label2 & " configure -text {X: " &
          $playerShip.destinationX & " Y: " & $playerShip.destinationY & "}")
    tclEval(script = "grid " & label)
    label = shipInfoFrame & ".destinationlbl"
    tclEval(script = "grid " & label)
    tclEval(script = "grid " & cancelButton)
  label = shipInfoFrame & ".homelabel"
  tclEval(script = label & " configure -text {Home: }")
  label = shipInfoFrame & ".homelbl"
  tclEval(script = label & " configure -text {" & skyBases[
      playerShip.homeBase].name & "}")
  label = shipInfoFrame & ".weight"
  tclEval(script = label & " configure -text {Weight: }")
  label = shipInfoFrame & ".weight2"
  try:
    discard tclEval(script = label & " configure -text {" &
        $countShipWeight(ship = playerShip) & "kg}")
  except:
    return showError(message = "Can't show the weight of the ship.")
  tclEval(script = "update")
  tclEval(script = shipCanvas & " configure -scrollregion [list " & tclEval2(
      script = shipCanvas & " bbox all") & "]")
  tclEval(script = shipCanvas & " xview moveto 0.0")
  tclEval(script = shipCanvas & " yview moveto 0.0")
  # Setting ship module info
  updateModulesInfo()
  # Setting crew info
  updateCrewInfo()
  # Setting cargo info
  let typeBox = mainPaned & ".shipinfoframe.cargo.canvas.frame.selecttype.combo"
  tclEval(script = typeBox & " set All")
  tclEval(script = "event generate " & typeBox & " <<ComboboxSelected>>")
  # Show ship info
  showScreen(newScreenName = "shipinfoframe")
  return tclOk

proc setShipNameCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults {.sideEffect, raises: [], tags: [], exportc.} =
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
    argv: openArray[cstring]): TclResults {.sideEffect, raises: [], tags: [], exportc.} =
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
