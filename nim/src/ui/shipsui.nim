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
import coreui, shipsuicrew, shipsuimodules, utilsui2

proc showShipInfoCommand*(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults =
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
  # Show or hide upgrade module info
  if playerShip.upgradeModule == -1:
    tclEval(script = "grid remove " & label)
    tclEval(script = "grid remove " & upgradeProgress)
    tclEval(script = "grid remove " & cancelButton)
  else:
    var
      upgradeInfo = "Upgrade:" & playerShip.modules[
          playerShip.upgradeModule].name & " "
      maxUpgrade = 0
    case playerShip.modules[playerShip.upgradeModule].upgradeAction
    of durability:
      upgradeInfo.add("(durability)")
      maxUpgrade = modulesList[playerShip.modules[
          playerShip.upgradeModule].protoIndex].durability
    of maxValue:
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
    of value:
      case modulesList[playerShip.modules[
          playerShip.upgradeModule].protoIndex].mType
      of engine:
        upgradeInfo.add("(fuel usage)")
        maxUpgrade = modulesList[playerShip.modules[
            playerShip.upgradeModule].protoIndex].value * 20
      else:
        discard
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
    tclEval(script = label & " configure -text {" & upgradeInfo & "}")
    tclEval(script = "grid " & label)
    tclEval(script = "grid " & upgradeProgress)
    tclEval(script = "grid " & cancelButton)
  # Show or hide repair priority info
  label = shipInfoFrame & ".repairlabel"
  cancelButton = shipInfoFrame & ".cancelpriority"
  if playerShip.repairModule == -1:
    tclEval(script = "grid remove " & label)
    tclEval(script = "grid remove " & cancelButton)
  else:
    tclEval(script = label & " configure -text {Repair first: " &
        playerShip.modules[playerShip.repairModule].name & "}")
    tclEval(script = "grid " & label)
    tclEval(script = "grid " & cancelButton)
  # Show or hide destination info
  label = shipInfoFrame & ".destinationlabel"
  cancelButton = shipInfoFrame & ".canceldestination"
  if playerShip.destinationX == 0 and playerShip.destinationY == 0:
    tclEval(script = "grid remove " & label)
    tclEval(script = "grid remove " & cancelButton)
  else:
    if skyMap[playerShip.destinationX][playerShip.destinationY].baseIndex > 0:
      tclEval(script = label & " configure -text {Destination: " & skyBases[
          skyMap[playerShip.destinationX][
          playerShip.destinationY].baseIndex].name & "}")
    else:
      tclEval(script = label & " configure -text {Destination: X: " &
          $playerShip.destinationX & " Y: " & $playerShip.destinationY & "}")
    tclEval(script = "grid " & label)
    tclEval(script = "grid " & cancelButton)
  label = shipInfoFrame & ".homelabel"
  tclEval(script = label & " configure -text {Home: " & skyBases[
      playerShip.homeBase].name & "}")
  label = shipInfoFrame & ".weight"
  tclEval(script = label & " configure -text {Weight: " & $countShipWeight(
      ship = playerShip) & "kg}")
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
  let typeBox = mainPaned & "shipinfoframe.cargo.canvas.frame.selecttype.combo"
  tclEval(script = typeBox & " set All")
  tclEval(script = "event generate " & typeBox & " <<ComboboxSelected>>")
  # Show ship info
  showScreen(newScreenName = "shipinfoframe")
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
