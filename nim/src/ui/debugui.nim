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

import std/[os, strutils, tables]
import ../[game, items, tk]

proc refreshModuleCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [], exportc.} =
  ## Refresh the information about selected module
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## RefreshModule
  let
    frameName = ".debugdialog.main.ship"
    moduleCombo = frameName & ".module"
    moduleIndex = try:
        tclEval2(script = moduleCombo & " current").parseInt
      except:
        return showError(message = "Can't get the module's index.")
    protoCombo = frameName & ".proto"
  try:
    tclEval(script = protoCombo & " set {" & modulesList[playerShip.modules[
        moduleIndex].protoIndex].name & "}")
  except:
    return showError(message = "Can't get the proto module.")
  var spinBox = frameName & ".weight"
  tclEval(script = spinBox & " set " & $playerShip.modules[moduleIndex].weight)
  spinBox = frameName & ".dur"
  tclEval(script = spinBox & " set " & $playerShip.modules[
      moduleIndex].durability)
  spinBox = frameName & ".maxdur"
  tclEval(script = spinBox & " set " & $playerShip.modules[
      moduleIndex].maxDurability)
  spinBox = frameName & ".upgrade"
  tclEval(script = spinBox & " set " & $playerShip.modules[
      moduleIndex].upgradeProgress)
  return tclOk

proc refreshMemberCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [], exportc.} =
  ## Refresh the information about selected crew member
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## RefreshMember
  let frameName = ".debugdialog.main.crew"
  var comboBox = frameName & ".member"
  let
    memberIndex = try:
        tclEval2(script = comboBox & " current").parseInt
      except:
        return showError(message = "Can't get the member index.")
    member = playerShip.crew[memberIndex]
  var spinBox = frameName & ".stats2.health"
  tclEval(script = spinBox & " set " & $member.health)
  spinBox = frameName & ".stats2.thirst"
  tclEval(script = spinBox & " set " & $member.thirst)
  spinBox = frameName & ".stats2.hunger"
  tclEval(script = spinBox & " set " & $member.hunger)
  spinBox = frameName & ".stats2.tired"
  tclEval(script = spinBox & " set " & $member.tired)
  spinBox = frameName & ".stats2.morale"
  tclEval(script = spinBox & " set " & $member.morale[1])
  spinBox = frameName & ".stats2.loyalty"
  tclEval(script = spinBox & " set " & $member.loyalty)
  var
    memberFrame = frameName & ".stats"
    rows = try:
        tclEval2(script = "grid size " & memberFrame).split[1].parseInt
      except:
        return showError(message = "Can't get the amount of rows.")
  deleteWidgets(startIndex = 1, endIndex = rows - 1, frame = memberFrame)
  for index, attribute in member.attributes:
    let label = memberFrame & ".label" & $(index + 1)
    tclEval(script = "ttk::label " & label & " -text {" & attributesList[
        index].name & "}")
    tclEval(script = "grid " & label)
    spinBox = memberFrame & ".value" & $(index + 1)
    tclEval(script = "ttk::spinbox " & spinBox &
        " -from 1 -to 50 -validate key -validatecommand {ValidateSpinbox %W %P " &
        frameName & ".change} -width 5")
    tclEval(script = spinBox & " set " & $attribute.level)
    tclEval(script = "grid " & spinBox & " -column 1 -row " & $(index + 1))
  memberFrame = frameName & ".skills"
  rows = try:
      tclEval2(script = "grid size " & memberFrame).split[1].parseInt
    except:
      return showError(message = "Can't get the amount of rows (2).")
  deleteWidgets(startIndex = 1, endIndex = rows - 1, frame = memberFrame)
  var skillsIndexes: seq[Natural]
  for index, skill in member.skills:
    let label = memberFrame & ".label" & $(index + 1)
    try:
      tclEval(script = "ttk::label " & label & " -text {" & skillsList[
          skill.index].name & "}")
    except:
      return showError(message = "Can't add the skill label.")
    tclEval(script = "grid " & label)
    spinBox = memberFrame & ".value" & $(index + 1)
    tclEval(script = "ttk::spinbox " & spinBox &
        " -from 1 -to 100 -validate key -validatecommand {ValidateSpinbox %W %P " &
        frameName & ".change} -width 5")
    tclEval(script = spinBox & " set " & $skill.level)
    tclEval(script = "grid " & spinBox & " -column 1 -row " & $(index + 1))
    skillsIndexes.add(y = skill.index)
  var skillsListValues = ""
  for index, skill in skillsList:
    if index notin skillsIndexes:
      skillsListValues.add(y = " " & skill.name)
  comboBox = frameName & ".addskill.skills"
  tclEval(script = comboBox & " configure -values [list" & $skillsListValues & "]")
  tclEval(script = comboBox & " current 0")
  return tclOk

proc refreshCargoCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [], exportc.} =
  let
    frameName = ".debugdialog.main.cargo"
    cargoCombo = frameName & ".update"
    itemIndex = try:
        tclEval2(script = cargoCombo & " current").parseInt
      except:
        return showError(message = "Can't get the item index.")
    amountBox = frameName & ".updateamount"
  tclEval(script = amountBox & " set " & $playerShip.cargo[itemIndex].amount)
  return tclOk

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
  discard refreshModuleCommand(clientData = clientData, interp = interp,
      argc = argc, argv = argv)
  comboBox = frameName & ".crew.member"
  valuesList = ""
  for member in playerShip.crew:
    valuesList.add(y = " {" & member.name & "}")
  tclEval(script = comboBox & " configure -values [list" & valuesList & "]")
  tclEval(script = comboBox & " current 0")
  discard refreshMemberCommand(clientData = clientData, interp = interp,
      argc = argc, argv = argv)
  comboBox = frameName & ".cargo.update"
  valuesList = ""
  for item in playerShip.cargo:
    valuesList.add(y = " {" & getItemName(item = item, damageInfo = false,
        toLower = false) & "}")
  tclEval(script = comboBox & " configure -values [list" & valuesList & "]")
  tclEval(script = comboBox & " current 0")
  return tclOk

proc showDebugUi*() =
  tclEvalFile(fileName = dataDirectory & DirSep & "debug.tcl")
#    addCommand("Refresh", refreshCommand)
#    addCommand("RefreshModule", refreshModuleCommand)
#    addCommand("RefreshMember", refreshMemberCommand)
#    addCommand("RefreshCargo", refreshCargoCommand)
