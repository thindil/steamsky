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
import ../[basestrade, crew, crewinventory, game, tk, types]
import coreui, dialogs, mapsui, utilsui2

proc setSchoolSkillsCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [], exportc.} =
  ## Set list of available to train skills for the selected crew member
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## SetSchoolSkills
  let frameName = mainPaned & ".schoolframe.canvas.school"
  var comboBox = frameName & ".setting.crew"
  let memberIndex = try:
      tclEval2(script = comboBox & " current").parseInt
    except:
      return showError(message = "Can't get the member's index")
  var comboList = ""
  for index, skill in skillsList:
    var skillLevel = 0
    for skill2 in playerShip.crew[memberIndex].skills:
      if skill2.index == index:
        skillLevel = skill2.level
        if skillLevel == 100:
          break
    if skillLevel != 100:
      comboList.add(y = " {" & skill.name & ": " & (if skillLevel ==
          0: "Untrained" else: getSkillLevelName(
              skillLevel = skillLevel).strip) & "}")
  comboBox = frameName & ".setting.skill"
  let
    oldComboList = tclEval2(script = comboBox & " cget -values")
    spinBox = frameName & ".amountbox.amount"
  if oldComboList != comboList:
    tclEval(script = comboBox & " configure -values [list " & comboList & "]")
    tclEval(script = comboBox & " current 0")
    tclEval(script = spinBox & " set 1")
  tclEval(script = "UpdateSchoolCost " & spinBox & " " & tclEval2(
      script = spinBox & " get"))
  tclEval(script = "UpdateSchoolSelectedCost")
  return tclOk

proc showSchoolCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [], exportc.} =
  ## Show the selected base school
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ShowSchool
  var schoolFrame = mainPaned & ".schoolframe"
  let schoolCanvas = schoolFrame & ".canvas"
  if tclEval2(script = "winfo exists " & schoolCanvas) == "0":
    tclEvalFile(fileName = dataDirectory & "ui" & DirSep & "school.tcl")
    tclEval(script = "bind " & schoolFrame & " <Configure> {ResizeCanvas %W.canvas %w %h}")
  elif tclEval2(script = "winfo ismapped " & schoolCanvas) == "1" and argc == 1:
    tclEval(script = "grid remove " & closeButton)
    showSkyMap(clear = true)
    return tclOk
  tclSetVar(varName = "gamestate", newValue = "crew")
  let
    moneyIndex2 = findItem(inventory = playerShip.cargo,
        protoIndex = moneyIndex)
    moneyLabel = schoolCanvas & ".school.money"
  if moneyIndex2 > -1:
    tclEval(script = moneyLabel & " configure -text {You have " &
        $playerShip.cargo[moneyIndex2].amount & " " & moneyName & ".}")
  else:
    tclEval(script = moneyLabel & " configure -text {You don't have any " &
        moneyName & " to pay for learning.}")
  schoolFrame = schoolCanvas & ".school"
  if argc == 1:
    var comboList = ""
    for member in playerShip.crew:
      comboList.add(y = " " & member.name)
    let comboBox = schoolCanvas & ".school.setting.crew"
    tclEval(script = comboBox & " configure -values [list" & comboList & "]")
    tclEval(script = comboBox & " current 0")
  discard setSchoolSkillsCommand(clientData = clientData, interp = interp,
      argc = argc, argv = argv)
  tclEval(script = "grid " & closeButton & " -row 0 -column 1")
  tclEval(script = schoolCanvas & " configure -height [expr " & tclEval2(
      script = mainPaned & " sashpos 0") & " - 20] -width " & tclEval2(
      script = mainPaned & " cget -width"))
  tclEval(script = "update")
  tclEval(script = schoolCanvas & " create window 0 0 -anchor nw -window " & schoolFrame)
  tclEval(script = "update")
  tclEval(script = schoolCanvas & " configure -scrollregion [list " & tclEval2(
      script = schoolCanvas & " bbox all") & "]")
  let
    comboBox = schoolCanvas & ".school.costbox.amount"
    trainButton = schoolCanvas & ".school.setting.train"
  tclEval(script = "bind " & comboBox & " <Tab> {focus " & trainButton & ";break}")
  showScreen(newScreenName = "schoolframe")
  tclEval(script = "focus -force " & trainButton)
  return tclOk

proc getMemberIndex(): Natural {.sideEffect, raises: [], tags: [].} =
  ## Get the index in the player ship of the currently selected member
  ##
  ## Returns the crew member's index
  let memberBox = mainPaned & ".schoolframe.canvas.school.setting.crew"
  result = 0
  for member in playerShip.crew:
    if member.name == tclEval2(script = memberBox & " get"):
      break
    result.inc

proc getSkillIndex(): Positive =
  ## Get the index of the currently selected skill
  ##
  ## Returns the index of the skill
  let
    skillBox = mainPaned & ".schoolframe.canvas.school.setting.skill"
    comboBoxValue = tclEval2(script = skillBox & " get")
    skillName = comboBoxValue[0 .. comboBoxValue.find(sub = ':') - 1]
  result = 1
  for index, skill in skillsList:
    if skill.name == skillName:
      result = index
      break

proc trainSkillCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [
    WriteIOEffect, RootEffect], exportc.} =
  ## Train the selected skill
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## TrainSkill
  let amountBox = mainPaned & ".schoolframe.canvas.school." & tclGetVar(
      varName = "traintype") & "box.amount"
  if tclEval2(script = amountBox & " get") == "0":
    return tclOk
  try:
    trainSkill(memberIndex = getMemberIndex(), skillIndex = getSkillIndex(),
        amount = tclEval2(script = amountBox & " get").parseInt,
        isAmount = tclGetVar(varName = "traintype") == "amount")
  except NoMoneyError:
    showMessage(text = "You don't have any " & moneyName &
        " to pay for learning.", title = "Can't train")
    return tclOk
  except NotEnoughMoneyError:
    showMessage(text = "You don't have enough " & moneyName &
        " to pay for learning this skill.", title = "Can't train")
    return tclOk
  except:
    return showError(message = "Can't train the skill.")
  updateMessages()
  return showSchoolCommand(clientData = clientData, interp = interp, argc = 2,
      argv = @["TrainSkill", $getMemberIndex()].allocCStringArray)

proc addCommands*() {.sideEffect, raises: [], tags: [].} =
  ## Adds Tcl commands related to the trades UI
  try:
    discard
#    addCommand("SetSchoolSkills", setSchoolSkillsCommand)
#    addCommand("ShowSchool", showSchoolCommand)
#    addCommand("TrainSkill", trainSkillCommand)
  except:
    showError(message = "Can't add a Tcl command.")

# Temporary code for interfacing with Ada

proc getAdaMemberIndex(): cint {.sideEffect, raises: [], tags: [], exportc.} =
  return getMemberIndex().cint + 1

proc getAdaSkillIndex(): cint {.sideEffect, raises: [], tags: [], exportc.} =
  return getSkillIndex().cint
