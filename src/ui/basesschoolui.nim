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

## Provides code related to UI for training the player's ship's crew members
## in bases, like showing the UI, selecting crew member, skill or amount
## of training, etc.

import std/[strutils, tables]
import contracts, nimalyzer
import ../[basestrade, crew, game, items, shipscrew, tk, types]
import coreui, dialogs, errordialog, mapsui, updateheader, utilsui2

proc setSchoolSkillsCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [WriteIOEffect,
        TimeEffect, RootEffect], cdecl, contractual, ruleOff: "params".} =
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
  let frameName: string = mainPaned & ".schoolframe.canvas.school"
  var comboBox: string = frameName & ".setting.crew"
  let memberIndex: int = try:
      tclEval2(script = comboBox & " current").parseInt
    except:
      return showError(message = "Can't get the member's index")
  var comboList: string = ""
  for index, skill in skillsList:
    var skillLevel: Natural = 0
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
    oldComboList: string = tclEval2(script = comboBox & " cget -values")
    spinBox: string = frameName & ".amountbox.amount"
  if oldComboList != comboList:
    tclEval(script = comboBox & " configure -values [list " & comboList & "]")
    tclEval(script = comboBox & " current 0")
    tclEval(script = spinBox & " set 1")
  tclEval(script = "UpdateSchoolCost " & spinBox & " " & tclEval2(
      script = spinBox & " get"))
  tclEval(script = "UpdateSchoolSelectedCost")
  return tclOk

proc showSchoolCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [WriteIOEffect,
        TimeEffect, RootEffect], cdecl, contractual.} =
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
  var schoolFrame: string = mainPaned & ".schoolframe"
  let schoolCanvas: string = schoolFrame & ".canvas"
  if tclEval2(script = "winfo exists " & schoolCanvas) == "0":
    tclEval(script = """
      ttk::frame .gameframe.paned.schoolframe
      set schoolcanvas [canvas .gameframe.paned.schoolframe.canvas \
         -yscrollcommand [list .gameframe.paned.schoolframe.scrolly set] \
         -xscrollcommand [list .gameframe.paned.schoolframe.scrollx set]]
      pack [ttk::scrollbar .gameframe.paned.schoolframe.scrolly -orient vertical \
         -command [list $schoolcanvas yview]] -side right -fill y
      pack $schoolcanvas -side top -fill both
      SetScrollbarBindings $schoolcanvas .gameframe.paned.schoolframe.scrolly
      pack [ttk::scrollbar .gameframe.paned.schoolframe.scrollx -orient horizontal \
         -command [list $schoolcanvas xview]] -fill x
      ::autoscroll::autoscroll .gameframe.paned.schoolframe.scrolly
      ::autoscroll::autoscroll .gameframe.paned.schoolframe.scrollx
      set schoolframe [ttk::frame $schoolcanvas.school]
      SetScrollbarBindings $schoolframe .gameframe.paned.schoolframe.scrolly
      set traintype amount
      grid [ttk::frame $schoolframe.money] -sticky w -pady 5 -padx 5
      grid [ttk::label $schoolframe.money.moneylbl] -sticky w
      ttk::label $schoolframe.money.money -style Golden.TLabel
      grid [ttk::frame $schoolframe.setting] -padx 5
      grid [ttk::button $schoolframe.setting.train -text {Train:} \
         -command TrainSkill]
      tooltip::tooltip $schoolframe.setting.train \
         {Train the selected skill of the selected crew member}
      grid [ttk::combobox $schoolframe.setting.crew -state readonly] -row 0 \
         -column 1
      tooltip::tooltip $schoolframe.setting.crew \
         {Select the crew member which skills will be trained}
      bind $schoolframe.setting.crew <<ComboboxSelected>> SetSchoolSkills
      grid [ttk::label $schoolframe.setting.skilllbl -text {in}] -row 0 -column 2 \
         -padx 5
      grid [ttk::combobox $schoolframe.setting.skill -state readonly -width 27] -row 0 \
         -column 3
      tooltip::tooltip $schoolframe.setting.skill \
         {Select the skill which will be trained}
      bind $schoolframe.setting.skill <<ComboboxSelected>> {
         $schoolframe.amountbox.amount set 1
         UpdateSchoolCost $schoolframe.amountbox.amount 1
         UpdateSchoolSelectedCost
      }
      grid [ttk::frame $schoolframe.amountbox] -sticky w
      grid [ttk::radiobutton $schoolframe.amountbox.radioamount \
         -text {Selected amount of times} -variable traintype -value amount] \
         -columnspan 2 -sticky w -padx 5 -pady {5 0}
      tooltip::tooltip $schoolframe.amountbox.radioamount \
         {Train the selected skill the selected amount of times}
      grid [ttk::label $schoolframe.amountbox.amountlbl -text {Amount:}] -sticky w \
         -padx {50 0}
      tooltip::tooltip $schoolframe.amountbox.amountlbl \
         {Enter amount of training sessions between 1 and 100}
      grid [ttk::spinbox $schoolframe.amountbox.amount -from 1 -to 100 \
         -validate key \
         -validatecommand {ValidateSpinbox %W %P $schoolcanvas.school.setting.train;UpdateSchoolCost %W %P} \
         -width 5] -row 1 -column 1 -sticky w
      tooltip::tooltip $schoolframe.amountbox.amount \
         {Enter amount of training sessions between 1 and 100}
      $schoolframe.amountbox.amount set 1
      bind $schoolframe.amountbox.amount <<Increment>> \
         {UpdateSchoolCost %W [expr [$schoolframe.amountbox.amount get] + 1]}
      bind $schoolframe.amountbox.amount <<Decrement>> \
         {UpdateSchoolCost %W [expr [$schoolframe.amountbox.amount get] - 1]}
      grid [ttk::label $schoolframe.amountbox.costlbl -text {Minimal cost:}] \
         -sticky w -padx {50 0}
      tooltip::tooltip $schoolframe.amountbox.costlbl \
         {Minimal cost of training. The real cost can be higher that this.}
      grid [ttk::label $schoolframe.amountbox.cost] -row 2 -column 1 -sticky w
      tooltip::tooltip $schoolframe.amountbox.cost \
         {Minimal cost of training. The real cost can be higher that this.}
      grid [ttk::frame $schoolframe.costbox] -sticky w
      grid [ttk::radiobutton $schoolframe.costbox.radioamount \
         -text {Selected maximum cost of training} -variable traintype -value cost] \
         -columnspan 2 -sticky w -padx 5 -pady {5 0}
      tooltip::tooltip $schoolframe.costbox.radioamount \
         "Train the selected skill as long as you don't spend the selected\namount of money"
      grid [ttk::label $schoolframe.costbox.amountlbl -text {Cost:}] -sticky w \
         -padx {50 0}
      tooltip::tooltip $schoolframe.costbox.amountlbl \
         {Enter amount of money which you want to spend}
      grid [ttk::spinbox $schoolframe.costbox.amount -validate key \
         -validatecommand {ValidateSpinbox %W %P $schoolcanvas.school.setting.train} \
         -width 10] -row 1 -column 1 -sticky w
      tooltip::tooltip $schoolframe.costbox.amount \
         {Enter amount of money which you want to spend}
      $schoolframe.costbox.amount set 1
    """)
    tclEval(script = "bind " & schoolFrame & " <Configure> {ResizeCanvas %W.canvas %w %h}")
  elif tclEval2(script = "winfo ismapped " & schoolCanvas) == "1" and argc == 1:
    tclEval(script = "grid remove " & closeButton)
    showSkyMap(clear = true)
    return tclOk
  tclSetVar(varName = "gamestate", newValue = "crew")
  let moneyAmount: Natural = moneyAmount(inventory = playerShip.cargo)
  var moneyLabel: string = schoolCanvas & ".school.money.moneylbl"
  if moneyAmount > 0:
    tclEval(script = moneyLabel & " configure -text {You have } -style TLabel")
    moneyLabel = schoolCanvas & ".school.money.money"
    tclEval(script = moneyLabel & " configure -text {" & $moneyAmount & " " &
        moneyName & "}")
    tclEval(script = "grid " & moneyLabel & " -column 1 -row 0")
  else:
    tclEval(script = moneyLabel & " configure -text {You don't have any " &
        moneyName & " to pay for learning.} -style Headerred.TLabel")
    moneyLabel = schoolCanvas & ".school.money.money"
    tclEval(script = "grid remove " & moneyLabel)
  schoolFrame = schoolCanvas & ".school"
  if argc == 1:
    var comboList: string = ""
    for member in playerShip.crew:
      comboList.add(y = " " & member.name)
    let comboBox: string = schoolCanvas & ".school.setting.crew"
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
    comboBox: string = schoolCanvas & ".school.costbox.amount"
    trainButton: string = schoolCanvas & ".school.setting.train"
  tclEval(script = "bind " & comboBox & " <Tab> {focus " & trainButton & ";break}")
  showScreen(newScreenName = "schoolframe")
  tclEval(script = "focus -force " & trainButton)
  return tclOk

proc getMemberIndex(): Natural {.raises: [], tags: [], contractual.} =
  ## Get the index in the player ship of the currently selected member
  ##
  ## Returns the crew member's index
  let memberBox: string = mainPaned & ".schoolframe.canvas.school.setting.crew"
  result = 0
  for member in playerShip.crew:
    if member.name == tclEval2(script = memberBox & " get"):
      break
    result.inc

proc getSkillIndex(): Positive {.raises: [], tags: [], contractual.} =
  ## Get the index of the currently selected skill
  ##
  ## Returns the index of the skill
  let
    skillBox: string = mainPaned & ".schoolframe.canvas.school.setting.skill"
    comboBoxValue: string = tclEval2(script = skillBox & " get")
    skillName: string = comboBoxValue[0..comboBoxValue.find(sub = ':') - 1]
  result = 1
  for index, skill in skillsList:
    if skill.name == skillName:
      result = index
      break

proc trainSkillCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [
    WriteIOEffect, TimeEffect, RootEffect, RootEffect], cdecl, contractual.} =
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
  let amountBox: string = mainPaned & ".schoolframe.canvas.school." & tclGetVar(
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
  updateHeader()
  updateMessages()
  return showSchoolCommand(clientData = clientData, interp = interp, argc = 2,
      argv = @["TrainSkill", $getMemberIndex()].allocCStringArray)

proc updateSchoolCostCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [WriteIOEffect,
        TimeEffect, RootEffect], cdecl, contractual.} =
  ## Update the cost of training
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## UpdateSchoolCost combobox amount
  ## Combobox is the Tk path to the ttk::combobox with the amount of
  ## training sessions, amount is the amount of the requested training
  ## sessions
  if argv[2] == "":
    tclSetResult(value = "1")
    return tclOk
  var amount: Natural = try:
      ($argv[2]).parseInt
    except:
      tclSetResult(value = "0")
      return tclOk
  if amount < 1:
    amount = 1
  elif amount > 100:
    amount = 100
  var cost: Natural = try:
      trainCost(memberIndex = getMemberIndex(), skillIndex = getSkillIndex(),
          traderindex = findMember(order = talk)) * amount
    except:
      tclSetResult(value = "1")
      return showError(message = "Can't count cost of training.")
  let
    comboBox: string = $argv[1]
    label: string = tclEval2(script = "winfo parent " & comboBox) & ".cost"
  tclEval(script = label & " configure -text {" & $cost & " " & moneyName & "}")
  tclSetResult(value = "1")
  return tclOk

proc updateSchoolSelectedCostCommand(clientData: cint; interp: PInterp;
    argc: cint; argv: cstringArray): TclResults {.raises: [],
    tags: [WriteIOEffect, TimeEffect, RootEffect], cdecl, contractual.} =
  ## Update the minimal and maximum values of spinbox with training cost
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## UpdateSchoolSelectedCost
  let
    moneyAmount: Natural = moneyAmount(inventory = playerShip.cargo)
    cost: Natural = try:
        trainCost(memberIndex = getMemberIndex(), skillIndex = getSkillIndex(),
            traderIndex = findMember(order = talk))
      except:
        return showError(message = "Can't get the training cost.")
    amountBox: string = mainPaned & ".schoolframe.canvas.school.costbox.amount"
  if cost <= moneyAmount:
    tclEval(script = amountBox & " configure -from " & $cost & " -to " & $moneyAmount)
    tclEval(script = "bind " & amountBox & " <<Increment>> {" & amountBox &
        " set [expr [" & amountBox & " get] + " & $cost & " - 1]}")
    tclEval(script = "bind " & amountBox & " <<Decrement>> {" & amountBox &
        " set [expr [" & amountBox & " get] - " & $cost & " + 1]}")
  else:
    tclEval(script = amountBox & " configure -from 0 to 0")
    tclEval(script = "bind " & amountBox & " <<Increment>> {}")
    tclEval(script = "bind " & amountBox & " <<Decrement>> {}")
  tclEval(script = amountBox & " set " & $cost)
  return tclOk

proc addCommands*() {.raises: [], tags: [WriteIOEffect, TimeEffect, RootEffect],
    contractual.} =
  ## Adds Tcl commands related to the trades UI
  try:
    addCommand(name = "SetSchoolSkills", nimProc = setSchoolSkillsCommand)
    addCommand(name = "ShowSchool", nimProc = showSchoolCommand)
    addCommand(name = "TrainSkill", nimProc = trainSkillCommand)
    addCommand(name = "UpdateSchoolCost", nimProc = updateSchoolCostCommand)
    addCommand(name = "UpdateSchoolSelectedCost",
        nimProc = updateSchoolSelectedCostCommand)
  except:
    showError(message = "Can't add a Tcl command.")
