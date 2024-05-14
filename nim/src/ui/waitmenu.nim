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
import ../[crew2, game, game2, shipsmovement, tk, types]
import coreui, dialogs, updateheader, utilsui2

proc showWaitCommand*(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [], exportc.} =
  ## Show the available wait orders to the player
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ShowWait
  var waitDialog = ".gameframe.wait"
  if tclEval2(script = "winfo exists " & waitDialog) == "1":
    let button = waitDialog & ".close"
    tclEval(script = button & " invoke")
    return tclOk
  waitDialog = createDialog(name = ".gameframe.wait", title = "Wait in place", columns = 3)

  proc addButton(time: Positive) =
    let button = waitDialog & ".wait" & $time
    tclEval(script = "ttk::button " & button & " -text {Wait " & $time &
        " minute" & (if time > 1: "s" else: "") & "} -command {Wait " & $time & "}")
    tclEval(script = "grid " & button & " -sticky we -columnspan 3 -padx 5" & (
        if time == 1: " -pady {5 0}" else: ""))
    tclEval(script = "bind " & button & " <Escape> {CloseDialog " & waitDialog & ";break}")
    tclEval(script = "tooltip::tooltip " & button & " \"Wait in place for " &
        $time & " minute" & (if time > 1: "s" else: "") & "\"")

  addButton(time = 1)
  addButton(time = 5)
  addButton(time = 10)
  addButton(time = 15)
  addButton(time = 30)
  var button = waitDialog & ".wait1h"
  tclEval(script = "ttk::button " & button & " -text {Wait 1 hour} -command {Wait 60}")
  tclEval(script = "grid " & button & " -sticky we -columnspan 3 -padx 5")
  tclEval(script = "tooltip::tooltip " & button & " \"Wait in place for 1 hour\"")
  tclEval(script = "bind " & button & " <Escape> {CloseDialog " & waitDialog & ";break}")
  button = waitDialog & ".wait"
  tclEval(script = "ttk::button " & button & " -text Wait -command {Wait amount}")
  tclEval(script = "grid " & button & " -padx {5 0}")
  tclEval(script = "bind " & button & " <Escape> {CloseDialog " & waitDialog & ";break}")
  tclEval(script = "tooltip::tooltip " & button & " \"Wait in place for the selected amount of minutes:\nfrom 1 to 1440 (the whole day)\"")
  let amountBox = waitDialog & ".amount"
  tclEval(script = "ttk::spinbox " & amountBox &
      " -from 1 -to 1440 -width 6 -validate key -validatecommand {ValidateSpinbox %W %P " &
      button & "} -textvariable customwaittime")
  tclEval(script = "grid " & amountBox & " -row 7 -column 1")
  tclEval(script = "bind " & button & " <Escape> {CloseDialog " & waitDialog & ";break}")
  if tclGetVar(varName = "customwaittime").len == 0:
    tclEval(script = amountBox & " set 1")
  tclEval(script = "tooltip::tooltip " & button & " \"Wait in place for the selected amount of time:\nfrom 1 to 1440\"")
  let amountCombo = waitDialog & ".mins"
  tclEval(script = "ttk::combobox " & amountCombo & " -state readonly -values [list minutes hours days] -width 8")
  tclEval(script = amountCombo & " current 0")
  tclEval(script = "grid " & amountCombo & " -row 7 -column 2 -padx {0 5}")
  var needRest, needHealing = false
  for index, member in playerShip.crew:
    if member.tired > 0 and member.order == rest:
      needRest = true
    if member.health in 1 .. 99 and member.order == rest:
      for module in playerShip.modules:
        if module.mType == ModuleType2.cabin:
          for owner in module.owner:
            if owner == index:
              needHealing = true
              break
  if needRest:
    button = waitDialog & ".rest"
    tclEval(script = "ttk::button " & button & " -text {Wait until crew is rested} -command {Wait rest}")
    tclEval(script = "grid " & button & " -sticky we -columnspan 3 -padx 5")
    tclEval(script = "bind " & button & " <Escape> {CloseDialog " & waitDialog & ";break}")
    tclEval(script = "tooltip::tooltip " & button & " \"Wait in place until the whole ship's crew is rested.\"")
  if needHealing:
    button = waitDialog & ".heal"
    tclEval(script = "ttk::button " & button & " -text {Wait until crew is healed} -command {Wait heal}")
    tclEval(script = "grid " & button & " -sticky we -columnspan 3 -padx 5")
    tclEval(script = "bind " & button & " <Escape> {CloseDialog " & waitDialog & ";break}")
    tclEval(script = "tooltip::tooltip " & button & " \"Wait in place until the whole ship's crew is rested\nCan take a large amount of time.\"")
  button = waitDialog & ".close"
  tclEval(script = "ttk::button " & button &
      " -text {Close} -command {CloseDialog " & waitDialog & ";break}")
  tclEval(script = "grid " & button & " -sticky we -columnspan 3 -padx {0 5}")
  tclEval(script = "bind " & button & " <Escape> {CloseDialog " & waitDialog & ";break}")
  tclEval(script = "tooltip::tooltip " & button & " \"Close dialog \\[Escape\\]\"")
  tclEval(script = "focus " & button)
  tclEval(script = "bind " & button & " <Tab> {focus " & waitDialog & ".wait1;break}")
  showDialog(dialog = waitDialog, relativeY = 0.15)
  return tclOk

proc waitCommand*(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults {.sideEffect, raises: [], tags: [
        WriteIOEffect, RootEffect], exportc.}
  ## Wait the selected amount of time
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## Wait

proc addCommands*() {.sideEffect, raises: [], tags: [].} =
  ## Adds Tcl commands related to the wait menu
  try:
    discard
#    addCommand("ShowWait", showWaitCommand)
#    addCommand("Wait", waitCommand)
  except:
    showError(message = "Can't add a Tcl command.")

import mapsui

proc waitCommand*(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults =
  try:
    if argv[1] == "1":
      updateGame(minutes = 1)
      waitInPlace(minutes = 1)
    elif argv[1] == "5":
      updateGame(minutes = 5)
      waitInPlace(minutes = 5)
    elif argv[1] == "10":
      updateGame(minutes = 10)
      waitInPlace(minutes = 10)
    elif argv[1] == "15":
      updateGame(minutes = 15)
      waitInPlace(minutes = 15)
    elif argv[1] == "30":
      updateGame(minutes = 30)
      waitInPlace(minutes = 30)
    elif argv[1] == "60":
      updateGame(minutes = 60)
      waitInPlace(minutes = 60)
    elif argv[1] == "rest":
      waitForRest()
    elif argv[1] == "heal":
      var timeNeeded = 0
      for index, member in playerShip.crew:
        if member.health in 1 .. 99 and member.order == rest:
          block checkModules:
            for module in playerShip.modules:
              if module.mType == ModuleType2.cabin:
                for owner in module.owner:
                  if owner == index:
                    if timeNeeded < (100 - member.health) * 15:
                      timeNeeded = (100 - member.health) * 15
                      break checkModules
      if timeNeeded == 0:
        return tclOk
      updateGame(minutes = timeNeeded)
      waitInPlace(minutes = timeNeeded)
    elif argv[1] == "amount":
      let amountBox = ".gameframe.wait.amount"
      var timeNeeded = try:
          tclEval2(script = amountBox & " get").parseInt
        except:
          return showError(message = "Can't get type of time to wait.")
      let amountCombo = ".gameframe.wait.mins"
      if tclEval2(script = amountCombo & " current") == "1":
        timeNeeded = timeNeeded * 60
      elif tclEval2(script = amountCombo & " current") == "2":
        timeNeeded = timeNeeded * 1_440
      updateGame(minutes = timeNeeded)
      waitInPlace(minutes = timeNeeded)
  except:
    return showError(message = "Can't wait selected amount of time.")
  updateHeader()
  updateMessages()
  var currentFrame = mainPaned & ".shipinfoframe"
  if tclEval2(script = "winfo exists " & currentFrame) == "1" and tclEval2(
      script = "winfo ismapped " & currentFrame) == "1":
    tclEval(script = "ShowShipInfo 1")
  else:
    currentFrame = mainPaned & ".knowledgeframe"
    if tclEval2(script = "winfo exists " & currentFrame) == "1" and tclEval2(
        script = "winfo ismapped " & currentFrame) == "1":
      tclEval(script = "ShowKnowledge 1")
    else:
      drawMap()
  let dialogCloseButton = ".gameframe.wait.close"
  tclEval(script = dialogCloseButton & " invoke")
  return tclOk

# Temporary code for interfacing with Ada

proc addAdaWaitCommands() {.raises: [], tags: [RootEffect], exportc.} =
  try:
    addCommands()
  except:
    echo getCurrentExceptionMsg()
