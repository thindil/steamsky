# Copyright 2023 Bartek thindil Jasicki
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
import ../[config, game, messages, shipscrew, tk, types]
import coreui

proc showScreen*(newScreenName: string) {.sideEffect, raises: [], tags: [].} =
  ## Clear the old screen and show the selected to the player
  ##
  ## * newScreenName - the Tcl name of the screen which will be show
  if tclGetVar(varName = "mappreview") == "1" and newScreenName != "mapframe":
    tclUnsetVar(varName = "mappreview")
  const
    paned = mainPaned & ".controls.buttons"
    messagesFrame = mainPaned & ".controls.messages"
  let interp = getInterp()
  if tclEval(script = mainPaned & " panes") == tclError:
    return
  let
    tclResult = $interp.tclGetResult()
    oldSubWindow = tclResult.split()[0]
    subWindow = mainPaned & "." & $newScreenName
  if tclEval(script = mainPaned & " forget " & oldSubWindow) == tclError:
    return
  if tclEval(script = mainPaned & " insert 0 " & subWindow &
      " -weight 1") == tclError:
    return
  if newScreenName in ["optionsframe", "messagesframe"] or
      not gameSettings.showLastMessages:
    if tclEval(script = "grid remove " & messagesFrame) == tclError:
      return
    if newScreenName != "mapframe":
      if tclEval(script = "winfo height " & mainPaned) == tclError:
        return
      let newPos = $interp.tclGetResult()
      if tclEval(script = mainPaned & " sashpos 0 " & newPos) == tclError:
        return
  else:
    if oldSubWindow in [mainPaned & ".messagesframe", mainPaned &
        ".optionsframe"]:
      if tclEval(script = mainPaned & " sashpos 0 " & $(
          gameSettings.windowHeight - gameSettings.messagesPosition)) == tclError:
        return
    if tclEval(script = "grid " & messagesFrame) == tclError:
      return
  if newScreenName == "mapframe":
    if tclEval(script = "grid " & paned) == tclError:
      return
  else:
    if tclEval(script = "grid remove " & paned) == tclError:
      return

proc updateMessages*() {.sideEffect, raises: [], tags: [].} =
  ## Update the list of in-game messages, delete old ones and show the
  ## newest to the player
  let messagesView = mainPaned & ".controls.messages.view"
  tclEval(script = messagesView & " configure -state normal")
  tclEval(script = messagesView & " delete 1.0 end")
  var loopStart = 0 - messagesAmount()
  if loopStart == 0:
    return
  if loopStart < -10:
    loopStart = -10

  proc showMessage(message: MessageData) =
    let tagNames: array[1 .. 5, string] = ["yellow", "green", "red", "blue", "cyan"]
    if message.color == white:
      tclEval(script = messagesView & " insert end {" & message.message & "}")
    else:
      tclEval(script = messagesView & " insert end {" & message.message &
          "} [list " & tagNames[message.color.ord] & "]")

  if gameSettings.messagesOrder == olderFirst:
    for i in loopStart .. -1:
      showMessage(getMessage(messageIndex = i + 1))
      if i < -1:
        tclEval(script = messagesView & " insert end {\n}")
    tclEval(script = "update")
    tclEval(script = messagesView & " see end")
  else:
    for i in countdown(-1, loopStart):
      showMessage(getMessage(messageIndex = i + 1))
      if i > loopStart:
        tclEval(script = messagesView & " insert end {\n}")
  tclEval(script = messagesView & " configure -state disable")

proc getSkillMarks*(skillIndex: Positive;
    memberIndex: Natural): string {.sideEffect, raises: [], tags: [].} =
  ## Get the marks with information about the skill level for the selected
  ## skill for the selected crew member
  ##
  ## * skillIndex  - the index of the skill to check
  ## * memberIndex - the index of the player's ship's crew member to check
  ##
  ## The string with one "+" sign if the crew member known the skill, the
  ## string with twi "+" sings if the crew member has the highest level in
  ## the skill of the all crew members. Otherwise return an empty string.
  var
    skillValue = 0
    crewIndex = -1
  try:
    for index, member in playerShip.crew:
      if getSkillLevel(member = member, skillIndex = skillIndex) > skillValue:
        skillValue = getSkillLevel(member = member, skillIndex = skillIndex)
        crewIndex = index
    if getSkillLevel(member = playerShip.crew[memberIndex],
        skillIndex = skillIndex) > 0:
      result = " +"
  except:
    tclEval(script = "bgerror {Can't get the crew member skill level. Reason: " &
        getCurrentExceptionMsg() & "}")
    return ""
  if memberIndex == crewIndex:
    result = result & "+"

# Temporary code for interfacing with Ada

proc showAdaScreen(newScreenName: cstring) {.exportc, raises: [], tags: [].} =
  showScreen($newScreenName)

proc updateAdaMessages() {.exportc, raises: [], tags: [].} =
  updateMessages()

proc getAdaSkillMarks(skillIndex, memberIndex: cint): cstring {.exportc,
    raises: [], tags: [].} =
  try:
    return getSkillMarks(skillIndex, memberIndex - 1).cstring
  except:
    return ""
