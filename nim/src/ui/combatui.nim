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

import std/[os, strutils, tables]
import ../[combat, game, maps, shipscrew, tk, types]
import coreui, mapsui, utilsui2

proc updateCombatUi() =
  var frame = mainPaned & ".combatframe.crew.canvas.frame"
  tclEval(script = "bind . <" & generalAccelerators[0] & "> {InvokeButton " &
      frame & ".maxmin}")
  tclEval(script = "bind . <" & generalAccelerators[2] & "> {InvokeButton " &
      mainPaned & ".combatframe.damage.canvas.frame.maxmin}")
  tclEval(script = "bind . <" & generalAccelerators[1] & "> {InvokeButton " &
      mainPaned & ".combatframe.enemy.canvas.frame.maxmin}")
  tclEval(script = "bind . <" & generalAccelerators[3] & "> {InvokeButton " &
      mainPaned & ".combatframe.status.canvas.frame.maxmin}")
  var comboBox = frame & ".pilotcrew"

  proc getCrewList(position: Natural): string =
    result = "Nobody"
    for index, member in playerShip.crew:
      if member.skills.len > 0:
        result = result & " {" & member.name & getSkillMarks(skillIndex = (
            if position == 0: pilotingSkill elif position ==
            1: engineeringSkill else: gunnerySkill), memberIndex = index) & "}"

  tclEval(script = comboBox & " configure -values [list " & getCrewList(
      position = 0) & "]")
  tclEval(script = comboBox & " current " & $(findMember(order = pilot) + 1))
  comboBox = frame & ".pilotorder"
  tclEval(script = comboBox & " current " & $(pilotOrder - 1))
  let faction = factionsList[playerShip.crew[0].faction]
  if "sentientships" notin faction.flags and findMember(order = pilot) == -1:
    tclEval(script = "grid remove " & comboBox)
  else:
    tclEval(script = "grid " & comboBox)
  comboBox = frame & ".engineercrew"
  tclEval(script = comboBox & " configure -values [list " & getCrewList(
      position = 1) & "]")
  tclEval(script = comboBox & " current " & $(findMember(order = engineer) + 1))
  comboBox = frame & ".engineerorder"
  tclEval(script = comboBox & " current " & $(engineerOrder - 1))
  if "sentientships" notin faction.flags and findMember(order = engineer) == -1:
    tclEval(script = "grid remove " & comboBox)
  else:
    tclEval(script = "grid " & comboBox)
  let
    tclResult = tclEval2(script = "grid size " & frame).split(" ")
    rows: Positive = tclResult[1].parseInt()
  deleteWidgets(startIndex = 4, endIndex = rows - 1, frame = frame)
  var
    haveAmmo = false
    ammoAmount = 0
  for gun in guns:
    let aIndex = (if playerShip.modules[gun[1]].mType ==
        ModuleType2.gun: playerShip.modules[gun[1]].ammoIndex else: playerShip.modules[gun[
        1]].harpoonIndex)
    if aIndex in playerShip.cargo.low .. playerShip.cargo.high and itemsList[
        playerShip.cargo[aIndex].protoIndex].itemType == itemsTypesList[modulesList[
        playerShip.modules[gun[1]].protoIndex].value]:
      ammoAmount = playerShip.cargo[aIndex].amount
      haveAmmo = true

proc nextTurnCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults =
  combatTurn()
  updateHeader()
  if endCombat:
    for accel in generalAccelerators:
      tclEval(script = "bind . <" & accel & "> {}")
    updateCombatUi()
  return tclOk

proc showCombatUi*(newCombat: bool = true) =
  tclEval(script = "grid remove " & closeButton)
  var combatStarted = false
  let combatFrame = mainPaned & ".combatframe"
  if newCombat:
    if skyMap[playerShip.skyX][playerShip.skyY].eventIndex > -1 and enemyName !=
        protoShipsList[eventsList[skyMap[playerShip.skyX][
        playerShip.skyY].eventIndex].shipIndex].name:
      combatStarted = startCombat(enemyIndex = eventsList[skyMap[
          playerShip.skyX][playerShip.skyY].eventIndex].shipIndex,
          newCombat = false)
      if not combatStarted:
        return
      if tclEval2(script = "winfo exists " & combatFrame) == "0":
        tclEval(script = "eval {" & dataDirectory & "ui" & DirSep & "combat.tcl}")
        pilotOrder = 2
        engineerOrder = 3
        addCommand("NextTurn", nextTurnCommand)
