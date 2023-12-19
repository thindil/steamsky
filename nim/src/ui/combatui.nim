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

import std/[os, math, strutils, tables]
import ../[combat, crewinventory, game, maps, shipscrew, tk, types]
import coreui, mapsui, utilsui2

proc updateCombatUi() =
  let frame = mainPaned & ".combatframe.crew.canvas.frame"
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
    haveAmmo, hasGunner = false
    ammoAmount = 0
  let gunnersOrders: array[1..6, string] = ["{Don't shoot", "{Precise fire ",
      "{Fire at will ", "{Aim for their engine ", "{Aim for their weapon ", "{Aim for their hull "]

  proc getGunSpeed(position: Natural; index: Positive): string =
    result = ""
    var gunSpeed = modulesList[playerShip.modules[guns[position][
        1]].protoIndex].speed
    case index
    of 1:
      gunSpeed = 0
    of 3:
      discard
    else:
      gunSpeed = (if gunSpeed > 0: (gunSpeed.float /
          2.0).ceil.int else: gunSpeed - 1)
    if gunSpeed > 0:
      return "(" & $gunSpeed & "/round)"
    elif gunSpeed < 0:
      return "(1/" & $gunSpeed & " rounds)"

  # Show the guns settings
  for gunIndex, gun in guns:
    haveAmmo = false
    hasGunner = false
    let aIndex = (if playerShip.modules[gun[1]].mType ==
        ModuleType2.gun: playerShip.modules[gun[
        1]].ammoIndex else: playerShip.modules[gun[1]].harpoonIndex)
    if aIndex in playerShip.cargo.low .. playerShip.cargo.high and itemsList[
        playerShip.cargo[aIndex].protoIndex].itemType == itemsTypesList[modulesList[
        playerShip.modules[gun[1]].protoIndex].value]:
      ammoAmount = playerShip.cargo[aIndex].amount
      haveAmmo = true
    if not haveAmmo:
      ammoAmount = 0
      for itemIndex, item in itemsList:
        if item.itemType == itemsTypesList[modulesList[playerShip.modules[gun[
            1]].protoIndex].value]:
          let ammoIndex = findItem(inventory = playerShip.cargo,
              protoIndex = itemIndex)
          if ammoIndex > -1:
            ammoAmount = ammoAmount + playerShip.cargo[ammoIndex].amount
    let label = frame & ".gunlabel" & $gunIndex
    tclEval(script = "ttk::label " & label & " -text {" & playerShip.modules[
        gun[1]].name & ": \n(Ammo: " & $ammoAmount & ")}")
    tclEval(script = "grid " & label & " -row " & $(gunIndex + 4) & " -padx {5 0}")
    tclEval(script = "SetScrollbarBindings " & label & " $combatframe.crew.scrolly")
    var comboBox = frame & ".guncrew" & $gunIndex
    tclEval(script = "ttk::combobox " & comboBox & " -values [list " &
        getCrewList(position = 2) & "] -width 10 -state readonly")
    if playerShip.modules[gun[1]].owner[0] == 0:
      tclEval(script = comboBox & " current 0")
    else:
      if playerShip.crew[playerShip.modules[gun[1]].owner[0]].order == gunner:
        tclEval(script = comboBox & " current " & $(playerShip.modules[gun[
            1]].owner[0] + 1))
        hasGunner = true
      else:
        tclEval(script = comboBox & " current 0")
    tclEval(script = "grid " & comboBox & " -row " & $(gunIndex + 4) & " -column 1")
    tclEval(script = "bind " & comboBox & " <Return> {InvokeButton " &
        mainPaned & ".combatframe.next}")
    tclEval(script = "bind " & comboBox &
        " <<ComboboxSelected>> {SetCombatPosition gunner " & $gunIndex & "}")
    tclEval(script = "tooltip::tooltip " & comboBox & " \"Select the crew member which will be the operate the gun during\nthe combat. The sign + after name means that this crew member\nhas gunnery skill, the sign ++ after name means that they\ngunnery skill is the best in the crew\"")
    var gunnerOrders = ""
    for orderIndex, order in gunnersOrders:
      gunnerOrders = gunnerOrders & " " & order & getGunSpeed(
          position = gunIndex, index = orderIndex) & "}"
    comboBox = frame & ".gunorder" & $gunIndex
    if tclEval2(script = "winfo exists " & comboBox) == "0":
      tclEval(script = "ttk::combobox " & comboBox & " -values [list " &
          gunnerOrders & "] -state readonly")
    tclEval(script = comboBox & " current " & $(gun[2] - 1))
    if hasGunner:
      tclEval(script = "grid " & comboBox & " -row " & $(gunIndex + 4) & " -column 2 -padx {0 5}")
    else:
      tclEval(script = "grid remove " & comboBox)
    tclEval(script = "bind " & comboBox & " <Return> {InvokeButton " &
        mainPaned & ".combatframe.next}")
    tclEval(script = "bind " & comboBox &
        " <<ComboboxSelected>> {SetCombatOrder " & $gunIndex & "}")
    tclEval(script = "tooltip::tooltip " & comboBox & " \"Select the order for the gunner. Shooting in the selected\npart of enemy ship is less precise but always hit the\nselected part.\"")
  # Show boarding/defending settings
  if (harpoonDuration > 0 or enemy.harpoonDuration > 0) and protoShipsList[
      enemyShipIndex].crew.len > 0:
    var button = frame & ".boarding"
    tclEval(script = "ttk::button " & button & " -text {Boarding party:} -command {SetCombatParty boarding}")
    tclEval(script = "grid " & button & " -padx 5")
    tclEval(script = "tooltip::tooltip " & comboBox & " \"Set your boarding party. If you join it, you will be able\nto give orders them, but not your gunners or engineer.\"")
    button = frame & ".defending"
    tclEval(script = "ttk::button " & button & " -text {Defenders:} -command {SetCombatParty defenders}")
    tclEval(script = "grid " & button & " -sticky we -padx 5 -pady 5")
    tclEval(script = "tooltip::tooltip " & comboBox & " \"Set your ship's defenders against the enemy party.\"")
    var boardingParty, defenders = ""
    for member in playerShip.crew:
      case member.order
      of boarding:
        boardingParty = boardingParty & member.name & ", "
      of defend:
        defenders = defenders & member.name & ", "
      else:
        discard
    if boardingParty.len > 0:
      boardingParty = boardingParty[0 .. ^2]
    let
      label = frame & ".boardparty"
      labelLength = tclEval2(script = "winfo reqwidth " & frame &
          ".engineercrew").parseInt + tclEval2(script = "winfo reqwidth " &
          frame & ".engineerorder").parseInt
    if tclEval2(script = "winfo exists " & label) == "0":
      tclEval(script = "ttk::label " & label & " -text {" & boardingParty &
          "} -wraplength " & $labelLength)

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
