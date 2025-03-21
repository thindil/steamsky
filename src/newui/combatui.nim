# Copyright 2025 Bartek thindil Jasicki
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
# along with Steam Sky.  if, see <http://www.gnu.org/licenses/>.

## Provides code related to all types of combat, like between the ships,
## boarding, giving orders to crew members, etc.

import std/[math, tables]
import contracts, nuklear/nuklear_sdl_renderer
import ../[combat, config, crewinventory, game, maps, shipscrew, types]
import coreui, dialogs, errordialog, header, themes, utilsui2

const
  pilotOrders: array[4, string] = ["Go closer", "Keep distance", "Evade", "Escape"]
  engineerOrders: array[4, string] = ["All stop", "Quarter speed", "Half speed", "Full speed"]
  gunnersOrders: array[1..6, string] = ["Don't shoot", "Precise fire",
      "Fire at will", "Aim for their engine", "Aim for their weapon", "Aim for their hull"]

var
  pilotList, engineerList, gunnerList: seq[string] = @["Nobody"]
  pilotIndex, engineerIndex: Natural = 0
  expandedSection: Natural = 0
  gunnersIndex: seq[Natural] = @[]

proc updateCrewLists() {.raises: [], tags: [RootEffect], contractual.} =
  ## Update the list of available crew members for all positions in combat
  pilotList = @["Nobody"]
  engineerList = @["Nobody"]
  for index, member in playerShip.crew:
    if member.skills.len > 0:
      pilotList.add(y = member.name & getSkillMarks(skillIndex = pilotingSkill,
          memberIndex = index))
      engineerList.add(y = member.name & getSkillMarks(
          skillIndex = engineeringSkill, memberIndex = index))
      gunnerList.add(y = member.name & getSkillMarks(skillIndex = gunnerySkill,
          memberIndex = index))

proc setCombat*(state: var GameState; dialog: var GameDialog) {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Set the combat UI and combat itself
  ##
  ## * state - the current game's state
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameters state and dialog. The latter is modified if
  ## any error happened.
  try:
    if skyMap[playerShip.skyX][playerShip.skyY].eventIndex > -1 and
        enemyName != protoShipsList[eventsList[skyMap[playerShip.skyX][
        playerShip.skyY].eventIndex].shipIndex].name:
      let combatStarted = startCombat(enemyIndex = eventsList[skyMap[
          playerShip.skyX][playerShip.skyY].eventIndex].shipIndex,
          newCombat = false)
      if not combatStarted:
        return
  except:
    dialog = setError(message = "Can't start the combat.")
    return
  state = combat
  dialog = none
  engineerOrder = 3
  pilotIndex = findMember(order = pilot) + 1
  engineerIndex = findMember(order = engineer) + 1
  updateCrewLists()

proc getGunSpeed(position: Natural; index: Positive): string {.raises: [
    KeyError], tags: [], contractual.} =
  ## Get the information about the fire rate of the selected gun
  ##
  ## * position - the index of the gun on the guns list
  ## * index    - the index of the order for the gun
  ##
  ## Returns the string with information about the gun's speed.
  result = ""
  var gunSpeed: int = modulesList[playerShip.modules[guns[position][
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

proc showCombat*(state: var GameState; dialog: var GameDialog) {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Show the combat UI
  ##
  ## * state - the current game's state
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameters state and dialog. The latter is modified if
  ## any error happened.
  showHeader(dialog = dialog)
  # Draw dialogs
  showQuestion(dialog = dialog, state = state)
  showMessage(dialog = dialog)
  showInfo(dialog = dialog)
  # Draw UI
  if pilotList.len != playerShip.crew.len + 1:
    updateCrewLists()
  let height: float = (windowHeight - 35 - gameSettings.messagesPosition.float)
  if expandedSection == 0:
    setLayoutRowDynamic(height = height / 2, cols = 2)
  else:
    setLayoutRowDynamic(height = height, cols = 1)
  if expandedSection in {0, 1}:
    group(title = "Your ship crew orders:", flags = {windowBorder, windowTitle}):
      setLayoutRowStatic(height = 35, cols = 1, width = 35)
      imageButton(image = images[expandIcon]):
        if expandedSection == 1:
          expandedSection = 0
        else:
          expandedSection = 1
      setLayoutRowDynamic(height = 35, cols = 3)
      label(str = "Position", alignment = centered)
      label(str = "Name", alignment = centered)
      label(str = "Order", alignment = centered)
      label(str = "Pilot:", alignment = centered)
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Select the crew member which will be the pilot during the combat. The sign + after name means that this crew member has piloting skill, the sign ++ after name means that his/her piloting skill is the best in the crew")
      var newPilot = comboList(items = pilotList,
          selected = pilotIndex, itemHeight = 25, x = 200, y = 150)
      if newPilot != pilotIndex:
        pilotIndex = newPilot
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Select the order for the pilot")
      var newOrder = comboList(items = pilotOrders,
          selected = (pilotOrder - 1), itemHeight = 25, x = 200, y = 150)
      if newOrder != pilotOrder - 1:
        pilotOrder = newOrder + 1
      label(str = "Engineer:", alignment = centered)
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Select the crew member which will be the engineer during the combat. The sign + after name means that this crew member has engineering skill, the sign ++ after name means that his/her engineering skill is the best in the crew")
      var newEngineer = comboList(items = engineerList,
          selected = engineerIndex, itemHeight = 25, x = 200, y = 150)
      if newEngineer != engineerIndex:
        engineerIndex = newEngineer
      if gameSettings.showTooltips:
        addTooltip(bounds = getWidgetBounds(),
            text = "Select the order for the engineer")
      newOrder = comboList(items = engineerOrders,
          selected = (engineerOrder - 1), itemHeight = 25, x = 200, y = 150)
      if newOrder != engineerOrder - 1:
        engineerOrder = newOrder + 1
      # Show the guns settings
      for gunIndex, gun in guns:
        var
          haveAmmo, hasGunner: bool = false
          ammoAmount: Natural = 0
        let aIndex: int = (if playerShip.modules[gun[1]].mType ==
            ModuleType2.gun: playerShip.modules[gun[
            1]].ammoIndex else: playerShip.modules[gun[1]].harpoonIndex)
        try:
          if aIndex in playerShip.cargo.low .. playerShip.cargo.high and
              itemsList[playerShip.cargo[aIndex].protoIndex].itemType ==
                  itemsTypesList[modulesList[
              playerShip.modules[gun[1]].protoIndex].value]:
            ammoAmount = playerShip.cargo[aIndex].amount
            haveAmmo = true
        except:
          dialog = setError(message = "Can't show the player's ship's gun settings. No proto item with index: " &
              $playerShip.cargo[aIndex].protoIndex, e = nil)
          return
        if not haveAmmo:
          ammoAmount = 0
          for itemIndex, item in itemsList:
            try:
              if item.itemType == itemsTypesList[modulesList[playerShip.modules[
                  gun[1]].protoIndex].value - 1]:
                let ammoIndex: int = findItem(inventory = playerShip.cargo,
                    protoIndex = itemIndex)
                if ammoIndex > -1:
                  ammoAmount = ammoAmount + playerShip.cargo[ammoIndex].amount
            except:
              dialog = setError(message = "Can't show the gun's ammo information. No proto module with index: " &
                  $playerShip.modules[gun[1]].protoIndex, e = nil)
              return
        label(str = playerShip.modules[gun[1]].name & ": \n(Ammo: " &
            $ammoAmount & ")", alignment = centered)
#        var comboBox: string = frame & ".guncrew" & $(gunIndex + 1)
#        tclEval(script = "ttk::combobox " & comboBox & " -values [list " &
#            getCrewList(position = 2) & "] -width 10 -state readonly")
#        if playerShip.modules[gun[1]].owner[0] == 0:
#          tclEval(script = comboBox & " current 0")
#        else:
#          if playerShip.crew[playerShip.modules[gun[1]].owner[0]].order == gunner:
#            tclEval(script = comboBox & " current " & $(playerShip.modules[gun[
#                1]].owner[0] + 1))
#            hasGunner = true
#          else:
#            tclEval(script = comboBox & " current 0")
#        tclEval(script = "grid " & comboBox & " -row " & $(gunIndex + 4) & " -column 1")
#        tclEval(script = "bind " & comboBox & " <Return> {InvokeButton " &
#            mainPaned & ".combatframe.next}")
#        tclEval(script = "bind " & comboBox &
#            " <<ComboboxSelected>> {SetCombatPosition gunner " & $(gunIndex + 1) & "}")
#        tclEval(script = "tooltip::tooltip " & comboBox & " \"Select the crew member which will be the operate the gun during\nthe combat. The sign + after name means that this crew member\nhas gunnery skill, the sign ++ after name means that they\ngunnery skill is the best in the crew\"")
#        var gunnerOrders: string = ""
#        for orderIndex, order in gunnersOrders:
#          try:
#            gunnerOrders = gunnerOrders & " " & order & getGunSpeed(
#                position = gunIndex, index = orderIndex) & "}"
#          except:
#            showError(message = "Can't show gunner's order.")
#            return
#        comboBox = frame & ".gunorder" & $(gunIndex + 1)
#        if tclEval2(script = "winfo exists " & comboBox) == "0":
#          tclEval(script = "ttk::combobox " & comboBox & " -values [list " &
#              gunnerOrders & "] -state readonly")
#        tclEval(script = comboBox & " current " & $(gun[2] - 1))
#        if hasGunner:
#          tclEval(script = "grid " & comboBox & " -row " & $(gunIndex + 4) & " -column 2 -padx {0 5}")
#        else:
#          tclEval(script = "grid remove " & comboBox)
#        tclEval(script = "bind " & comboBox & " <Return> {InvokeButton " &
#            mainPaned & ".combatframe.next}")
#        tclEval(script = "bind " & comboBox &
#            " <<ComboboxSelected>> {SetCombatOrder " & $(gunIndex + 1) & "}")
#        tclEval(script = "tooltip::tooltip " & comboBox & " \"Select the order for the gunner. Shooting in the selected\npart of enemy ship is less precise but always hit the\nselected part.\"")
  # Show boarding/defending settings
  state = combat
  showGameMenu(dialog = dialog)
