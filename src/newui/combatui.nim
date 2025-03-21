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

import std/tables
import contracts, nuklear/nuklear_sdl_renderer
import ../[combat, config, game, maps, shipscrew, types]
import coreui, dialogs, errordialog, header, themes, utilsui2

const
  pilotOrders: array[4, string] = ["Go closer", "Keep distance", "Evade", "Escape"]
  engineerOrders: array[4, string] = ["All stop", "Quarter speed", "Half speed", "Full speed"]

var
  pilotList, engineerList, gunnerList: seq[string] = @["Nobody"]
  pilotIndex, engineerIndex: Natural = 0
  expandedSection: Natural = 0

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
  pilotList = @["Nobody"]
  engineerList = @["Nobody"]
  pilotIndex = findMember(order = pilot) + 1
  engineerIndex = findMember(order = engineer) + 1
  for index, member in playerShip.crew:
    if member.skills.len > 0:
      pilotList.add(y = member.name & getSkillMarks(skillIndex = pilotingSkill,
          memberIndex = index))
      engineerList.add(y = member.name & getSkillMarks(
          skillIndex = engineeringSkill, memberIndex = index))
      gunnerList.add(y = member.name & getSkillMarks(skillIndex = gunnerySkill,
          memberIndex = index))

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
  # draw dialogs
  showQuestion(dialog = dialog, state = state)
  showMessage(dialog = dialog)
  showInfo(dialog = dialog)
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
  state = combat
  showGameMenu(dialog = dialog)
