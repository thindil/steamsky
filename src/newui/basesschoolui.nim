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

## Provides code related to training skills in bases, like show the UI,
## set the skill to train, etc.

import std/[strutils, tables]
import contracts, nuklear/nuklear_sdl_renderer
import ../[crew, crewinventory, game]
import coreui, dialogs, errordialog, header, themes

type
  TrainingType = enum
    times, cost

var
  moneyIndex2: int = -1
  moneyText, crewList, schoolSkillsList: seq[string] = @[]
  moneyWidth: seq[cfloat] = @[]
  crewIndex, skillIndex: Natural = 0
  amount: Positive = 1
  tType: TrainingType = times

proc setSchoolSkills*(){.raises: [], tags: [], contractual.} =
  ## Set the skills list for the selected crew member
  schoolSkillsList = @[]
  for index, skill in skillsList:
    var skillLevel = 0
    for skill2 in playerShip.crew[crewIndex].skills:
      if skill2.index == index:
        skillLevel = skill2.level
        if skillLevel == 100:
          break
    if skillLevel != 100:
      schoolSkillsList.add(y = skill.name & ": " & (if skillLevel ==
          0: "Untrained" else: getSkillLevelName(
          skillLevel = skillLevel).strip))

proc setSchool*(dialog: var GameDialog) {.raises: [], tags: [RootEffect],
    contractual.} =
  ## Set the data for school UI
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  moneyIndex2 = findItem(inventory = playerShip.cargo, protoIndex = moneyIndex)
  moneyText = @[]
  moneyWidth = @[]
  if moneyIndex == -1:
    moneyText.add(y = "You don't have " & moneyName & " to buy anything")
  else:
    moneyText.add(y = "You have ")
    moneyText.add(y = $playerShip.cargo[moneyIndex2].amount & " " & moneyName)
  for text in moneyText:
    try:
      moneyWidth.add(y = text.getTextWidth)
    except:
      dialog = setError(message = "Can't get the width of the money text.")
      return
  crewList = @[]
  for member in playerShip.crew:
    crewList.add(y = member.name)
  setSchoolSkills()
  amount = 1
  tType = times

proc showSchool*(state: var GameState; dialog: var GameDialog) {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Show the school UI
  ##
  ## * state - the current game's state
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameters state and dialog. The latter is modified if
  ## any error happened.
  showHeader(dialog = dialog, close = CloseDestination.map, state = state)
  if state != GameState.school:
    return
  # Draw dialogs
  showQuestion(dialog = dialog, state = state)
  if state != school:
    return
  showMessage(dialog = dialog)
  showInfo(dialog = dialog)
  # Show information about money owned by the player
  setLayoutRowStatic(height = 30, cols = moneyWidth.len, ratio = moneyWidth)
  for index, text in moneyText:
    if index mod 2 == 0:
      label(str = text)
    else:
      colorLabel(str = text, color = theme.colors[goldenColor])
  setLayoutRowStatic(height = 30, cols = 4, ratio = [200.cfloat, 200, 50, 250])
  labelButton(title = "Train"):
    discard
  let newMember = comboList(items = crewList,
      selected = crewIndex, itemHeight = 25, x = 200, y = 150)
  if newMember != crewIndex:
    crewIndex = newMember
    setSchoolSkills()
  label(str = "in", alignment = centered)
  let newSkill = comboList(items = schoolSkillsList,
      selected = skillIndex, itemHeight = 25, x = 300, y = 150)
  if newSkill != skillIndex:
    skillIndex = newSkill
    setSchoolSkills()
  setLayoutRowDynamic(height = 30, cols = 1)
  if option(label = "Selected amount of times", selected = tType == times):
    tType = times
  setLayoutRowDynamic(height = 30, cols = 2)
  label(str = "Amount:")
  let newAmount: int = property2(name = "#", min = 1, val = amount, max = 100, step = 1, incPerPixel = 1)
  if newAmount != amount:
    amount = newAmount
  label(str = "Minimal cost:")
  label(str = "")
  setLayoutRowDynamic(height = 30, cols = 1)
  if option(label = "Selected maximum cost of training", selected = tType == cost):
    tType = cost
