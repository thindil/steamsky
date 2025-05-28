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
import ../[basestrade, config, crew, crewinventory, game, types]
import coreui, dialogs, errordialog, header, messagesui, themes

type
  TrainingType = enum
    times, cost

var
  moneyIndex2: int = -1
  moneyText, crewList, schoolSkillsList: seq[string] = @[]
  moneyWidth: seq[cfloat] = @[]
  crewIndex, skillIndex, minCost, maxCost: Natural = 0
  amount, timesCost, oneTrainCost: Positive = 1
  tType: TrainingType = times
  skillsIndexes: seq[Natural] = @[]

proc setSchoolSkills*(){.raises: [], tags: [], contractual.} =
  ## Set the skills list for the selected crew member
  schoolSkillsList = @[]
  for index, skill in skillsList:
    var skillLevel = 0
    for skill2 in playerShip.crew[crewIndex].skills:
      if skill2.index == index:
        skillLevel = skill2.level
        break
    if skillLevel != 100:
      schoolSkillsList.add(y = skill.name & ": " & (if skillLevel ==
          0: "Untrained" else: getSkillLevelName(
          skillLevel = skillLevel).strip))
      skillsIndexes.add(y = index)

proc setTrainingCost(dialog: var GameDialog){.raises: [], tags: [RootEffect],
    contractual.} =
  ## Set the one training session cost for the selected skill of the selected
  ## crew member
  ##
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameter dialog. It is modified if any error
  ## happened.
  oneTrainCost = try:
      trainCost(memberIndex = crewIndex, skillIndex = skillsIndexes[skillIndex])
    except:
      dialog = setError(message = "Can't count the training cost.")
      return
  timesCost = oneTrainCost * amount
  minCost = oneTrainCost
  let moneyIndex2: int = findItem(inventory = playerShip.cargo,
      protoIndex = moneyIndex)
  if moneyIndex2 < 0:
    minCost = 0
    maxCost = 0
  else:
    maxCost = playerShip.cargo[moneyIndex2].amount

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
  setTrainingCost(dialog = dialog)

proc showSchool*(state: var GameState; dialog: var GameDialog) {.raises: [],
    tags: [RootEffect], contractual.} =
  ## Show the school UI
  ##
  ## * state - the current game's state
  ## * dialog - the current in-game dialog displayed on the screen
  ##
  ## Returns the modified parameters state and dialog. The latter is modified if
  ## any error happened.
  if showHeader(dialog = dialog, close = CloseDestination.map, state = state):
    return
  let tableHeight: float = windowHeight - gameSettings.messagesPosition.float - 20
  setLayoutRowDynamic(height = tableHeight, cols = 1)
  group(title = "SchoolGroup", flags = {windowNoFlags}):
    # Show information about money owned by the player
    setLayoutRowStatic(height = 30, cols = moneyWidth.len, ratio = moneyWidth)
    for index, text in moneyText:
      if index mod 2 == 0:
        label(str = text)
      else:
        colorLabel(str = text, color = theme.colors[goldenColor])
    setLayoutRowStatic(height = 30, cols = 4, ratio = [200.cfloat, 200, 50, 250])
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Train the selected skill of the selected crew member")
    labelButton(title = "Train"):
      try:
        trainSkill(memberIndex = crewIndex, skillIndex = skillsIndexes[
            skillIndex], amount = amount, isAmount = tType == times)
      except NoMoneyError:
        dialog = setMessage(message = "You don't have any " & moneyName &
            " to pay for learning.", title = "Can't train")
      except NotEnoughMoneyError:
        dialog = setMessage(message = "You don't have enough " & moneyName &
            " to pay for learning this skill.", title = "Can't train")
      except:
        dialog = setError(message = "Can't train the skill.")
      setSchool(dialog = dialog)
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Select the crew member which skills will be trained")
    let newMember = comboList(items = crewList,
        selected = crewIndex, itemHeight = 25, x = 200, y = 150)
    if newMember != crewIndex:
      crewIndex = newMember
      setSchoolSkills()
      setTrainingCost(dialog = dialog)
    label(str = "in", alignment = centered)
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Select the skill which will be trained")
    let newSkill = comboList(items = schoolSkillsList,
        selected = skillIndex, itemHeight = 25, x = 300, y = 150)
    if newSkill != skillIndex:
      skillIndex = newSkill
      setTrainingCost(dialog = dialog)
    setLayoutRowDynamic(height = 30, cols = 1)
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Train the selected skill the selected amount of times")
    if option(label = "Selected amount of times", selected = tType == times):
      tType = times
    setLayoutRowDynamic(height = 30, cols = 2)
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Enter amount of training sessions between 1 and 100")
    label(str = "Amount:")
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Enter amount of training sessions between 1 and 100")
    let newAmount: int = property2(name = "#", min = 1, val = amount, max = 100,
        step = 1, incPerPixel = 1)
    if newAmount != amount:
      amount = newAmount
      timesCost = oneTrainCost * amount
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Minimal cost of training. The real cost can be higher that this.")
    label(str = "Minimal cost:")
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Minimal cost of training. The real cost can be higher that this.")
    colorLabel(str = $timesCost & " " & moneyName, color = theme.colors[goldenColor])
    setLayoutRowDynamic(height = 30, cols = 1)
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Train the selected skill as long as you don't spend the selected amount of money")
    if option(label = "Selected maximum cost of training", selected = tType == cost):
      tType = cost
    setLayoutRowDynamic(height = 30, cols = 2)
    label(str = "Cost:")
    if gameSettings.showTooltips:
      addTooltip(bounds = getWidgetBounds(),
          text = "Enter amount of money which you want to spend")
    let newCost: int = property2(name = "#", min = oneTrainCost, val = minCost,
        max = maxCost, step = oneTrainCost, incPerPixel = 1)
    if newCost != minCost:
      minCost = newCost
  showLastMessages(theme = theme, dialog = dialog, height = windowHeight - tableHeight)
  showGameMenu(dialog = dialog)
