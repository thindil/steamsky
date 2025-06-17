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
import ../[basestrade, crew, crewinventory, game, types]
import coreui, errordialog

var
  moneyIndex2*: int = -1
  moneyText*, crewList*, schoolSkillsList*: seq[string] = @[]
  moneyWidth*: seq[cfloat] = @[]
  crewIndex*, skillIndex*, minCost*, maxCost*: Natural = 0
  amount*, timesCost*, oneTrainCost*: Positive = 1
  skillsIndexes*: seq[Natural] = @[]

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

proc setTrainingCost*(dialog: var GameDialog){.raises: [], tags: [RootEffect],
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
    moneyText.add(y = "You don't have any " & moneyName & " to pay for learning")
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
