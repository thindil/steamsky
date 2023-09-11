
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

import std/[strutils, tables]
import game, game2, shipscrew, stories, types, utils

proc progessStory*(nextStep: bool = false): bool =
  var
    step = if currentStory.currentStep == 0:
        storiesList[currentStory.index].startingStep
      elif currentStory.currentStep > 0:
        storiesList[currentStory.index].steps[currentStory.currentStep]
      else:
        storiesList[currentStory.index].finalStep
    finishCondition = getStepData(finishData = step.finishData,
        name = "condition")
  let maxRandom = if step.finishCondition == destroyShip and nextStep:
        1
      else:
        getStepData(finishData = step.finishData, name = "chance").parseInt
  if finishCondition == "random" and getRandom(min = 1, max = maxRandom) > 1:
    updateGame(minutes = 10)
    return false
  var chance = 0
  case step.finishCondition
  of askInBase:
    let traderIndex = findMember(order = talk)
    if traderIndex > 0:
      chance = getSkillLevel(member = playerShip.crew[traderIndex],
          skillIndex = findSkillIndex(skillName = finishCondition))
  of destroyShip, stories.explore:
    for member in playerShip.crew:
      if member.order in {pilot, gunner}:
        chance = chance + getSkillLevel(member = member,
            skillIndex = findSkillIndex(skillName = finishCondition))
  of loot:
    for member in playerShip.crew:
      if member.order == boarding:
        chance = chance + getSkillLevel(member = member,
            skillIndex = findSkillIndex(skillName = finishCondition))
  of stories.any:
    discard
  if chance < maxRandom:
    updateGame(minutes = 10)
    return false
