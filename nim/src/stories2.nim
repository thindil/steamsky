
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

proc progressStory*(nextStep: bool = false): bool =
  var
    step = if currentStory.currentStep == -1:
        storiesList[currentStory.index].startingStep
      elif currentStory.currentStep > -1:
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
    if traderIndex > -1:
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
  if step.finishCondition == destroyShip and not nextStep:
    return true
  if finishCondition != "random":
    case step.finishCondition
    of askInBase:
      let traderIndex = findMember(order = talk)
      if traderIndex > -1:
        gainExp(amount = 10, skillNumber = findSkillIndex(
            skillName = finishCondition), crewIndex = traderIndex)
    of destroyShip, stories.explore:
      for index, member in playerShip.crew:
        if member.order in {pilot, gunner}:
          gainExp(amount = 10, skillNumber = findSkillIndex(
              skillName = finishCondition), crewIndex = index)
    of loot:
      for index, member in playerShip.crew:
        if member.order == boarding:
          gainExp(amount = 10, skillNumber = findSkillIndex(
              skillName = finishCondition), crewIndex = index)
    of stories.any:
      discard
  updateGame(minutes = 30)
  for finishedStory in finishedStories.mitems:
    if finishedStory.index == currentStory.index:
      finishedStory.stepsTexts.add(getCurrentStoryText())
      break
  currentStory.step.inc
  currentStory.finishedStep = step.finishCondition
  currentStory.showText = true
  if currentStory.step < currentStory.maxSteps:
    currentStory.currentStep = getRandom(min = storiesList[
        currentStory.index].steps.low, max = storiesList[
        currentStory.index].steps.high)
    step = storiesList[currentStory.index].steps[currentStory.currentStep]
  elif currentStory.step == currentStory.maxSteps:
    currentStory.currentStep = -1
    step = storiesList[currentStory.index].finalStep
  else:
    currentStory.currentStep = -2
  if currentStory.currentStep != -2:
    case step.finishCondition
    of askInBase:
      currentStory.data = selectBase(value = getStepData(
          finishData = step.finishData, name = "base"))
    of destroyShip:
      currentStory.data = selectEnemy(step = step.finishData)
    of stories.explore:
      currentStory.data = selectLocation(step = step.finishData)
    of loot:
      currentStory.data = selectLoot(step = step.finishData)
    of stories.any:
      discard
  return true

# Temporary code for interfacing with Ada

proc progressAdaStory(nextStep: cint): cint {.raises: [], tags: [WriteIOEffect,
    RootEffect], exportc.} =
  try:
    return progressStory(nextStep = nextStep == 1).cint
  except ValueError, IOError, Exception:
    return 0
