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

import std/tables
import careers, config, game, goals, types

proc gainExp*(amount: Natural; skillNumber: Positive;
    crewIndex: Natural) {.sideEffect, raises: [], tags: [].} =
  ## Raise the crew member experience in the selected skill and associated
  ## attribute
  ##
  ## * amount      - the amount of experience gained by the crew mmeber
  ## * skillNumber - the index of the skill in which the experience is gained
  ## * crewIndex   - the index of the crew member who gains experience
  let attributeIndex = try:
      skillsList[skillNumber].attribute
    except KeyError:
      Positive.high
  if attributeIndex == Positive.high:
    return
  var
    skillExp, newAmount, skillLevel = 0
    skillIndex = -1

  proc gainExpInAttribute(attribute: Positive) =
    var memberAttribute = playerShip.crew[crewIndex].attributes[attribute]
    if memberAttribute.level == 50:
      return
    var
      attributeExp = memberAttribute.experience + newAmount
      attributeLevel = memberAttribute.level
    if attributeExp >= attributeLevel * 250:
      attributeExp = attributeExp - (attributeLevel * 250)
      attributeLevel.inc
    playerShip.crew[crewIndex].attributes[attribute].level = attributeLevel
    playerShip.crew[crewIndex].attributes[attribute].experience = attributeExp

  newAmount = try:
      if skillsList[skillNumber].name in careersList[playerCareer].skills:
        amount + (amount / 2).int
      else:
        amount
    except KeyError:
      -1
  if newAmount == -1:
    return
  newAmount = (newAmount.float * newGameSettings.experienceBonus).int
  if newAmount == 0:
    return
  gainExpInAttribute(attribute = conditionIndex)
  gainExpInAttribute(attribute = attributeIndex)
  for i in playerShip.crew[crewIndex].skills.low..playerShip.crew[
      crewIndex].skills.high:
    if playerShip.crew[crewIndex].skills[i].index == skillNumber:
      skillIndex = i
      break
  if skillIndex > -1:
    if playerShip.crew[crewIndex].skills[skillIndex].level == SkillRange.high:
      return
    skillLevel = playerShip.crew[crewIndex].skills[skillIndex].level
    skillExp = playerShip.crew[crewIndex].skills[skillIndex].experience + newAmount
  if skillExp >= skillLevel * 25:
    skillExp = skillExp - (skillLevel * 25)
    skillLevel.inc
  if skillIndex > -1:
    playerShip.crew[crewIndex].skills[skillIndex] = SkillInfo(
        index: skillNumber, level: skillLevel, experience: skillExp)
  else:
    playerShip.crew[crewIndex].skills.add(y = SkillInfo(index: skillNumber,
        level: skillLevel, experience: skillExp))

proc gainRep*(baseIndex: BasesRange; points: int) {.sideEffect, raises: [],
    tags: [].} =
  ## Change the player reputation in the selected sky base
  ##
  ## * baseIndex - the index of the base in which the reputation will change
  ## * points    - the amount of reputation points about which the reputation
  ##               will change
  if skyBases[baseIndex].reputation.level == -100 or skyBases[
      baseIndex].reputation.level == 100:
    return
  var newPoints = skyBases[baseIndex].reputation.experience + (points.float *
      newGameSettings.reputationBonus).int
  if baseIndex == playerShip.homeBase:
    newPoints = newPoints + points
  while newPoints < 0:
    skyBases[baseIndex].reputation.level.dec
    newPoints = newPoints + abs(x = skyBases[baseIndex].reputation.level * 5)
    if newPoints >= 0:
      skyBases[baseIndex].reputation.experience = newPoints
      return
  while newPoints > abs(x = skyBases[baseIndex].reputation.level * 5):
    newPoints = newPoints - abs(x = skyBases[baseIndex].reputation.level * 5)
    skyBases[baseIndex].reputation.level.inc
  skyBases[baseIndex].reputation.experience = newPoints
  if skyBases[baseIndex].reputation.level == 100:
    updateGoal(goalType = reputation, targetIndex = skyBases[baseIndex].owner)

# Temporary code for interfacing with Ada

proc gainAdaExp(amount, skillNumber, crewIndex: cint) {.raises: [], tags: [], exportc.} =
  gainExp(amount = amount.Natural, skillNumber = skillNumber.Positive,
      crewIndex = crewIndex.Natural - 1)

proc gainAdaRep(baseIndex, points: cint) {.raises: [], tags: [], exportc.} =
  gainRep(baseIndex = baseIndex, points = points)
