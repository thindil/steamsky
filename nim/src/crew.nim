# Copyright 2022-2023 Bartek thindil Jasicki
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

import std/[tables]
import careers, config, game, utils, types

proc generateMemberName*(gender: char; factionIndex: string): string {.sideEffect,
    raises: [], tags: [].} =
  ## Generate the name for the mob, based on his/her faction. Based on
  ## libtcod names generator
  ##
  ## * gender       - The gender of the mob, M - male, F - female
  ## * factionIndex - The index of the faction to which the mob belongs
  ##
  ## Returns the randomly generated name of the mob
  try:
    if factionsList[factionIndex].namesType == robotic:
      return $generateRoboticName();
  except KeyError:
    discard
  if gender == 'M':
    result = malesSyllablesStartList[getRandom(min = 0, max = (
        malesSyllablesStartList.len - 1))]
    result = result & malesVocalsList[getRandom(min = 0, max = (
        malesVocalsList.len - 1))]
    if getRandom(min = 1, max = 100) < 36:
      result = result & malesSyllablesMiddleList[getRandom(min = 0, max = (
          malesSyllablesMiddleList.len - 1))]
    if getRandom(min = 1, max = 100) < 11:
      result = result & malesConsonantsList[getRandom(min = 0, max = (
          malesConsonantsList.len - 1))]
    result = result & malesSyllablesEndList[getRandom(min = 0, max = (
        malesSyllablesEndList.len - 1))]
    return
  result = femalesSyllablesStartList[getRandom(min = 0, max = (
      femalesSyllablesStartList.len - 1))]
  result = result & femalesVocalsList[getRandom(min = 0, max = (
      femalesVocalsList.len - 1))]
  if getRandom(min = 1, max = 100) < 36:
    result = result & femalesSyllablesMiddleList[getRandom(min = 0, max = (
        femalesSyllablesMiddleList.len - 1))]
  if getRandom(min = 1, max = 100) < 11:
    result = result & femalesSyllablesMiddleList[getRandom(min = 0, max = (
        femalesSyllablesMiddleList.len - 1))]
  result = result & femalesSyllablesEndList[getRandom(min = 0, max = (
      femalesSyllablesEndList.len - 1))]

proc getTrainingToolQuality*(memberIndex: Natural;
    skillIndex: Positive): Positive {.sideEffect, raises: [KeyError], tags: [].} =
  ## Get the required tools quality for the selected skill
  ##
  ## * memberIndex - the index of the crew member in the player ship
  ## * skillIndex  - the index of the skill which training tool quality will be get
  ##
  ## Returns numeric value for the minimum training tool quality required to
  ## train the selected skill.
  result = 100
  for skill in playerShip.crew[memberIndex].skills:
    if skill.index == skillIndex:
      for quality in skillsList[skillIndex].toolsQuality:
        if skill.level <= quality.level:
          return quality.quality

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

# Temporary code for interfacing with Ada

proc generateAdaMemberName(gender: char;
    factionIndex: cstring): cstring {.raises: [], tags: [], exportc.} =
  return generateMemberName(gender = gender,
      factionIndex = $factionIndex).cstring

proc getAdaTrainingToolQuality(memberIndex, skillIndex: cint): cint {.raises: [
    ], tags: [], exportc.} =
  try:
    return getTrainingToolQuality(memberIndex = memberIndex - 1,
        skillIndex = skillIndex).cint
  except KeyError:
    return 100

proc gainAdaExp(amount, skillNumber, crewIndex: cint) {.raises: [], tags: [], exportc.} =
  gainExp(amount = amount.Natural, skillNumber = skillNumber.Positive,
      crewIndex = crewIndex.Natural - 1)
