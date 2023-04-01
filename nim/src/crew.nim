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
import careers, crewinventory, config, game, maps, messages, utils, shipscargo,
    shipscrew3, types

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

proc dailyPayment*() =
  let moneyIndex2 = findItem(inventory = playerShip.cargo,
      protoIndex = moneyIndex)
  for index, member in playerShip.crew.pairs:
    if member.payment[1] > 0:
      var haveMoney = true
      if moneyIndex2 < playerShip.cargo.low:
        addMessage(message = "You don't have any " & moneyName &
            " to pay your crew members.", mType = tradeMessage, color = red)
        haveMoney = false
      if haveMoney:
        if playerShip.cargo[moneyIndex2].amount < member.payment[1]:
          let moneyNeeded = playerShip.cargo[moneyIndex2].amount
          updateCargo(ship = playerShip, protoIndex = moneyIndex, amount = (0 - moneyNeeded))
          addMessage(message = "You don't have enough " & moneyName &
              " to pay your crew members.", mType = tradeMessage, color = red)
          haveMoney = false
        if haveMoney:
          updateCargo(ship = playerShip, cargoIndex = moneyIndex2, amount = (0 -
              member.payment[1]))
          var payMessage = "You pay " & member.name
          if member.gender == 'M':
            payMessage.add(y = " his ")
          else:
            payMessage.add(y = " her ")
          payMessage.add(y = "daily payment.")
          addMessage(message = payMessage, mType = tradeMessage)
          updateMorale(ship = playerShip, memberIndex = index,
              value = getRandom(min = 1, max = 5))
      if not haveMoney:
        updateMorale(ship = playerShip, memberIndex = index,
            value = getRandom(min = -50, max = -10))
  var memberIndex = 1
  while memberIndex < playerShip.crew.len:
    if playerShip.crew[memberIndex].contractLength > 0:
      playerShip.crew[memberIndex].contractLength.dec
      if playerShip.crew[memberIndex].contractLength == 0:
        addMessage(message = "Your contract with " & playerShip.crew[
            memberIndex].name & " has ended.", mType = tradeMessage, color = red)
        if playerShip.speed == docked:
#          deleteMember(memberIndex = memberIndex, ship = playerShip)
          skyBases[skyMap[playerShip.skyX][
              playerShip.skyY].baseIndex].population.inc
          memberIndex.dec
        else:
          for order in playerShip.crew[memberIndex].orders.mitems:
            order = 0
 #         giveOrders(ship = playerShip, memberIndex = memberIndex,
 #             givenOrder = rest)
          memberIndex.inc

# Temporary code for interfacing with Ada

proc generateAdaMemberName(gender: char;
    factionIndex: cstring): cstring {.raises: [], tags: [], exportc.} =
  return generateMemberName(gender = gender,
      factionIndex = $factionIndex).cstring

proc gainAdaExp(amount, skillNumber, crewIndex: cint) {.raises: [], tags: [], exportc.} =
  gainExp(amount = amount.Natural, skillNumber = skillNumber.Positive,
      crewIndex = crewIndex.Natural - 1)
