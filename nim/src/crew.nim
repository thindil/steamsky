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
import config, crewinventory, game, maps, messages, utils, shipscargo,
    shipscrew, shipscrew2, types

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

proc dailyPayment*() {.sideEffect, raises: [KeyError, Exception], tags: [RootEffect].} =
  ## Pay daily payments to the player's ship crew members and update the lenght
  ## of their contracts
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
  while memberIndex <= playerShip.crew.high:
    if playerShip.crew[memberIndex].contractLength > 0:
      playerShip.crew[memberIndex].contractLength.dec
      if playerShip.crew[memberIndex].contractLength == 0:
        addMessage(message = "Your contract with " & playerShip.crew[
            memberIndex].name & " has ended.", mType = tradeMessage, color = red)
        if playerShip.speed == docked:
          deleteMember(memberIndex = memberIndex, ship = playerShip)
          skyBases[skyMap[playerShip.skyX][
              playerShip.skyY].baseIndex].population.inc
          memberIndex.dec
        else:
          for order in playerShip.crew[memberIndex].orders.mitems:
            order = 0
          giveOrders(ship = playerShip, memberIndex = memberIndex,
              givenOrder = rest)
    memberIndex.inc

proc getAttributeLevelName*(attributeLevel: Positive): string {.sideEffect,
    raises: [], tags: [].} =
  ## Get the attribute level name for the selected attribute or its numerical
  ## value if the player set it in the configuration.
  ##
  ## * attributeLevel - the level of an attribute which value will be get
  ##
  ## Returns the string representation of the selected level of an attribute.
  if gameSettings.showNumbers == 1:
    return $attributeLevel
  case attributeLevel
  of 1 .. 5:
    return "Very low"
  of 6 .. 10:
    return "Low"
  of 11 .. 15:
    return "Below average"
  of 16 .. 30:
    return "Average"
  of 31 .. 35:
    return "Above average"
  of 36 .. 40:
    return "High"
  of 41 .. 49:
    return "Very high"
  else:
    return "Outstanding"

proc getSkillLevelName*(skillLevel: SkillRange): string =
  if gameSettings.showNumbers == 1:
    return $skillLevel
  case skillLevel
  of 0:
    return "Untrained"
  of 1 .. 10:
    return "Beginner"
  of 11 .. 20:
    return "Novice"
  of 21 .. 30:
    return "Apprentice"
  of 31 .. 40:
    return "Practitioner"
  of 41 .. 50:
    return "Competent"
  of 51 .. 60:
    return "Respected"
  of 61 .. 70:
    return "Renowned"
  of 71 .. 80:
    return "Master"
  of 81 .. 90:
    return "Grand-Master"
  of 91 .. 99:
    return "Legendary"
  else:
    return "Ultimate"

# Temporary code for interfacing with Ada

proc generateAdaMemberName(gender: char;
    factionIndex: cstring): cstring {.raises: [], tags: [], exportc.} =
  return generateMemberName(gender = gender,
      factionIndex = $factionIndex).cstring

proc dailyAdaPayment() {.raises: [], tags: [RootEffect], exportc.} =
  try:
    dailyPayment()
  except KeyError, Exception:
    discard

proc getAdaAttributeLevelName(attributeLevel: cint): cstring {.raises: [],
    tags: [], exportc.} =
  return getAttributeLevelName(attributeLevel = attributeLevel.Positive).cstring

proc getAdaSkillLevelName(skillLevel: cint): cstring {.raises: [], tags: [], exportc.} =
  return getSkillLevelName(skillLevel = skillLevel.Positive).cstring
