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
import combat, config, crewinventory, game, maps, messages, utils, shipscargo,
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

proc getSkillLevelName*(skillLevel: SkillRange): string {.sideEffect, raises: [
    ], tags: [].} =
  ## Get the skill level name for the selected skill or its numerical
  ## value if the player set it in the configuration.
  ##
  ## * attributeLevel - the level of a skill which value will be get
  ##
  ## Returns the string representation of the selected level of a skill.
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

proc findCabin*(memberIndex: Natural): int {.sideEffect, raises: [], tags: [].} =
  ## Find index of cabin which belongs to the selected crew member
  ##
  ## * memberIndex - the index of the player's ship crew member which will be
  ##                 checked
  ##
  ## Returns the index of the cabin which belongs to the selected crew member
  ## or -1 if nothing found
  for index, module in playerShip.modules.pairs:
    if module.mType == ModuleType2.cabin:
      for owner in module.owner:
        if owner == memberIndex:
          return index
  return -1

proc updateCrew*(minutes: Positive; tiredPoints: Natural;
    inCombat: bool = false) =

  var
    i: Natural = 0
    tiredLevel, hungerLevel, thirstLevel: int = 0

  proc updateMember(member: var MemberData) =

    func normalizeStat(stat: var int; maxValue: Positive = 100) {.raises: [],
        tags: [].} =
      if stat > maxValue:
        stat = maxValue
      elif stat < 0:
        stat = 0

    proc consume(itemType: string): Natural =
      var
        itemIndex: int = findItem(inventory = playerShip.cargo,
            itemType = itemType)
        consumeValue: Natural = 0
      if itemIndex > 0:
        consumeValue = itemsList[playerShip.cargo[itemIndex].protoIndex].value[1]
        if itemsList[playerShip.cargo[itemIndex].protoIndex].value.len > 1 and
            itemsList[playerShip.cargo[itemIndex].protoIndex].value[2] != 0:
          updateMorale(ship = playerShip, memberIndex = i, value = itemsList[
              playerShip.cargo[itemIndex].protoIndex].value[2])
        updateCargo(ship = playerShip, protoIndex = playerShip.cargo[
            itemIndex].protoIndex, amount = -1)
        return consumeValue
      itemIndex = findItem(inventory = playerShip.crew[i].inventory,
          itemType = itemType)
      if itemIndex > 0:
        consumeValue = itemsList[playerShip.crew[i].inventory[
            itemIndex].protoIndex].value[1]
        if itemsList[playerShip.crew[i].inventory[
            itemIndex].protoIndex].value.len > 1 and itemsList[playerShip.crew[
            i].inventory[itemIndex].protoIndex].value[2] != 0:
          updateMorale(ship = playerShip, memberIndex = i, value = itemsList[
              playerShip.crew[i].inventory[itemIndex].protoIndex].value[2])
        updateInventory(memberIndex = i, amount = -1,
            inventoryIndex = itemIndex, ship = playerShip)
        return consumeValue
      return 0

    if "nofatigue" in factionsList[member.faction].flags:
      tiredLevel = 0
    var backToWork = true
    if tiredLevel == 0 and member.order == rest and member.previousOrder != rest:
      if member.previousOrder notin [repair, clean] and findMember(
          order = member.previousOrder) > -1:
        backToWork = false
      if member.previousOrder in [gunner, craft]:
        block moduleLoop:
          for module in playerShip.modules.mitems:
            if (member.previousOrder == gunner and module.mType ==
                ModuleType2.gun) and module.owner[0] in [i, -1]:
              backToWork = true
              module.owner[0] = i
              break moduleLoop
            elif (member.previousOrder == craft and module.mType ==
                ModuleType2.workshop) and module.craftingIndex.len > 0:
              for owner in module.owner.mitems:
                if owner == i:
                  backToWork = true
                  owner = i
                  break moduleLoop
              for owner in module.owner.mitems:
                if owner == -1:
                  backToWork = true
                  owner = i
                  break moduleLoop
      if backToWork:
        member.order = member.previousOrder
        member.orderTime = 15
        addMessage(message = member.name & " returns to work fully rested.",
            mType = orderMessage, color = yellow)
        updateMorale(ship = playerShip, memberIndex = i, value = 1)
      member.previousOrder = rest
    if (tiredLevel > 80 + member.attributes[conditionIndex].level) and
        member.order != rest and not inCombat:
      var canRest: bool = true
      if member.order == boarding and harpoonDuration == 0 and
          combat.enemy.harpoonDuration == 0:
        canRest = false
      if canRest:
        member.previousOrder = member.order
        member.order = rest
        member.orderTime = 15
        if member.equipment[tool] > -1:
          updateCargo(ship = playerShip, protoIndex = member.inventory[
              member.equipment[tool]].protoIndex, amount = 1,
              durability = member.inventory[member.equipment[tool]].durability)
          updateInventory(memberIndex = i, amount = -1,
              inventoryIndex = member.equipment[tool], ship = playerShip)
          member.equipment[tool] = -1
        addMessage(message = member.name &
            " is too tired to work, they're going to rest.",
            mType = orderMessage, color = yellow)
        block findNewCabin:
          if findCabin(memberIndex = i) == -1:
            for module in playerShip.modules.mitems:
              if module.mType == ModuleType2.cabin and module.durability > 0:
                for owner in module.owner.mitems:
                  if owner == -1:
                    owner = i
                    addMessage(message = member.name & " take " & module.name &
                        " as own cabin.", mType = otherMessage)
                    break findNewCabin
      else:
        addMessage(message = member.name &
            " is very tired but they can't go to rest.", mType = orderMessage, color = red)
        updateMorale(ship = playerShip, memberIndex = i, value = getRandom(
            min = -5, max = -1))
    normalizeStat(stat = tiredLevel, maxValue = 150)
    member.tired = tiredLevel
    if hungerLevel > 80:
      var consumeResult = 0
      for foodType in factionsList[member.faction].foodTypes.items:
        consumeResult = consume(itemType = foodType)
        if consumeResult > 0:
          break
      if hungerLevel - consumeResult < SkillRange.low:
        hungerLevel = SkillRange.low
      else:
        hungerLevel = hungerLevel - consumeResult
      if consumeResult == 0:
        addMessage(message = member.name &
            " is hungry, but they can't find anything to eat.",
            mType = otherMessage, color = red)
        updateMorale(ship = playerShip, memberIndex = i, value = getRandom(
            min = -10, max = -5))
    normalizeStat(stat = hungerLevel)
    member.hunger = hungerLevel
    if thirstLevel > 40:
      var consumeResult = 0
      for drinksType in factionsList[member.faction].drinksTypes.items:
        consumeResult = consume(itemType = drinksType)
        if consumeResult > 0:
          break
      if thirstLevel - consumeResult < SkillRange.low:
        thirstLevel = SkillRange.low
      else:
        thirstLevel = thirstLevel - consumeResult
      if consumeResult == 0:
        addMessage(message = member.name &
            " is thirsty, but they can't find anything to drink.",
            mType = otherMessage, color = red)
        updateMorale(ship = playerShip, memberIndex = i, value = getRandom(
            min = -20, max = -10))
    normalizeStat(stat = thirstLevel)
    member.thirst = thirstLevel

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

proc findAdaCabin(memberIndex: cint): cint {.raises: [], tags: [], exportc.} =
  return findCabin(memberIndex = memberIndex - 1).cint + 1
