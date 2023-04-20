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

import std/tables
import basestypes, config, crew, factions, game, goals, items, maps, shipscrew,
    types, utils

proc generateBaseName*(factionIndex: string): string {.sideEffect, raises: [],
    tags: [].} =
  ## Generate the name for the sky base, based on its owner's faction. Based
  ## on libtcod names generator
  ##
  ## * factionIndex - the index of the faction which owns the base
  ##
  ## Returns the randomly generated name of the base
  try:
    if factionsList[factionIndex].namesType == robotic:
      return $generateRoboticName();
  except KeyError:
    discard
  if getRandom(min = 1, max = 100) < 16:
    result = basesSyllablesPreList[getRandom(min = 0, max = (
        basesSyllablesPreList.len() - 1))] & " "
  result = result & basesSyllablesStartList[getRandom(min = 0, max = (
      basesSyllablesStartList.len - 1))]
  result = result & basesSyllablesEndList[getRandom(min = 0, max = (
      basesSyllablesEndList.len - 1))]
  if getRandom(min = 1, max = 100) < 16:
    result = result & " " & basesSyllablesPostList[getRandom(min = 0, max = (
        basesSyllablesPostList.len - 1))]

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

proc countPrice*(price: var Natural; traderIndex: int;
    reduce: bool = true) {.sideEffect, raises: [KeyError], tags: [].} =
  ## Count the price of the action, like selling, buying, docking in a base
  ##
  ## * price       - the price which will be checked for bonuses or maluses
  ## * traderIndex - the index of the crew member who is on the trader position
  ## * reduce      - if true, reduce the price, otherwise increase it
  ##
  ## Returns the updated parameter price as a new price
  if price == 0:
    return
  var bonus: int = 0
  if traderIndex > -1:
    bonus = (price.float * (getSkillLevel(member = playerShip.crew[traderIndex],
        skillIndex = talkingSkill).float / 200.0)).int
  if skyMap[playerShip.skyX][playerShip.skyY].baseIndex > 0:
    case skyBases[skyMap[playerShip.skyX][
        playerShip.skyY].baseIndex].reputation.level
    of -24 .. -1:
      bonus = bonus - (price.float * 0.05).int
    of 26 .. 50:
      bonus = bonus + (price.float * 0.05).int
    of 51 .. 75:
      bonus = bonus + (price.float * 0.1).int
    of 76 .. 100:
      bonus = bonus + (price.float * 0.15).int
    else:
      discard
  if bonus < 0:
    bonus = 0
  if reduce:
    if bonus >= price:
      bonus = price - 1
    price = price - bonus
  else:
    price = price + bonus

proc updatePopulation*() {.sideEffect, raises: [], tags: [].} =
  ## Update the base population if needed
  let baseIndex = skyMap[playerShip.skyX][playerShip.skyY].baseIndex
  if daysDifference(dateToCompare = skyBases[baseIndex].recruitDate,
      currentDate = gameDate) < 30:
    return
  if skyBases[baseIndex].population > 0:
    if getRandom(min = 1, max = 100) > 30:
      return
    var populationDiff = (if getRandom(min = 1, max = 100) < 20: getRandom(
        min = -10, max = -1) else: getRandom(min = 1, max = 10))
    if skyBases[baseIndex].population + populationDiff < 0:
      populationDiff = -(skyBases[baseIndex].population)
    skyBases[baseIndex].population = skyBases[baseIndex].population + populationDiff
    if skyBases[baseIndex].population == 0:
      skyBases[baseIndex].reputation = ReputationData(level: 0, experience: 0)
  else:
    if getRandom(min = 1, max = 100) > 5:
      return
    skyBases[baseIndex].population = getRandom(min = 5, max = 10)
    skyBases[baseIndex].owner = getRandomFaction()

proc generateRecruits*() =
  let baseIndex = skyMap[playerShip.skyX][playerShip.skyY].baseIndex
  var
    highestLevel: SkillRange = 0
    skills: seq[SkillInfo]
    recruitFaction: string
    inventory: seq[Positive]
    equipment: EquipmentArray
    price, payment: Natural = 0

  proc addInventory(itemsIndexes: seq[Positive];
      equipIndex: EquipmentLocations) =
    if getRandom(min = 1, max = 100) > 80:
      return
    let itemIndex = getRandomItem(itemsIndexes = itemsIndexes,
        equipIndex = equipIndex, highestLevel = highestLevel,
        weaponSkillLevel = skills[0].level, factionIndex = recruitFaction)
    if itemIndex == 0:
      return
    inventory.add(y = itemIndex)
    equipment[equipIndex] = inventory.high
    price = price + getPrice(baseType = skyBases[baseIndex].baseType,
        itemIndex = itemIndex)
    payment = payment + (getPrice(baseType = skyBases[baseIndex].baseType,
        itemIndex = itemIndex) / 10).int

  if daysDifference(dateToCompare = skyBases[baseIndex].recruitDate,
      currentDate = gameDate) < 30 or skyBases[baseIndex].population == 0:
    return
  var maxRecruits = (if skyBases[baseIndex].population < 150: 5 elif skyBases[
      baseIndex].population < 300: 10 else: 15)
  if "barracks" in basesTypesList[skyBases[baseIndex].baseType].flags:
    maxRecruits = maxRecruits * 2
  if maxRecruits > (skyBases[baseIndex].population / 10).int:
    maxRecruits = (skyBases[baseIndex].population / 10).int + 1
  let recruitsAmount = getRandom(min = 1, max = maxRecruits)
  var
    maxSkillAmount: int = (skillsList.len.float * (skyBases[
      baseIndex].reputation.level.float / 100.0)).int
    baseRecruits: seq[RecruitData]
    gender: char
  for i in 1 .. recruitsAmount:
    skills = @[]
    price = 0
    inventory = @[]
    payment = 0
    highestLevel = 1
    var
      attributes: seq[MobAttributeRecord]
      tempToolsList: seq[Positive]
    for item in equipment.mitems:
      item = -1
    recruitFaction = (if getRandom(min = 1, max = 100) < 99: skyBases[
        baseIndex].owner else: getRandomFaction())
    let faction = factionsList[recruitFaction]
    var highestSkill = 1
    if "nogender" in faction.flags:
      gender = 'M'
    else:
      gender = (if getRandom(min = 1, max = 2) == 1: 'M' else: 'F')
    var localSkillAmount = getRandom(min = 1, max = skillsList.len)
    if localSkillAmount > maxSkillAmount:
      localSkillAmount = maxSkillAmount
    var maxSkillLevel = skyBases[baseIndex].reputation.level
    if maxSkillLevel < 20:
      maxSkillLevel = 20
    if getRandom(min = 1, max = 100) > 95:
      maxSkillLevel = getRandom(min = maxSkillLevel, max = 100)
    for j in 1 .. localSkillAmount:
      let
        skillNumber = (if j > 1: getRandom(min = 1,
            max = skillsList.len) else: faction.weaponSkill)
        skillLevel = getRandom(min = 1, max = maxSkillLevel)
      if skillLevel > highestLevel:
        highestLevel = skillLevel
        highestSkill = skillNumber
      var skillIndex = -1
      for index, skill in skills.pairs:
        if skill.index == skillNumber:
          skillIndex = (if skills[index].level < skillLevel: index else: -2)
          break
      if skillIndex == -1:
        skills.add(y = SkillInfo(index: skillNumber, level: skillLevel,
            experience: 0))
      elif skillIndex > -1:
        skills[skillIndex] = SkillInfo(index: skillNumber, level: skillLevel, experience: 0)
    for attribute in attributes.mitems:
      attribute = MobAttributeRecord(level: getRandom(min = 3, max = (
          maxSkillLevel / 3).int), experience: 0)
    for skill in skills.items:
      price = price + skill.level
      payment = payment + skill.level
    for attribute in attributes.items:
      price = price + (attribute.level * 2)
      payment = payment + (attribute.level * 2)
    addInventory(itemsIndexes = weaponsList, equipIndex = weapon)
    addInventory(itemsIndexes = shieldsList, equipIndex = shield)
    addInventory(itemsIndexes = headArmorsList, equipIndex = helmet)
    addInventory(itemsIndexes = chestArmorsList, equipIndex = torso)
    addInventory(itemsIndexes = armsArmorsList, equipIndex = arms)
    addInventory(itemsIndexes = legsArmorsList, equipIndex = legs)
    for recipe in recipesList.values:
      if highestSkill == recipe.skill:
        for index, item in itemsList.pairs:
          if item.itemType == recipe.tool:
            tempToolsList.add(y = index)
        break
    addInventory(itemsIndexes = tempToolsList, equipIndex = tool)
    if "barracks" in basesTypesList[skyBases[baseIndex].baseType].flags:
      price = (price / 2).int
      payment = (payment / 2).int
    price = ((price.float * 100.0) * newGameSettings.pricesBonus).int
    if price < 1:
      price = 1
    let recruitBase = (if getRandom(min = 1, max = 100) <
        99: baseIndex else: getRandom(min = skyBases.low, max = skyBases.high))
    baseRecruits.add(y = RecruitData(attributes: attributes, skills: skills,
        name: generateMemberName(gender = gender,
        factionIndex = recruitFaction), gender: gender, price: price,
        inventory: inventory, equipment: equipment, payment: payment,
        homeBase: recruitBase, faction: recruitFaction))

# Temporary code for interfacing with Ada

proc generateAdaBaseName(factionIndex: cstring): cstring {.exportc, raises: [],
    tags: [].} =
  return generateBaseName(factionIndex = $factionIndex).cstring

proc gainAdaRep(baseIndex, points: cint) {.raises: [], tags: [], exportc.} =
  gainRep(baseIndex = baseIndex, points = points)

proc getAdaBaseReputation(baseIndex, level, experience: cint) {.raises: [],
    tags: [], exportc.} =
  skyBases[baseIndex].reputation = ReputationData(level: level,
      experience: experience)

proc setAdaBaseReputation(baseIndex: cint; level,
    experience: var cint) {.raises: [], tags: [], exportc.} =
  level = skyBases[baseIndex].reputation.level
  experience = skyBases[baseIndex].reputation.experience.cint

proc countAdaPrice(price: var cint; traderIndex, reduce: cint) {.exportc,
    raises: [], tags: [].} =
  try:
    var newPrice: Natural = price
    countPrice(price = newPrice, traderIndex = traderIndex - 1, reduce = (
        if reduce == 1: true else: false))
    price = newPrice.cint
  except KeyError:
    discard

proc getAdaBaseLocation(baseIndex, x, y: cint) {.raises: [], tags: [], exportc.} =
  skyBases[baseIndex].skyX = x
  skyBases[baseIndex].skyY = y

proc getAdaBaseOwner(baseIndex: cint; owner: cstring) {.raises: [], tags: [], exportc.} =
  skyBases[baseIndex].owner = $owner

proc getAdaBasePopulation(baseIndex, population: cint) {.raises: [], tags: [], exportc.} =
  skyBases[baseIndex].population = population

proc setAdaBasePopulation(baseIndex: cint; population: var cint) {.raises: [],
    tags: [], exportc.} =
  population = skyBases[baseIndex].population.cint

proc updateAdaPopulation() {.raises: [], tags: [], exportc.} =
  updatePopulation()

proc getAdaBaseRecruitDate(baseIndex, year, month, day, hour,
    minutes: cint) {.raises: [], tags: [], exportc.} =
  skyBases[baseIndex].recruitDate = DateRecord(year: year, month: month,
      day: day, hour: hour, minutes: minutes)
