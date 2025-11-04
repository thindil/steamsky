# Copyright 2022-2025 Bartek thindil Jasicki
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

## Provides code related to the sky bases like generating their names,
## counting prices in them, updating populations, generating recruits, etc.

import std/tables
import contracts
import basestypes, config, factions, game, goals, items, maps, reputation,
    shipscrew, types, utils

proc generateBaseName*(factionIndex: string): string {.raises: [],
    tags: [], contractual.} =
  ## Generate the name for the sky base, based on its owner's faction. Based
  ## on libtcod names generator
  ##
  ## * factionIndex - the index of the faction which owns the base
  ##
  ## Returns the randomly generated name of the base
  require:
    factionsList.hasKey(key = factionIndex)
  body:
    try:
      if factionsList[factionIndex].namesType == robotic:
        return generateRoboticName()
    except KeyError:
      discard
    if getRandom(min = 1, max = 100) < 16:
      result = basesSyllablesPreList[getRandom(min = 0, max = (
          basesSyllablesPreList.len() - 1))] & " "
    result &= basesSyllablesStartList[getRandom(min = 0, max = (
        basesSyllablesStartList.len - 1))]
    result &= basesSyllablesEndList[getRandom(min = 0, max = (
        basesSyllablesEndList.len - 1))]
    if getRandom(min = 1, max = 100) < 16:
      result &= " " & basesSyllablesPostList[getRandom(min = 0, max = (
          basesSyllablesPostList.len - 1))]

proc countPrice*(price: var Natural; traderIndex: int;
    reduce: bool = true) {.raises: [KeyError], tags: [],
        contractual.} =
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
    of -24.. -1:
      bonus -= (price.float * 0.05).int
    of 26..50:
      bonus += (price.float * 0.05).int
    of 51..75:
      bonus += (price.float * 0.1).int
    of 76..100:
      bonus += (price.float * 0.15).int
    else:
      discard
  if bonus < 0:
    bonus = 0
  if reduce:
    if bonus >= price:
      bonus = price - 1
    price -= bonus
  else:
    price += bonus

proc getBasePopulation*(baseIndex: BasesRange): BasePopulation {.raises: [],
    tags: [], contractual.} =
  ## Get the size of the selected base's population
  ##
  ## * baseIndex - the index of the base
  ##
  ## Returns the size of the base's population
  case skyBases[baseIndex].population
  of 0:
    return empty
  of 1..149:
    return small
  of 150..299:
    return medium
  else:
    return large

proc updatePopulation*() {.raises: [], tags: [], contractual.} =
  ## Update the base population if needed
  let baseIndex: BasesRange = skyMap[playerShip.skyX][playerShip.skyY].baseIndex
  if daysDifference(dateToCompare = skyBases[baseIndex].recruitDate) < 30:
    return
  if getBasePopulation(baseIndex = baseIndex) == empty:
    if getRandom(min = 1, max = 100) > 5:
      return
    skyBases[baseIndex].population = getRandom(min = 5, max = 10)
    skyBases[baseIndex].owner = getRandomFaction()
  else:
    if getRandom(min = 1, max = 100) > 30:
      return
    let populationDiff: int = (if getRandom(min = 1, max = 100) < 20: getRandom(
        min = -10, max = -1) else: getRandom(min = 1, max = 10))
    var newPopulation: int = skyBases[baseIndex].population + populationDiff
    if newPopulation < 0:
      newPopulation = 0
    skyBases[baseIndex].population = newPopulation
    if skyBases[baseIndex].population == 0:
      skyBases[baseIndex].reputation = ReputationData(level: 0, experience: 0)

proc generateRecruits*() {.raises: [KeyError], tags: [],
    contractual.} =
  ## Generate available recruits in the base if needed
  let baseIndex: BasesRange = skyMap[playerShip.skyX][playerShip.skyY].baseIndex
  var
    highestLevel: SkillRange = 0
    skills: seq[SkillInfo] = @[]
    recruitFaction: string = ""
    inventory: seq[RecruitItem] = @[]
    equipment: EquipmentArray = [weapon: -1, shield: -1, helmet: -1, torso: -1,
        arms: -1, legs: -1, tool: -1]
    price, payment: Natural = 0

  proc addInventory(itemsIndexes: seq[Positive];
      equipIndex: EquipmentLocations) {.raises: [KeyError], tags: [],
      contractual.} =
    ## Add an item to a recruit's inventory
    ##
    ## * itemsIndexes - the list of items' indexes from which the item will
    ##                  be added
    ## * equipIndex   - the equipment location to which the new item will be
    ##                  added
    if getRandom(min = 1, max = 100) > 80:
      return
    let itemIndex: Natural = getRandomItem(itemsIndexes = itemsIndexes,
        equipIndex = equipIndex, highestLevel = highestLevel,
        weaponSkillLevel = skills[0].level, factionIndex = recruitFaction)
    if itemIndex == 0:
      return
    let quality: ObjectQuality = case getRandom(min = 1, max = 100)
          of 1:
            poor
          of 2..3:
            low
          of 4..97:
            normal
          of 98..99:
            good
          else:
            excellent
    inventory.add(y = RecruitItem(index : itemIndex, quality: quality))
    equipment[equipIndex] = inventory.high
    price += getPrice(baseType = skyBases[baseIndex].baseType,
        itemIndex = itemIndex, quality = quality)
    payment += (getPrice(baseType = skyBases[baseIndex].baseType,
        itemIndex = itemIndex, quality = quality) / 10).int

  if daysDifference(dateToCompare = skyBases[baseIndex].recruitDate) < 30:
    return
  var maxRecruits: Positive = case getBasePopulation(baseIndex = baseIndex)
      of empty:
        return
      of small:
        5
      of medium:
        10
      of large:
        15
  if "barracks" in basesTypesList[skyBases[baseIndex].baseType].flags:
    maxRecruits *= 2
  if maxRecruits > (skyBases[baseIndex].population / 10).int:
    maxRecruits = (skyBases[baseIndex].population / 10).int + 1
  let recruitsAmount: Positive = getRandom(min = 1, max = maxRecruits)
  var
    maxSkillAmount: int = (skillsList.len.float * (skyBases[
      baseIndex].reputation.level.float / 100.0)).int
    baseRecruits: seq[RecruitData] = @[]
    gender: char = 'M'
  if maxSkillAmount < 5:
    maxSkillAmount = 5
  for i in 1..recruitsAmount:
    skills = @[]
    price = 0
    inventory = @[]
    payment = 0
    highestLevel = 1
    var
      attributes: seq[MobAttributeRecord] = @[]
      tempToolsList: seq[Positive] = @[]
    for item in equipment.mitems:
      item = -1
    recruitFaction = (if getRandom(min = 1, max = 100) < 99: skyBases[
        baseIndex].owner else: getRandomFaction())
    let faction: FactionData = factionsList[recruitFaction]
    var highestSkill: SkillRange = 1
    if "nogender" in faction.flags:
      gender = 'M'
    else:
      gender = (if getRandom(min = 1, max = 2) == 1: 'M' else: 'F')
    var localSkillAmount: Positive = getRandom(min = 1, max = skillsList.len)
    if localSkillAmount > maxSkillAmount:
      localSkillAmount = maxSkillAmount
    var maxSkillLevel: int = skyBases[baseIndex].reputation.level
    if maxSkillLevel < 20:
      maxSkillLevel = 20
    if getRandom(min = 1, max = 100) > 95:
      maxSkillLevel = getRandom(min = maxSkillLevel, max = 100)
    for j in 1..localSkillAmount:
      let
        skillNumber: Natural = (if j > 1: getRandom(min = 1,
            max = skillsList.len) else: faction.weaponSkill)
        skillLevel: Positive = getRandom(min = 1, max = maxSkillLevel)
      if skillLevel > highestLevel:
        highestLevel = skillLevel
        highestSkill = skillNumber
      var skillIndex: int = -1
      for index, skill in skills:
        if skill.index == skillNumber:
          skillIndex = (if skills[index].level < skillLevel: index else: -2)
          break
      if skillIndex == -1:
        skills.add(y = SkillInfo(index: skillNumber, level: skillLevel,
            experience: 0))
      elif skillIndex > -1:
        skills[skillIndex] = SkillInfo(index: skillNumber, level: skillLevel, experience: 0)
    for j in 1..attributesList.len:
      attributes.add(y = MobAttributeRecord(level: getRandom(min = 3, max = (
          maxSkillLevel / 3).int), experience: 0))
    for skill in skills:
      price += skill.level
      payment += skill.level
    for attribute in attributes:
      price += (attribute.level * 2)
      payment += (attribute.level * 2)
    addInventory(itemsIndexes = weaponsList, equipIndex = weapon)
    addInventory(itemsIndexes = shieldsList, equipIndex = shield)
    addInventory(itemsIndexes = headArmorsList, equipIndex = helmet)
    addInventory(itemsIndexes = chestArmorsList, equipIndex = torso)
    addInventory(itemsIndexes = armsArmorsList, equipIndex = arms)
    addInventory(itemsIndexes = legsArmorsList, equipIndex = legs)
    for recipe in recipesList.values:
      if highestSkill == recipe.skill:
        for index, item in itemsList:
          if item.itemType == recipe.tool:
            tempToolsList.add(y = index)
        addInventory(itemsIndexes = tempToolsList, equipIndex = tool)
        break
    if "barracks" in basesTypesList[skyBases[baseIndex].baseType].flags:
      price = (price / 2).int
      payment = (payment / 2).int
    price = ((price.float * 100.0) * newGameSettings.pricesBonus).int
    if price < 1:
      price = 1
    let recruitBase: Positive = (if getRandom(min = 1, max = 100) <
        99: baseIndex else: getRandom(min = skyBases.low, max = skyBases.high))
    baseRecruits.add(y = RecruitData(attributes: attributes, skills: skills,
        name: generateMemberName(gender = gender,
        factionIndex = recruitFaction), gender: gender, price: price,
        inventory: inventory, equipment: equipment, payment: payment,
        homeBase: recruitBase, faction: recruitFaction))
  skyBases[baseIndex].recruitDate = gameDate
  skyBases[baseIndex].recruits = baseRecruits

proc gainRep*(baseIndex: BasesRange; points: int) {.raises: [ReputationError],
    tags: [], contractual.} =
  ## Change the player reputation in the selected sky base and factions
  ##
  ## * baseIndex - the index of the base in which the reputation will change
  ## * points    - the amount of reputation points about which the reputation
  ##               will change
  # Update the factions' reputation
  updateReputation(baseIndex = baseIndex, amount = points)
  # Don't lose reputation below the lowest value
  if skyBases[baseIndex].reputation.level == -100 and points < 0:
    return
  # Don't gain reputation above the highest value
  if skyBases[baseIndex].reputation.level == 100 and points > 0:
    return
  var newPoints: int = skyBases[baseIndex].reputation.experience + (
      points.float * newGameSettings.reputationBonus).int
  if baseIndex == playerShip.homeBase:
    newPoints += points
  while newPoints < 0:
    skyBases[baseIndex].reputation.level.dec
    newPoints += abs(x = skyBases[baseIndex].reputation.level * 5)
    if newPoints >= 0:
      skyBases[baseIndex].reputation.experience = newPoints
      return
  while newPoints > abs(x = skyBases[baseIndex].reputation.level * 5):
    newPoints -= abs(x = skyBases[baseIndex].reputation.level * 5)
    skyBases[baseIndex].reputation.level.inc
  skyBases[baseIndex].reputation.experience = newPoints
  if skyBases[baseIndex].reputation.level == 100:
    updateGoal(goalType = GoalTypes.reputation, targetIndex = skyBases[
        baseIndex].owner)

proc updatePrices*() {.raises: [], tags: [], contractual.} =
  ## Random changes to the items' prices in the selected base
  let baseIndex: BasesRange = skyMap[playerShip.skyX][playerShip.skyY].baseIndex
  var chance: Natural = case getBasePopulation(baseIndex = baseIndex)
      of empty:
        return
      of small:
        1
      of medium:
        2
      of large:
        5
  chance += (daysDifference(dateToCompare = skyBases[
      baseIndex].visited) / 10).int
  if getRandom(min = 1, max = 100) > chance:
    return
  for item in skyBases[baseIndex].cargo.mitems:
    let roll: Positive = getRandom(min = 1, max = 100)
    if roll < 30 and item.price > 1:
      item.price.dec
    elif roll < 60 and item.price > 0:
      item.price.inc
