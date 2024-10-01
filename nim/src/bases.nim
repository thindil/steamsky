# Copyright 2022-2024 Bartek thindil Jasicki
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
import basestypes, config, factions, game, goals, items, maps, shipscrew, types, utils

proc generateBaseName*(factionIndex: string): string {.sideEffect, raises: [],
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
    reduce: bool = true) {.sideEffect, raises: [KeyError], tags: [],
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

proc updatePopulation*() {.sideEffect, raises: [], tags: [], contractual.} =
  ## Update the base population if needed
  let baseIndex: BasesRange = skyMap[playerShip.skyX][playerShip.skyY].baseIndex
  if daysDifference(dateToCompare = skyBases[baseIndex].recruitDate) < 30:
    return
  if skyBases[baseIndex].population > 0:
    if getRandom(min = 1, max = 100) > 30:
      return
    var populationDiff: int = (if getRandom(min = 1, max = 100) < 20: getRandom(
        min = -10, max = -1) else: getRandom(min = 1, max = 10))
    if skyBases[baseIndex].population + populationDiff < 0:
      populationDiff = -(skyBases[baseIndex].population)
    skyBases[baseIndex].population += populationDiff
    if skyBases[baseIndex].population == 0:
      skyBases[baseIndex].reputation = ReputationData(level: 0, experience: 0)
  else:
    if getRandom(min = 1, max = 100) > 5:
      return
    skyBases[baseIndex].population = getRandom(min = 5, max = 10)
    skyBases[baseIndex].owner = getRandomFaction()

proc generateRecruits*() {.sideEffect, raises: [KeyError], tags: [],
    contractual.} =
  ## Generate available recruits in the base if needed
  let baseIndex: BasesRange = skyMap[playerShip.skyX][playerShip.skyY].baseIndex
  var
    highestLevel: SkillRange = 0
    skills: seq[SkillInfo] = @[]
    recruitFaction: string = ""
    inventory: seq[Positive] = @[]
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
    inventory.add(y = itemIndex)
    equipment[equipIndex] = inventory.high
    price += getPrice(baseType = skyBases[baseIndex].baseType,
        itemIndex = itemIndex)
    payment += (getPrice(baseType = skyBases[baseIndex].baseType,
        itemIndex = itemIndex) / 10).int

  if daysDifference(dateToCompare = skyBases[baseIndex].recruitDate) < 30 or
      skyBases[baseIndex].population == 0:
    return
  var maxRecruits: Positive = (if skyBases[baseIndex].population <
      150: 5 elif skyBases[baseIndex].population < 300: 10 else: 15)
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

proc gainRep*(baseIndex: BasesRange; points: int) {.sideEffect, raises: [],
    tags: [], contractual.} =
  ## Change the player reputation in the selected sky base
  ##
  ## * baseIndex - the index of the base in which the reputation will change
  ## * points    - the amount of reputation points about which the reputation
  ##               will change
  if skyBases[baseIndex].reputation.level == -100 or skyBases[
      baseIndex].reputation.level == 100:
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
    updateGoal(goalType = reputation, targetIndex = skyBases[baseIndex].owner)

proc updatePrices*() {.sideEffect, raises: [], tags: [], contractual.} =
  ## Random changes to the items' prices in the selected base
  let baseIndex: BasesRange = skyMap[playerShip.skyX][playerShip.skyY].baseIndex
  if skyBases[baseIndex].population == 0:
    return
  var chance: Positive = (if skyBases[baseIndex].population <
      150: 1 elif skyBases[baseIndex].population < 300: 2 else: 5)
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

# Temporary code for interfacing with Ada

proc generateAdaBaseName(factionIndex: cstring): cstring {.exportc, raises: [],
    tags: [], contractual.} =
  ## Temporary C binding
  return generateBaseName(factionIndex = $factionIndex).cstring

proc getAdaBaseReputation(baseIndex, level, experience: cint) {.raises: [],
    tags: [], exportc, contractual.} =
  ## Temporary C binding
  skyBases[baseIndex].reputation = ReputationData(level: level,
      experience: experience)

proc setAdaBaseReputation(baseIndex: cint; level,
    experience: var cint) {.raises: [], tags: [], exportc, contractual.} =
  ## Temporary C binding
  level = skyBases[baseIndex].reputation.level
  experience = skyBases[baseIndex].reputation.experience.cint

proc countAdaPrice(price: var cint; traderIndex, reduce: cint) {.exportc,
    raises: [], tags: [], contractual.} =
  ## Temporary C binding
  try:
    var newPrice: Natural = price
    countPrice(price = newPrice, traderIndex = traderIndex - 1, reduce = (
        if reduce == 1: true else: false))
    price = newPrice.cint
  except KeyError:
    discard

proc getAdaBaseLocation(baseIndex, x, y: cint) {.raises: [], tags: [], exportc,
    contractual.} =
  ## Temporary C binding
  skyBases[baseIndex].skyX = x
  skyBases[baseIndex].skyY = y

proc setAdaBaseLocation(baseIndex: cint; x, y: var cint) {.raises: [], tags: [],
    exportc, contractual.} =
  ## Temporary C binding
  x = skyBases[baseIndex].skyX
  y = skyBases[baseIndex].skyY

proc getAdaBaseOwner(baseIndex: cint; owner: cstring) {.raises: [], tags: [],
    exportc, contractual.} =
  ## Temporary C binding
  skyBases[baseIndex].owner = $owner

proc setAdaBaseOwner(baseIndex: cint; owner: var cstring) {.raises: [], tags: [
    ], exportc, contractual.} =
  ## Temporary C binding
  owner = skyBases[baseIndex].owner.cstring

proc getAdaBasePopulation(baseIndex, population: cint) {.raises: [], tags: [],
    exportc, contractual.} =
  ## Temporary C binding
  skyBases[baseIndex].population = population

proc setAdaBasePopulation(baseIndex: cint; population: var cint) {.raises: [],
    tags: [], exportc, contractual.} =
  ## Temporary C binding
  population = skyBases[baseIndex].population.cint

proc updateAdaPopulation() {.raises: [], tags: [], exportc, contractual.} =
  ## Temporary C binding
  updatePopulation()

proc generateAdaRecruits() {.raises: [], tags: [], exportc, contractual.} =
  ## Temporary C binding
  try:
    generateRecruits()
  except KeyError:
    discard

type
  AdaRecruitData* = object
    ## Temporary C binding
    attributes*: array[1..16, array[2, cint]]
    skills*: array[1..64, array[3, cint]]
    name*: cstring
    gender*: char
    equipment*: array[7, cint]
    payment*: cint
    homeBase*: cint
    faction*: cstring
    price*: cint
    inventory*: array[7, cint]

proc adaRecruitToNim(adaRecruit: AdaRecruitData): RecruitData {.raises: [],
    tags: [], exportc, contractual.} =
  ## Temporary C binding
  result = RecruitData(name: $adaRecruit.name, gender: adaRecruit.gender,
      homeBase: adaRecruit.homeBase, faction: $adaRecruit.faction,
      payment: adaRecruit.payment, price: adaRecruit.price)
  for attribute in adaRecruit.attributes:
    if attribute[0] == 0:
      break
    result.attributes.add(y = MobAttributeRecord(level: attribute[0],
        experience: attribute[1]))
  for skill in adaRecruit.skills:
    if skill[0] == 0:
      break
    result.skills.add(y = SkillInfo(index: skill[0], level: skill[1],
        experience: skill[2]))
  for index, item in adaRecruit.equipment:
    result.equipment[index.EquipmentLocations] = item - 1
  for item in adaRecruit.inventory:
    if item == 0:
      break
    result.inventory.add(y = item)

proc adaRecruitFromNim(recruit: RecruitData): AdaRecruitData {.raises: [],
    tags: [], exportc, contractual.} =
  ## Temporary C binding
  result = AdaRecruitData()
  for attribute in result.attributes.mitems:
    attribute = [0.cint, 0.cint]
  for index, attribute in recruit.attributes:
    result.attributes[index + 1] = [attribute.level.cint,
        attribute.experience.cint]
  for skill in result.skills.mitems:
    skill = [0.cint, 0.cint, 0.cint]
  for index, skill in recruit.skills:
    result.skills[index + 1] = [skill.index.cint, skill.level.cint,
        skill.experience.cint]
  result.name = recruit.name.cstring
  result.gender = recruit.gender
  for item in result.equipment.mitems:
    item = 0
  for index, item in recruit.equipment:
    result.equipment[index.ord.cint] = item.cint
  for item in result.inventory.mitems:
    item = 0
  for index, item in recruit.inventory:
    result.inventory[index] = item.cint
  result.payment = recruit.payment.cint
  result.price = recruit.price.cint
  result.homeBase = recruit.homeBase
  result.faction = recruit.faction.cstring

proc getAdaRecruits(recruits: array[1..20, AdaRecruitData];
    baseIndex: cint) {.raises: [], tags: [], exportc, contractual.} =
  ## Temporary C binding
  skyBases[baseIndex].recruits = @[]
  for adaRecruit in recruits:
    if adaRecruit.name.len == 0:
      return
    skyBases[baseIndex].recruits.add(y = adaRecruitToNim(
        adaRecruit = adaRecruit))

proc setAdaRecruits(recruits: var array[1..20, AdaRecruitData];
    baseIndex: cint) {.raises: [], tags: [], exportc, contractual.} =
  ## Temporary C binding
  var index: Positive = 1
  for recruit in recruits.mitems:
    recruit.name = "".cstring
  for recruit in skyBases[baseIndex].recruits:
    recruits[index] = adaRecruitFromNim(recruit = recruit)
    index.inc

proc getAdaBaseType(baseIndex: cint; baseType: cstring) {.raises: [], tags: [],
    exportc, contractual.} =
  ## Temporary C binding
  skyBases[baseIndex].baseType = $baseType

proc setAdaBaseType(baseIndex: cint; baseType: var cstring) {.raises: [],
    tags: [], exportc, contractual.} =
  ## Temporary C binding
  baseType = skyBases[baseIndex].baseType.cstring

type
  AdaBaseCargo = object
    protoIndex: cint
    amount: cint
    durability: cint
    price: cint

proc getAdaBaseCargo(baseIndex: cint; cargo: array[128,
    AdaBaseCargo]) {.raises: [], tags: [], exportc, contractual.} =
  ## Temporary C binding
  var nimCargo: seq[BaseCargo] = @[]
  for adaItem in cargo:
    if adaItem.protoIndex == 0:
      break
    nimCargo.add(y = BaseCargo(protoIndex: adaItem.protoIndex,
        amount: adaItem.amount, durability: adaItem.durability,
        price: adaItem.price))
  if baseIndex > 0:
    skyBases[baseIndex].cargo = nimCargo
  else:
    traderCargo = nimCargo

proc setAdaBaseCargo(baseIndex: cint; cargo: var array[128,
    AdaBaseCargo]) {.raises: [], tags: [], exportc, contractual.} =
  ## Temporary C binding
  let nimCargo: seq[BaseCargo] = if baseIndex > 0:
      skyBases[baseIndex].cargo
    else:
      traderCargo
  for index in cargo.low..cargo.high:
    if index <= nimCargo.len - 1:
      cargo[index] = AdaBaseCargo(protoIndex: nimCargo[index].protoIndex.cint,
          amount: nimCargo[index].amount.cint, durability: nimCargo[
          index].durability.cint, price: nimCargo[index].price.cint)
    else:
      cargo[index] = AdaBaseCargo(protoIndex: 0)

proc updateAdaPrices() {.raises: [], tags: [], exportc, contractual.} =
  ## Temporary C binding
  updatePrices()

proc getAdaBaseDate(baseIndex, year, month, day, hour,
    minutes, dateType: cint) {.raises: [], tags: [], exportc, contractual.} =
  ## Temporary C binding
  let nimDate: DateRecord = DateRecord(year: year, month: month, day: day,
      hour: hour, minutes: minutes)
  case dateType
  of 0:
    skyBases[baseIndex].visited = nimDate
  of 1:
    skyBases[baseIndex].missionsDate = nimDate
  of 2:
    skyBases[baseIndex].recruitDate = nimDate
  of 3:
    skyBases[baseIndex].askedForEvents = nimDate
  else:
    discard

proc setAdaBaseDate(baseIndex, dateType: cint; year, month, day, hour,
    minutes: var cint) {.raises: [], tags: [], exportc, contractual.} =
  ## Temporary C binding
  case dateType
  of 0:
    year = skyBases[baseIndex].visited.year
    month = skyBases[baseIndex].visited.month
    day = skyBases[baseIndex].visited.day
    hour = skyBases[baseIndex].visited.hour
    minutes = skyBases[baseIndex].visited.minutes
  of 1:
    year = skyBases[baseIndex].missionsDate.year
    month = skyBases[baseIndex].missionsDate.month
    day = skyBases[baseIndex].missionsDate.day
    hour = skyBases[baseIndex].missionsDate.hour
    minutes = skyBases[baseIndex].missionsDate.minutes
  of 2:
    year = skyBases[baseIndex].recruitDate.year
    month = skyBases[baseIndex].recruitDate.month
    day = skyBases[baseIndex].recruitDate.day
    hour = skyBases[baseIndex].recruitDate.hour
    minutes = skyBases[baseIndex].recruitDate.minutes
  of 3:
    year = skyBases[baseIndex].askedForEvents.year
    month = skyBases[baseIndex].askedForEvents.month
    day = skyBases[baseIndex].askedForEvents.day
    hour = skyBases[baseIndex].askedForEvents.hour
    minutes = skyBases[baseIndex].askedForEvents.minutes
  else:
    discard

proc getAdaBaseName(baseIndex: cint; name: cstring) {.raises: [], tags: [],
    exportc, contractual.} =
  ## Temporary C binding
  skyBases[baseIndex].name = $name

proc setAdaBaseName(baseIndex: cint; name: var cstring) {.raises: [], tags: [],
    exportc, contractual.} =
  ## Temporary C binding
  name = skyBases[baseIndex].name.cstring

proc getAdaBaseKnown(baseIndex, known: cint) {.raises: [], tags: [], exportc,
    contractual.} =
  ## Temporary C binding
  skyBases[baseIndex].known = known == 1

proc setAdaBaseKnown(baseIndex: cint; known: var cint) {.raises: [], tags: [],
    exportc, contractual.} =
  ## Temporary C binding
  known = skyBases[baseIndex].known.cint

proc getAdaBaseAskedForBases(baseIndex, askedForBases: cint) {.raises: [],
    tags: [], exportc, contractual.} =
  ## Temporary C binding
  skyBases[baseIndex].askedForBases = askedForBases == 1

proc setAdaBaseAskedForBases(baseIndex: cint;
    askedForBases: var cint) {.raises: [], tags: [], exportc, contractual.} =
  ## Temporary C binding
  askedForBases = skyBases[baseIndex].askedForBases.cint

proc getAdaBaseSize(baseIndex, size: cint) {.raises: [], tags: [], exportc,
    contractual.} =
  ## Temporary C binding
  skyBases[baseIndex].size = size.BasesSize

proc setAdaBaseSize(baseIndex: cint; size: var cint) {.raises: [], tags: [],
    exportc, contractual.} =
  ## Temporary C binding
  size = skyBases[baseIndex].size.ord.cint

proc gainAdaRep(baseIndex, points: cint) {.raises: [], tags: [], exportc,
    contractual.} =
  ## Temporary C binding
  gainRep(baseIndex = baseIndex, points = points)
