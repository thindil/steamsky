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
import basestypes, config, factions, game, goals, items, maps, shipscrew, types, utils

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
      return generateRoboticName()
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

proc generateRecruits*() {.sideEffect, raises: [KeyError], tags: [].} =
  ## Generate available recruits in the base if needed
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
  if maxSkillAmount < 5:
    maxSkillAmount = 5
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
    for j in 1 .. attributesList.len:
      attributes.add(y = MobAttributeRecord(level: getRandom(min = 3, max = (
          maxSkillLevel / 3).int), experience: 0))
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
        addInventory(itemsIndexes = tempToolsList, equipIndex = tool)
        break
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
  skyBases[baseIndex].recruitDate = gameDate
  skyBases[baseIndex].recruits = baseRecruits

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

proc updatePrices*() {.sideEffect, raises: [], tags: [].} =
  ## Random changes to the items' prices in the selected base
  let baseIndex = skyMap[playerShip.skyX][playerShip.skyY].baseIndex
  if skyBases[baseIndex].population == 0:
    return
  var chance = (if skyBases[baseIndex].population < 150: 1 elif skyBases[
      baseIndex].population < 300: 2 else: 5)
  chance = chance + (daysDifference(dateToCompare = skyBases[baseIndex].visited,
      currentDate = gameDate) / 10).int
  if getRandom(min = 1, max = 100) > chance:
    return
  for item in skyBases[baseIndex].cargo.mitems:
    let roll = getRandom(min = 1, max = 100)
    if roll < 30 and item.price > 1:
      item.price.dec
    elif roll < 60 and item.price > 0:
      item.price.inc

import std/math
import events, game2, messages, ships2

proc askForEvents*() {.sideEffect, raises: [KeyError, Exception], tags: [
    WriteIOEffect, RootEffect].} =
  ## Ask for known events in a base or a friendly ship. Generates new
  ## events
  let traderIndex = findMember(order = talk)
  if traderIndex == -1:
    return
  let baseIndex = skyMap[playerShip.skyX][playerShip.skyY].baseIndex
  var maxEvents: Natural
  # Asking in base
  if baseIndex > 0:
    maxEvents = (if skyBases[baseIndex].population < 150: 5 elif skyBases[
        baseIndex].population < 300: 10 else: 15)
    skyBases[baseIndex].askedForEvents = gameDate
    addMessage(message = playerShip.crew[traderIndex].name &
        " asked for recent events known at base '" & skyBases[baseIndex].name &
        "'.", mType = orderMessage)
    gainRep(baseIndex = baseIndex, points = 1)
  else:
    let shipIndex = eventsList[skyMap[playerShip.skyX][
        playerShip.skyY].eventIndex].shipIndex
    maxEvents = (if protoShipsList[shipIndex].crew.len <
        5: 1 elif protoShipsList[shipIndex].crew.len < 10: 3 else: 5)
    addMessage(message = playerShip.crew[traderIndex].name &
        " asked ship '" & generateShipName(factionIndex = protoShipsList[
            shipIndex].owner) & "' for recent events.", mType = orderMessage)
    deleteEvent(eventIndex = skyMap[playerShip.skyX][
        playerShip.skyY].eventIndex)
    updateOrders(ship = playerShip)
  let eventsAmount = getRandom(min = 1, max = maxEvents)
  var minX: int = playerShip.skyX - 100
  normalizeCoord(coord = minX)
  var maxX: int = playerShip.skyX + 100
  normalizeCoord(coord = maxX)
  var minY: int = playerShip.skyY - 100
  normalizeCoord(coord = minY, isXAxis = false)
  var maxY: int = playerShip.skyY + 100
  normalizeCoord(coord = maxY, isXAxis = false)
  var enemies: seq[Positive]
  generateEnemies(enemies)
  for i in 1 .. eventsAmount:
    var
      event = getRandom(min = 1, max = 5).EventsTypes
      attempts = 10
      eventX, eventY = 0
    while true:
      if event == enemyShip:
        eventX = getRandom(min = minX, max = maxX)
        eventY = getRandom(min = minY, max = maxY)
        if skyMap[eventX][eventY].baseIndex == 0 and eventX !=
            playerShip.skyX and eventY != playerShip.skyY and skyMap[eventX][
            eventY].eventIndex == -1:
          break
      else:
        let tmpBaseIndex = getRandom(min = 1, max = 1024)
        eventX = skyBases[tmpBaseIndex].skyX
        eventY = skyBases[tmpBaseIndex].skyY
        attempts.dec
        if attempts == 0:
          event = enemyShip
          while true:
            eventX = getRandom(min = minX, max = maxX)
            eventY = getRandom(min = minY, max = maxY)
            if skyMap[eventX][eventY].baseIndex == 0 and eventX !=
                playerShip.skyX and eventY != playerShip.skyY and skyMap[
                    eventX][
                eventY].eventIndex == -1:
              break
          break
        if eventX != playerShip.skyX and eventY != playerShip.skyY and
          skyMap[eventX][eventY].eventIndex == -1 and
          skyBases[skyMap[eventX][eventY].baseIndex].known:
          if event == attackOnBase and skyBases[skyMap[eventX][
              eventY].baseIndex].population > 0:
            break
          if event == doublePrice and isFriendly(
              sourceFaction = playerShip.crew[0].faction,
              targetFaction = skyBases[skyMap[eventX][eventY].baseIndex].owner):
            break
          if event == disease and "diseaseimmnune" in factionsList[skyBases[
              skyMap[eventX][eventY].baseIndex].owner].flags and isFriendly(
              sourceFaction = playerShip.crew[0].faction,
              targetFaction = skyBases[skyMap[eventX][eventY].baseIndex].owner):
            break
          if event == baseRecovery and skyBases[skyMap[eventX][
              eventY].baseIndex].population == 0:
            break
    let
      diffX = abs(playerShip.skyX - eventX)
      diffY = abs(playerShip.skyY - eventY)
      eventTime = (60 * sqrt((diffX ^ 2).float + (diffY ^ 2).float)).Natural
    case event
    of enemyShip:
      eventsList.add(EventData(eType: enemyShip, skyX: eventX,
          skyY: eventY, time: getRandom(min = eventTime, max = eventTime + 60),
          shipIndex: enemies[getRandom(min = 0, max = enemies.len - 1)]))
    of attackOnBase:
      generateEnemies(enemies = enemies, owner = "Any", withTraders = false)
      eventsList.add(EventData(eType: attackOnBase, skyX: eventX,
          skyY: eventY, time: getRandom(min = eventTime, max = eventTime + 120),
          shipIndex: enemies[getRandom(min = 0, max = enemies.len - 1)]))
      generateEnemies(enemies = enemies)
    of disease:
      eventsList.add(EventData(eType: disease, skyX: eventX,
          skyY: eventY, time: getRandom(min = 10_000, max = 12_000), data: 1))
    of doublePrice:
      var newItemIndex = 0
      block setDoublePrice:
        while true:
          var itemIndex = getRandom(min = 1, max = itemsList.len)
          for j in 1 .. itemsList.len:
            itemIndex.dec
            if itemIndex <= 0 and getPrice(baseType = skyBases[skyMap[eventX][
                eventY].baseIndex].baseType, itemIndex = j) > 0:
              newItemIndex = j
              break setDoublePrice
      eventsList.add(EventData(eType: doublePrice, skyX: eventX,
          skyY: eventY, time: getRandom(min = eventTime * 3, max = eventTime *
              4), itemIndex: newItemIndex))
    of baseRecovery:
      recoverBase(baseIndex = skyMap[eventX][eventY].baseIndex)
    else:
      discard
    if event != baseRecovery:
      skyMap[eventX][eventY].eventIndex = eventsList.len - 1
  gainExp(amount = 1, skillNumber = talkingSkill, crewIndex = traderIndex)
  updateGame(minutes = 30)

proc askForBases*() {.sideEffect, raises: [KeyError, Exception], tags: [
    WriteIOEffect, RootEffect].} =
  ## Ask for known bases in a base or a friendly ship.
  let traderIndex = findMember(order = talk)
  if traderIndex == -1:
    return
  let
    baseIndex = skyMap[playerShip.skyX][playerShip.skyY].baseIndex
    shipIndex = (if baseIndex == 0: eventsList[skyMap[playerShip.skyX][
        playerShip.skyY].eventIndex].shipIndex else: 0)
  var amount, radius: Natural
  # Asking in base
  if baseIndex > 0:
    if skyBases[baseIndex].population < 150:
      amount = 10
      radius = 10
    elif skyBases[baseIndex].population < 300:
      amount = 20
      radius = 20
    else:
      amount = 40
      radius = 40
    skyBases[baseIndex].askedForBases = true
    gainRep(baseIndex = baseIndex, points = 1)
    addMessage(message = playerShip.crew[traderIndex].name &
        " asked for directions to other bases in base '" & skyBases[
            baseIndex].name &
        "'.", mType = orderMessage)
  # Asking friendly ship
  else:
    radius = 40
    amount = (if protoShipsList[shipIndex].crew.len <
        5: 3 elif protoShipsList[shipIndex].crew.len < 10: 5 else: 10)
    addMessage(message = playerShip.crew[traderIndex].name &
        " asked ship '" & generateShipName(factionIndex = protoShipsList[
            shipIndex].owner) & "' for directions to other bases.",
            mType = orderMessage)
    deleteEvent(eventIndex = skyMap[playerShip.skyX][
        playerShip.skyY].eventIndex)
    updateOrders(ship = playerShip)
  block findBases:
    for x in -radius .. radius:
      for y in -radius .. radius:
        var
          tempX: int = playerShip.skyX + x
          tempY: int = playerShip.skyY + y
        normalizeCoord(coord = tempX)
        normalizeCoord(coord = tempY, isXAxis = false)
        let tmpBaseIndex = skyMap[tempX][tempY].baseIndex
        if tmpBaseIndex > 0 and not skyBases[tmpBaseIndex].known:
          skyBases[tmpBaseIndex].known = true
          amount.dec
          if amount == 0:
            break findBases
  if amount > 0:
    if baseIndex > 0:
      if skyBases[baseIndex].population < 150 and amount > 1:
        amount = 1
      elif skyBases[baseIndex].population < 300 and amount > 2:
        amount = 2
      elif amount > 4:
        amount = 4
    else:
      amount = (if protoShipsList[shipIndex].crew.len <
          5: 1 elif protoShipsList[shipIndex].crew.len < 10: 2 else: 4)
    var unknownBases = 0
    for base in skyBases:
      if not base.known:
        unknownBases.inc
      if unknownBases >= amount:
        break
    if unknownBases >= amount:
      while true:
        let tmpBaseIndex = getRandom(min = 1, max = 1_024)
        if not skyBases[tmpBaseIndex].known:
          skyBases[tmpBaseIndex].known = true
          amount.dec
        if amount == 0:
          break
    else:
      for base in skyBases.mitems:
        if not base.known:
          base.known = true
  gainExp(amount = 1, skillNumber = talkingSkill, crewIndex = traderIndex)
  updateGame(minutes = 30)

# Temporary code for interfacing with Ada

proc generateAdaBaseName(factionIndex: cstring): cstring {.exportc, raises: [],
    tags: [].} =
  return generateBaseName(factionIndex = $factionIndex).cstring

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

proc setAdaBaseLocation(baseIndex: cint; x, y: var cint) {.raises: [], tags: [], exportc.} =
  x = skyBases[baseIndex].skyX
  y = skyBases[baseIndex].skyY

proc getAdaBaseOwner(baseIndex: cint; owner: cstring) {.raises: [], tags: [], exportc.} =
  skyBases[baseIndex].owner = $owner

proc setAdaBaseOwner(baseIndex: cint; owner: var cstring) {.raises: [], tags: [], exportc.} =
  owner = skyBases[baseIndex].owner.cstring

proc getAdaBasePopulation(baseIndex, population: cint) {.raises: [], tags: [], exportc.} =
  skyBases[baseIndex].population = population

proc setAdaBasePopulation(baseIndex: cint; population: var cint) {.raises: [],
    tags: [], exportc.} =
  population = skyBases[baseIndex].population.cint

proc updateAdaPopulation() {.raises: [], tags: [], exportc.} =
  updatePopulation()

proc generateAdaRecruits() {.raises: [], tags: [], exportc.} =
  try:
    generateRecruits()
  except KeyError:
    discard

type
  AdaRecruitData* = object
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
    tags: [], exportc.} =
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
  for index, item in adaRecruit.equipment.pairs:
    result.equipment[index.EquipmentLocations] = item - 1
  for item in adaRecruit.inventory.items:
    if item == 0:
      break
    result.inventory.add(y = item)

proc adaRecruitFromNim(recruit: RecruitData): AdaRecruitData {.raises: [],
    tags: [], exportc.} =
  result = AdaRecruitData()
  for attribute in result.attributes.mitems:
    attribute = [0.cint, 0.cint]
  for index, attribute in recruit.attributes.pairs:
    result.attributes[index + 1] = [attribute.level.cint,
        attribute.experience.cint]
  for skill in result.skills.mitems:
    skill = [0.cint, 0.cint, 0.cint]
  for index, skill in recruit.skills.pairs:
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
    baseIndex: cint) {.raises: [], tags: [], exportc.} =
  skyBases[baseIndex].recruits = @[]
  for adaRecruit in recruits:
    if adaRecruit.name.len == 0:
      return
    skyBases[baseIndex].recruits.add(y = adaRecruitToNim(
        adaRecruit = adaRecruit))

proc setAdaRecruits(recruits: var array[1..20, AdaRecruitData];
    baseIndex: cint) {.raises: [], tags: [], exportc.} =
  var index = 1
  for recruit in skyBases[baseIndex].recruits:
    recruits[index] = adaRecruitFromNim(recruit = recruit)
    index.inc
  recruits[index].name = "".cstring

proc getAdaBaseType(baseIndex: cint; baseType: cstring) {.raises: [], tags: [], exportc.} =
  skyBases[baseIndex].baseType = $baseType

proc setAdaBaseType(baseIndex: cint; baseType: var cstring) {.raises: [],
    tags: [], exportc.} =
  baseType = skyBases[baseIndex].baseType.cstring

type
  AdaBaseCargo = object
    protoIndex: cint
    amount: cint
    durability: cint
    price: cint

proc getAdaBaseCargo(baseIndex: cint; cargo: array[128,
    AdaBaseCargo]) {.raises: [], tags: [], exportc.} =
  var nimCargo: seq[BaseCargo]
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
    AdaBaseCargo]) {.raises: [], tags: [], exportc.} =
  let nimCargo = if baseIndex > 0:
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

proc updateAdaPrices() {.raises: [], tags: [], exportc.} =
  updatePrices()

proc getAdaBaseDate(baseIndex, year, month, day, hour,
    minutes, dateType: cint) {.raises: [], tags: [], exportc.} =
  let nimDate = DateRecord(year: year, month: month, day: day, hour: hour,
      minutes: minutes)
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
    minutes: var cint) {.raises: [], tags: [], exportc.} =
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

proc getAdaBaseName(baseIndex: cint; name: cstring) {.raises: [], tags: [], exportc.} =
  skyBases[baseIndex].name = $name

proc setAdaBaseName(baseIndex: cint; name: var cstring) {.raises: [], tags: [], exportc.} =
  name = skyBases[baseIndex].name.cstring

proc getAdaBaseKnown(baseIndex, known: cint) {.raises: [], tags: [], exportc.} =
  skyBases[baseIndex].known = known == 1

proc setAdaBaseKnown(baseIndex: cint; known: var cint) {.raises: [], tags: [], exportc.} =
  known = skyBases[baseIndex].known.cint

proc getAdaBaseAskedForBases(baseIndex, askedForBases: cint) {.raises: [],
    tags: [], exportc.} =
  skyBases[baseIndex].askedForBases = askedForBases == 1

proc setAdaBaseAskedForBases(baseIndex: cint;
    askedForBases: var cint) {.raises: [], tags: [], exportc.} =
  askedForBases = skyBases[baseIndex].askedForBases.cint

proc getAdaBaseSize(baseIndex, size: cint) {.raises: [], tags: [], exportc.} =
  skyBases[baseIndex].size = size.BasesSize

proc setAdaBaseSize(baseIndex: cint; size: var cint) {.raises: [], tags: [], exportc.} =
  size = skyBases[baseIndex].size.ord.cint

proc gainAdaRep(baseIndex, points: cint) {.raises: [], tags: [], exportc.} =
  gainRep(baseIndex = baseIndex, points = points)

proc askAdaForEvents() {.raises: [], tags: [WriteIOEffect, RootEffect], exportc.} =
  try:
    askForEvents()
  except KeyError, IOError, Exception:
    discard

proc askAdaForBases() {.raises: [], tags: [WriteIOEffect, RootEffect], exportc.} =
  try:
    askForBases()
  except KeyError, IOError, Exception:
    discard
