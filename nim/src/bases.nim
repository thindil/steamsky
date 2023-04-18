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
import config, factions, game, goals, maps, shipscrew, types, utils

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

proc updatePopulation*() =
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
