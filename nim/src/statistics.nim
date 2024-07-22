# Copyright 2023-2024 Bartek thindil Jasicki
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

## Provides code related to the game's player's statistics, like their data
## structure, updating or resetting.

import std/[strutils, tables]
import contracts
import config, game, types

type
  StatisticsData* = object
    ## Used to store detailed information about some the player's game's
    ## statistics
    ##
    ## * index  - The index of the prototype object
    ## * amount - The amount of the object
    index*: string = ""
    amount*: Positive = 1

  GameStatsData = object
    ## Used to store information about the player's game's statistics
    ##
    ## * destroyedShips   - The list of destroyed ships
    ## * basesVisited     - The amount of visited bases
    ## * mapVisited       - The amount of visited map fields
    ## * distanceTraveled - The length of the traveled distance
    ## * craftingOrders   - The list of finished crafting orders
    ## * acceptedMissions - The amount of accepted missions
    ## * finishedMissions - The list of finished missions
    ## * finishedGoals    - The list of finished goals
    ## * killedMobs       - The list of enemies killed
    ## * points           - The amount of points gained in the game's session
    destroyedShips*: seq[StatisticsData]
    basesVisited*: BasesRange
    mapVisited*: Positive
    distanceTraveled*: Natural
    craftingOrders*: seq[StatisticsData]
    acceptedMissions*: Natural
    finishedMissions*: seq[StatisticsData]
    finishedGoals*: seq[StatisticsData]
    killedMobs*: seq[StatisticsData]
    points*: Natural

var gameStats*: GameStatsData = GameStatsData(basesVisited: 1, mapVisited: 1,
    distanceTraveled: 0, acceptedMissions: 0, points: 0) ## The player's game's statistics

proc updateCraftingOrders*(index: string) {.sideEffect, raises: [], tags: [],
    contractual.} =
  ## Update the list of finished crafting orders in the game statistics
  ##
  ## * index - the index of the crafting order to update
  require:
    index.len > 0
  body:
    var updated = false
    for craftingOrder in gameStats.craftingOrders.mitems:
      if craftingOrder.index == index:
        craftingOrder.amount.inc
        updated = true
        break
    if not updated:
      gameStats.craftingOrders.add(y = StatisticsData(index: index, amount: 1))
    gameStats.points = gameStats.points + 5

proc updateFinishedGoals*(index: string) {.sideEffect, raises: [], tags: [],
    contractual.} =
  ## Update the list of finished goals in the game statistics
  ##
  ## * index - the index of the goal to update
  require:
    index.len > 0
  body:
    var updated = false
    for goal in goalsList.values:
      if goal.index == index:
        gameStats.points = gameStats.points + (goal.amount * goal.multiplier)
        break
    for goal in gameStats.finishedGoals.mitems:
      if goal.index == index:
        goal.amount.inc
        updated = true
        break
    if not updated:
      for goal in goalsList.values:
        if goal.index == index:
          gameStats.finishedGoals.add(y = StatisticsData(index: goal.index, amount: 1))

proc getGamePoints*(): Natural {.sideEffect, raises: [], tags: [],
    contractual.} =
  ## Get the real amount of the player's game's points, multiplied or divided
  ## by the game's difficulty settings
  ##
  ## Returns the real amount of the player's game's points
  const malusIndexes = [1, 3, 4, 5]
  let difficultyValues = [newGameSettings.enemyDamageBonus,
      newGameSettings.playerDamageBonus, newGameSettings.enemyMeleeDamageBonus,
      newGameSettings.playerMeleeDamageBonus, newGameSettings.experienceBonus,
      newGameSettings.reputationBonus, newGameSettings.upgradeCostBonus]
  var pointsBonus, value = 0.0
  for index, difficulty in difficultyValues.pairs:
    value = difficulty.float
    for malus in malusIndexes:
      if index == malus:
        if value < 1.0:
          value = 1.0 + ((1.0 - value) * 4.0)
        elif value > 1.0:
          value = 1.0 - value
        break
    pointsBonus = pointsBonus + value
  pointsBonus = pointsBonus / difficultyValues.len.float
  if pointsBonus < 0.01:
    pointsBonus = 0.01
  return (gameStats.points.float * pointsBonus).Natural

proc updateFinishedMissions*(mType: string) {.sideEffect, raises: [], tags: [],
    contractual.} =
  ## Update the list of finished missions in the game statistics
  ##
  ## * mType - the type of missions which will be updated
  require:
    mType.len > 0
  body:
    var updated = false
    for finishedMission in gameStats.finishedMissions.mitems:
      if finishedMission.index == mType:
        finishedMission.amount.inc
        updated = true
        break
    if not updated:
      gameStats.finishedMissions.add(y = StatisticsData(index: mType, amount: 1))
    gameStats.points = gameStats.points + 50

proc clearGameStats*() {.sideEffect, raises: [], tags: [], contractual.} =
  ## Reset the game statistics
  ensure:
    gameStats.points == 0
  body:
    gameStats.destroyedShips = @[]
    gameStats.basesVisited = 1
    gameStats.mapVisited = 1
    gameStats.distanceTraveled = 0
    gameStats.craftingOrders = @[]
    gameStats.acceptedMissions = 0
    gameStats.finishedMissions = @[]
    gameStats.finishedGoals = @[]
    gameStats.killedMobs = @[]
    gameStats.points = 0

proc updateKilledMobs*(mob: MemberData; factionName: string) {.sideEffect,
    raises: [], tags: [], contractual.} =
  ## Update the list of killed mobs in the game statistics
  ##
  ## * mob         - the killed mobile data, needed to count the gained points
  ## * factionName - the name of the faction to which the mob belongs
  require:
    factionName.len > 0
  body:
    for attribute in mob.attributes:
      gameStats.points = gameStats.points + attribute.level
    for skill in mob.skills:
      gameStats.points = gameStats.points + skill.level
    var updated = false
    for killedMob in gameStats.killedMobs.mitems:
      if killedMob.index.toLowerAscii == factionName.tolowerAscii:
        killedMob.amount.inc
        updated = true
        break
    if not updated:
      gameStats.killedMobs.add(y = StatisticsData(
          index: factionName.capitalizeAscii, amount: 1))

proc updateDestroyedShips*(shipName: string) {.sideEffect, raises: [], tags: [],
    contractual.} =
  ## Update the list of destroyed ships in the game statistics
  ##
  ## * shipName - the name of the prototype of the destroyed NPC ship
  require:
    shipName.len > 0
  body:
    var shipIndex = 0
    for index, ship in protoShipsList:
      if ship.name == shipName:
        shipIndex = index
        gameStats.points = gameStats.points + (ship.combatValue / 10).Natural
    if shipIndex == 0:
      return
    var updated = false
    for destroyedShip in gameStats.destroyedShips.mitems:
      if destroyedShip.index == $shipIndex:
        destroyedShip.amount.inc
        updated = true
        break
    if not updated:
      gameStats.destroyedShips.add(y = StatisticsData(index: $shipIndex, amount: 1))

# Temporary code for interfacing with Ada

type
  AdaGameStats = object
    basesVisited: cint
    mapVisited: cint
    distanceTraveled: cint
    acceptedMissions: cint
    points: cint

  AdaStatisticsData = object
    index: cstring
    amount: cint

proc updateAdaCraftingOrders(index: cstring) {.raises: [], tags: [], exportc,
    contractual.} =
  ## Temporary C binding
  updateCraftingOrders(index = $index)

proc getAdaGameStats(value, stat: cint) {.raises: [], tags: [], exportc,
    contractual.} =
  ## Temporary C binding
  case stat
  of 0:
    gameStats.distanceTraveled = gameStats.distanceTraveled + value
  of 1:
    gameStats.points = gameStats.points + value
  else:
    discard

proc getAdaGameStatsList(name: cstring; statsList: array[512,
    AdaStatisticsData]) {.raises: [], tags: [], exportc, contractual.} =
  ## Temporary C binding
  var list = case $name
    of "destroyedShips":
      gameStats.destroyedShips
    of "craftingOrders":
      gameStats.craftingOrders
    of "finishedMissions":
      gameStats.finishedMissions
    of "finishedGoals":
      gameStats.finishedGoals
    else:
      gameStats.killedMobs
  list = @[]
  for stat in statsList:
    if stat.index.len == 0:
      break
    list.add(y = StatisticsData(index: $stat.index,
        amount: stat.amount.Positive))
  case $name
  of "destroyedShips":
    gameStats.destroyedShips = list
  of "craftingOrders":
    gameStats.craftingOrders = list
  of "finishedMissions":
    gameStats.finishedMissions = list
  of "finishedGoals":
    gameStats.finishedGoals = list
  else:
    gameStats.killedMobs = list

proc setAdaGameStats(stats: var AdaGameStats) {.raises: [], tags: [], exportc,
    contractual.} =
  ## Temporary C binding
  stats.basesVisited = gameStats.basesVisited.cint
  stats.mapVisited = gameStats.mapVisited.cint
  stats.distanceTraveled = gameStats.distanceTraveled.cint
  stats.acceptedMissions = gameStats.acceptedMissions.cint
  stats.points = gameStats.points.cint

proc setAdaGameStatsNumber(name: cstring; statsValue: var cint) {.raises: [],
    tags: [], exportc, contractual.} =
  ## Temporary C binding
  case $name
  of "basesVisited":
    statsValue = gameStats.basesVisited.cint
  of "mapVisited":
    statsValue = gameStats.mapVisited.cint
  of "distanceTraveled":
    statsValue = gameStats.distanceTraveled.cint
  of "acceptedMissions":
    statsValue = gameStats.acceptedMissions.cint

proc setAdaGameStatsList(name: cstring; statsList: var array[512,
    AdaStatisticsData]) {.raises: [], tags: [], exportc, contractual.} =
  ## Temporary C binding
  var list = case $name
    of "destroyedShips":
      gameStats.destroyedShips
    of "craftingOrders":
      gameStats.craftingOrders
    of "finishedMissions":
      gameStats.finishedMissions
    of "finishedGoals":
      gameStats.finishedGoals
    else:
      gameStats.killedMobs
  for i in 0..statsList.high:
    statsList[i] = AdaStatisticsData(index: "".cstring, amount: 1)
  for index, stat in list.pairs:
    statsList[index] = AdaStatisticsData(index: stat.index.cstring,
        amount: stat.amount.cint)

proc updateAdaFinishedGoals(index: cstring) {.raises: [], tags: [], exportc,
    contractual.} =
  ## Temporary C binding
  updateFinishedGoals(index = $index)

proc getAdaGamePoints(): cint {.raises: [], tags: [], exportc, contractual.} =
  ## Temporary C binding
  return getGamePoints().cint

proc updateAdaFinishedMissions(mType: cstring) {.raises: [], tags: [], exportc,
    contractual.} =
  ## Temporary C binding
  updateFinishedMissions(mType = $mType)

proc clearAdaGameStats() {.raises: [], tags: [], exportc, contractual.} =
  ## Temporary C binding
  clearGameStats()

proc updateAdaKilledMobs(mob: AdaMemberData; factionName: cstring) {.raises: [],
    tags: [], exportc, contractual.} =
  ## Temporary C binding
  updateKilledMobs(mob = adaMemberToNim(adaMember = mob),
      factionName = $factionName)

proc updateAdaDestroyedShips(shipName: cstring) {.raises: [], tags: [], exportc,
    contractual.} =
  ## Temporary C binding
  updateDestroyedShips(shipName = $shipName)
