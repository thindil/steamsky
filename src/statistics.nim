# Copyright 2023-2026 Bartek thindil Jasicki
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
  StatisticIndex* = string
    ## Used to store index of a statistic

  StatisticsData* = object
    ## Used to store detailed information about some the player's game's
    ## statistics
    ##
    ## * index  - The index of the prototype object
    ## * amount - The amount of the object
    index: StatisticIndex = ""
    amount: Positive = 1

  StatsSeq = seq[StatisticsData]
    ## Used to store data for various in-game statistics

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
    destroyedShips*: StatsSeq
    basesVisited*: BasesRange
    mapVisited*: Positive
    distanceTraveled*: Natural
    craftingOrders*: StatsSeq
    acceptedMissions*: Natural
    finishedMissions*: StatsSeq
    finishedGoals*: StatsSeq
    killedMobs*: StatsSeq
    points*: Natural

proc index*(statData: StatisticsData): StatisticIndex {.raises: [], tags: [],
    contractual.} =
  ## The getter of a field of StatisticsData type
  ##
  ## * statData - the StatisticData object which field will be get
  ##
  ## Returns the value of the selected field
  statData.index

proc amount*(statData: StatisticsData): Positive {.raises: [], tags: [],
    contractual.} =
  ## The getter of a field of StatisticsData type
  ##
  ## * statData - the StatisticData object which field will be get
  ##
  ## Returns the value of the selected field
  statData.amount

proc initStatisticsData*(index: StatisticIndex = "";
    amount: Positive = 1): StatisticsData {.raises: [], tags: [],
    contractual.} =
  ## Create a new data structure for the statistics' type data
  ##
  ## * index  - the index of the statistic
  ## * amount - the amount of the statistic, like destroyed ships, visited
  ##            bases, etc
  ##
  ## Returns the new structure with information about the selected statistic
  return StatisticsData(index: index, amount: amount)

proc destroyedShips*(gameStats: GameStatsData): StatsSeq {.raises: [
    ], tags: [], contractual.} =
  ## The getter of a field of GameStatsData type
  ##
  ## * gameStats - the GameStatsData object which field will be get
  ##
  ## Returns the value of the selected field
  gameStats.destroyedShips

proc basesVisited*(gameStats: GameStatsData): BasesRange {.raises: [], tags: [],
    contractual.} =
  ## The getter of a field of GameStatsData type
  ##
  ## * gameStats - the GameStatsData object which field will be get
  ##
  ## Returns the value of the selected field
  gameStats.basesVisited

proc `basesVisited=`*(gameStats: var GameStatsData, value: BasesRange) {.raises: [],
    tags: [], contractual.} =
  ## The setter of a field of GameStatsData type
  ##
  ## * skyCell - the GameStatsData object which field will be modified
  ## * value   - the new value for the field
  ##
  ## Returns the modified GameStatsData object
  gameStats.basesVisited = value

proc mapVisited*(gameStats: GameStatsData): Positive {.raises: [], tags: [],
    contractual.} =
  ## The getter of a field of GameStatsData type
  ##
  ## * gameStats - the GameStatsData object which field will be get
  ##
  ## Returns the value of the selected field
  gameStats.mapVisited

proc `mapVisited=`*(gameStats: var GameStatsData, value: Positive) {.raises: [],
    tags: [], contractual.} =
  ## The setter of a field of GameStatsData type
  ##
  ## * skyCell - the GameStatsData object which field will be modified
  ## * value   - the new value for the field
  ##
  ## Returns the modified GameStatsData object
  gameStats.mapVisited = value

proc distanceTraveled*(gameStats: GameStatsData): Natural {.raises: [], tags: [
    ], contractual.} =
  ## The getter of a field of GameStatsData type
  ##
  ## * gameStats - the GameStatsData object which field will be get
  ##
  ## Returns the value of the selected field
  gameStats.distanceTraveled

proc `distanceTraveled=`*(gameStats: var GameStatsData, value: Natural) {.raises: [],
    tags: [], contractual.} =
  ## The setter of a field of GameStatsData type
  ##
  ## * skyCell - the GameStatsData object which field will be modified
  ## * value   - the new value for the field
  ##
  ## Returns the modified GameStatsData object
  gameStats.distanceTraveled = value

proc craftingOrders*(gameStats: GameStatsData): StatsSeq {.raises: [
    ], tags: [], contractual.} =
  ## The getter of a field of GameStatsData type
  ##
  ## * gameStats - the GameStatsData object which field will be get
  ##
  ## Returns the value of the selected field
  gameStats.craftingOrders

proc acceptedMissions*(gameStats: GameStatsData): Natural {.raises: [], tags: [
    ], contractual.} =
  ## The getter of a field of GameStatsData type
  ##
  ## * gameStats - the GameStatsData object which field will be get
  ##
  ## Returns the value of the selected field
  gameStats.acceptedMissions

proc `acceptedMissions=`*(gameStats: var GameStatsData, value: Natural) {.raises: [],
    tags: [], contractual.} =
  ## The setter of a field of GameStatsData type
  ##
  ## * skyCell - the GameStatsData object which field will be modified
  ## * value   - the new value for the field
  ##
  ## Returns the modified GameStatsData object
  gameStats.acceptedMissions = value

proc finishedMissions*(gameStats: GameStatsData): StatsSeq {.raises: [
    ], tags: [], contractual.} =
  ## The getter of a field of GameStatsData type
  ##
  ## * gameStats - the GameStatsData object which field will be get
  ##
  ## Returns the value of the selected field
  gameStats.finishedMissions

proc finishedGoals*(gameStats: GameStatsData): StatsSeq {.raises: [
    ], tags: [], contractual.} =
  ## The getter of a field of GameStatsData type
  ##
  ## * gameStats - the GameStatsData object which field will be get
  ##
  ## Returns the value of the selected field
  gameStats.finishedGoals

proc killedMobs*(gameStats: GameStatsData): StatsSeq {.raises: [
    ], tags: [], contractual.} =
  ## The getter of a field of GameStatsData type
  ##
  ## * gameStats - the GameStatsData object which field will be get
  ##
  ## Returns the value of the selected field
  gameStats.killedMobs

proc points*(gameStats: GameStatsData): Natural {.raises: [], tags: [],
    contractual.} =
  ## The getter of a field of GameStatsData type
  ##
  ## * gameStats - the GameStatsData object which field will be get
  ##
  ## Returns the value of the selected field
  gameStats.points

proc `points=`*(gameStats: var GameStatsData, value: Natural) {.raises: [],
    tags: [], contractual.} =
  ## The setter of a field of GameStatsData type
  ##
  ## * skyCell - the GameStatsData object which field will be modified
  ## * value   - the new value for the field
  ##
  ## Returns the modified GameStatsData object
  gameStats.points = value

proc initGameStatsData*(destroyedShips: StatsSeq = @[];
    basesVisited: BasesRange = 1; mapVisited: Positive = 1;
    distanceTraveled: Natural = 0; craftingOrders: StatsSeq = @[];
    acceptedMissions: Natural = 0; finishedMissions: StatsSeq = @[];
    finishedGoals: StatsSeq = @[]; killedMobs: StatsSeq = @[];
    points: Natural = 0): GameStatsData {.raises: [], tags: [], contractual.} =
  ## Create a new data structure for the game statistics' type data
  ##
  ## * destroyedShips   - the list of destroyed ships
  ## * basesVisted      - the amount of visited bases
  ## * mapVisited       - the amount of map fields visited
  ## * distanceTraveled - the distance traveled via ship
  ## * craftingOrders   - the list of crafing orders finished
  ## * acceptedMissions - the amount of accepted missions
  ## * finishedMissions - the list of finished missions
  ## * finishedGoals    - the list of finished goals
  ## * killedMobs       - the list of killed mobs
  ## * points           - the amount of points gained during the game
  ##
  ## Returns the new structure with information about the game statistic
  return GameStatsData(destroyedShips: destroyedShips,
      basesVisited: basesVisited, mapVisited: mapVisited,
      distanceTraveled: distanceTraveled, craftingOrders: craftingOrders,
      acceptedMissions: acceptedMissions, finishedMissions: finishedMissions,
      finishedGoals: finishedGoals, killedMobs: killedMobs, points: points)

var gameStats*: GameStatsData = GameStatsData(basesVisited: 1, mapVisited: 1,
    distanceTraveled: 0, acceptedMissions: 0, points: 0) ## The player's game's statistics

proc updateCraftingOrders*(index: StatisticIndex) {.raises: [], tags: [],
    contractual.} =
  ## Update the list of finished crafting orders in the game statistics
  ##
  ## * index - the index of the crafting order to update
  require:
    index.len > 0
  body:
    var updated: bool = false
    for craftingOrder in gameStats.craftingOrders.mitems:
      if craftingOrder.index == index:
        craftingOrder.amount.inc
        updated = true
        break
    if not updated:
      gameStats.craftingOrders.add(y = initStatisticsData(index = index))
    gameStats.points += 5

proc updateFinishedGoals*(index: StatisticIndex) {.raises: [], tags: [],
    contractual.} =
  ## Update the list of finished goals in the game statistics
  ##
  ## * index - the index of the goal to update
  require:
    index.len > 0
  body:
    var updated: bool = false
    for goal in goalsList.values:
      if goal.index == index:
        gameStats.points += (goal.amount * goal.multiplier)
        break
    for goal in gameStats.finishedGoals.mitems:
      if goal.index == index:
        goal.amount.inc
        updated = true
        break
    if not updated:
      for goal in goalsList.values:
        if goal.index == index:
          gameStats.finishedGoals.add(y = initStatisticsData(
              index = goal.index))

proc getGamePoints*(): Natural {.raises: [], tags: [],
    contractual.} =
  ## Get the real amount of the player's game's points, multiplied or divided
  ## by the game's difficulty settings
  ##
  ## Returns the real amount of the player's game's points
  const malusIndexes: array[4, Positive] = [1, 3, 4, 5]
  let difficultyValues: array[7, BonusType] = [newGameSettings.enemyDamageBonus,
      newGameSettings.playerDamageBonus, newGameSettings.enemyMeleeDamageBonus,
      newGameSettings.playerMeleeDamageBonus, newGameSettings.experienceBonus,
      newGameSettings.reputationBonus, newGameSettings.upgradeCostBonus]
  var pointsBonus, value: float = 0.0
  for index, difficulty in difficultyValues:
    value = difficulty.float
    for malus in malusIndexes:
      if index == malus:
        if value < 1.0:
          value = 1.0 + ((1.0 - value) * 4.0)
        elif value > 1.0:
          value = 1.0 - value
        break
    pointsBonus += value
  pointsBonus /= difficultyValues.len.float
  if pointsBonus < 0.01:
    pointsBonus = 0.01
  return (gameStats.points.float * pointsBonus).Natural

proc updateFinishedMissions*(mType: StatisticIndex) {.raises: [], tags: [],
    contractual.} =
  ## Update the list of finished missions in the game statistics
  ##
  ## * mType - the type of missions which will be updated
  require:
    mType.len > 0
  body:
    var updated: bool = false
    for finishedMission in gameStats.finishedMissions.mitems:
      if finishedMission.index == mType:
        finishedMission.amount.inc
        updated = true
        break
    if not updated:
      gameStats.finishedMissions.add(y = initStatisticsData(index = mType))
    gameStats.points += 50

proc clearGameStats*() {.raises: [], tags: [], contractual.} =
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

proc updateKilledMobs*(mob: MemberData; factionName: StatisticIndex) {.raises: [
    ], tags: [], contractual.} =
  ## Update the list of killed mobs in the game statistics
  ##
  ## * mob         - the killed mobile data, needed to count the gained points
  ## * factionName - the name of the faction to which the mob belongs
  require:
    factionName.len > 0
  body:
    for attribute in mob.attributes:
      gameStats.points += attribute.level
    for skill in mob.skills:
      gameStats.points += skill.level
    var updated: bool = false
    for killedMob in gameStats.killedMobs.mitems:
      if killedMob.index.toLowerAscii == factionName.toLowerAscii:
        killedMob.amount.inc
        updated = true
        break
    if not updated:
      gameStats.killedMobs.add(y = StatisticsData(
          index: factionName.capitalizeAscii, amount: 1))

proc updateDestroyedShips*(shipName: string) {.raises: [], tags: [],
    contractual.} =
  ## Update the list of destroyed ships in the game statistics
  ##
  ## * shipName - the name of the prototype of the destroyed NPC ship
  require:
    shipName.len > 0
  body:
    var shipIndex: Natural = 0
    for index, ship in protoShipsList:
      if ship.name == shipName:
        shipIndex = index
        gameStats.points += (ship.combatValue / 10).Natural
    if shipIndex == 0:
      return
    var updated: bool = false
    for destroyedShip in gameStats.destroyedShips.mitems:
      if destroyedShip.index == $shipIndex:
        destroyedShip.amount.inc
        updated = true
        break
    if not updated:
      gameStats.destroyedShips.add(y = initStatisticsData(index = $shipIndex))
