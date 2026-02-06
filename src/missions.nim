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

## Provides code related to update the missions, like the list of missions
## accepted by the player, updating or deleting missions, etc.

import std/[math, tables]
import contracts
import bases, config, events, game, maps, messages, shipscrew, shipscargo,
    types, utils

var acceptedMissions*: seq[MissionData] = @[] ## The list of accepted missions by the player

proc deleteMission*(missionIndex: Natural; failed: bool = true) {.raises: [
    KeyError, ReputationError], tags: [], contractual.} =
  ## Delete the selected accepted mission, update the player's repuration in
  ## connected bases and update the sky map
  ##
  ## * missionIndex - the index of the mission to delete
  ## * failed       - if true, mission failed, default value is true
  require:
    missionIndex < acceptedMissions.len
  body:
    let mission: MissionData = acceptedMissions[missionIndex]
    var reputation: Natural = (mission.reward / 50).Natural
    if reputation < 2:
      reputation = 2
    reputation = (reputation.float + (reputation.float * (mission.multiplier - 1.0))).Natural
    if failed:
      gainRep(baseIndex = mission.startBase, points = -reputation)
      updateMorale(ship = playerShip, memberIndex = 0, value = getRandom(
          min = -10, max = -5))
      var messageText: string = "You failed your mission to "
      case mission.mType
      of deliver:
        messageText.add(y = "'Deliver " & itemsList[mission.itemIndex].name & "'.")
      of destroy:
        messageText.add(y = "'Destroy " & protoShipsList[
            mission.shipIndex].name & "'.")
      of patrol:
        messageText.add(y = "'Patrol selected area.'.")
      of explore:
        messageText.add(y = "'Explore selected area'.")
      of passenger:
        messageText.add(y = "'Transport passenger to base'.")
      addMessage(message = messageText, mType = missionMessage, color = red)
    else:
      if mission.mType in {deliver, passenger}:
        gainRep(baseIndex = skyMap[playerShip.skyX][playerShip.skyY].baseIndex,
            points = (reputation / 2).int)
        gainRep(baseIndex = mission.startBase, points = (reputation / 2).int)
      else:
        gainRep(baseIndex = mission.startBase, points = reputation)
      updateMorale(ship = playerShip, memberIndex = 0, value = 1)
      let traderIndex: int = findMember(order = talk)
      var rewardAmount: Natural = (mission.reward.float *
          mission.multiplier).Natural
      countPrice(price = rewardAmount, traderIndex = traderIndex,
          reduce = false)
      if traderIndex > -1:
        gainExp(amount = 1, skillNumber = talkingSkill, crewIndex = traderIndex)
      let freeSpace: int = freeCargo(amount = -(rewardAmount))
      if freeSpace < 0:
        rewardAmount += freeSpace
      if rewardAmount > 0:
        addMessage(message = "You received " & $rewardAmount & " " & moneyName &
            " for finishing your mission.", mType = missionMessage)
        updateCargo(ship = playerShip, protoIndex = moneyIndex,
            amount = rewardAmount, quality = normal)
    skyMap[mission.targetX][mission.targetY].missionIndex = -1
    skyMap[skyBases[mission.startBase].skyX][skyBases[
        mission.startBase].skyY].missionIndex = -1
    {.warning[UnsafeSetLen]: off.}
    acceptedMissions.delete(i = missionIndex)
    {.warning[UnsafeSetLen]: on.}
    if mission.mType == deliver:
      updateCargo(ship = playerShip, protoIndex = mission.itemIndex,
          amount = -1, quality = normal)
    elif mission.mType == passenger and mission.data < playerShip.crew.len:
      {.warning[UnsafeSetLen]: off.}
      playerShip.crew.delete(i = mission.data)
      {.warning[UnsafeSetLen]: on.}
      for module in playerShip.modules.mitems:
        for owner in module.owner.mitems:
          if owner == mission.data:
            owner = 0
          elif owner > mission.data:
            owner.dec
      for aMission in acceptedMissions.mitems:
        if aMission.mType == passenger and aMission.data > mission.data:
          aMission.data.dec
    for index, aMission in acceptedMissions:
      if aMission.finished:
        skyMap[skyBases[aMission.startBase].skyX][skyBases[
            aMission.startBase].skyY].missionIndex = index
      else:
        skyMap[aMission.targetX][aMission.targetY].missionIndex = index

proc generateMissions*() {.raises: [KeyError], tags: [],
    contractual.} =
  ## Generate available missions in the selected base if needed
  let
    baseIndex: ExtendedBasesRange = skyMap[playerShip.skyX][
      playerShip.skyY].baseIndex
    population: BasePopulation = getBasePopulation(baseIndex = baseIndex)
  if daysDifference(dateToCompare = skyBases[baseIndex].missionsDate) < 7:
    return
  var missionsAmount: Natural = case population
    of empty:
      return
    of small:
      getRandom(min = 1, max = 5)
    of medium:
      getRandom(min = 1, max = 10)
    of large:
      getRandom(min = 1, max = 15)
  missionsAmount = case skyBases[baseIndex].reputation.level
    of 1..25:
      missionsAmount + 1
    of 26..50:
      missionsAmount + 3
    of 51..75:
      missionsAmount + 5
    of 76..100:
      missionsAmount + 10
    else:
      missionsAmount
  var missionsItems: seq[Positive] = @[]
  for index, item in itemsList:
    if item.itemType == missionItemsType:
      missionsItems.add(y = index)
  var minX: int = playerShip.skyX - 100
  normalizeCoord(coord = minX)
  var maxX: int = playerShip.skyX + 100
  normalizeCoord(coord = maxX)
  var minY: int = playerShip.skyY - 100
  normalizeCoord(coord = minY, isXAxis = false)
  var maxY: int = playerShip.skyY + 100
  normalizeCoord(coord = maxY, isXAxis = false)
  var basesInRange: seq[Positive] = @[]
  for index, base in skyBases:
    if index != baseIndex and skyBases[index].skyX in minX..maxX and skyBases[
        index].skyY in minY..maxY and getBasePopulation(baseIndex = index) > empty:
      basesInRange.add(y = index)
  while missionsAmount > basesInRange.len:
    let tmpBaseIndex: Positive = getRandom(min = 1, max = 1024)
    if tmpBaseIndex notin basesInRange and getBasePopulation(
        baseIndex = tmpBaseIndex) > empty:
      basesInRange.add(y = tmpBaseIndex)
  skyBases[baseIndex].missions = @[]
  var enemies: seq[Positive] = @[]
  if getRandom(min = 1, max = 100) < 75:
    generateEnemies(enemies = enemies, withTraders = false)
  else:
    generateEnemies(enemies = enemies)
  var missionX, missionY: int = 1
  const qualitiesArray: array[10, int] = [1, 11, 21, 31, 41, 51, 61, 71, 81, 91]
  for i in 1..missionsAmount:
    var mission: MissionData = MissionData(time: 1, reward: 1, startBase: 1)
    let mType: MissionsTypes = getRandom(min = MissionsTypes.low.int,
        max = MissionsTypes.high.int).MissionsTypes
    case mType
    of deliver:
      mission = MissionData(mtype: deliver, time: 1, targetX: 0, targetY: 0,
          reward: 1, startBase: 1, finished: false, itemIndex: missionsItems[
          getRandom(min = 0, max = missionsItems.high)], multiplier: 1.0)
    of destroy:
      mission = MissionData(mtype: destroy, time: 1, targetX: 0, targetY: 0,
          reward: 1, startBase: 1, finished: false, shipIndex: enemies[
          getRandom(min = 0, max = enemies.high)], multiplier: 1.0)
      if mission.shipIndex == 0:
        continue
      while true:
        missionX = getRandom(min = minX, max = maxX)
        missionY = getRandom(min = minY, max = maxY)
        if skyMap[missionX][missionY].baseIndex == 0 and missionX !=
            playerShip.skyX and missionY != playerShip.skyY:
          break
    of patrol:
      mission = MissionData(mtype: patrol, time: 1, targetX: 0, targetY: 0,
          reward: 1, startBase: 1, finished: false, multiplier: 1.0, target: 1)
      for j in 1..10:
        missionX = getRandom(min = minX, max = maxX)
        missionY = getRandom(min = minY, max = maxY)
        if skyMap[missionX][missionY].visited and skyMap[missionX][
            missionY].baseIndex == 0:
          mission.target = 0
          break
      if mission.target == 1:
        continue
    of explore:
      mission = MissionData(mtype: explore, time: 1, targetX: 0, targetY: 0,
          reward: 1, startBase: 1, finished: false, multiplier: 1.0, target: 1)
      for j in 1..10:
        missionX = getRandom(min = minX, max = maxX)
        missionY = getRandom(min = minY, max = maxY)
        if not skyMap[missionX][missionY].visited and skyMap[missionX][
            missionY].baseIndex == 0:
          mission.target = 0
          break
      if mission.target == 1:
        continue
    of passenger:
      mission = MissionData(mtype: passenger, time: 1, targetX: 0, targetY: 0,
          reward: 1, startBase: 1, finished: false, multiplier: 1.0,
          data: qualitiesArray[getRandom(min = qualitiesArray.low,
              max = qualitiesArray.high)])
    if mission.mType in {deliver, passenger}:
      while true:
        let tmpBaseIndex: Natural = getRandom(min = basesInRange.low,
            max = basesInRange.high)
        missionX = skyBases[basesInRange[tmpBaseIndex]].skyX
        missionY = skyBases[basesInRange[tmpBaseIndex]].skyY
        if missionX != playerShip.skyX and missionY != playerShip.skyY:
          break
    mission.targetX = missionX
    mission.targetY = missionY
    let
      diffX: Natural = (playerShip.skyX - missionX).abs
      diffY: Natural = (playerShip.skyY - missionY).abs
    case mission.mType
    of deliver:
      mission.time = (90.0 * sqrt(x = ((diffX ^ 2) + (diffY ^
          2)).float)).Positive
      mission.reward = (mission.time / 4).Positive
    of destroy, passenger:
      mission.time = (180.0 * sqrt(x = ((diffX ^ 2) + (diffY ^
          2)).float)).Positive
      mission.reward = (mission.time / 4).Positive
    of patrol, explore:
      mission.time = (180.0 * sqrt(x = ((diffX ^ 2) + (diffY ^
          2)).float)).Positive
      mission.reward = (mission.time / 5).Positive
    mission.startBase = baseIndex
    mission.finished = false
    skyBases[baseIndex].missions.add(y = mission)
  skyBases[baseIndex].missionsDate = gameDate

proc updateMissions*(minutes: Positive) {.raises: [KeyError, ReputationError],
    tags: [], contractual.} =
  ## Update accepted missions timers and delete expired ones.
  ##
  ## * minutes - the amount of minutes passed in the game
  var i: Natural = acceptedMissions.low
  while i < acceptedMissions.len:
    let time: int = acceptedMissions[i].time - minutes
    if time < 1:
      deleteMission(missionIndex = i)
    else:
      acceptedMissions[i].time = time
      i.inc

func getMissionType*(mType: MissionsTypes): string {.raises: [], tags: [],
    contractual.} =
  ## Get the name of the type of a mission
  ##
  ## * mType - the type of missions which name will be get
  ##
  ## Returns string with the name of the selected missions' type.
  case mType
  of deliver:
    return "Deliver item to base"
  of patrol:
    return "Patrol area"
  of destroy:
    return "Destroy ship"
  of explore:
    return "Explore area"
  of passenger:
    return "Transport passenger to base"

proc updateMission*(missionIndex: Natural) {.raises: [KeyError],
    tags: [], contractual.} =
  ## Update the status of the selected mission
  ##
  ## * missionIndex - the index of the mission which will be updated
  require:
    missionIndex < acceptedMissions.len
  body:
    let mission: MissionData = acceptedMissions[missionIndex]
    skyMap[mission.targetX][mission.targetY].missionIndex = -1
    acceptedMissions[missionIndex].finished = true
    skyMap[skyBases[mission.startBase].skyX][skyBases[
        mission.startBase].skyY].missionIndex = missionIndex
    var messageText: string = "Return to " & skyBases[mission.startBase].name & " to finish mission "
    case mission.mType
    of deliver:
      messageText.add(y = "'Deliver " & itemsList[mission.itemIndex].name & "'.")
    of destroy:
      messageText.add(y = "'Destroy " & protoShipsList[mission.shipIndex].name & "'.")
    of patrol:
      messageText.add(y = "'Patrol selected area'.")
    of explore:
      messageText.add(y = "'Explore selected area'.")
    of passenger:
      messageText.add(y = "'Transport passenger to base'.")
    addMessage(message = messageText, mType = missionMessage)
    if gameSettings.autoReturn:
      playerShip.destinationX = skyBases[mission.startBase].skyX
      playerShip.destinationY = skyBases[mission.startBase].skyY
      addMessage(message = "You set the travel destination for your ship.",
          mType = orderMessage)
