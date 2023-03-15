# Copyright 2023 Bartek thindil Jasicki
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
import bases, game, maps, messages, shipscrew, types, utils

var acceptedMissions*: seq[MissionData] ## The list of accepted missions by the player

proc deleteMission*(missionIndex: Natural; failed: bool = true) =
  let mission = acceptedMissions[missionIndex]
  var reputation: Natural = (mission.reward / 50).Natural
  if reputation < 2:
    reputation = 2
  reputation = (reputation.float + (reputation.float * (mission.multiplier - 1.0))).Natural
  if failed:
    gainRep(baseIndex = mission.startBase, points = -reputation)
    updateMorale(ship = playerShip, memberIndex = 0, value = getRandom(
        min = -10, max = -5))
    var messageText = "You failed your mission to "
    case mission.mType
    of deliver:
      messageText.add(y = "'Deliver " & itemsList[mission.itemIndex].name & "'.")
    of destroy:
      messageText.add(y = "'Destroy " & protoShipsList[mission.shipIndex].name & "'.")
    of patrol:
      messageText.add(y = "'Patrol selected area.'.")
    of explore:
      messageText.add(y = "'Explore selected area'.")
    of passenger:
      messageText.add(y = "'Transport passenger to base'.")
    addMessage(message = messageText, mType = missionMessage, color = red)
  else:
    if mission.mType in {deliver, passenger}:
      gainRep(baseIndex = skyMap[playerShip.skyX][playerShip.skyY].baseIndex, points = (reputation / 2).int)
      gainRep(baseIndex = mission.startBase, points = (reputation / 2).int)
    else:
      gainRep(baseIndex = mission.startBase, points = reputation)
    updateMorale(ship = playerShip, memberIndex = 0, value = 1)

# Temporary code for interfacing with Ada

type
  AdaMissionData = object
    time: cint
    targetX: cint
    targetY: cint
    reward: cint
    startBase: cint
    finished: cint
    multiplier: cfloat
    mType: cint
    data: cint

proc getAdaAcceptedMissions(adaMissions: array[50, AdaMissionData]) {.raises: [
    ], tags: [], exportc.} =
  acceptedMissions = @[]
  for mission in adaMissions.items:
    if mission.time == 0:
      break
    case mission.mType
    of 0:
      acceptedMissions.add(y = MissionData(mType: deliver, time: mission.time,
          targetX: mission.targetX, targetY: mission.targetY,
          reward: mission.reward, startBase: mission.startBase, finished: (
          if mission.finished == 1: true else: false),
          multiplier: mission.multiplier, itemIndex: mission.data))
    of 1:
      acceptedMissions.add(y = MissionData(mType: destroy, time: mission.time,
          targetX: mission.targetX, targetY: mission.targetY,
          reward: mission.reward, startBase: mission.startBase, finished: (
          if mission.finished == 1: true else: false),
          multiplier: mission.multiplier, shipIndex: mission.data))
    of 2:
      acceptedMissions.add(y = MissionData(mType: patrol, time: mission.time,
          targetX: mission.targetX, targetY: mission.targetY,
          reward: mission.reward, startBase: mission.startBase, finished: (
          if mission.finished == 1: true else: false),
          multiplier: mission.multiplier, target: mission.data))
    of 3:
      acceptedMissions.add(y = MissionData(mType: explore, time: mission.time,
          targetX: mission.targetX, targetY: mission.targetY,
          reward: mission.reward, startBase: mission.startBase, finished: (
          if mission.finished == 1: true else: false),
          multiplier: mission.multiplier, target: mission.data))
    of 4:
      acceptedMissions.add(y = MissionData(mType: passenger, time: mission.time,
          targetX: mission.targetX, targetY: mission.targetY,
          reward: mission.reward, startBase: mission.startBase, finished: (
          if mission.finished == 1: true else: false),
          multiplier: mission.multiplier, data: mission.data))
    else:
      discard

proc setAdaAcceptedMissions(adaMissions: var array[50,
    AdaMissionData]) {.raises: [], tags: [], exportc.} =
  for mission in adaMissions.mitems:
    mission = AdaMissionData(time: 0, targetX: 0, targetY: 0, reward: 0,
        startBase: 0, finished: 0, multiplier: 0.0, mtype: 0, data: 0)
  for index, mission in acceptedMissions.pairs:
    adaMissions[index].time = mission.time.cint
    adaMissions[index].targetX = mission.targetX
    adaMissions[index].targetY = mission.targetY
    adaMissions[index].reward = mission.reward.cint
    adaMissions[index].startBase = mission.startBase
    adaMissions[index].finished = (if mission.finished: 1 else: 0)
    adaMissions[index].multiplier = mission.multiplier
    adaMissions[index].mType = mission.mType.ord.cint
    adaMissions[index].data = case mission.mType
      of deliver:
        mission.itemIndex.cint
      of destroy:
        mission.shipIndex.cint
      of passenger:
        mission.data.cint
      else:
        mission.target.cint
