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

import types

var acceptedMissions*: seq[MissionData] ## The list of accepted missions by the player

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
