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

proc getAdaAcceptedMissions(adaMissions: array[10, AdaMissionData]) {.raises: [],
    tags: [], exportc.} =
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
    else:
      discard
