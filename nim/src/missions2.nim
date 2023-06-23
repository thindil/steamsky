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
import game, game2, goals, maps, messages, missions, shipscrew, shipsmovement,
    statistics, types

type MissionFinishingError* = object of CatchableError
  ## Raised when there is a problem with finishing an accepted mission

proc finishMission*(missionIndex: Natural) {.sideEffect, raises: [
    MissionFinishingError, KeyError, IOError, Exception], tags: [WriteIOEffect,
    RootEffect].} =
  ## Finish the selected accepted mission
  ##
  ## * missionIndex - the index of the accepted mission to finish
  let missionsAmount = acceptedMissions.len
  if playerShip.speed != docked:
    let message = dockShip(docking = true)
    if message.len > 0:
      raise newException(MissionFinishingError, message)
  updateGame(minutes = 5)
  if missionsAmount > acceptedMissions.len:
    return
  case acceptedMissions[missionIndex].mType
  of deliver:
    addMessage(message = "You finished mission 'Deliver " & itemsList[
        acceptedMissions[missionIndex].itemIndex].name & "'.",
        mType = missionMessage, color = green)
  of destroy:
    addMessage(message = "You finished mission 'Destroy " & protoShipsList[
        acceptedMissions[missionIndex].shipIndex].name & ".'",
        mType = missionMessage, color = green)
  of patrol:
    addMessage(message = "You finished mission 'Patrol selected area'.",
        mType = missionMessage, color = green)
  of explore:
    addMessage(message = "You finished mission 'Explore selected area'.",
        mType = missionMessage, color = green)
  of passenger:
    addMessage(message = "You finished mission 'Transport passenger to base'.",
        mType = missionMessage, color = green)
  updateGoal(goalType = mission, targetIndex = $acceptedMissions[
      missionIndex].mType)
  updateFinishedMissions(mType = $acceptedMissions[missionIndex].mType)
  deleteMission(missionIndex = missionIndex, failed = false)

proc autoFinishMission*(): string =
  result = ""
  let baseIndex = skyMap[playerShip.skyX][playerShip.skyY].baseIndex
  if baseIndex == 0:
    return
  if skyMap[playerShip.skyX][playerShip.skyY].eventIndex > -1 and eventsList[
      skyMap[playerShip.skyX][playerShip.skyY].eventIndex].eType != doublePrice:
    return
  if findMember(order = talk) == -1:
    return
  var i = 0
  while i <= acceptedMissions.high:
    if (acceptedMissions[i].finished and acceptedMissions[i].startBase ==
        baseIndex) or (acceptedMissions[i].targetX == playerShip.skyX and
        acceptedMissions[i].targetY == playerShip.skyY):
      try:
        finishMission(missionIndex = i)
        i.dec
      except MissionFinishingError:
        return getCurrentExceptionMsg()
    i.inc

# Temporary code for interfacing with Ada

proc finishAdaMission(missionIndex: cint): cstring {.raises: [], tags: [
    WriteIOEffect, RootEffect], exportc.} =
  try:
    finishMission(missionIndex = (missionIndex - 1).Natural)
  except MissionFinishingError:
    return getCurrentExceptionMsg().cstring
  except KeyError, IOError, Exception:
    discard
  return ""

proc autoAdaFinishMissions(): cstring {.raises: [], tags: [WriteIOEffect,
    RootEffect], exportc.} =
  try:
    return autoFinishMission().cstring
  except KeyError, IOError, Exception:
    return ""
