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
import game, game2, goals, messages, missions, shipsmovement, statistics, types

type MissionFinishingError* = object of CatchableError
  ## Raised when there is a problem with finishing an accepted mission

proc finishMission*(missionIndex: Natural) =
  let missionsAmount = acceptedMissions.len
  if playerShip.speed == docked:
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

# Temporary code for interfacing with Ada

proc finishAdaMission(missionIndex: cint): cstring {.raises: [], tags: [
    WriteIOEffect, RootEffect], exportc.} =
  try:
    finishMission(missionIndex = (missionIndex - 1).Natural)
  except MissionFinishingError:
    return getCurrentExceptionMsg().cstring
  except KeyError, IOError, Exception:
    discard
