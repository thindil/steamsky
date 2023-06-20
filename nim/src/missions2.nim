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

import game, game2, missions, shipsmovement, types

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
