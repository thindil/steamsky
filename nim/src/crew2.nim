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

import crew, game, game2, types, shipsmovement

proc waitForRest*() {.sideEffect, raises: [KeyError, IOError, Exception],
    tags: [WriteIOEffect, RootEffect].} =
  ## Wait until the whole player's ship crew is rested
  var timeNeeded = 0
  for index, member in playerShip.crew.pairs:
    if member.tired > 0 and member.order == rest:
      var tempTimeNeeded = 0
      let cabinIndex = findCabin(memberIndex = index)
      if cabinIndex > -1:
        let damage = 1.0 - (playerShip.modules[cabinIndex].durability.float /
            playerShip.modules[cabinIndex].maxDurability.float)
        var cabinBonus = playerShip.modules[cabinIndex].cleanliness - (
            playerShip.modules[cabinIndex].cleanliness.float * damage).Natural
        if cabinBonus == 0:
          cabinBonus = 1
        tempTimeNeeded = (member.tired / cabinBonus).Natural * 15
        if tempTimeNeeded == 0:
          tempTimeNeeded = 15
      else:
        tempTimeNeeded = member.tired * 15
      tempTimeNeeded = tempTimeNeeded + 15
      if tempTimeNeeded > timeNeeded:
        timeNeeded = tempTimeNeeded
  if timeNeeded > 0:
    updateGame(minutes = timeNeeded)
    waitInPlace(minutes = timeNeeded)

# Temporary code for interfacing with Ada

proc waitAdaForRest() {.raises: [], tags: [WriteIOEffect, RootEffect], exportc.} =
  try:
    waitForRest()
  except KeyError, IOError, Exception:
    discard
