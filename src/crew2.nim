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

## Provides code related to the player's ship's crew like resting. Split from
## the crew module to avoid circular dependencies.

import contracts
import crew, game, game2, types, shipsmovement

proc waitForRest*() {.raises: [KeyError, IOError, Exception],
    tags: [WriteIOEffect, RootEffect], contractual.} =
  ## Wait until the whole player's ship crew is rested
  var timeNeeded: Natural = 0
  for index, member in playerShip.crew:
    if member.tired > 0 and member.order == rest:
      var tempTimeNeeded: Natural = 0
      let cabinIndex: ExtendedNatural = findCabin(memberIndex = index)
      if cabinIndex > -1:
        let damage: float = 1.0 - (playerShip.modules[
            cabinIndex].durability.float / playerShip.modules[
            cabinIndex].maxDurability.float)
        var cabinBonus: Natural = playerShip.modules[cabinIndex].cleanliness - (
            playerShip.modules[cabinIndex].cleanliness.float * damage).Natural
        if cabinBonus == 0:
          cabinBonus = 1
        tempTimeNeeded = (member.tired / cabinBonus).Natural * 15
        if tempTimeNeeded == 0:
          tempTimeNeeded = 15
      else:
        tempTimeNeeded *= 15
      tempTimeNeeded += 15
      if tempTimeNeeded > timeNeeded:
        timeNeeded = tempTimeNeeded
  if timeNeeded > 0:
    updateGame(minutes = timeNeeded)
    waitInPlace(minutes = timeNeeded)
