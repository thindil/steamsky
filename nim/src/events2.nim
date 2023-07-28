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

import combat, game, maps, shipscrew, types, utils

proc checkForEvent*(): bool =
  if skyMap[playerShip.skyX][playerShip.skyY].eventIndex > -1:
    if eventsList[skyMap[playerShip.skyX][playerShip.skyY].eventIndex].eType == enemyShip:
      return startCombat(enemyIndex = eventsList[skyMap[playerShip.skyX][playerShip.skyY].eventIndex].shipIndex)
    else:
      return false
  if getRandom(min = 1, max = 100) > 6:
    return false
  let
    roll = getRandom(min = 1, max = 100)
    baseIndex = skyMap[playerShip.skyX][playerShip.skyY].baseIndex
  # Event outside a sky base
  if baseIndex == 0:
    case roll
    # Engine damaged
    of 1 .. 5:
      let engineerIndex = findMember(order = engineer)
      if engineerIndex > -1 and playerShip.speed != fullStop:
        discard
    else:
      discard

