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
import game, types

proc getPlayerShips*(playerShips: var seq[Positive]) {.sideEffect, raises: [],
    tags: [].} =
  for index, faction in factionsList.pairs:
    for career in faction.careers.values:
      playerShips.add(y = career.shipIndex)

# Temporary code for interfacing with Ada

proc getAdaEvent(index, x, y, time, eType, data: cint) {.raises: [], tags: [], exportc.} =
  var event = EventData(skyX: x, skyY: y, time: time, eType: eType.EventsTypes)
  case event.eType
    of doublePrice:
      event.itemIndex = data
    of attackOnBase, enemyShip, enemyPatrol, trader, friendlyShip:
      event.shipIndex = data
    else:
      event.data = data
  eventsList[index] = event

proc getAdaPlayerShips(playerShips: var array[30, cint]) {.raises: [], tags: [], exportc.} =
  for ship in playerShips.mitems:
    ship = 0
  var nimShips: seq[Positive]
  getPlayerShips(playerShips = nimShips)
  for index, ship in nimShips.pairs:
    playerShips[index] = ship.cint
