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
import game, ships, types, utils

proc updateMorale*(ship: var ShipRecord; memberIndex: Natural;
    value: int) {.sideEffect, raises: [KeyError], tags: [].} =
  var newMorale, newLoyalty, newValue: int
  let factionIndex = ship.crew[memberIndex].faction
  if "nomorale" in factionsList[factionIndex].flags:
    return
  newValue = value
  if "fanaticism" in factionsList[factionIndex].flags:
    if value > 0:
      newValue = value * 5
    else:
      newValue = (value / 10).int
      if newValue == 0 and getRandom(min = 1, max = 10) <= value.abs:
        newValue = -1
      if newValue == 0:
        return
  newValue = ship.crew[memberIndex].morale[2] + newValue
  newMorale = ship.crew[memberIndex].morale[1]
  while newValue >= 5:
    newValue = newValue - 5
    newMorale = newMorale + 1
  while newValue < 0:
    newValue = newValue + 5
    newMorale = newMorale - 1
  if newMorale > 100:
    newMorale = 100
  elif newMorale < 0:
    newMorale = 0
  ship.crew[memberIndex].morale = [1: newMorale.Natural, 2: newValue.Natural]
  if ship.crew == playerShip.crew and memberIndex == 1:
    return
  newLoyalty = ship.crew[memberIndex].loyalty
  if newMorale > 74 and newLoyalty < 100:
    newLoyalty.inc
  if newMorale < 25 and newLoyalty > 0:
    newLoyalty = newLoyalty - getRandom(min = 5, max = 10)
  if newLoyalty > 100:
    newLoyalty = 100
  elif newLoyalty < 0:
    newLoyalty = 0
  ship.crew[memberIndex].loyalty = newLoyalty

# Temporary code for interfacing with Ada

proc updateAdaMorale(isPlayerShip, memberIndex, value: cint) {.exportc.} =
  if isPlayerShip == 1:
    updateMorale(ship = playerShip, memberIndex = memberIndex - 1, value = value)
  else:
    updateMorale(ship = npcShip, memberIndex = memberIndex - 1, value = value)
