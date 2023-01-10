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
import crew, game, ships, types, utils

proc updateOrders*(ship: var ShipRecord; combat: bool = false)

proc giveOrders*(ship: var ShipRecord; memberIndex: Natural;
    givenOrder: CrewOrders; moduleIndex: int = -1;
    checkPriorities: bool = true) =
  if givenOrder == ship.crew[memberIndex].order:
    if givenOrder in [craft, gunner]:
      for index, module in ship.modules.pairs:
        if index == moduleIndex:
          for owner in module.owner:
            if owner == memberIndex:
              return
    else:
      return
  let memberName = ship.crew[memberIndex].name
  if givenOrder != rest and ((ship.crew[memberIndex].morale[1] < 11 and
      getRandom(min = 1, max = 100) < 50) or ship.crew[memberIndex].loyalty < 20):
    if ship.crew == playerShip.crew:
      raise newException(exceptn = CrewOrderError, message = memberName & " refuses to execute order.")
  if givenOrder == train and ship.modules[moduleIndex].trainedSkill == 0:
    raise newException(exceptn = CrewOrderError, message = memberName &
        " can't start training because " & ship.modules[moduleIndex].name & " isn't prepared.")
  if givenOrder in [pilot, engineer, upgrading, talk]:
    for index, member in ship.crew.pairs:
      if member.order == givenOrder:
        giveOrders(ship = ship, memberIndex = index, givenOrder = rest,
            moduleIndex = -1, checkPriorities = false)
        break
  elif givenOrder in [gunner, craft, train] or (givenOrder == heal and
      moduleIndex > 0):
    var freePosition: bool = false
    for owner in ship.modules[moduleIndex].owner:
      if owner == 0:
        freePosition = true
        break
    if not freePosition:
      giveOrders(ship = ship, memberIndex = ship.modules[moduleIndex].owner[0],
          givenOrder = rest, moduleIndex = -1, checkPriorities = false)
  # TODO: continue work

proc updateOrders(ship: var ShipRecord; combat: bool = false) =
  discard

proc updateMorale*(ship: var ShipRecord; memberIndex: Natural;
    value: int) {.sideEffect, raises: [KeyError], tags: [].} =
  ## FUNCTION
  ##
  ## Update the morale of the selected crew member in the selected ship
  ##
  ## PARAMETERS
  ##
  ## * ship        - the ship in which the crew member's morale will be changed
  ## * memberIndex - the index of the crew member which morale will be changed
  ## * value       - the value with which the morale will be changed
  ##
  ## RETURNS
  ##
  ## The modified ship parameter with updated morale of the selected crew
  ## member.
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
