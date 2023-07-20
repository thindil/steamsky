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
import crafts, game, halloffame, messages, missions, shipscrew, types, utils

proc deleteMember*(memberIndex: Natural; ship: var ShipRecord) {.sideEffect,
    raises: [KeyError], tags: [].} =
  ## Delete the selected member from the selected ship crew list, update
  ## the ship modules with the new crew list and delete accepted missions
  ## if neccessary.
  ##
  ## * memberIndex - the crew index of the member to delete
  ## * ship        - the ship from which the crew member will be deleted
  ##
  ## Returns parameter ship with the updated list of crew members and modules
  var deleted = false
  if ship.crew == playerShip.crew:
    for index, mission in acceptedMissions.pairs:
      if mission.mType == passenger and mission.data == memberIndex:
        deleteMission(missionIndex = index)
        deleted = true
        break
    for mission in acceptedMissions.mitems:
      if mission.mType == passenger and mission.data > memberIndex:
        mission.data.inc
    if deleted:
      return
  ship.crew.delete(i = memberIndex)
  for module in ship.modules.mitems:
    for owner in module.owner.mitems:
      if owner == memberIndex:
        owner = 0
      elif owner > memberIndex:
        owner.dec

proc death*(memberIndex: Natural; reason: string; ship: var ShipRecord;
    createBody: bool = true) {.sideEffect, raises: [KeyError, IOError], tags: [
    WriteIOEffect].} =
  ## Handle the death of a crew member in ships
  ##
  ## * memberIndex - the crew index of the member which died
  ## * reason      - the reason of death of the crew member
  ## * ship        - the ship to which the crew member belongs
  ## * createBody  - if true, create the body for the crew member. Default
  ##                 value is true
  ##
  ## Returns the parameter ship with updated crew, modules and cargo lists
  let memberName = ship.crew[memberIndex].name
  if ship.crew == playerShip.crew:
    if memberIndex == 0:
      addMessage(message = "You died from " & reason & ".",
          mType = combatMessage, color = red)
      playerShip.crew[memberIndex].order = rest
      playerShip.crew[memberIndex].health = 0
      updateHallOfFame(playerName = playerShip.crew[memberIndex].name,
          deathReason = reason)
      return
    addMessage(message = memberName & " died from " & reason & ".",
        mType = combatMessage, color = red)
  if createBody:
    ship.cargo.add(y = InventoryData(protoIndex: corpseIndex, amount: 1,
        name: memberName & "'s corpse", durability: 100, price: 0))
  deleteMember(memberIndex = memberIndex, ship = ship)
  for index, _ in ship.crew.pairs:
    updateMorale(ship = ship, memberIndex = index, value = getRandom(min = -25, max = -10))

proc getCurrentOrder*(memberIndex: Natural): string {.sideEffect, raises: [
    ValueError], tags: [].} =
  ## Get the full information about the order of the selected crew member
  ##
  ## * memberIndex - the crew index of the member which order will be checked
  ##
  ## Returns a string with detailed information about the current order of the
  ## selected player's ship's crew member.
  proc getModuleName(mType: ModuleType2): string =
    result = ""
    for module in playerShip.modules:
      if module.mType == mType:
        for owner in module.owner:
          if owner == memberIndex:
            return module.name

  let member = playerShip.crew[memberIndex]
  case member.order
  of pilot:
    result = "Piloting the ship"
  of engineer:
    result = "Engineering the ship"
  of gunner:
    result = "Operating " & getModuleName(mType = ModuleType2.gun)
  of repair:
    result = "Repairing the ship"
  of craft:
    for index, module in playerShip.modules:
      if module.mType == ModuleType2.workshop:
        for owner in module.owner:
          if owner == memberIndex:
            result = getWorkshopRecipeName(workshop = index) & " in " & module.name
            break
  of upgrading:
    result = "Upgrading " & playerShip.modules[playerShip.upgradeModule].name
  of talk:
    result = "Talking with others"
  of heal:
    result = "Healig the wounded in " & getModuleName(
        mType = ModuleType2.medicalRoom)
  of clean:
    result = "Cleaning the ship"
  of rest:
    result = "Resting in " & getModuleName(mType = ModuleType2.cabin) & ", no order"
  of defend:
    result = "Defending the ship"
  of boarding:
    result = "Boarding the enemy's ship"
  of train:
    block findTrainingRoom:
      for module in playerShip.modules:
        if module.mType == ModuleType2.trainingRoom:
          for owner in module.owner:
            if owner == memberIndex:
              result = "Training " & skillsList[module.trainedSkill].name &
                  " in " & module.name
              break findTrainingRoom

# Temporary code for interfacing with Ada

proc deleteAdaMember(memberIndex, inPlayerShip: cint) {.raises: [], tags: [], exportc.} =
  try:
    if inPlayerShip == 1:
      deleteMember(memberIndex = memberIndex - 1, ship = playerShip)
    else:
      deleteMember(memberIndex = memberIndex - 1, ship = npcShip)
  except KeyError:
    discard

proc deathAda(memberIndex: cint; reason: cstring; inPlayerShip,
    createBody: cint) {.raises: [], tags: [WriteIOEffect], exportc.} =
  try:
    if inPlayerShip == 1:
      death(memberIndex = memberIndex - 1, reason = $reason, ship = playerShip,
          createBody = (if createBody == 1: true else: false))
    else:
      death(memberIndex = memberIndex - 1, reason = $reason, ship = npcShip,
          createBody = (if createBody == 1: true else: false))
  except IOError, KeyError:
    discard

proc getAdaCurrentOrder(memberIndex: cint): cstring {.raises: [], tags: [], exportc.} =
  try:
    return getCurrentOrder(memberIndex = memberIndex - 1).cstring
  except ValueError:
    return ""
