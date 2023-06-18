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
import bases2, config, crewinventory, game, game2, gamesaveload, maps, messages,
    ships, shipscargo, shipscrew, shipscrew2, types, utils

proc waitInPlace*(minutes: Positive) {.sideEffect, raises: [KeyError, IOError],
    tags: [WriteIOEffect].} =
  ## Count the fuel usage when the player waits in the open space
  ##
  ## * minutes - the amount of minutes passed
  if playerShip.speed == docked:
    return
  var baseFuelNeeded: int = 0
  for module in playerShip.modules:
    if module.mType == ModuleType2.engine and not module.disabled:
      baseFuelNeeded = baseFuelNeeded - 1
  var fuelNeeded = baseFuelNeeded * (minutes / 10).int
  if getRandom(min = 1, max = 10) < (minutes mod 10):
    fuelNeeded = fuelNeeded * baseFuelNeeded
  let fuelIndex = findItem(inventory = playerShip.cargo, itemType = fuelType)
  if fuelIndex == -1:
    addMessage(message = "Ship falls from the sky due to a lack of fuel.",
        mType = otherMessage, color = red)
    death(memberIndex = 0, reason = "fall of the ship", ship = playerShip)
    return
  if playerShip.cargo[fuelIndex].amount <= fuelNeeded.abs:
    addMessage(message = "Ship falls from the sky due to a lack of fuel.",
        mType = otherMessage, color = red)
    death(memberIndex = 0, reason = "fall of the ship", ship = playerShip)
    return
  updateCargo(ship = playerShip, protoIndex = playerShip.cargo[
      fuelIndex].protoIndex, amount = fuelNeeded)

proc haveOrderRequirements(): string {.sideEffect, raises: [KeyError], tags: [].} =
  ## Check if all requirements for the ship's moving orders are valid
  ##
  ## Returns empty string if everything is ok, otherwise the message about the
  ## missing requirement for the movement order.
  var haveCockpit, haveEngine: bool = false
  for module in playerShip.modules:
    if module.mType == ModuleType2.cockpit and module.durability > 0:
      haveCockpit = true
    elif module.mType == ModuleType2.engine and (module.durability > 1 and
        not module.disabled):
      haveEngine = true
    if haveCockpit and haveEngine:
      break
  if not haveEngine:
    return "You don't have a working engine on your ship or all of the engines are destroyed."
  if not haveCockpit:
    return "You don't have a cockpit on your ship or the cockpit is destroyed."
  var havePilot, haveEngineer: bool = false
  if "sentientships" in factionsList[playerShip.crew[0].faction].flags:
    havePilot = true
    haveEngineer = true
  for member in playerShip.crew:
    if member.order == pilot:
      havePilot = true
    elif member.order == engineer:
      haveEngineer = true
    if havePilot and haveEngineer:
      break
  if not havePilot:
    return "You don't have a pilot on duty."
  if not haveEngineer:
    return "You don't have an engineer on duty."
  return ""

proc realSpeed*(ship: ShipRecord; infoOnly: bool = false): Natural =
  result = 0
  if ship.name == playerShip.name and not infoOnly:
    if haveOrderRequirements().len > 0:
      return
  var baseSpeed = 0
  for module in ship.modules:
    if module.mType == ModuleType2.engine and not module.disabled:
      baseSpeed = module.power * 10
      var damage = 1.0 - (module.durability.float / module.maxDurability.float)
      result = result + (baseSpeed - (baseSpeed.float * damage).Natural)
  result = ((result.float / countShipWeight(ship = ship).float) *
      100_000.0).Natural
  if ship.crew.len > 0:
    if "sentientships" notin factionsList[ship.crew[0].faction].flags:
      for member in ship.crew:
        if member.order == pilot:
          result = result + (result.float * (getSkillLevel(member = member,
              skillIndex = pilotingSkill).float / 300.0)).Natural
    else:
      for module in ship.modules:
        if module.mType == ModuleType2.hull:
          result = result + (result.float * ((module.maxModules * 2).float /
              300.0)).Natural
  var shipSetSpeed = ship.speed
  if ship.name == playerShip.name and ship.speed in {docked, fullStop} and infoOnly:
    shipSetSpeed = gameSettings.undockSpeed.ShipSpeed
    if shipSetSpeed == fullStop:
      shipSetSpeed = quarterSpeed
  case shipSetSpeed
  of quarterSpeed:
    result = (result.float * 0.25).Natural
  of halfSpeed:
    result = (result.float * 0.5).Natural
  of fullSpeed:
    discard
  else:
    return 0
  result = (result / 60).Natural

proc dockShip*(docking: bool; escape: bool = false): string =
  let baseIndex = skyMap[playerShip.skyX][playerShip.skyY].baseIndex
  result = haveOrderRequirements()
  if result.len > 0:
    return
  if docking:
    if skyBases[baseIndex].population > 0:
      addMessage(message = "Ship docked to base " & skyBases[baseIndex].name,
          mType = orderMessage)
      if $gameSettings.autoSave == $dock:
        saveGame()
      var memberIndex = 0
      while memberIndex < playerShip.crew.len:
        if playerShip.crew[memberIndex].contractLength == 0:
          deleteMember(memberIndex = memberIndex, ship = playerShip)
          skyBases[baseIndex].population.inc
        elif playerShip.crew[memberIndex].loyalty < 20 and getRandom(min = 0,
            max = playerShip.crew[memberIndex].loyalty) < 10:
          addMessage(message = playerShip.crew[memberIndex].name &
              " resigns from working for you.", mType = orderMessage)
          deleteMember(memberIndex = memberIndex, ship = playerShip)
          skyBases[baseIndex].population.inc
          for i in playerShip.crew.low .. playerShip.crew.high:
            updateMorale(ship = playerShip, memberIndex = i, value = getRandom(
                min = -5, max = -1))
        else:
          memberIndex.inc
      if gameSettings.autoAskForBases == 1:
        askForBases()
      if gameSettings.autoAskForEvents == 1:
        askForEvents()
    else:
      addMessage(message = "Ship docked to base " & skyBases[baseIndex].name &
          ".", mType = orderMessage)
    playerShip.speed = docked
    updateGame(minutes = 10)
  else:
    playerShip.speed = gameSettings.undockSpeed.ShipSpeed

# Temporary code for interfacing with Ada

proc waitAdaInPlace(minutes: cint) {.raises: [], tags: [WriteIOEffect], exportc.} =
  try:
    waitInPlace(minutes = minutes.Positive)
  except KeyError, IOError:
    discard

proc haveAdaOrderRequirements(): cstring {.raises: [], tags: [], exportc.} =
  try:
    return haveOrderRequirements().cstring
  except KeyError:
    return ""

proc realAdaSpeed(ofPlayerShip, infoOnly: cint): cint {.raises: [ValueError],
    tags: [], exportc.} =
  if ofPlayerShip == 1:
    return realSpeed(playerShip, infoOnly == 1).cint
  else:
    return realSpeed(npcShip, infoOnly == 1).cint
