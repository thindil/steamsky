# Copyright 2023-2025 Bartek thindil Jasicki
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

## Provides code related to moving ships on the map, like waiting in place,
## counting a ship's speed, docking or undocking, etc.

import std/[strutils, tables]
import contracts
import bases, bases2, config, crewinventory, game, game2, gamesaveload, items,
    maps, messages, ships, shipscargo, shipscrew, shipscrew2, types, utils

proc waitInPlace*(minutes: Positive) {.raises: [KeyError, IOError,
    ReputationError], tags: [WriteIOEffect], contractual.} =
  ## Count the fuel usage when the player waits in the open space
  ##
  ## * minutes - the amount of minutes passed
  if playerShip.speed == docked:
    return
  var baseFuelNeeded: int = 0
  for module in playerShip.modules:
    if module.mType == ModuleType2.engine and not module.disabled:
      baseFuelNeeded -= 1
  var fuelNeeded: int = baseFuelNeeded * (minutes / 10).int
  if getRandom(min = 1, max = 10) < (minutes mod 10):
    fuelNeeded *= baseFuelNeeded
  let fuelIndex: int = findItem(inventory = playerShip.cargo,
      itemType = fuelType, itemQuality = any)
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
      fuelIndex].protoIndex, amount = fuelNeeded, quality = playerShip.cargo[
          fuelIndex].quality)

proc haveOrderRequirements(): string {.raises: [KeyError], tags: [],
    contractual.} =
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

proc realSpeed*(ship: ShipRecord; infoOnly: bool = false): Natural {.raises: [
    KeyError, ValueError], tags: [], contractual.} =
  ## Count the real speed of the ship in meters per minute
  ##
  ## * ship     - the ship which speed will be count
  ## * infoOnly - if true, the ship is docked, count with its max speed
  ##
  ## Returns the ships' speed in meters per minute
  var newSpeed: int = 0
  if ship.name == playerShip.name and not infoOnly:
    if haveOrderRequirements().len > 0:
      return
  var baseSpeed: int = 0
  for module in ship.modules:
    if module.mType == ModuleType2.engine and not module.disabled:
      baseSpeed = module.power * 10
      var damage: float = 1.0 - (module.durability.float /
          module.maxDurability.float)
      newSpeed += (baseSpeed - (baseSpeed.float * damage).Natural)
  newSpeed = ((newSpeed.float - countShipWeight(ship = ship).float) *
      25.0).int
  if ship.crew.len > 0:
    if "sentientships" in factionsList[ship.crew[0].faction].flags:
      for module in ship.modules:
        if module.mType == ModuleType2.hull:
          newSpeed += (newSpeed.float * ((module.maxModules * 2).float /
              300.0)).int
    else:
      for member in ship.crew:
        if member.order == pilot:
          newSpeed += (newSpeed.float * (getSkillLevel(member = member,
              skillIndex = pilotingSkill).float / 300.0)).int
  var shipSetSpeed: ShipSpeed = ship.speed
  if ship.name == playerShip.name and ship.speed in {docked, fullStop} and infoOnly:
    shipSetSpeed = parseEnum[ShipSpeed](s = (
        $gameSettings.undockSpeed).toLowerAscii)
    if shipSetSpeed == fullStop:
      shipSetSpeed = quarterSpeed
  case shipSetSpeed
  of quarterSpeed:
    newSpeed = (newSpeed.float * 0.25).int
  of halfSpeed:
    newSpeed = (newSpeed.float * 0.5).int
  of fullSpeed:
    discard
  else:
    return 0
  newSpeed = (newSpeed / 60).int
  if newSpeed < 0:
    return 0
  return newSpeed.Natural

proc dockShip*(docking: bool; escape: bool = false): string {.raises: [KeyError,
    IOError, Exception], tags: [WriteIOEffect, RootEffect], contractual.} =
  ## Dock, undock or escape the player's ship from the currently visited base
  ##
  ## * docking - if true, the player is docking to the base
  ## * escape  - if true, the player is escaping from the base without paying.
  ##             The default value is false.
  ##
  ## Returns empty string if the player successfully docked, undocked or
  ## escaped from the base. Otherwise return message what goes wrong.
  let baseIndex: BasesRange = skyMap[playerShip.skyX][playerShip.skyY].baseIndex
  result = haveOrderRequirements()
  if result.len > 0:
    return
  if docking:
    if getBasePopulation(baseIndex = baseIndex) == empty:
      addMessage(message = "Ship docked to base " & skyBases[baseIndex].name &
          ".", mType = orderMessage)
    else:
      addMessage(message = "Ship docked to base " & skyBases[baseIndex].name,
          mType = orderMessage)
      if $gameSettings.autoSave == $dock:
        saveGame()
      var memberIndex: int = 0
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
      if gameSettings.autoAskForBases:
        askForBases()
      if gameSettings.autoAskForEvents:
        askForEvents()
    playerShip.speed = docked
    updateGame(minutes = 10)
  else:
    playerShip.speed = parseEnum[ShipSpeed](s = (
        $gameSettings.undockSpeed).toLowerAscii)
    if (realSpeed(ship = playerShip).float / 1_000.0) < 0.5:
      return "You can't undock because your ship is overloaded."
    if escape:
      let roll: Positive = getRandom(min = 1, max = 100)
      var
        messageText: string = "Ship escaped from base " & skyBases[
            baseIndex].name & " without paying."
        color: MessageColor = white
      if roll in 1..40:
        let moduleIndex: Natural = getRandom(min = playerShip.modules.low,
            max = playerShip.modules.high)
        messageText = messageText & " But your ship (" & playerShip.modules[
            moduleIndex].name & ") takes damage."
        color = red
        damageModule(ship = playerShip, moduleIndex = moduleIndex,
            damage = getRandom(min = 1, max = 30),
            deathReason = "damage during escaping from the base")
      addMessage(message = messageText, mType = orderMessage, color = color)
      gainRep(baseIndex = baseIndex, points = -(getRandom(min = 10, max = 30)))
      if playerShip.crew[0].health > 0:
        playerShip.speed = parseEnum[ShipSpeed](s = (
            $gameSettings.undockSpeed).toLowerAscii)
        updateGame(minutes = 5)
        if $gameSettings.autoSave == $undock:
          saveGame()
    else:
      if getBasePopulation(baseIndex = baseIndex) == empty:
        let fuelIndex: int = findItem(inventory = playerShip.cargo,
            itemType = fuelType, itemQuality = any)
        if fuelIndex == -1:
          return "You can't undock from base because you don't have any fuel."
        addMessage(message = "Ship undocked from base " & skyBases[
            baseIndex].name & ".", mType = orderMessage)
      else:
        let moneyAmount = moneyAmount(inventory = playerShip.cargo)
        if moneyAmount == 0:
          return "You can't undock from this base because you don't have any " &
              moneyName & " to pay for docking."
        var dockingCost: Natural = 0
        for module in playerShip.modules:
          if module.mType == ModuleType2.hull:
            dockingCost = module.maxModules
            break
        dockingCost = (dockingCost.float * newGameSettings.pricesBonus).int
        if dockingCost == 0:
          dockingCost = 1
        let traderIndex: int = findMember(order = talk)
        countPrice(price = dockingCost, traderIndex = traderIndex)
        if dockingCost > moneyAmount:
          return "You can't undock to this base because you don't have enough " &
              moneyName & " to pay for docking."
        updateMoney(memberIndex = -1, amount = -(dockingCost), quality = any)
        if traderIndex > -1:
          gainExp(amount = 1, skillNumber = talkingSkill,
              crewIndex = traderIndex)
        let fuelIndex: int = findItem(inventory = playerShip.cargo,
            itemType = fuelType, itemQuality = any)
        if fuelIndex == -1:
          return "You can't undock from base because you don't have any fuel."
        addMessage(message = "Ship undocked from base " & skyBases[
            baseIndex].name & ". You also paid " & $dockingCost & " " &
            moneyName & " of docking fee.", mType = orderMessage)

proc countFuelNeeded*(): int {.raises: [], tags: [], contractual.} =
  ## Count the amount of needed fuel to travel by one map cell by the player's
  ## ship
  ##
  ## Returns the amount of needed fuel.
  result = 0
  var speed: ShipSpeed = playerShip.speed
  if speed in {docked, fullStop}:
    speed = gameSettings.undockSpeed
  for module in playerShip.modules:
    if module.mType == ModuleType2.engine and not module.disabled:
      case speed
      of quarterSpeed:
        result -= (module.fuelUsage / 4).int
      of halfSpeed:
        result -= (module.fuelUsage / 2).int
      of fullSpeed:
        result -= module.fuelUsage
      else:
        discard

proc changeShipSpeed*(speedValue: ShipSpeed): string {.raises: [
    KeyError], tags: [], contractual.} =
  ## Change the player's ship's speed
  ##
  ## * speeedValue - the new value for the player's ship's speed
  ##
  ## Returns an empty string if the speed was changed successfully, otherwise
  ## returns a message with information what goes wrong.
  var haveEngine: bool = false
  for module in playerShip.modules:
    if module.mType == ModuleType2.engine and (module.durability > 0 and
        not module.disabled):
      haveEngine = true
      break
  if not haveEngine:
    return "You don't have a working engine on your ship or all of the engines are destroyed."
  if findMember(order = engineer) == -1 and "sentientships" notin factionsList[
      playerShip.crew[0].faction].flags:
    return "You don't have an engineer on duty."
  playerShip.speed = speedValue
  return ""

proc moveShip*(x, y: int; message: var string): Natural {.raises: [
    KeyError, ValueError, IOError, Exception], tags: [WriteIOEffect,
    RootEffect], contractual.} =
  ## Move the player's ship on the map
  ##
  ## * x       - the amount of fields in X axis by which the ship will be moved
  ## * y       - the amount of fields in Y axis by which the ship will be moved
  ## * message - if the ship can't be moved, a message with information why,
  ##             otherwise an empty string.
  ##
  ## 1 if ship was moved, 0 if not, 8 if moved but the crew members need a rest,
  ## 6 if moved and the pilot was on rest after and 7 if moved and the engineer
  ## was on rest. Last two are only when auto rest option is enabled.
  case playerShip.speed
  of docked:
    message = "First you must undock your ship from the base."
    return 0
  of fullStop:
    message = "First you must set the speed of your ship."
    return 0
  else:
    discard
  message = haveOrderRequirements()
  if message.len > 0:
    return 0
  var fuelIndex: int = findItem(inventory = playerShip.cargo,
      itemType = fuelType, itemQuality = any)
  if fuelIndex == -1:
    message = "You don't have any fuel."
    return 0
  let fuelNeeded: int = countFuelNeeded()
  if playerShip.cargo[fuelIndex].amount < fuelNeeded:
    message = "You don't have enough fuel (" & itemsList[playerShip.cargo[
        fuelIndex].protoIndex].name & ")."
    return 0
  let speed: float = realSpeed(ship = playerShip).float / 1_000.0
  if speed < 0.5:
    message = "You can't fly because your ship is overloaded."
    return 0
  let
    newX: int = playerShip.skyX + x
    newY: int = playerShip.skyY + y
  if newX < 1 or newX > 1_024 or newY < 1 or newY > 1_024:
    return 0;
  playerShip.skyX = newX
  playerShip.skyY = newY
  updateCargo(ship = playerShip, protoIndex = playerShip.cargo[
      fuelIndex].protoIndex, amount = fuelNeeded, quality = playerShip.cargo[
          fuelIndex].quality)
  var timePassed: int = (100.0 / speed).int
  if timePassed > 0:
    case playerShip.speed
    of quarterSpeed:
      if timePassed < 60:
        timePassed = 60
    of halfSpeed:
      if timePassed < 30:
        timePassed = 30
    of fullSpeed:
      if timePassed < 15:
        timePassed = 15
    else:
      discard
    updateGame(minutes = timePassed)
    fuelIndex = findItem(inventory = playerShip.cargo, itemType = fuelType,
        itemQuality = any)
    if fuelIndex == -1:
      addMessage(message = "Ship falls from the sky due to a lack of fuel.",
          mType = otherMessage, color = red)
      death(memberIndex = 0, reason = "fall ot the ship", ship = playerShip)
      return 0

  proc needRest(order: CrewOrders): bool {.raises: [], tags: [], contractual.} =
    ## Check if a crew member with the selected order need rest
    ##
    ## * order - the order for a crew member to check
    ##
    ## Returns true if a crew member with the selected order need rest,
    ## otherwise false
    if findMember(order = order) == -1:
      for member in playerShip.crew:
        if member.previousOrder == order:
          return true
    return false

  if "sentientships" notin factionsList[playerShip.crew[0].faction].flags:
    if needRest(order = pilot):
      if not gameSettings.autoRest:
        return 6
      return 8
    if needRest(order = engineer):
      if not gameSettings.autoRest:
        return 7
      return 8
  return 1
