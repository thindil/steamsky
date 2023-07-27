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

import std/[math, strutils, tables]
import crewinventory, game, log, messages, ships, ships2, shipscargo, shipscrew,
    shipsmovement, trades, types, utils

var
  enemyShipIndex: Natural     ## The index of the enemy's ship's prototype
  factionName: string         ## The name of the enemy's faction (ship and its crew)
  boardingOrders: seq[int]    ## The list of orders for the boarding party
  pilotOrder: Natural = 0     ## The player's ship pilot order
  engineerOrder: Natural      ## The player's ship engineer order
  endCombat: bool = false     ## If true, the combat ends
  enemyName: string           ## The name of the enemy's ship
  messagesStarts: int = -1    ## The starting index of messages to show
  guns: seq[array[1..3, int]] ## The list of guns installed on the player's ship
  oldSpeed = fullSpeed        ## The speed of the player's ship before combat
  turnNumber: Natural = 0     ## The number of the combat's turn

proc startCombat*(enemyIndex: Positive; newCombat: bool = true): bool =
  enemyShipIndex = enemyIndex
  factionName = factionsList[protoShipsList[enemyIndex].owner].name
  harpoonDuration = 0
  boardingOrders = @[]
  var enemyShip = createShip(protoIndex = enemyIndex, name = "",
      x = playerShip.skyX, y = playerShip.skyY, speed = fullSpeed)
  # Enemy ship is a trader, generate a cargo for it
  if protoShipsList[enemyIndex].name.contains(tradersName):
    generateTraderCargo(protoIndex = enemyIndex)
    for item in traderCargo:
      updateCargo(ship = enemyShip, protoIndex = item.protoIndex,
          amount = item.amount)
    traderCargo = @[]
  var minFreeSpace = 0
  for module in enemyShip.modules:
    if module.mType == ModuleType2.cargoRoom and module.durability > 0:
      minFreeSpace = minFreeSpace + modulesList[module.protoIndex].maxValue
  minFreeSpace = (minFreeSpace.float * (1.0 - (getRandom(min = 20,
      max = 70).float / 100.0))).Natural
  while freeCargo(amount = 0, ship = enemyShip) > minFreeSpace:
    var
      itemIndex = getRandom(min = 1, max = itemsList.len)
      newItemIndex = 0
    for i in 1 .. itemsList.len:
      itemIndex.dec
      if itemIndex == 0:
        newItemIndex = i
        break
    let
      itemAmount = if enemyShip.crew.len < 5:
          getRandom(min = 1, max = 100)
        elif enemyShip.crew.len < 10:
          getRandom(min = 1, max = 500)
        else:
          getRandom(min = 1, max = 1000)
      cargoItemIndex = findItem(inventory = enemyShip.cargo,
          protoIndex = newItemIndex)
    if cargoItemIndex > -1:
      enemyShip.cargo[cargoItemIndex].amount = enemyShip.cargo[
          cargoItemIndex].amount + itemAmount
    else:
      if freeCargo(amount = 0 - (itemsList[newItemIndex].weight * itemAmount)) > -1:
        enemyShip.cargo.add(InventoryData(protoIndex: newItemIndex,
            amount: itemAmount, durability: defaultItemDurability, name: "", price: 0))
  var enemyGuns: seq[array[1..3, int]] = @[]
  for index, module in enemyShip.modules:
    if module.mType in {ModuleType2.gun, harpoonGun} and module.durability > 0:
      var shootingSpeed = 0
      if modulesList[module.protoIndex].speed > 0:
        shootingSpeed = if protoShipsList[enemyIndex].combatAi == disarmer:
            ((modulesList[module.protoIndex].speed.float / 2.0).ceil).Natural
          else:
            modulesList[module.protoIndex].speed
      else:
        shootingSpeed = if protoShipsList[enemyIndex].combatAi == disarmer:
            modulesList[module.protoIndex].speed - 1
          else:
            modulesList[module.protoIndex].speed
      enemyGuns.add([1: index, 2: 1, 3: shootingSpeed])
  enemy = EnemyRecord(ship: enemyShip, accuracy: (if protoShipsList[
      enemyIndex].accuracy.maxValue == 0: protoShipsList[
      enemyIndex].accuracy.minValue else: getRandom(min = protoShipsList[
      enemyIndex].accuracy.minValue, max = protoShipsList[
      enemyIndex].accuracy.maxValue)), distance: 10_000,
      combatAi: protoShipsList[enemyIndex].combatAi, evasion: (
      if protoShipsList[enemyIndex].evasion.maxValue == 0: protoShipsList[
      enemyIndex].evasion.minValue else: getRandom(min = protoShipsList[
      enemyIndex].evasion.minValue, max = protoShipsList[
      enemyIndex].evasion.maxValue)), loot: (if protoShipsList[
      enemyIndex].loot.maxValue == 0: protoShipsList[
      enemyIndex].loot.minValue else: getRandom(min = protoShipsList[
      enemyIndex].loot.minValue, max = protoShipsList[
      enemyIndex].loot.maxValue)), perception: (if protoShipsList[
      enemyIndex].perception.maxValue == 0: protoShipsList[
      enemyIndex].perception.minValue else: getRandom(min = protoShipsList[
      enemyIndex].perception.minValue, max = protoShipsList[
      enemyIndex].perception.maxValue)), harpoonDuration: 0, guns: enemyGuns)
  if pilotOrder == 0:
    pilotOrder = 2
    engineerOrder = 3
  endCombat = false
  enemyName = generateShipName(factionIndex = protoShipsList[enemyIndex].owner)
  messagesStarts = getLastMessageIndex()
  let oldGunsList = guns
  var sameList = true
  guns = @[]
  for index, module in playerShip.modules:
    if module.mType in {ModuleType2.gun, harpoonGun} and module.durability > 0:
      guns.add([1: index, 2: 1, 3: modulesList[module.protoIndex].speed])
  if oldGunsList.len == guns.len:
    for index, gun in guns:
      if gun[1] != oldGunsList[index][1]:
        sameList = false
        break
    if sameList:
      guns = oldGunsList
  if newCombat:

    proc countPerception(spotter, spotted: ShipRecord): Natural =
      for index, member in spotter.crew:
        case member.order
        of pilot:
          result = result + getSkillLevel(member = member,
              skillIndex = perceptionSkill)
          if spotter.crew == playerShip.crew:
            gainExp(amount = 1, skillNumber = perceptionSkill,
                crewIndex = index)
        of gunner:
          result = result + getSkillLevel(member = member,
              skillIndex = perceptionSkill)
          if spotter.crew == playerShip.crew:
            gainExp(amount = 1, skillNumber = perceptionSkill,
                crewIndex = index)
        else:
          discard
      for module in spotted.modules:
        if module.mType == ModuleType2.hull:
          result = result + module.maxModules
          break

    let
      playerPerception = countPerception(spotter = playerShip,
        spotted = enemyShip)
      enemyPerception = (if enemy.perception >
          0: enemy.perception else: countPerception(spotter = enemy.ship,
          spotted = playerShip))
    oldSpeed = playerShip.speed
    if playerPerception + getRandom(min = 1, max = 50) > enemyPerception +
        getRandom(min = 1, max = 50):
      addMessage(message = "You spotted " & enemy.ship.name & ".",
          mType = otherMessage)
    else:
      if realSpeed(ship = playerShip) < realSpeed(ship = enemy.ship):
        logMessage(message = "You were attacked by " & enemy.ship.name,
            debugType = DebugTypes.combat)
        addMessage(message = enemy.ship.name & " intercepted you.",
            mType = combatMessage)
        return true
      addMessage(message = "You spotted " & enemy.ship.name & ".",
          mType = otherMessage)
    return false
  turnNumber = 0
  logMessage(message = "Started combat with " & enemy.ship.name,
      debugType = DebugTypes.combat)
  return true

# Temporary code for interfacing with Ada

proc getAdaHarpoonDuration(playerDuration, enemyDuration: cint) {.raises: [],
    tags: [], exportc.} =
  harpoonDuration = playerDuration
  enemy.harpoonDuration = enemyDuration

proc startAdaCombat(enemyIndex, newCombat: cint): cint {.raises: [], tags: [
    RootEffect], exportc.} =
  try:
    return startCombat(enemyIndex = enemyIndex, newCombat = newCombat == 1).cint
  except ValueError:
    return 0

type
  AdaEnemyData = object
    accuracy: cint
    combatAi: cint
    evasion: cint
    loot: cint
    perception: cint
    guns: array[10, array[3, cint]]
    name: cstring
    playerGuns: array[10, array[3, cint]]

proc getAdaEnemy(adaEnemy: var AdaEnemyData) {.raises: [], tags: [], exportc.} =
  adaEnemy.accuracy = enemy.accuracy.cint
  adaEnemy.combatAi = enemy.combatAi.ord.cint
  adaEnemy.evasion = enemy.evasion.cint
  adaEnemy.loot = enemy.loot.cint
  adaEnemy.perception = enemy.perception.cint
  adaEnemy.name = enemyName.cstring
  for index, gun in enemy.guns:
    adaEnemy.guns[index] = [gun[1].cint, gun[2].cint, gun[3].cint]
  if enemy.guns.len < 10:
    for index in enemy.guns.len .. 9:
      adaEnemy.guns[index] = [-1, -1, -1]
  for index, gun in guns:
    adaEnemy.playerGuns[index] = [gun[1].cint, gun[2].cint, gun[3].cint]
  if guns.len < 10:
    for index in guns.len .. 9:
      adaEnemy.playerGuns[index] = [-1, -1, -1]
  npcShip = enemy.ship
