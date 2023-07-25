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
import crewinventory, game, messages, ships, ships2, shipscargo, trades, types, utils

type
  EnemyRecord* = object
    ## Used to store information about the enemy
    ##
    ## * ship            - The enemy's ship
    ## * accuracy        - The enemy's bonus to accuracy
    ## * distance        - The distance to the enemy's ship in combat
    ## * combatAi        - The type of enemy's AI
    ## * evasion         - The enemy's bonus to evasion
    ## * loot            - The amount of money looted from the enemy after win
    ##                     the fight with it
    ## * perception      - The enemy's bonus to perception
    ## * harpoonDuration - How long in combat rounds the enemy's ship will be
    ##                     stopped by the player's harpoon
    ## * guns            - The list of guns on the enemy's ship, 0 - gun index
    ##                     in ship modules list, 1 - gunner order, 2 - amount
    ##                     of shoots from the gun, value below zero means that
    ##                     gun shoot once per that amount of rounds
    ship*: ShipRecord
    accuracy*: Natural
    distance*: int
    combatAi*: ShipCombatAi
    evasion*: Natural
    loot*: Natural
    perception*: Natural
    harpoonDuration*: Natural
    guns*: seq[array[1..3, int]]

var
  harpoonDuration*: Natural = 0 ## How long in combat rounds the player's ship will be stopped by an enemy's harpoon
  enemy*: EnemyRecord = EnemyRecord(ship: ShipRecord(skyX: 1,
      skyY: 1))               ## The enemy information
  enemyShipIndex: Natural     ## The index of the enemy's ship's prototype
  factionName: string         ## The name of the enemy's faction (ship and its crew)
  boardingOrders: seq[int]    ## The list of orders for the boarding party
  pilotOrder: Natural = 0     ## The player's ship pilot order
  engineerOrder: Natural      ## The player's ship engineer order
  endCombat: bool = false     ## If true, the combat ends
  enemyName: string           ## The name of the enemy's ship
  messagesStarts: int = -1 ## The starting index of messages to show

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

# Temporary code for interfacing with Ada

proc getAdaHarpoonDuration(playerDuration, enemyDuration: cint) {.raises: [],
    tags: [], exportc.} =
  harpoonDuration = playerDuration
  enemy.harpoonDuration = enemyDuration
