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
import crewinventory, config, game, log, messages, ships, ships2, shipscargo,
    shipscrew, shipscrew2, shipsmovement, trades, types, utils

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

proc startCombat*(enemyIndex: Positive; newCombat: bool = true): bool {.sideEffect,
    raises: [KeyError, ValueError], tags: [RootEffect].} =
  ## Generate the enemy and start the ship to ship combat if enemy spotted
  ## the player first
  ##
  ## * enemyIndex - the index of the enemy ship's prototype from which the enemy's
  ##                ship will be generated
  ## * newCombat  - if true, start a new combat, otherwise regenerate the enemy
  ##                only
  ##
  ## Returns true if the combat started automatically, enemy spotted the player
  ## as first. Otherwise returns false.
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
  game.enemy = EnemyRecord(ship: enemyShip, accuracy: (if protoShipsList[
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
      enemyPerception = (if game.enemy.perception >
          0: game.enemy.perception else: countPerception(
          spotter = game.enemy.ship,
          spotted = playerShip))
    oldSpeed = playerShip.speed
    if playerPerception + getRandom(min = 1, max = 50) > enemyPerception +
        getRandom(min = 1, max = 50):
      addMessage(message = "You spotted " & game.enemy.ship.name & ".",
          mType = otherMessage)
    else:
      if realSpeed(ship = playerShip) < realSpeed(ship = game.enemy.ship):
        logMessage(message = "You were attacked by " & game.enemy.ship.name,
            debugType = DebugTypes.combat)
        addMessage(message = game.enemy.ship.name & " intercepted you.",
            mType = combatMessage)
        return true
      addMessage(message = "You spotted " & game.enemy.ship.name & ".",
          mType = otherMessage)
    return false
  turnNumber = 0
  logMessage(message = "Started combat with " & game.enemy.ship.name,
      debugType = DebugTypes.combat)
  return true

proc combatTurn*() =

  var accuracyBonus, evadeBonus = 0

  proc attack(ship, enemyShip: var ShipRecord) =

    var
      hitLocation: int = -1
      speedBonus = 0

    proc removeGun(moduleIndex: Natural) =
      if enemyShip.crew == playerShip.crew:
        for index, gun in guns:
          if gun[1] == moduleIndex:
            guns.delete(index)
            break
    proc findEnemyModule(mType: ModuleType): int =
      for index, module in enemyShip.modules:
        if modulesList[module.protoIndex].mType == mType and module.durability > 0:
          return index
      return -1
    proc findHitWeapon() =
      for index, module in enemyShip.modules:
        if ((module.mType == ModuleType2.turret and module.gunIndex > -1) or
            modulesList[module.protoIndex].mType == ModuleType.batteringRam) and
            module.durability > 0:
          hitLocation = index
          return

    if ship.crew == playerShip.crew:
      logMessage(message = "Player's round", debugType = DebugTypes.combat)
    else:
      logMessage(message = "Enemy's round.", debugType = DebugTypes.combat)
    block attackLoop:
      for mIndex, module in ship.modules.mpairs:
        if module.durability == 0 or module.mType notin {ModuleType2.gun,
            batteringRam, harpoonGun}:
          continue
        var
          gunnerIndex = -1
          ammoIndex = -1
          ammoIndex2 = -1
          gunnerOrder = 1
          shoots = 0
          currentAccuracyBonus = 0
          evadeBonus = 0
        if module.mType == ModuleType2.harpoonGun:
          ammoIndex2 = module.harpoonIndex
        elif module.mType == ModuleType2.gun:
          ammoIndex2 = module.ammoIndex
        if module.mType in {ModuleType2.gun, harpoonGun}:
          gunnerIndex = module.owner[0]
          logMessage(message = "Gunner index: " & $gunnerIndex & ".",
              debugType = DebugTypes.combat)
          if ship.crew == playerShip.crew:
            if gunnerIndex > -1:
              for gun in guns.mitems:
                if gun[0] == mIndex:
                  var shoots = gun[2]
                  gunnerOrder = gun[1]
                  if gun[2] > 0:
                    if gunnerOrder != 3:
                      shoots = (shoots.float / 2.0).ceil.int
                    logMessage(message = "Player shoots (no cooldown): " &
                        $shoots, debugType = DebugTypes.combat)
                  elif gun[2] < 0:
                    shoots = 0
                    gun[2].inc
                    if gun[2] == 0:
                      shoots = 1
                      gun[2] = if gunnerOrder == 3:
                          modulesList[playerShip.modules[gun[
                              0]].protoIndex].speed
                        else:
                          modulesList[playerShip.modules[gun[
                              0]].protoIndex].speed - 1
                    logMessage(message = "Player shoots (after cooldown): " &
                        $shoots, debugType = DebugTypes.combat)
              logMessage(message = "Shoots test3: " & $shoots,
                  debugType = DebugTypes.combat)
              if ship.crew[gunnerIndex].order != gunner:
                gunnerOrder = 1
              case gunnerOrder
              of 1:
                if shoots > 0:
                  shoots = 0
              of 2:
                currentAccuracyBonus = accuracyBonus + 20
              of 4:
                currentAccuracyBonus = accuracyBonus - 10
              of 5:
                currentAccuracyBonus = accuracyBonus - 20
              else:
                discard
          else:
            for gun in game.enemy.guns.mitems:
              if gun[0] == mIndex:
                if gun[2] > 0:
                  shoots = gun[2]
                elif gun[2] < 0:
                  shoots = 0
                  gun[2].inc
                  if gun[2] == 0:
                    shoots = 1
                    gun[2] = if game.enemy.combatAi == disarmer:
                        modulesList[ship.modules[gun[0]].protoIndex].speed - 1
                      else:
                        modulesList[ship.modules[gun[0]].protoIndex].speed
            if ship.crew.len > 0 and gunnerIndex > -1:
              shoots = 0
          if ammoIndex2 < ship.cargo.len and itemsList[ship.cargo[
              ammoIndex2].protoIndex].itemType == $(modulesList[
              module.protoIndex].value - 1):
            ammoIndex = ammoIndex2
          if ammoIndex == -1:
            for iIndex, item in itemsList.pairs:
              if item.itemType == $(modulesList[module.protoIndex].value - 1):
                for iIndex2, item2 in ship.cargo:
                  if item2.protoIndex == iIndex:
                    ammoIndex = iIndex2
                    if module.mType == ModuleType2.harpoonGun:
                      module.harpoonIndex = ammoIndex
                    elif module.mType == ModuleType2.gun:
                      module.ammoIndex = ammoIndex
                    break
              if ammoIndex > -1:
                break
          if ammoIndex == -1:
            if ship.crew == playerShip.crew:
              addMessage(message = "You don't have ammo to " & module.name &
                  "!", mType = combatMessage, color = red)
            shoots = 0
          elif ship.cargo[ammoIndex].amount < shoots:
            shoots = ship.cargo[ammoIndex].amount
          if game.enemy.distance > 5_000:
            shoots = 0
          if module.mType == ModuleType2.harpoonGun and shoots > 0:
            shoots = 1
            if game.enemy.distance > 2_000:
              shoots = 0
            if findEnemyModule(mType = ModuleType.armor) > -1:
              shoots = 0
          if module.mType == ModuleType2.gun and shoots > 0:
            case itemsList[ship.cargo[ammoIndex].protoIndex].value[1]
            of 2:
              if ship.crew == playerShip.crew:
                currentAccuracyBonus -= 10
              else:
                evadeBonus += 10
            of 3:
              if ship.crew == playerShip.crew:
                currentAccuracyBonus += 10
              else:
                evadeBonus -= 10
            else:
              discard
        else:
          if game.enemy.distance > 100:
            shoots = 0
          else:
            shoots = (if module.coolingDown: 0 else: 1)
          module.coolingDown = not module.coolingDown
        logMessage(message = "Shoots: " & $shoots,
            debugType = DebugTypes.combat)
        if shoots > 0:
          var hitChance = if ship.crew == playerShip.crew:
              currentAccuracyBonus - game.enemy.evasion
            else:
              game.enemy.accuracy - evadeBonus
          if gunnerIndex > 0:
            hitChance += getSkillLevel(member = ship.crew[gunnerIndex],
                skillIndex = gunnerySkill)
          if hitChance < -48:
            hitChance = -48
          logMessage(message = "Player accuracy: " & $currentAccuracyBonus &
              " Player evasion: " & $evadeBonus, debugType = DebugTypes.combat)
          logMessage(message = "Enemy evasion: " & $game.enemy.evasion &
              " Enemy accuracy: " & $game.enemy.accuracy,
              debugType = DebugTypes.combat)
          logMessage(message = "Chance to hit: " & $hitChance,
              debugType = DebugTypes.combat)
          let enemyNameOwner = enemyName & " (" & factionName & ")"
          for shoot in 1 .. shoots:
            var shootMessage: string
            if ship.crew == playerShip.crew:
              shootMessage = if module.mType in {ModuleType2.gun, harpoonGun}:
                  ship.crew[gunnerIndex].name & " shoots at " & enemyNameOwner
                else:
                  "You ram " & enemyNameOwner
            else:
              shootMessage = enemyNameOwner & " attacks"
            if hitChance + getRandom(min = 1, max = 50) > getRandom(min = 1,
                max = hitChance + 50):
              shootMessage = shootMessage & " and hits "
              let armorIndex = findEnemyModule(mType = ModuleType.armor)
              if armorIndex > -1:
                hitLocation = armorIndex
              else:
                if ship.crew == playerShip.crew:
                  if gunnerIndex > -1 and gunnerOrder in 4 .. 6:
                    hitLocation = -1
                    case gunnerOrder
                    of 4:
                      hitLocation = findEnemyModule(mType = ModuleType.engine)
                    of 5:
                      hitLocation = -1
                      findHitWeapon()
                      if hitLocation == -1:
                        hitLocation = findEnemyModule(
                            mType = ModuleType.batteringRam)
                    of 6:
                      hitLocation = findEnemyModule(mType = ModuleType.hull)
                    else:
                      hitLocation = 0
                  else:
                    hitLocation = getRandom(min = 0,
                        max = game.enemy.ship.modules.high)
                else:
                  while enemyShip.modules[hitLocation].durability == 0:
                    hitLocation.dec
                    if hitLocation == -1:
                      break attackLoop
              shootMessage = shootMessage & enemyShip.modules[
                  hitLocation].name & "."
              let damage = 1.0 - (module.durability.float /
                  module.maxDurability.float)
              var weaponDamage = 0
              if module.mType == ModuleType2.harpoonGun:
                weaponDamage = module.duration - (module.duration.float * damage).int
              elif module.mType == ModuleType2.gun:
                weaponDamage = module.damage - (module.damage.float * damage).int
              elif module.mType == ModuleType2.batteringRam:
                weaponDamage = module.damage2 - (module.damage2.float * damage).int
                weaponDamage = if speedBonus < 0:
                    weaponDamage + (speedBonus.abs * (countShipWeight(
                        ship = ship) / 5_000).int)
                  else:
                    weaponDamage + (countShipWeight(ship = ship) / 5_000).int
              if weaponDamage < 1:
                weaponDamage = 1
              if ammoIndex > -1:
                weaponDamage += itemsList[ship.cargo[
                    ammoIndex].protoIndex].value[0]
              weaponDamage = if ship.crew == playerShip.crew:
                    (weaponDamage.float * newGameSettings.playerDamageBonus).int
                  else:
                    (weaponDamage.float * newGameSettings.enemyDamageBonus).int
              if armorIndex == -1:
                if module.mType == ModuleType2.harpoonGun:
                  for eModule in enemyShip.modules:
                    if eModule.mType == ModuleType2.hull:
                      weaponDamage = weaponDamage - (eModule.maxModules / 10).int
                      if weaponDamage < 1:
                        weaponDamage = 1
                      break
                  if ship.crew == playerShip.crew:
                    game.enemy.harpoonDuration += weaponDamage
                  else:
                    harpoonDuration += weaponDamage
                  weaponDamage = 1
                elif module.mType == ModuleType2.batteringRam:
                  if ship.crew == playerShip.crew:
                    game.enemy.harpoonDuration += 2
                  else:
                    harpoonDuration += 2
              damageModule(ship = enemyShip, moduleIndex = hitLocation,
                  damage = weaponDamage,
                  deathReason = "enemy fire in ship combat")
              if enemyShip.modules[hitLocation].durability == 0:
                case modulesList[enemyShip.modules[
                    hitLocation].protoIndex].mType
                of ModuleType.hull, ModuleType.engine:
                  endCombat = true
                of ModuleType.turret:
                  if enemyShip.crew == playerShip.crew:
                    let weaponIndex = enemyShip.modules[hitLocation].gunIndex
                    if weaponIndex > -1:
                      enemyShip.modules[weaponIndex].durability = 0
                      removeGun(moduleIndex = weaponIndex)
                else:
                  discard
              if ship.crew == playerShip.crew:
                addMessage(message = shootMessage, mType = combatMessage, color = green)
              else:
                addMessage(message = shootMessage, mType = combatMessage,
                    color = yellow)
            else:
              shootMessage = shootMessage & " and misses."
              if ship.crew == playerShip.crew:
                addMessage(message = shootMessage, mType = combatMessage, color = blue)
              else:
                addMessage(message = shootMessage, mType = combatMessage, color = cyan)
            if ammoIndex > -1:
              updateCargo(ship = ship, cargoIndex = ammoIndex, amount = -1)
            if ship.crew == playerShip.crew and gunnerIndex > -1:
              gainExp(amount = 2, skillNumber = gunnerySkill,
                  crewIndex = gunnerIndex)
            if playerShip.crew[0].health == 0:
              endCombat = true
            if endCombat:
              break attackLoop

  if findItem(inventory = playerShip.cargo, itemType = fuelType) == -1:
    addMessage(message = "Ship fall from sky due to lack of fuel.",
        mType = otherMessage, color = red)
    death(memberIndex = 0, reason = "fall of the ship", ship = playerShip)
    endCombat = true
    return
  var chanceForRun: int = 0
  turnNumber.inc
  case game.enemy.combatAi
  of attacker:
    chanceForRun = turnNumber - 120
  of berserker:
    chanceForRun = turnNumber - 200
  of disarmer:
    chanceForRun = turnNumber - 60
  else:
    discard
  if chanceForRun > 1 and getRandom(min = 1, max = 100) < chanceForRun:
    game.enemy.combatAi = coward
  var pilotIndex, engineerIndex: int = -1
  for index, member in playerShip.crew:
    case member.order
    of pilot:
      pilotIndex = index
      gainExp(amount = 2, skillNumber = pilotingSkill, crewIndex = index)
    of engineer:
      engineerIndex = index
      gainExp(amount = 2, skillNumber = engineeringSkill, crewIndex = index)
    else:
      discard
  if pilotIndex > -1:
    case pilotOrder
    of 1:
      accuracyBonus = 20
      evadeBonus = -10
    of 2:
      accuracyBonus = 10
      evadeBonus = 0
    of 3:
      accuracyBonus = 0
      evadeBonus = 10
    of 4:
      accuracyBonus = -10
      evadeBonus = 20
    else:
      discard
    evadeBonus = evadeBonus + getSkillLevel(member = playerShip.crew[
        pilotIndex], skillIndex = pilotingSkill)
  else:
    accuracyBonus = 20
    evadeBonus = -10
  var enemyPilotIndex = findMember(order = pilot,
      shipCrew = game.enemy.ship.crew)
  if enemyPilotIndex > -1:
    accuracyBonus = accuracyBonus - getSkillLevel(member = game.enemy.ship.crew[
        enemyPilotIndex], skillIndex = pilotingSkill)

# Temporary code for interfacing with Ada

proc getAdaHarpoonDuration(playerDuration, enemyDuration: cint) {.raises: [],
    tags: [], exportc.} =
  harpoonDuration = playerDuration
  game.enemy.harpoonDuration = enemyDuration

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
  adaEnemy.accuracy = game.enemy.accuracy.cint
  adaEnemy.combatAi = game.enemy.combatAi.ord.cint
  adaEnemy.evasion = game.enemy.evasion.cint
  adaEnemy.loot = game.enemy.loot.cint
  adaEnemy.perception = game.enemy.perception.cint
  adaEnemy.name = enemyName.cstring
  for index, gun in game.enemy.guns:
    adaEnemy.guns[index] = [gun[1].cint + 1, gun[2].cint, gun[3].cint]
  if game.enemy.guns.len < 10:
    for index in game.enemy.guns.len .. 9:
      adaEnemy.guns[index] = [-1, -1, -1]
  for index, gun in guns:
    adaEnemy.playerGuns[index] = [gun[1].cint + 1, gun[2].cint, gun[3].cint]
  if guns.len < 10:
    for index in guns.len .. 9:
      adaEnemy.playerGuns[index] = [-1, -1, -1]
  npcShip = game.enemy.ship
