# Copyright 2023-2024 Bartek thindil Jasicki
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
import contracts
import bases, crewinventory, config, game, game2, goals, events, log, maps,
    messages, missions, ships, ships2, shipscargo, shipscrew, shipscrew2,
        shipsmovement,
    statistics, stories, stories2, trades, types, utils

var
  enemyShipIndex*: Natural     ## The index of the enemy's ship's prototype
  factionName: string          ## The name of the enemy's faction (ship and its crew)
  boardingOrders*: seq[int]    ## The list of orders for the boarding party
  pilotOrder*: Natural = 0     ## The player's ship pilot order
  engineerOrder*: Natural      ## The player's ship engineer order
  endCombat*: bool = false     ## If true, the combat ends
  enemyName*: string           ## The name of the enemy's ship
  messagesStarts*: int = -1    ## The starting index of messages to show
  guns*: seq[array[1..3, int]] ## The list of guns installed on the player's ship
  oldSpeed = fullSpeed         ## The speed of the player's ship before combat
  turnNumber: Natural = 0      ## The number of the combat's turn

proc startCombat*(enemyIndex: Positive; newCombat: bool = true): bool {.sideEffect,
    raises: [KeyError, ValueError], tags: [RootEffect], contractual.} =
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
  if protoShipsList[enemyIndex].name.contains(sub = tradersName):
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
        enemyShip.cargo.add(y = InventoryData(protoIndex: newItemIndex,
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
      enemyGuns.add(y = [1: index, 2: 1, 3: shootingSpeed])
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
      guns.add(y = [1: index, 2: 1, 3: modulesList[module.protoIndex].speed])
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

proc combatTurn*() {.sideEffect, raises: [KeyError, IOError, ValueError,
    CrewNoSpaceError, CrewOrderError, Exception], tags: [WriteIOEffect,
    RootEffect], contractual.} =
  ## One turn in the combat, between the ships and the crew members if there
  ## is boarding party on any ship.
  var
    accuracyBonus, evadeBonus = 0
    speedBonus = 0
    ammoIndex2 = -1

  proc attack(ship, enemyShip: var ShipRecord) {.sideEffect, raises: [KeyError,
      IOError], tags: [RootEffect], contractual.} =

    var hitLocation: int = -1

    proc removeGun(moduleIndex: Natural; enemyShip: ShipRecord) {.sideEffect,
        raises: [], tags: [], contractual.} =
      if enemyShip.crew == playerShip.crew:
        for index, gun in guns:
          if gun[1] == moduleIndex:
            guns.delete(i = index)
            break
    proc findEnemyModule(mType: ModuleType;
        enemyShip: ShipRecord): int {.sideEffect, raises: [KeyError], tags: [],
        contractual.} =
      for index, module in enemyShip.modules:
        if modulesList[module.protoIndex].mType == mType and module.durability > 0:
          return index
      return -1
    proc findHitWeapon(enemyShip: ShipRecord) {.sideEffect, raises: [KeyError],
        tags: [], contractual.} =
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
                if gun[1] == mIndex:
                  shoots = gun[3]
                  gunnerOrder = gun[2]
                  if gun[3] > 0:
                    if gunnerOrder != 3:
                      shoots = (shoots.float / 2.0).ceil.int
                    logMessage(message = "Player shoots (no cooldown): " &
                        $shoots, debugType = DebugTypes.combat)
                  elif gun[3] < 0:
                    shoots = 0
                    gun[3].inc
                    if gun[3] == 0:
                      shoots = 1
                      gun[3] = if gunnerOrder == 3:
                          modulesList[playerShip.modules[gun[
                              1]].protoIndex].speed
                        else:
                          modulesList[playerShip.modules[gun[
                              1]].protoIndex].speed - 1
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
              if gun[1] == mIndex:
                if gun[3] > 0:
                  shoots = gun[3]
                elif gun[3] < 0:
                  shoots = 0
                  gun[3].inc
                  if gun[3] == 0:
                    shoots = 1
                    gun[3] = if game.enemy.combatAi == disarmer:
                        modulesList[ship.modules[gun[1]].protoIndex].speed - 1
                      else:
                        modulesList[ship.modules[gun[1]].protoIndex].speed
            if ship.crew.len > 0 and gunnerIndex == -1:
              shoots = 0
          if ammoIndex2 in ship.cargo.low .. ship.cargo.high and itemsList[
              ship.cargo[ammoIndex2].protoIndex].itemType == itemsTypesList[
              modulesList[module.protoIndex].value - 1]:
            ammoIndex = ammoIndex2
          if ammoIndex == -1:
            for iIndex, item in itemsList.pairs:
              if item.itemType == itemsTypesList[modulesList[
                  module.protoIndex].value - 1]:
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
            if findEnemyModule(mType = ModuleType.armor,
                enemyShip = enemyShip) > -1:
              shoots = 0
          if module.mType == ModuleType2.gun and shoots > 0:
            case itemsList[ship.cargo[ammoIndex].protoIndex].value[2]
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
              let armorIndex = findEnemyModule(mType = ModuleType.armor,
                  enemyShip = enemyShip)
              if armorIndex > -1:
                hitLocation = armorIndex
              else:
                if ship.crew == playerShip.crew:
                  if gunnerIndex > -1 and gunnerOrder in 4 .. 6:
                    hitLocation = -1
                    case gunnerOrder
                    of 4:
                      hitLocation = findEnemyModule(mType = ModuleType.engine,
                          enemyShip = enemyShip)
                    of 5:
                      hitLocation = -1
                      findHitWeapon(enemyShip = enemyShip)
                      if hitLocation == -1:
                        hitLocation = findEnemyModule(
                            mType = ModuleType.batteringRam,
                            enemyShip = enemyShip)
                    of 6:
                      hitLocation = findEnemyModule(mType = ModuleType.hull,
                          enemyShip = enemyShip)
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
                    ammoIndex].protoIndex].value[1]
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
                      removeGun(moduleIndex = weaponIndex,
                          enemyShip = enemyShip)
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
  if engineerIndex > -1 or "sentientships" in factionsList[playerShip.crew[
      0].faction].flags:
    let message = changeShipSpeed(speedValue = engineerOrder.ShipSpeed)
    if message.len > 0:
      addMessage(message = message, mType = orderMessage, color = red)
  speedBonus = 20 - (realSpeed(ship = playerShip) / 100).int
  if speedBonus < -10:
    speedBonus = -10
  accuracyBonus = accuracyBonus + speedBonus
  evadeBonus = evadeBonus - speedBonus
  var
    damageRange = 10_000
    enemyWeaponIndex = -1
  for index, module in game.enemy.ship.modules:
    if module.durability == 0 or module.mType notin {ModuleType2.gun,
        batteringRam, harpoonGun}:
      continue
    if module.mType in {ModuleType2.gun, harpoonGun}:
      if module.mType == ModuleType2.gun and damageRange > 5_000:
        damageRange = 5_000
      elif damageRange > 2_000:
        damageRange = 2_000
      ammoIndex2 = if module.mType == ModuleType2.gun:
          module.ammoIndex
        else:
          module.harpoonIndex
      var enemyAmmoIndex = -1
      if ammoIndex2 in game.enemy.ship.cargo.low .. game.enemy.ship.cargo.high:
        if itemsList[game.enemy.ship.cargo[ammoIndex2].protoIndex].itemType ==
            itemsTypesList[modulesList[module.protoIndex].value - 1]:
          enemyAmmoIndex = ammoIndex2
      if enemyAmmoIndex == -1:
        for iindex, item in itemsList:
          if item.itemType == itemsTypesList[modulesList[
              module.protoIndex].value - 1]:
            for cindex, cargo in game.enemy.ship.cargo:
              if cargo.protoIndex == iindex:
                enemyAmmoIndex = cindex
                break
            if enemyAmmoIndex > -1:
              break
      if enemyAmmoIndex == -1 and game.enemy.combatAi in {attacker, disarmer}:
        game.enemy.combatAi = coward
        break
    elif damageRange > 100:
      damageRange = 100
    enemyWeaponIndex = index
  if enemyWeaponIndex == -1 and game.enemy.combatAi in {attacker, disarmer}:
    game.enemy.combatAi = coward
  var enemyPilotOrder = 2
  case game.enemy.combatAi
  of berserker:
    if game.enemy.distance > 10 and game.enemy.ship.speed != fullSpeed:
      game.enemy.ship.speed.inc
      addMessage(message = enemyName & " increases speed.",
          mType = combatMessage)
      enemyPilotOrder = 1
    elif game.enemy.distance <= 10 and game.enemy.ship.speed == fullSpeed:
      game.enemy.ship.speed.dec
      addMessage(message = enemyName & " decreases speed.",
          mType = combatMessage)
      enemyPilotOrder = 2
  of attacker, disarmer:
    if game.enemy.distance > damageRange and game.enemy.ship.speed != fullSpeed:
      game.enemy.ship.speed.inc
      addMessage(message = enemyName & " increases speed.",
          mType = combatMessage)
      enemyPilotOrder = 1
    elif game.enemy.distance <= damageRange and game.enemy.ship.speed == fullSpeed:
      game.enemy.ship.speed.dec
      addMessage(message = enemyName & " decreases speed.",
          mType = combatMessage)
      enemyPilotOrder = 2
  of coward:
    if game.enemy.distance < 15_000 and game.enemy.ship.speed != fullSpeed:
      game.enemy.ship.speed.inc
      addMessage(message = enemyName & " increases speed.",
          mType = combatMessage)
    enemyPilotOrder = 4
  else:
    discard
  if game.enemy.harpoonDuration > 0:
    game.enemy.ship.speed = fullStop
    addMessage(message = enemyName & " is stopped by your ship.",
        mType = combatMessage)
  elif game.enemy.ship.speed == fullStop:
    game.enemy.ship.speed = quarterSpeed
  if harpoonDuration > 0:
    playerShip.speed = fullStop
    addMessage(message = "You are stopped by the enemy's ship.",
        mType = combatMessage)
  case enemyPilotOrder
  of 1:
    accuracyBonus = accuracyBonus + 20
    evadeBonus = evadeBonus - 20
  of 2:
    accuracyBonus = accuracyBonus + 10
    evadeBonus = evadeBonus - 10
  of 3:
    accuracyBonus = accuracyBonus - 10
    evadeBonus = evadeBonus + 10
  of 4:
    accuracyBonus = accuracyBonus - 20
    evadeBonus = evadeBonus + 20
  else:
    discard
  speedBonus = 20 - (realSpeed(ship = game.enemy.ship) / 100).int
  if speedBonus < -10:
    speedBonus = -10
  accuracyBonus = accuracyBonus + speedBonus
  evadeBonus = evadeBonus - speedBonus
  var distanceTraveled = if enemyPilotOrder < 4: -(realSpeed(
      ship = game.enemy.ship))
      else:
        realSpeed(ship = game.enemy.ship)
  if pilotIndex > -1:
    case pilotOrder
    of 1, 3:
      distanceTraveled = distanceTraveled - realSpeed(ship = playerShip)
    of 2:
      distanceTraveled = distanceTraveled + realSpeed(ship = playerShip)
      if distanceTraveled > 0 and enemyPilotOrder != 4:
        distanceTraveled = 0
    of 4:
      distanceTraveled = distanceTraveled + realSpeed(ship = playerShip)
    else:
      discard
  else:
    distanceTraveled = distanceTraveled - realSpeed(ship = playerShip)
  game.enemy.distance = game.enemy.distance + distanceTraveled
  if game.enemy.distance < 10:
    game.enemy.distance = 10
  if game.enemy.distance >= 15_000:
    if pilotOrder == 4:
      addMessage(message = "You escaped the " & enemyName & ".",
          mType = combatMessage)
    else:
      addMessage(message = enemyName & " escaped from you.",
          mType = combatMessage)
    for index, member in playerShip.crew.mpairs:
      if member.order == boarding:
        death(memberIndex = index, reason = "enemy crew", ship = playerShip,
            createBody = false)
    endCombat = true
    return
  elif game.enemy.distance < 15_000 and game.enemy.distance >= 10_000:
    accuracyBonus = accuracyBonus - 10
    evadeBonus = evadeBonus + 10
    logMessage(message = "Distance: long", debugType = DebugTypes.combat)
  elif game.enemy.distance < 5_000 and game.enemy.distance >= 1_000:
    accuracyBonus = accuracyBonus + 10
    logMessage(message = "Distance: medium", debugType = DebugTypes.combat)
  elif game.enemy.distance < 1_000:
    accuracyBonus = accuracyBonus + 20
    evadeBonus = evadeBonus - 10
    logMessage(message = "Distance: short or close",
        debugType = DebugTypes.combat)
  attack(ship = playerShip, enemyShip = game.enemy.ship)
  if not endCombat:
    attack(ship = game.enemy.ship, enemyShip = playerShip)
  if not endCombat:
    var haveBoardingParty = false

    proc meleeCombat(attackers, defenders: var seq[MemberData];
        playerAttack: bool) =
      var
        attackDone, riposte = false
        attackerIndex, defenderIndex, orderIndex = 0

      proc characterAttack(attackerIndex2, defenderIndex2: Natural;
          playerAttack2: bool): bool =
        let
          hitLocation = getRandom(min = helmet.int,
              max = legs.int).EquipmentLocations
          locationNames: array[helmet .. legs, string] = ["head", "torso",
              "arm", "leg"]
        var
          attacker = if playerAttack2: playerShip.crew[attackerIndex2]
            else:
              game.enemy.ship.crew[attackerIndex2]
          defender = if playerAttack2: game.enemy.ship.crew[defenderIndex2]
            else:
              playerShip.crew[defenderIndex2]
          baseDamage = attacker.attributes[strengthIndex].level
        if attacker.equipment[weapon] > -1:
          baseDamage = baseDamage + itemsList[attacker.inventory[
              attacker.equipment[weapon]].protoIndex].value[2]
        var
          wounds = 1.0 - (attacker.health.float / 100.0)
          damage = (baseDamage - (baseDamage.float * wounds.float).int)
        if attacker.thirst > 40:
          wounds = 1.0 - (attacker.thirst.float / 100.0)
          damage = damage - (baseDamage.float * wounds.float).int
        if attacker.hunger > 80:
          wounds = 1.0 - (attacker.hunger.float / 100.0)
          damage = damage - (baseDamage.float * wounds.float).int
        damage = if playerAttack2:
            (damage.float * newGameSettings.playerMeleeDamageBonus).int
          else:
            (damage.float * newGameSettings.enemyMeleeDamageBonus).int
        var
          hitChance = 0
          attackSkill = 0
        if attacker.equipment[weapon] > -1:
          attackSkill = getSkillLevel(member = attacker,
              skillIndex = itemsList[attacker.inventory[attacker.equipment[
              weapon]].protoIndex].value[3])
          hitChance = attackSkill + getRandom(min = 1, max = 50)
        else:
          hitChance = getSkillLevel(member = attacker,
              skillIndex = unarmedSkill) + getRandom(min = 1, max = 50)
        hitChance = hitChance - (getSkillLevel(member = defender,
            skillIndex = dodgeSkill) + getRandom(min = 1, max = 50))
        for i in helmet .. legs:
          if defender.equipment[i] > -1:
            hitChance = hitChance + itemsList[defender.inventory[
                defender.equipment[i]].protoIndex].value[3]
        if defender.equipment[hitLocation] > -1:
          damage = damage - itemsList[defender.inventory[defender.equipment[
              hitLocation]].protoIndex].value[2]
        if defender.equipment[shield] > -1:
          damage = damage - itemsList[defender.inventory[defender.equipment[
              shield]].protoIndex].value[2]
        if attacker.equipment[weapon] == -1:
          var damageBonus = getSkillLevel(member = attacker,
              skillIndex = unarmedSkill) / 200
          if damageBonus == 0:
            damageBonus = 1
          damage = damage + damageBonus.int
        let faction = factionsList[defender.faction]
        if "naturalarmor" in faction.flags:
          damage = (damage / 2).int
        if "toxicattack" in factionsList[attacker.faction].flags and
            attacker.equipment[weapon] == -1 and "diseaseimmune" notin faction.flags:
          damage = if damage * 10 < 30:
              damage * 10
            else:
              damage + 30
        if damage < 1:
          damage = 1
        if attacker.equipment[weapon] > -1:
          if itemsList[attacker.inventory[attacker.equipment[
              weapon]].protoIndex].value[5] == 1:
            damage = (damage.float * 1.5).int
          elif itemsList[attacker.inventory[attacker.equipment[
              weapon]].protoIndex].value[5] == 2:
            damage = damage * 2
        var
          attackMessage = if playerAttack2:
              attacker.name & " attacks " & defender.name & " (" & factionName & ")"
            else:
              attacker.name & " (" & factionName & ") attacks " & defender.name
          messageColor = white
        if hitChance < 1:
          attackMessage = attackMessage & " and misses."
          messageColor = if playerAttack: blue else: cyan
          if not playerAttack:
            gainExp(amount = 2, skillNumber = dodgeSkill,
                crewIndex = defenderIndex2)
            defender.skills = playerShip.crew[defenderIndex2].skills
            defender.attributes = playerShip.crew[defenderIndex2].attributes
        else:
          attackMessage = attackMessage & " and hit " & locationNames[
              hitLocation] & "."
          messageColor = if playerAttack2: green else: yellow
          if attacker.equipment[weapon] > -1:
            if playerAttack:
              damageItem(inventory = attacker.inventory,
                  itemIndex = attacker.equipment[weapon],
                  skillLevel = attackSkill, memberIndex = attackerIndex2,
                  ship = playerShip)
            else:
              damageItem(inventory = attacker.inventory,
                  itemIndex = attacker.equipment[weapon],
                  skillLevel = attackSkill, memberIndex = attackerIndex2,
                  ship = game.enemy.ship)
          if defender.equipment[hitLocation] > -1:
            if playerAttack:
              damageItem(inventory = defender.inventory,
                  itemIndex = defender.equipment[hitLocation], skillLevel = 0,
                  memberIndex = defenderIndex2, ship = game.enemy.ship)
            else:
              damageItem(inventory = defender.inventory,
                  itemIndex = defender.equipment[hitLocation], skillLevel = 0,
                  memberIndex = defenderIndex2, ship = playerShip)
          if playerAttack2:
            if attacker.equipment[weapon] > -1:
              gainExp(amount = 2, skillNumber = itemsList[attacker.inventory[
                  attacker.equipment[weapon]].protoIndex].value[3],
                  crewIndex = attackerIndex2)
            else:
              gainExp(amount = 2, skillNumber = unarmedSkill,
                  crewIndex = attackerIndex2)
            attacker.skills = playerShip.crew[attackerIndex2].skills
            attacker.attributes = playerShip.crew[attackerIndex2].attributes
          defender.health = if damage > defender.health: 0 else: defender.health - damage
        addMessage(message = attackMessage, mType = combatMessage,
            color = messageColor)
        if attacker.tired + 1 <= SkillRange.high:
          attacker.tired.inc
        if defender.tired + 1 <= SkillRange.high:
          defender.tired.inc
        if playerAttack2:
          playerShip.crew[attackerIndex2] = attacker
          game.enemy.ship.crew[defenderIndex2] = defender
        else:
          playerShip.crew[defenderIndex2] = defender
          game.enemy.ship.crew[attackerIndex2] = attacker
        if defender.health == 0:
          if playerAttack2:
            death(memberIndex = defenderIndex2, reason = attacker.name &
                " blow in melee combat", ship = game.enemy.ship)
            for order in boardingOrders.mitems:
              if order > defenderIndex2:
                order.dec
            updateKilledMobs(mob = defender, factionName = factionName)
            updateGoal(goalType = kill, targetIndex = factionName)
            if game.enemy.ship.crew.len == 0:
              endCombat = true
          else:
            orderIndex = -1
            for index, member in playerShip.crew:
              if member.order == boarding:
                orderIndex.inc
              if index == defenderIndex2:
                boardingOrders.delete(i = orderIndex)
                orderIndex.dec
                break
            death(memberIndex = defenderIndex2, reason = attacker.name &
                " blow in melee combat", ship = playerShip)
            if defenderIndex2 == 0:
              endCombat = true
          return false
        return true

      attackerIndex = attackers.low
      orderIndex = 0
      while attackerIndex < attackers.len:
        riposte = true
        if attackers[attackerIndex].order != boarding:
          attackerIndex.inc
          continue
        attackDone = false
        if playerAttack:
          if orderIndex notin boardingOrders.low .. boardingOrders.high:
            break
          if boardingOrders[orderIndex] in defenders.low .. defenders.high:
            defenderIndex = boardingOrders[orderIndex]
            riposte = characterAttack(attackerIndex2 = attackerIndex,
                defenderIndex2 = defenderIndex, playerAttack2 = playerAttack)
            if not endCombat and riposte:
              if game.enemy.ship.crew[defenderIndex].order != defend:
                giveOrders(ship = game.enemy.ship, memberIndex = defenderIndex,
                    givenOrder = defend, moduleIndex = 0,
                    checkPriorities = false)
              riposte = characterAttack(attackerIndex2 = defenderIndex,
                  defenderIndex2 = attackerIndex,
                  playerAttack2 = not playerAttack)
            else:
              riposte = true
            attackDone = true
          elif boardingOrders[orderIndex] == -1:
            giveOrders(ship = playerShip, memberIndex = attackerIndex,
                givenOrder = rest)
            boardingOrders.delete(i = orderIndex)
            orderIndex.dec
            attackDone = true
          orderIndex.inc
        if not attackDone:
          for dIndex, defender in defenders:
            if defender.order == defend:
              riposte = characterAttack(attackerIndex2 = attackerIndex,
                  defenderIndex2 = dIndex, playerAttack2 = playerAttack)
              if not endCombat and riposte:
                riposte = characterAttack(attackerIndex2 = dIndex,
                    defenderIndex2 = attackerIndex,
                    playerAttack2 = not playerAttack)
              else:
                riposte = true
              attackDone = true
              break
        if not attackDone:
          defenderIndex = getRandom(min = defenders.low, max = defenders.high)
          if playerAttack:
            giveOrders(ship = game.enemy.ship, memberIndex = defenderIndex,
                givenOrder = defend, moduleIndex = 0, checkPriorities = false)
          else:
            giveOrders(ship = playerShip, memberIndex = defenderIndex,
                givenOrder = defend, moduleIndex = 0, checkPriorities = false)
          riposte = characterAttack(attackerIndex2 = attackerIndex,
              defenderIndex2 = defenderIndex, playerAttack2 = playerAttack)
          if not endCombat and riposte:
            riposte = characterAttack(attackerIndex2 = defenderIndex,
                defenderIndex2 = attackerIndex,
                playerAttack2 = not playerAttack)
          else:
            riposte = true
        if endCombat:
          break
        if riposte:
          attackerIndex.inc
      defenderIndex = defenders.low
      while defenderIndex < defenders.len:
        riposte = true
        if defenders[defenderIndex].order == defend:
          for aIndex, attacker in attackers:
            if attacker.order == boarding:
              riposte = characterAttack(attackerIndex2 = defenderIndex,
                  defenderIndex2 = aIndex, playerAttack2 = not playerAttack)
              if not endCombat and riposte:
                riposte = characterAttack(attackerIndex2 = aIndex,
                    defenderIndex2 = defenderIndex,
                    playerAttack2 = playerAttack)
              break
        if riposte:
          defenderIndex.inc
        if findMember(order = boarding) == -1:
          updateOrders(ship = game.enemy.ship)

    for member in playerShip.crew:
      if member.order == boarding:
        haveBoardingParty = true
        break
    for member in game.enemy.ship.crew:
      if member.order == boarding:
        haveBoardingParty = true
        break
    if game.enemy.harpoonDuration > 0 or harpoonDuration > 0 or haveBoardingParty:
      if not endCombat and game.enemy.ship.crew.len > 0:
        meleeCombat(attackers = playerShip.crew,
            defenders = game.enemy.ship.crew, playerAttack = true)
      if not endCombat and game.enemy.ship.crew.len > 0:
        meleeCombat(attackers = game.enemy.ship.crew,
            defenders = playerShip.crew, playerAttack = false)
  if not endCombat:
    if game.enemy.harpoonDuration > 0:
      game.enemy.harpoonDuration.dec
    if harpoonDuration > 0:
      harpoonDuration.dec
    if game.enemy.harpoonDuration > 0 or harpoonDuration > 0:
      updateOrders(ship = playerShip, combat = true)
    updateGame(minutes = 1, inCombat = true)
  elif playerShip.crew[0].health > 0:
    var
      wasBoarded = false
      lootAmount = 0
    if findMember(order = boarding) > -1:
      wasBoarded = true
    game.enemy.ship.modules[0].durability = 0
    addMessage(message = enemyName & " is destroyed!", mType = combatMessage)
    lootAmount = game.enemy.loot
    var shipFreeSpace = freeCargo(amount = -lootAmount)
    if shipFreeSpace < 0:
      lootAmount = lootAmount + shipFreeSpace
    if lootAmount > 0:
      addMessage(message = "You looted " & $lootAmount & " " & moneyName &
          " from " & enemyName & ".", mType = combatMessage)
      updateCargo(ship = playerShip, protoIndex = moneyIndex,
          amount = lootAmount)
    shipFreeSpace = freeCargo(amount = 0)
    if wasBoarded and shipFreeSpace > 0:
      var message = "Additionally, your boarding party takes from " &
          enemyName & ":"
      for item in game.enemy.ship.cargo:
        lootAmount = (item.amount / 5).int
        shipFreeSpace = freeCargo(amount = -lootAmount)
        if shipFreeSpace < 0:
          lootAmount = lootAmount + shipFreeSpace
        if itemsList[item.protoIndex].price == 0 and item.protoIndex != moneyIndex:
          lootAmount = 0
        if lootAmount > 0:
          if item != game.enemy.ship.cargo[0]:
            message = message & ","
          updateCargo(ship = playerShip, protoIndex = item.protoIndex,
              amount = lootAmount)
          message = message & " " & $lootAmount & " " & itemsList[
              item.protoIndex].name
          shipFreeSpace = freeCargo(amount = 0)
          if item == game.enemy.ship.cargo[game.enemy.ship.cargo.high] or
              shipFreeSpace == 0:
            break
      addMessage(message = message & ".", mType = combatMessage)
      if currentStory.index.len == 0:
        startStory(factionName = factionName, condition = dropItem)
      else:
        let step = if currentStory.currentStep == 0:
            storiesList[currentStory.index].startingStep
          elif currentStory.currentStep > 0:
            storiesList[currentStory.index].steps[currentStory.currentStep]
          else:
            storiesList[currentStory.index].finalStep
        if step.finishCondition == loot:
          let stepData = currentStory.data.split(sep = ';')
          if stepData[1] == "any" or stepData[1] == $enemyShipIndex:
            if progressStory():
              case step.finishCondition
              of loot:
                updateCargo(ship = playerShip, protoIndex = stepData[
                    0].parseInt, amount = 1)
              else:
                discard
    for mIndex, member in playerShip.crew:
      if member.order in {boarding, defend}:
        giveOrders(ship = playerShip, memberIndex = mIndex, givenOrder = rest)
    game.enemy.ship.speed = fullStop
    playerShip.speed = oldSpeed
    if skyMap[playerShip.skyX][playerShip.skyY].eventIndex > -1:
      if eventsList[skyMap[playerShip.skyX][
          playerShip.skyY].eventIndex].eType == attackOnBase:
        gainRep(baseIndex = skyMap[playerShip.skyX][playerShip.skyY].baseIndex, points = 5)
      deleteEvent(eventIndex = skyMap[playerShip.skyX][
          playerShip.skyY].eventIndex)
    if skyMap[playerShip.skyX][playerShip.skyY].missionIndex > -1 and
        acceptedMissions[skyMap[playerShip.skyX][
        playerShip.skyY].missionIndex].mType == destroy and protoShipsList[
        acceptedMissions[skyMap[playerShip.skyX][
        playerShip.skyY].missionIndex].shipIndex].name == game.enemy.ship.name:
      updateMission(missionIndex = skyMap[playerShip.skyX][
          playerShip.skyY].missionIndex)
    var lostReputationChance = 10
    if protoShipsList[enemyShipIndex].owner == playerShip.crew[0].faction:
      lostReputationChance = 40
    if getRandom(min = 1, max = 100) < lostReputationChance:
      gainRep(baseIndex = game.enemy.ship.homeBase, points = -100)
    updateDestroyedShips(shipName = game.enemy.ship.name)
    updateGoal(goalType = GoalTypes.destroy, targetIndex = $enemyShipIndex)
    if currentGoal.targetIndex.len > 0:
      updateGoal(goalType = GoalTypes.destroy, targetIndex = protoShipsList[
          enemyShipIndex].owner)
    if currentStory.index.len > 0:
      let finishCondition = if currentStory.currentStep == 0:
          storiesList[currentStory.index].startingStep.finishCondition
        elif currentStory.currentStep > 0:
          storiesList[currentStory.index].steps[
              currentStory.currentStep].finishCondition
        else: storiesList[currentStory.index].finalStep.finishCondition
      if finishCondition != destroyShip:
        return
      let storyData = currentStory.data.split(sep = ';')
      if playerShip.skyX == storyData[0].parseInt and playerShip.skyY ==
          storyData[1].parseInt and enemyShipIndex == storyData[2].parseInt:
        if not progressStory(nextStep = true):
          return

# Temporary code for interfacing with Ada

proc getAdaHarpoonDuration(playerDuration, enemyDuration: cint) {.raises: [],
    tags: [], exportc, contractual.} =
  harpoonDuration = playerDuration
  game.enemy.harpoonDuration = enemyDuration

proc startAdaCombat(enemyIndex, newCombat: cint): cint {.raises: [], tags: [
    RootEffect], exportc, contractual.} =
  try:
    return startCombat(enemyIndex = enemyIndex, newCombat = newCombat == 1).cint
  except ValueError:
    return 0

type
  AdaGunsArray = array[10, array[3, cint]]

  AdaEnemyData = object
    loot: cint
    guns: AdaGunsArray
    playerGuns: AdaGunsArray
    distance: cint
    harpoonDuration: cint
    enemyHarpoonDuration: cint

  AdaBoardingOrders = array[50, cint]

proc getAdaEnemy(adaEnemy: var AdaEnemyData) {.raises: [], tags: [], exportc,
    contractual.} =
  adaEnemy.loot = game.enemy.loot.cint
  adaEnemy.distance = game.enemy.distance.cint
  adaEnemy.harpoonDuration = harpoonDuration.cint
  adaEnemy.enemyHarpoonDuration = game.enemy.harpoonDuration.cint
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

proc combatAdaTurn() {.raises: [], tags: [WriteIOEffect, RootEffect], exportc,
    contractual.} =
  try:
    combatTurn()
    npcShip = game.enemy.ship
  except:
    discard

proc setAdaGuns(adaGuns: AdaGunsArray) {.raises: [], tags: [], exportc,
    contractual.} =
  guns = @[]
  for gun in adaGuns:
    if gun[0] == -1:
      break
    guns.add(y = [gun[0].int - 1, gun[1].int, gun[2].int])

proc getAdaBoardingOrders(adaOrders: var AdaBoardingOrders) {.raises: [],
    tags: [], exportc, contractual.} =
  for order in adaOrders.mitems:
    order = -1
  for index, order in boardingOrders:
    adaOrders[index] = order.cint

proc setAdaBoardingOrders(adaOrders: AdaBoardingOrders) {.raises: [], tags: [],
    exportc, contractual.} =
  boardingOrders = @[]
  for order in adaOrders:
    if order == -1:
      break
    boardingOrders.add(y = order)

proc setAdaEnemyName(): cstring {.raises: [], tags: [], exportc, contractual.} =
  return enemyName.cstring

proc getAdaPilotOrder(order: cint) {.raises: [], tags: [], exportc,
    contractual.} =
  pilotOrder = order

proc setAdaPilotOrder(): cint {.raises: [], tags: [], exportc, contractual.} =
  return pilotOrder.cint

proc getAdaEngineerOrder(order: cint) {.raises: [], tags: [], exportc,
    contractual.} =
  engineerOrder = order

proc setAdaEngineerOrder(): cint {.raises: [], tags: [], exportc,
    contractual.} =
  return engineerOrder.cint

proc getAdaEndCombat(): cint {.raises: [], tags: [], exportc, contractual.} =
  return endCombat.cint
