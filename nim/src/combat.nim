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

## Provides code related to combat with NPC enemies, like combat between
## the player's ship and NPC's ship or combat between ships' crews.

import std/[math, strutils, tables]
import contracts
import bases, crewinventory, config, game, game2, goals, events, log, maps,
    messages, missions, ships, ships2, shipscargo, shipscrew, shipscrew2,
    shipsmovement, statistics, stories, stories2, trades, types, utils

var
  enemyShipIndex*: Natural = 0
    ## The index of the enemy's ship's prototype
  factionName: string = ""
    ## The name of the enemy's faction (ship and its crew)
  boardingOrders*: seq[int] = @[]
    ## The list of orders for the boarding party
  pilotOrder*: Natural = 0
    ## The player's ship pilot order
  engineerOrder*: Natural = 0
    ## The player's ship engineer order
  endCombat*: bool = false
    ## If true, the combat ends
  enemyName*: string = ""
    ## The name of the enemy's ship
  messagesStarts*: int = -1
    ## The starting index of messages to show
  guns*: seq[array[1..3, int]] = @[]
    ## The list of guns installed on the player's ship
  oldSpeed: ShipSpeed = fullSpeed
    ## The speed of the player's ship before combat
  turnNumber: Natural = 0
    ## The number of the combat's turn

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
  var enemyShip: ShipRecord = createShip(protoIndex = enemyIndex, name = "",
      x = playerShip.skyX, y = playerShip.skyY, speed = fullSpeed)
  # Enemy ship is a trader, generate a cargo for it
  if protoShipsList[enemyIndex].name.contains(sub = tradersName):
    generateTraderCargo(protoIndex = enemyIndex)
    for item in traderCargo:
      updateCargo(ship = enemyShip, protoIndex = item.protoIndex,
          amount = item.amount)
    traderCargo = @[]
  var minFreeSpace: int = 0
  for module in enemyShip.modules:
    if module.mType == ModuleType2.cargoRoom and module.durability > 0:
      minFreeSpace += modulesList[module.protoIndex].maxValue
  minFreeSpace = (minFreeSpace.float * (1.0 - (getRandom(min = 20,
      max = 70).float / 100.0))).Natural
  while freeCargo(amount = 0, ship = enemyShip) > minFreeSpace:
    var
      itemIndex: Natural = getRandom(min = 1, max = itemsList.len)
      newItemIndex: Natural = 0
    for i in 1 .. itemsList.len:
      itemIndex.dec
      if itemIndex == 0:
        newItemIndex = i
        break
    let
      itemAmount: Positive = if enemyShip.crew.len < 5:
          getRandom(min = 1, max = 100)
        elif enemyShip.crew.len < 10:
          getRandom(min = 1, max = 500)
        else:
          getRandom(min = 1, max = 1000)
      cargoItemIndex: int = findItem(inventory = enemyShip.cargo,
          protoIndex = newItemIndex)
    if cargoItemIndex > -1:
      enemyShip.cargo[cargoItemIndex].amount += itemAmount
    else:
      if freeCargo(amount = 0 - (itemsList[newItemIndex].weight * itemAmount)) > -1:
        enemyShip.cargo.add(y = InventoryData(protoIndex: newItemIndex,
            amount: itemAmount, durability: defaultItemDurability, name: "", price: 0))
  var enemyGuns: seq[array[1..3, int]] = @[]
  for index, module in enemyShip.modules:
    if module.mType in {ModuleType2.gun, harpoonGun} and module.durability > 0:
      var shootingSpeed: int = 0
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
  let oldGunsList: seq[array[1..3, int]] = guns
  var sameList: bool = true
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
          result += getSkillLevel(member = member,
              skillIndex = perceptionSkill)
          if spotter.crew == playerShip.crew:
            gainExp(amount = 1, skillNumber = perceptionSkill,
                crewIndex = index)
        of gunner:
          result += getSkillLevel(member = member,
              skillIndex = perceptionSkill)
          if spotter.crew == playerShip.crew:
            gainExp(amount = 1, skillNumber = perceptionSkill,
                crewIndex = index)
        else:
          discard
      for module in spotted.modules:
        if module.mType == ModuleType2.hull:
          result += module.maxModules
          break

    let
      playerPerception: Natural = countPerception(spotter = playerShip,
        spotted = enemyShip)
      enemyPerception: Natural = (if game.enemy.perception >
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

proc finishCombat() {.sideEffect, raises: [KeyError, ValueError, CrewOrderError,
    CrewNoSpaceError, IOError, Exception], tags: [RootEffect], contractual.} =
  ## Finish the combat when the player wins, count loot and show information
  ## about it. Also update statistics plus the current goal.
  var
    wasBoarded: bool = false
    lootAmount: Natural = 0
  if findMember(order = boarding) > -1:
    wasBoarded = true
  game.enemy.ship.modules[0].durability = 0
  addMessage(message = enemyName & " is destroyed!", mType = combatMessage)
  lootAmount = game.enemy.loot
  var shipFreeSpace: int = freeCargo(amount = -lootAmount)
  if shipFreeSpace < 0:
    lootAmount += shipFreeSpace
  if lootAmount > 0:
    addMessage(message = "You looted " & $lootAmount & " " & moneyName &
        " from " & enemyName & ".", mType = combatMessage)
    updateCargo(ship = playerShip, protoIndex = moneyIndex,
        amount = lootAmount)
  shipFreeSpace = freeCargo(amount = 0)
  if wasBoarded and shipFreeSpace > 0:
    var message: string = "Additionally, your boarding party takes from " &
        enemyName & ":"
    for item in game.enemy.ship.cargo:
      lootAmount = (item.amount / 5).int
      shipFreeSpace = freeCargo(amount = -lootAmount)
      if shipFreeSpace < 0:
        lootAmount += shipFreeSpace
      if itemsList[item.protoIndex].price == 0 and item.protoIndex != moneyIndex:
        lootAmount = 0
      if lootAmount > 0:
        if item != game.enemy.ship.cargo[0]:
          message &= ","
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
      let step: StepData = if currentStory.currentStep == 0:
          storiesList[currentStory.index].startingStep
        elif currentStory.currentStep > 0:
          storiesList[currentStory.index].steps[currentStory.currentStep]
        else:
          storiesList[currentStory.index].finalStep
      if step.finishCondition == loot:
        let stepData: seq[string] = currentStory.data.split(sep = ';')
        if stepData[1] == "any" or stepData[1] == $enemyShipIndex:
          if progressStory():
            if step.finishCondition == loot:
              updateCargo(ship = playerShip, protoIndex = stepData[
                  0].parseInt, amount = 1)
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
  var lostReputationChance: Positive = 10
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
    let finishCondition: StepConditionType = if currentStory.currentStep == 0:
        storiesList[currentStory.index].startingStep.finishCondition
      elif currentStory.currentStep > 0:
        storiesList[currentStory.index].steps[
            currentStory.currentStep].finishCondition
      else: storiesList[currentStory.index].finalStep.finishCondition
    if finishCondition != destroyShip:
      return
    let storyData: seq[string] = currentStory.data.split(sep = ';')
    if playerShip.skyX == storyData[0].parseInt and playerShip.skyY ==
        storyData[1].parseInt and enemyShipIndex == storyData[2].parseInt:
      if not progressStory(nextStep = true):
        return

proc countMeleeDamage(attacker, defender: MemberData; playerAttack2: bool;
    hitLocation: EquipmentLocations): tuple [damage, hitChance,
    attackSkill: int] {.sideEffect, raises: [KeyError], tags: [], contractual.} =
  ## Count damage, hit chance and skill used by attacker for melee combat
  ##
  ## * attacker      - the crew member who was attacking
  ## * defender      - the crew member who was hit
  ## * playerAttack2 - if true, the attacker is from the player's ship's
  ##                   crew
  ## * hitLocation   - the location on defender body where damage is taken
  ##
  ## Returns tuple with information about damage done, hit chance and index
  ## of the skill used by the attacker.
  var baseDamage: Natural = attacker.attributes[strengthIndex].level
  if attacker.equipment[weapon] > -1:
    baseDamage += itemsList[attacker.inventory[
        attacker.equipment[weapon]].protoIndex].value[2]
  var wounds: float = 1.0 - (attacker.health.float / 100.0)
  result.damage = (baseDamage - (baseDamage.float * wounds.float).int)
  if attacker.thirst > 40:
    wounds = 1.0 - (attacker.thirst.float / 100.0)
    result.damage -= (baseDamage.float * wounds.float).int
  if attacker.hunger > 80:
    wounds = 1.0 - (attacker.hunger.float / 100.0)
    result.damage -= (baseDamage.float * wounds.float).int
  result.damage = if playerAttack2:
      (result.damage.float * newGameSettings.playerMeleeDamageBonus).int
    else:
      (result.damage.float * newGameSettings.enemyMeleeDamageBonus).int
  result.hitChance = 0
  result.attackSkill = 0
  if attacker.equipment[weapon] > -1:
    result.attackSkill = getSkillLevel(member = attacker,
        skillIndex = itemsList[attacker.inventory[attacker.equipment[
        weapon]].protoIndex].value[3])
    result.hitChance = result.attackSkill + getRandom(min = 1, max = 50)
  else:
    result.hitChance = getSkillLevel(member = attacker,
        skillIndex = unarmedSkill) + getRandom(min = 1, max = 50)
  result.hitChance -= (getSkillLevel(member = defender,
      skillIndex = dodgeSkill) + getRandom(min = 1, max = 50))
  for i in helmet .. legs:
    if defender.equipment[i] > -1:
      result.hitChance += itemsList[defender.inventory[
          defender.equipment[i]].protoIndex].value[3]
  if defender.equipment[hitLocation] > -1:
    result.damage -= itemsList[defender.inventory[defender.equipment[
        hitLocation]].protoIndex].value[2]
  if defender.equipment[shield] > -1:
    result.damage -= itemsList[defender.inventory[defender.equipment[
        shield]].protoIndex].value[2]
  if attacker.equipment[weapon] == -1:
    var damageBonus: float = getSkillLevel(member = attacker,
        skillIndex = unarmedSkill) / 200
    if damageBonus == 0:
      damageBonus = 1
    result.damage += damageBonus.int
  let faction: FactionData = factionsList[defender.faction]
  if "naturalarmor" in faction.flags:
    result.damage = (result.damage / 2).int
  if "toxicattack" in factionsList[attacker.faction].flags and
      attacker.equipment[weapon] == -1 and "diseaseimmune" notin faction.flags:
    result.damage = if result.damage * 10 < 30:
        result.damage * 10
      else:
        result.damage + 30
  if result.damage < 1:
    result.damage = 1
  if attacker.equipment[weapon] > -1:
    if itemsList[attacker.inventory[attacker.equipment[
        weapon]].protoIndex].value[5] == 1:
      result.damage = (result.damage.float * 1.5).int
    elif itemsList[attacker.inventory[attacker.equipment[
        weapon]].protoIndex].value[5] == 2:
      result.damage *= 2

proc characterAttack(attackerIndex2, defenderIndex2: Natural; playerAttack,
    playerAttack2: bool; orderIndex: var int): bool {.sideEffect, raises: [
    KeyError, CrewNoSpaceError, IOError], tags: [WriteIOEffect], contractual.} =
  ## The attack of the selected crew member on its target
  ##
  ## * attackerIndex2 - the index of the crew member which attacks
  ## * defenderIndex2 - the index of the crew member which is attacked
  ## * playerAttack   - if true, the attacker is the player's ship
  ## * playerAttack2  - if true, the attacker is from the player's ship's
  ##                    crew
  ## * orderIndex     - the index of the boarding order for the attacker
  ##
  ## Returns true if the defender survived the attack, otherwise false
  let hitLocation: EquipmentLocations = getRandom(min = helmet.int,
        max = legs.int).EquipmentLocations
  const locationNames: array[helmet .. legs, string] = ["head", "torso",
        "arm", "leg"]
  var
    attacker: MemberData = if playerAttack2: playerShip.crew[attackerIndex2]
      else:
        game.enemy.ship.crew[attackerIndex2]
    defender: MemberData = if playerAttack2: game.enemy.ship.crew[defenderIndex2]
      else:
        playerShip.crew[defenderIndex2]
  let (damage, hitChance, attackSkill) = countMeleeDamage(attacker = attacker,
      defender = defender, playerAttack2 = playerAttack2,
      hitLocation = hitLocation)
  var
    attackMessage: string = if playerAttack2:
        attacker.name & " attacks " & defender.name & " (" & factionName & ")"
      else:
        attacker.name & " (" & factionName & ") attacks " & defender.name
    messageColor: MessageColor = white
  if hitChance < 1:
    attackMessage &= " and misses."
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

proc meleeCombat(attackers, defenders: var seq[MemberData];
    playerAttack: bool) {.sideEffect, raises: [KeyError, IOError,
    CrewNoSpaceError, CrewOrderError, Exception], tags: [WriteIOEffect,
    RootEffect], contractual.} =
  ## Melee combat between the player's ship crew member and the enemy's ship's
  ## crew member
  ##
  ## * attackers    - the list of attackers
  ## * defenders    - the list of defenders
  ## * playerAttack - if true, the attacker is the player's ship's crew
  ##
  ## Returns modified params attackers and defenders
  var
    attackDone, riposte: bool = false
    attackerIndex, defenderIndex, orderIndex: int = 0

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
            defenderIndex2 = defenderIndex, playerAttack = playerAttack,
            playerAttack2 = playerAttack, orderIndex = orderIndex)
        if riposte and not endCombat:
          if game.enemy.ship.crew[defenderIndex].order != defend:
            giveOrders(ship = game.enemy.ship, memberIndex = defenderIndex,
                givenOrder = defend, moduleIndex = 0,
                checkPriorities = false)
          riposte = characterAttack(attackerIndex2 = defenderIndex,
              defenderIndex2 = attackerIndex, playerAttack = playerAttack,
              playerAttack2 = not playerAttack, orderIndex = orderIndex)
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
              defenderIndex2 = dIndex, playerAttack = playerAttack,
              playerAttack2 = playerAttack, orderIndex = orderIndex)
          if riposte and not endCombat:
            riposte = characterAttack(attackerIndex2 = dIndex,
                defenderIndex2 = attackerIndex, playerAttack = playerAttack,
                playerAttack2 = not playerAttack, orderIndex = orderIndex)
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
          defenderIndex2 = defenderIndex, playerAttack = playerAttack,
          playerAttack2 = playerAttack, orderIndex = orderIndex)
      if riposte and not endCombat:
        riposte = characterAttack(attackerIndex2 = defenderIndex,
            defenderIndex2 = attackerIndex, playerAttack = playerAttack,
            playerAttack2 = not playerAttack, orderIndex = orderIndex)
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
              defenderIndex2 = aIndex, playerAttack = playerAttack,
              playerAttack2 = not playerAttack, orderIndex = orderIndex)
          if not endCombat and riposte:
            riposte = characterAttack(attackerIndex2 = aIndex,
                defenderIndex2 = defenderIndex, playerAttack = playerAttack,
                playerAttack2 = playerAttack, orderIndex = orderIndex)
          break
    if riposte:
      defenderIndex.inc
    if findMember(order = boarding) == -1:
      updateOrders(ship = game.enemy.ship)

proc findEnemyModule(mType: ModuleType; enemyShip: ShipRecord): int {.sideEffect,
    raises: [KeyError], tags: [], contractual.} =
  ## Find the module in the enemy's ship
  ##
  ## * mType     - the type of the module to find
  ## * enemyShip - the ship which was attacked
  ##
  ## Return the index of the module of the selected type or -1 if a module
  ## was not found
  for index, module in enemyShip.modules:
    if modulesList[module.protoIndex].mType == mType and module.durability > 0:
      return index
  return -1

proc findHitWeapon(enemyShip: ShipRecord; hitLocation: var int) {.sideEffect,
    raises: [KeyError], tags: [], contractual.} =
  ## Find the weapon which was hit by the attack
  ##
  ## * enemyShip   - the ship which was attacked
  ## * hitLocation - the index of the module which was hit
  for index, module in enemyShip.modules:
    if ((module.mType == ModuleType2.turret and module.gunIndex > -1) or
        modulesList[module.protoIndex].mType == ModuleType.batteringRam) and
        module.durability > 0:
      hitLocation = index
      return

proc removeGun(moduleIndex: Natural; enemyShip: ShipRecord) {.sideEffect,
    raises: [], tags: [], contractual.} =
  ## Remove the gun from the player's ship's list of guns
  ##
  ## * moduleIndex - the index of the module to remove
  ## * enemyShip   - the ship which was attacked
  if enemyShip.crew == playerShip.crew:
    for index, gun in guns:
      if gun[1] == moduleIndex:
        guns.delete(i = index)
        break

proc countHitLocation(armorIndex, gunnerIndex, gunnerOrder: int;
    hitLocation: var int; ship, enemyShip: ShipRecord): bool {.sideEffect,
    raises: [KeyError], tags: [], contractual.} =
  ## Count location in which the enemy's ship was hit.
  ##
  ## * gunnerIndex - the index of the crew member who is shooting
  ## * gunnerOrder - the order for the crew member who is shooting
  ## * hitLocation - the location in the defender's ship which will be attacked
  ## * ship        - the ship which will shoot
  ## * enemyShip   - the ship which will be attacked
  ##
  ## Returns true if attacking should be stopped due to end of combat or the
  ## module was destroyed. Otherwise returns false. Also returns the modified
  ## parameter hitLocation
  result = false
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
          findHitWeapon(enemyShip = enemyShip, hitLocation = hitLocation)
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
      if game.enemy.combatAi == disarmer:
        hitLocation = 0
        findHitWeapon(enemyShip = enemyShip, hitLocation = hitLocation)
      else:
        hitLocation = getRandom(min = 0,
            max = game.enemy.ship.modules.high)
    while enemyShip.modules[hitLocation].durability == 0:
      hitLocation.dec
      if hitLocation == -1:
        return true

proc shooting(ship, enemyShip: var ShipRecord; currentAccuracyBonus, evadeBonus,
    gunnerIndex, shoots, gunnerOrder, speedBonus, ammoIndex: int;
    module: ModuleData; hitLocation: var int): bool {.sideEffect, raises: [
    KeyError, IOError], tags: [WriteIOEffect, RootEffect], contractual.} =
  ## Shoot to the enemy ship
  ##
  ## * ship                 - the ship which will shoot
  ## * enemyShip            - the ship which will be attacked
  ## * currentAccuracyBonus - the bonus to accuracy for the attacker
  ## * evadeBonus           - the bonut to evading for the defender
  ## * gunnerIndex          - the index of the crew member who is shooting
  ## * shoots               - the amount of shoots
  ## * gunnerOrder          - the order for the crew member who is shooting
  ## * speedBonus           - the bonus from the ships speed
  ## * ammoIndex            - the index of the ammunition in the attacker's cargo
  ## * module               - the module which will be attacked
  ## * hitLocation          - the location in the defender's ship which will be
  ##                          attacked
  ##
  ## Returns true if attacking should be stopped due to end of combat or the
  ## module was destroyed. Otherwise returns false.
  var hitChance: int = if ship.crew == playerShip.crew:
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
  let enemyNameOwner: string = enemyName & " (" & factionName & ")"
  for shoot in 1 .. shoots:
    var shootMessage: string = ""
    if ship.crew == playerShip.crew:
      shootMessage = if module.mType in {ModuleType2.gun, harpoonGun}:
          ship.crew[gunnerIndex].name & " shoots at " & enemyNameOwner
        else:
          "You ram " & enemyNameOwner
    else:
      shootMessage = enemyNameOwner & " attacks"
    if hitChance + getRandom(min = 1, max = 50) > getRandom(min = 1,
        max = hitChance + 50):
      shootMessage &= " and hits "
      let armorIndex: int = findEnemyModule(mType = ModuleType.armor,
          enemyShip = enemyShip)
      if countHitLocation(armorIndex = armorIndex, gunnerIndex = gunnerIndex,
          gunnerOrder = gunnerOrder, hitLocation = hitLocation, ship = ship,
          enemyShip = enemyShip):
        return true
      shootMessage = shootMessage & enemyShip.modules[
          hitLocation].name & "."
      let damage: float = 1.0 - (module.durability.float /
          module.maxDurability.float)
      var weaponDamage: int = 0
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
              weaponDamage -= (eModule.maxModules / 10).int
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
            let weaponIndex: int = enemyShip.modules[
                hitLocation].gunIndex
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
      shootMessage &= " and misses."
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
      return true

proc prepareEnemyGun(mIndex, gunnerIndex: int; shoots: var int;
    ship: ShipRecord) {.sideEffect, raises: [KeyError], tags: [],
    contractual.} =
  ## Count amount of shoots of the enemy's ship.
  ##
  ## * mIndex      - the index of currently checked module in the attacker's
  ##                 ship
  ## * gunnerIndex - the index of the gunner who is using the gun
  ## * shoots      - the amount of shoots from the gun
  ## * ship        - the attacking ship
  ##
  ## Returns the modified parameter shoots
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

proc prepareGun(gunnerIndex, shoots, gunnerOrder, currentAccuracyBonus,
    ammoIndex, evadeBonus: var int; module: var ModuleData; ship,
    enemyShip: ShipRecord; mIndex, accuracyBonus, ammoIndex2: int) {.sideEffect,
    raises: [KeyError], tags: [RootEffect], contractual.} =
  ## Count all needed data for shooting from the selected gun or harpoon gun
  ##
  ## * gunnerIndex          - the index of the gunner who is using the gun
  ## * shoots               - the amount of shoots from the gun
  ## * gunnerOrder          - the order for the gunner who is using the gun
  ## * currentAccuracyBonus - the current bonus to attacker accuracy
  ## * ammoIndex            - the index of ammunition used by the gun
  ## * evadeBonus           - the bonus to evade for the defender
  ## * module               - the information about the gun or harpoon gun
  ## * ship                 - the attacking ship
  ## * enemyShip            - the defending ship
  ## * mIndex               - the index of currently checked module in the
  ##                          attacker's ship
  ## * accuracyBonus        - the global bonus to accuracy for the attacker
  ## * ammoIndex2           - the index of the ammunition in the attacker's
  ##                          cargo
  ##
  ## Returns modified parameters gunnerIndex, shoots, gunnerOrder,
  ## currentAccuracyBonus, ammoIndex, evadeBonus and module
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
    prepareEnemyGun(mIndex = mIndex, gunnerIndex = gunnerIndex, shoots = shoots, ship = ship)
  if ammoIndex2 in ship.cargo.low .. ship.cargo.high and itemsList[
      ship.cargo[ammoIndex2].protoIndex].itemType == itemsTypesList[
      modulesList[module.protoIndex].value - 1]:
    ammoIndex = ammoIndex2
  if ammoIndex == -1:
    for iIndex, item in itemsList:
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

proc attack(ship, enemyShip: var ShipRecord; ammoIndex2: var int; accuracyBonus,
    speedBonus: int) {.sideEffect, raises: [KeyError, IOError], tags: [
    RootEffect], contractual.} =
  ## Made one attack of one of the ships in the combat
  ##
  ## * ship          - the ship which will be attacking
  ## * enemyShip     - the ship which will be attacked
  ## * ammoIndex2    - the index of the ammunition in the attacker's cargo
  ## * accuracyBonus - the global bonus to accuracy for the attacker
  ## * speedBonus    - the bonus from the ships speed
  ##
  ## Returns modified parameters ship and enemyShip

  var hitLocation: int = -1

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
        gunnerIndex: int = -1
        ammoIndex: int = -1
        gunnerOrder: Natural = 1
        shoots: int = 0
        currentAccuracyBonus: int = 0
        evadeBonus: int = 0
      if module.mType == ModuleType2.harpoonGun:
        ammoIndex2 = module.harpoonIndex
      elif module.mType == ModuleType2.gun:
        ammoIndex2 = module.ammoIndex
      if module.mType in {ModuleType2.gun, harpoonGun}:
        prepareGun(gunnerIndex = gunnerIndex, shoots = shoots,
            gunnerOrder = gunnerOrder,
            currentAccuracyBonus = currentAccuracyBonus,
            ammoIndex = ammoIndex, evadeBonus = evadeBonus, module = module,
            ship = ship, enemyShip = enemyShip, mIndex = mIndex,
            accuracyBonus = accuracyBonus, ammoIndex2 = ammoIndex2)
      else:
        if game.enemy.distance > 100:
          shoots = 0
        else:
          shoots = (if module.coolingDown: 0 else: 1)
        module.coolingDown = not module.coolingDown
      logMessage(message = "Shoots: " & $shoots,
          debugType = DebugTypes.combat)
      if shoots > 0:
        if shooting(ship = ship, enemyShip = enemyShip,
            currentAccuracyBonus = currentAccuracyBonus,
            evadeBonus = evadeBonus, gunnerIndex = gunnerIndex,
            shoots = shoots, gunnerOrder = gunnerOrder,
            speedBonus = speedBonus, ammoIndex = ammoIndex, module = module,
            hitLocation = hitLocation):
          break attackLoop

proc changeEnemySpeed(enemyPilotOrder: var Natural;
    damageRange: Natural) {.sideEffect, raises: [], tags: [], contractual.} =
  ## Change the enemy's ship's speed depending on the enemy's AI
  ##
  ## * enemyPilotOrder - the order of the enemy's ship's pilot
  ## * damageRange     - the distance between the ships in which some speed changes happen
  ##
  ## Returns modified parameter enemyPilotOrder
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

proc countDamageRange(damageRange: var Natural;
    ammoIndex2: var int) {.sideEffect, raises: [KeyError], tags: [],
    contractual.} =
  ## Count the distance between the ships in which the enemy can attack the
  ## player's ship
  ##
  ## * damageRange - the range between the ships in which damage can be done
  ## * ammoIndex2  - the index of the ammunition in the enemy ship
  ##
  ## Returns modified parameters damageRange and ammoIndex2
  var enemyWeaponIndex: int = -1
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
      var enemyAmmoIndex: int = -1
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

proc countBonuses(pilotIndex, engineerIndex: int; accuracyBonus, evadeBonus,
    speedBonus: var int) {.sideEffect, raises: [KeyError, ValueError], tags: [],
    contractual.} =
  ## Count the bonuses or maluses for the player's ship used in calculating the
  ## chance for hit an enemy's ship and evade its attacks
  ##
  ## * pilotIndex    - the index of the pilot in the player's ship's crew
  ## * engineerIndex - the index of the engineer in the player's ship's crew
  ## * accuracyBonus - the bonus or malus to hit an enemy's ship
  ## * evadeBonus    - the bonus or malus to evade an enemy's ship's attacks
  ## * speedBonus    - the bonus or malus to hit and evade from the player's
  ##                   ship's speed
  ##
  ## Returns modified parameters accuracyBonus, evadeBonus and speedBonus.
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
    evadeBonus += getSkillLevel(member = playerShip.crew[
        pilotIndex], skillIndex = pilotingSkill)
  else:
    accuracyBonus = 20
    evadeBonus = -10
  var enemyPilotIndex: int = findMember(order = pilot,
      shipCrew = game.enemy.ship.crew)
  if enemyPilotIndex > -1:
    accuracyBonus -= getSkillLevel(member = game.enemy.ship.crew[
        enemyPilotIndex], skillIndex = pilotingSkill)
  if engineerIndex > -1 or "sentientships" in factionsList[playerShip.crew[
      0].faction].flags:
    let message: string = changeShipSpeed(speedValue = engineerOrder.ShipSpeed)
    if message.len > 0:
      addMessage(message = message, mType = orderMessage, color = red)
  speedBonus = 20 - (realSpeed(ship = playerShip) / 100).int
  if speedBonus < -10:
    speedBonus = -10
  accuracyBonus += speedBonus
  evadeBonus -= speedBonus

proc combatTurn*() {.sideEffect, raises: [KeyError, IOError, ValueError,
    CrewNoSpaceError, CrewOrderError, Exception], tags: [WriteIOEffect,
    RootEffect], contractual.} =
  ## One turn in the combat, between the ships and the crew members if there
  ## is boarding party on any ship.
  var
    accuracyBonus, evadeBonus: int = 0
    speedBonus: int = 0
    ammoIndex2: int = -1

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
  countBonuses(pilotIndex = pilotIndex, engineerIndex = engineerIndex,
      accuracyBonus = accuracyBonus, evadeBonus = evadeBonus,
      speedBonus = speedBonus)
  var damageRange: Natural = 10_000
  countDamageRange(damageRange = damageRange, ammoIndex2 = ammoIndex2)
  var enemyPilotOrder: Natural = 2
  changeEnemySpeed(enemyPilotOrder = enemyPilotOrder, damageRange = damageRange)
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
    accuracyBonus += 20
    evadeBonus -= 20
  of 2:
    accuracyBonus += 10
    evadeBonus -= 10
  of 3:
    accuracyBonus -= 10
    evadeBonus += 10
  of 4:
    accuracyBonus -= 20
    evadeBonus += 20
  else:
    discard
  speedBonus = 20 - (realSpeed(ship = game.enemy.ship) / 100).int
  if speedBonus < -10:
    speedBonus = -10
  accuracyBonus += speedBonus
  evadeBonus -= speedBonus
  var distanceTraveled: int = if enemyPilotOrder < 4: -(realSpeed(
      ship = game.enemy.ship))
      else:
        realSpeed(ship = game.enemy.ship)
  if pilotIndex > -1:
    case pilotOrder
    of 1, 3:
      distanceTraveled -= realSpeed(ship = playerShip)
    of 2:
      distanceTraveled += realSpeed(ship = playerShip)
      if distanceTraveled > 0 and enemyPilotOrder != 4:
        distanceTraveled = 0
    of 4:
      distanceTraveled += realSpeed(ship = playerShip)
    else:
      discard
  else:
    distanceTraveled -= realSpeed(ship = playerShip)
  game.enemy.distance += distanceTraveled
  if game.enemy.distance < 10:
    game.enemy.distance = 10
  case game.enemy.distance:
  of 0 .. 999:
    accuracyBonus += 20
    evadeBonus -= 10
    logMessage(message = "Distance: short or close",
        debugType = DebugTypes.combat)
  of 1_000 .. 4_999:
    accuracyBonus += 10
    logMessage(message = "Distance: medium", debugType = DebugTypes.combat)
  of 5_000 .. 9_999:
    discard
  of 10_000 .. 14_999:
    accuracyBonus -= 10
    evadeBonus += 10
    logMessage(message = "Distance: long", debugType = DebugTypes.combat)
  else:
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
  attack(ship = playerShip, enemyShip = game.enemy.ship,
      ammoIndex2 = ammoIndex2, accuracyBonus = accuracyBonus,
      speedBonus = speedBonus)
  if not endCombat:
    attack(ship = game.enemy.ship, enemyShip = playerShip,
        ammoIndex2 = ammoIndex2, accuracyBonus = accuracyBonus,
        speedBonus = speedBonus)
  if not endCombat:
    var haveBoardingParty: bool = false

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
    finishCombat()

# Temporary code for interfacing with Ada

proc getAdaHarpoonDuration(playerDuration, enemyDuration: cint) {.raises: [],
    tags: [], exportc, contractual.} =
  ## Temporary C binding
  harpoonDuration = playerDuration
  game.enemy.harpoonDuration = enemyDuration

proc startAdaCombat(enemyIndex, newCombat: cint): cint {.raises: [], tags: [
    RootEffect], exportc, contractual.} =
  ## Temporary C binding
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
  ## Temporary C binding
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
  ## Temporary C binding
  try:
    combatTurn()
    npcShip = game.enemy.ship
  except:
    discard

proc setAdaGuns(adaGuns: AdaGunsArray) {.raises: [], tags: [], exportc,
    contractual.} =
  ## Temporary C binding
  guns = @[]
  for gun in adaGuns:
    if gun[0] == -1:
      break
    guns.add(y = [gun[0].int - 1, gun[1].int, gun[2].int])

proc getAdaBoardingOrders(adaOrders: var AdaBoardingOrders) {.raises: [],
    tags: [], exportc, contractual.} =
  ## Temporary C binding
  for order in adaOrders.mitems:
    order = -1
  for index, order in boardingOrders:
    adaOrders[index] = order.cint

proc setAdaBoardingOrders(adaOrders: AdaBoardingOrders) {.raises: [], tags: [],
    exportc, contractual.} =
  ## Temporary C binding
  boardingOrders = @[]
  for order in adaOrders:
    if order == -1:
      break
    boardingOrders.add(y = order)

proc setAdaEnemyName(): cstring {.raises: [], tags: [], exportc, contractual.} =
  ## Temporary C binding
  return enemyName.cstring

proc getAdaPilotOrder(order: cint) {.raises: [], tags: [], exportc,
    contractual.} =
  ## Temporary C binding
  pilotOrder = order

proc setAdaPilotOrder(): cint {.raises: [], tags: [], exportc, contractual.} =
  ## Temporary C binding
  return pilotOrder.cint

proc getAdaEngineerOrder(order: cint) {.raises: [], tags: [], exportc,
    contractual.} =
  ## Temporary C binding
  engineerOrder = order

proc setAdaEngineerOrder(): cint {.raises: [], tags: [], exportc,
    contractual.} =
  ## Temporary C binding
  return engineerOrder.cint

proc getAdaEndCombat(): cint {.raises: [], tags: [], exportc, contractual.} =
  ## Temporary C binding
  return endCombat.cint
