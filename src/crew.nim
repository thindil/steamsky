# Copyright 2022-2025 Bartek thindil Jasicki
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

## Provides code related to the player's ship's crew like daily payments,
## getting attributes or skills names, etc. Split from the crew module to
## avoid circular dependencies.

import std/[tables]
import contracts
import config, crewinventory, game, items, maps, messages, utils,
    shipscargo, shipscrew, shipscrew2, types

proc dailyPayment*() {.raises: [KeyError, Exception], tags: [
    RootEffect], contractual.} =
  ## Pay daily payments to the player's ship crew members and update the lenght
  ## of their contracts
  var moneyAmount: Natural = moneyAmount(inventory = playerShip.cargo)
  for index, member in playerShip.crew:
    if member.payment[1] > 0:
      var haveMoney: bool = true
      if moneyAmount == 0:
        addMessage(message = "You don't have any " & moneyName &
            " to pay your crew members.", mType = tradeMessage, color = red)
        haveMoney = false
      if haveMoney:
        if moneyAmount < member.payment[1]:
          let moneyNeeded: Natural = moneyAmount
          updateMoney(memberIndex = -1, amount = -(moneyNeeded), quality = any)
          addMessage(message = "You don't have enough " & moneyName &
              " to pay your crew members.", mType = tradeMessage, color = red)
          haveMoney = false
        if haveMoney:
          updateMoney(memberIndex = -1, amount = -(member.payment[1]), quality = any)
          var payMessage: string = "You pay " & member.name
          if member.gender == 'M':
            payMessage.add(y = " his ")
          else:
            payMessage.add(y = " her ")
          payMessage.add(y = "daily payment.")
          addMessage(message = payMessage, mType = tradeMessage)
          updateMorale(ship = playerShip, memberIndex = index,
              value = getRandom(min = 1, max = 5))
        moneyAmount = moneyAmount(inventory = playerShip.cargo)
      if not haveMoney:
        updateMorale(ship = playerShip, memberIndex = index,
            value = getRandom(min = -50, max = -10))
  var memberIndex: int = 1
  while memberIndex <= playerShip.crew.high:
    if playerShip.crew[memberIndex].contractLength > 0:
      playerShip.crew[memberIndex].contractLength.dec
      if playerShip.crew[memberIndex].contractLength == 0:
        addMessage(message = "Your contract with " & playerShip.crew[
            memberIndex].name & " has ended.", mType = tradeMessage, color = red)
        if playerShip.speed == docked:
          deleteMember(memberIndex = memberIndex, ship = playerShip)
          skyBases[skyMap[playerShip.skyX][
              playerShip.skyY].baseIndex].population.inc
          memberIndex.dec
        else:
          for order in playerShip.crew[memberIndex].orders.mitems:
            order = 0
          giveOrders(ship = playerShip, memberIndex = memberIndex,
              givenOrder = rest)
    memberIndex.inc

proc getAttributeLevelName*(attributeLevel: Positive): string {.raises: [],
    tags: [], contractual.} =
  ## Get the attribute level name for the selected attribute or its numerical
  ## value if the player set it in the configuration.
  ##
  ## * attributeLevel - the level of an attribute which value will be get
  ##
  ## Returns the string representation of the selected level of an attribute.
  ensure:
    result.len > 0
  body:
    if gameSettings.showNumbers:
      return $attributeLevel
    case attributeLevel
    of 1..5:
      return "Very low"
    of 6..10:
      return "Low"
    of 11..15:
      return "Below average"
    of 16..30:
      return "Average"
    of 31..35:
      return "Above average"
    of 36..40:
      return "High"
    of 41..49:
      return "Very high"
    else:
      return "Outstanding"

proc getSkillLevelName*(skillLevel: SkillRange): string {.raises: [
    ], tags: [], contractual.} =
  ## Get the skill level name for the selected skill or its numerical
  ## value if the player set it in the configuration.
  ##
  ## * attributeLevel - the level of a skill which value will be get
  ##
  ## Returns the string representation of the selected level of a skill.
  ensure:
    result.len > 0
  body:
    if gameSettings.showNumbers:
      return $skillLevel
    case skillLevel
    of 0:
      return "Untrained"
    of 1..10:
      return "Beginner"
    of 11..20:
      return "Novice"
    of 21..30:
      return "Apprentice"
    of 31..40:
      return "Practitioner"
    of 41..50:
      return "Competent"
    of 51..60:
      return "Respected"
    of 61..70:
      return "Renowned"
    of 71..80:
      return "Master"
    of 81..90:
      return "Grand-Master"
    of 91..99:
      return "Legendary"
    else:
      return "Ultimate"

proc findCabin*(memberIndex: Natural): int {.raises: [], tags: [],
    contractual.} =
  ## Find index of cabin which belongs to the selected crew member
  ##
  ## * memberIndex - the index of the player's ship crew member which will be
  ##                 checked
  ##
  ## Returns the index of the cabin which belongs to the selected crew member
  ## or -1 if nothing found
  ensure:
    result in -1..playerShip.modules.high
  body:
    for index, module in playerShip.modules:
      if module.mType == ModuleType2.cabin:
        for owner in module.owner:
          if owner == memberIndex:
            return index
    return -1

proc memberRest(memberIndex: Natural; tiredLevel, healthLevel: var int;
    times: int) {.raises: [KeyError], tags: [], contractual.} =
  ## Count the effect of the rest on the selected player's ship's crew member
  ##
  ## * memberIndex - the index of the crew member which rests
  ## * tiredLevel  - the level of tiredness of the crew member
  ## * healthLevel - the level of health of the crew member
  ## * times       - how many cycles the crew member rested
  ##
  ## Returns modified parameters tiredLevel and healthLevel
  require:
    memberIndex < playerShip.crew.len
    times > 0
  body:
    let cabinIndex: int = findCabin(memberIndex = memberIndex)
    var restAmount: int = 0
    if playerShip.crew[memberIndex].tired > 0:
      if cabinIndex > -1:
        var damage: float = 1.0 - (playerShip.modules[
            cabinIndex].durability.float / playerShip.modules[
            cabinIndex].maxDurability.float)
        restAmount = playerShip.modules[cabinIndex].cleanliness - (
            playerShip.modules[cabinIndex].cleanliness.float *
            damage).Natural
        if restAmount == 0:
          restAmount = 1
        tiredLevel -= (times * restAmount)
      else:
        tiredLevel -= times
      if tiredLevel < 0:
        tiredLevel = 0
    if "nofatigue" notin factionsList[playerShip.crew[
        memberIndex].faction].flags and healthLevel in 1..99 and cabinIndex > 0:
      healthLevel += times
      if healthLevel > 100:
        healthLevel = 100
    if playerShip.crew[memberIndex].morale[1] < 50:
      updateMorale(ship = playerShip, memberIndex = memberIndex, value = times + restAmount)
      if playerShip.crew[memberIndex].morale[1] > 50:
        playerShip.crew[memberIndex].morale = [1: 50.Natural, 2: 0]

proc memberHeal(memberIndex: Natural; times: int) {.raises: [
    KeyError, CrewNoSpaceError, CrewOrderError, Exception], tags: [RootEffect],
    contractual.} =
  ## Execute heal wounded crew members order for the selected crew member
  ##
  ## * memberIndex - the index of the member for which the order will be executed
  ## * times       - how many cycles the crew member healed wounded
  require:
    memberIndex < playerShip.crew.len
    times > 0
  body:
    var haveMedicalRoom: bool = false
    for module in playerShip.modules:
      if modulesList[module.protoIndex].mType ==
          ModuleType.medicalRoom and module.durability > 0 and memberIndex in module.owner:
        haveMedicalRoom = true
        break
    var
      healAmount: int = 1
      toolIndex: int = 0
    for member in playerShip.crew:
      let faction: FactionData = factionsList[member.faction]
      if member.name != playerShip.crew[memberIndex].name and member.health < 100:
        healAmount = times * (getSkillLevel(
          member = playerShip.crew[memberIndex],
          skillIndex = faction.healingSkill) / 20).int
        toolIndex = 0
        if healAmount < times:
          healAmount = times
        if not haveMedicalRoom:
          healAmount = (healAmount / 2).int
        if healAmount > 0:
          healAmount *= (-1)
          toolIndex = findItem(inventory = playerShip.cargo,
              itemType = faction.healingTools, itemQuality = any)
          if toolIndex > -1:
            if playerShip.cargo[toolIndex].amount < healAmount.abs:
              healAmount = playerShip.cargo[toolIndex].amount
            else:
              healAmount = healAmount.abs
            updateCargo(ship = playerShip, amount = -(healAmount),
                cargoIndex = toolIndex, quality = playerShip.cargo[
                toolIndex].quality)
          else:
            toolIndex = findItem(inventory = playerShip.crew[
                memberIndex].inventory, itemType = faction.healingTools,
                itemQuality = any)
            if toolIndex > -1:
              if playerShip.crew[memberIndex].inventory[toolIndex].amount <
                  healAmount.abs:
                healAmount = playerShip.crew[memberIndex].inventory[
                    toolIndex].amount
              else:
                healAmount = healAmount.abs
              updateInventory(memberIndex = memberIndex, amount = -(healAmount),
                  inventoryIndex = toolIndex, ship = playerShip,
                      quality = playerShip.crew[memberIndex].inventory[
                      toolIndex].quality)
        if healAmount > 0:
          for index, member in playerShip.crew.mpairs:
            if member.health < 100 and index != memberIndex:
              if member.health + healAmount > SkillRange.high:
                member.health = SkillRange.high
              else:
                member.health += healAmount
              addMessage(message = playerShip.crew[memberIndex].name &
                  " healed " & member.name & " a bit.", mType = orderMessage)
              gainExp(amount = times, skillNumber = faction.healingSkill,
                  crewIndex = memberIndex)
              break
        else:
          if toolIndex == -1:
            addMessage(message = "You don't have any " &
                faction.healingTools &
                " to continue healing the wounded " & member.name & ".",
                mType = orderMessage, color = red)
          else:
            addMessage(message = playerShip.crew[memberIndex].name &
                " is not enough experienced to heal " & member.name &
                " in that amount of time.", mType = orderMessage,
                color = red)
    healAmount = 1
    for index, member in playerShip.crew:
      if member.health < 100 and index != memberIndex:
        healAmount = 0
        var faction: FactionData = factionsList[member.faction]
        toolIndex = findItem(inventory = playerShip.cargo,
            itemType = faction.healingTools, itemQuality = any)
        if toolIndex == -1:
          toolIndex = findItem(inventory = playerShip.crew[
              memberIndex].inventory, itemType = faction.healingTools,
              itemQuality = any)
          if toolIndex == -1:
            healAmount = -1
        break
    if healAmount > 0:
      addMessage(message = playerShip.crew[memberIndex].name &
          " finished healing the wounded.", mType = orderMessage, color = green)
    if healAmount != 0:
      giveOrders(ship = playerShip, memberIndex = memberIndex,
          givenOrder = rest)

proc memberClean(memberIndex: Natural; times: int) {.raises: [
    KeyError, CrewNoSpaceError, CrewOrderError, Exception], tags: [RootEffect],
    contractual.} =
  ## Execute clean the ship order for the selected crew member
  ##
  ## * memberIndex - the index of the member for which the order will be executed
  ## * times       - how many cycles clean the ship
  require:
    memberIndex < playerShip.crew.len
    times > 0
  body:
    var
      toolIndex: int = findTools(memberIndex = memberIndex,
          itemType = cleaningTools, order = clean)
      needCleaning: bool = false
    if toolIndex > -1:
      for module in playerShip.modules.mitems:
        if module.mType == ModuleType2.cabin and module.cleanliness <
            module.quality:
          if module.cleanliness + times > module.quality:
            module.cleanliness = module.quality
          else:
            module.cleanliness += times
          damageItem(inventory = playerShip.crew[memberIndex].inventory,
              itemIndex = toolIndex, memberIndex = memberIndex,
                  ship = playerShip)
          break
    for module in playerShip.modules:
      if module.mType == ModuleType2.cabin and module.cleanliness <
          module.quality:
        needCleaning = true
        if toolIndex == -1:
          addMessage(message = playerShip.crew[memberIndex].name &
              " can't continue cleaning the ship because don't have any cleaning tools.",
              mtype = orderMessage, color = red)
          giveOrders(ship = playerShip, memberIndex = memberIndex,
              givenOrder = rest)
        break
    if not needCleaning:
      addMessage(message = "Cleaning the ship have been finished.",
          mType = orderMessage, color = green)
      for index, member in playerShip.crew:
        if member.order == clean:
          giveOrders(ship = playerShip, memberIndex = index,
              givenOrder = rest)

proc normalizeStat(stat: var int; maxValue: Positive = 100) {.raises: [],
    tags: [], contractual.} =
  ## Normalize the value for the selected statistic
  ##
  ## * stat     - the stat to normalize
  ## * maxValue - the max value for the stat. Default value is 100.
  ##
  ## Return the modified parameter stat
  if stat > maxValue:
    stat = maxValue
  elif stat < 0:
    stat = 0

proc consume(itemType: string; memberIndex: Natural): Natural {.raises: [
    KeyError, CrewNoSpaceError], tags: [], contractual.} =
  ## Eat or drink the selected type of consumable
  ##
  ## * itemType    - the type of item to consume
  ## * memberIndex - the index of the crew member which will consume
  ##
  ## Returns amount of bonus from the used consumable or 0 if nothing was
  ## consumed
  var itemIndex: int = findItem(inventory = playerShip.cargo,
      itemType = itemType, itemQuality = any)
  if itemIndex > -1:
    let item: InventoryData = playerShip.cargo[itemIndex]
    result = itemsList[item.protoIndex].value[1]
    if itemsList[item.protoIndex].value.len > 1 and itemsList[
        item.protoIndex].value[2] != 0:
      updateMorale(ship = playerShip, memberIndex = memberIndex,
          value = itemsList[item.protoIndex].value[2])
    updateCargo(ship = playerShip, protoIndex = item.protoIndex, amount = -1,
        quality = item.quality)
    return
  itemIndex = findItem(inventory = playerShip.crew[memberIndex].inventory,
      itemType = itemType, itemQuality = any)
  if itemIndex > -1:
    let item: InventoryData = playerShip.crew[memberIndex].inventory[itemIndex]
    result = itemsList[item.protoIndex].value[1]
    if itemsList[item.protoIndex].value.len > 1 and itemsList[
        item.protoIndex].value[2] != 0:
      updateMorale(ship = playerShip, memberIndex = memberIndex,
          value = itemsList[item.protoIndex].value[2])
    updateInventory(memberIndex = memberIndex, amount = -1,
        inventoryIndex = itemIndex, ship = playerShip, quality = item.quality)
    return
  return 0

proc memberSendRest(member: var MemberData; memberIndex: int) {.raises: [
    KeyError], tags: [], contractual.} =
  ## Send the selected member on rest or back to work if they are rested
  ##
  ## * member      - the crew member to send to rest
  ## * memberIndex - the index of the crew member which will be send to rest
  ##
  ## Returns the modified parameter member
  var backToWork: bool = true
  if member.previousOrder notin [repair, clean] and findMember(
      order = member.previousOrder) > -1:
    backToWork = false
  if member.previousOrder in [gunner, craft]:
    block moduleLoop:
      for module in playerShip.modules.mitems:
        if (member.previousOrder == gunner and module.mType ==
            ModuleType2.gun) and module.owner[0] in [memberIndex, -1]:
          backToWork = true
          module.owner[0] = memberIndex
          break moduleLoop
        elif (member.previousOrder == craft and module.mType ==
            ModuleType2.workshop) and module.craftingIndex.len > 0:
          for owner in module.owner.mitems:
            if owner == memberIndex:
              backToWork = true
              owner = memberIndex
              break moduleLoop
          for owner in module.owner.mitems:
            if owner == -1:
              backToWork = true
              owner = memberIndex
              break moduleLoop
  if backToWork:
    member.order = member.previousOrder
    member.orderTime = 15
    addMessage(message = member.name & " returns to work fully rested.",
        mType = orderMessage, color = yellow)
    updateMorale(ship = playerShip, memberIndex = memberIndex, value = 1)
  member.previousOrder = rest

proc updateMember(member: var MemberData; tiredLevel, healthLevel, hungerLevel,
    thirstLevel: var int; memberIndex, orderTime: int; minutes: Positive;
    inCombat: bool) {.raises: [KeyError, CrewNoSpaceError], tags: [],
    contractual.} =
  ## Update the selected crew member
  ##
  ## * member      - the crew member to update
  ## * tiredLevel  - the level of tiredness of the crew member
  ## * healthLevel - the level of health of the crew member
  ## * hungerLevel - the level of hunger of the crew member
  ## * thirstLevel - the level of thirst of the crew member
  ## * memberIndex - the index of the crew member which will be updated
  ## * orderTime   - the time spent on the current order
  ## * minutes     - the amount of minutes which passed
  ## * inCombat    - if true, the player is in combat
  ##
  ## Returns the modified parameters member, tiredLevel, healthLevel,
  ## hungerLevel and thirstLevel

  if "nofatigue" in factionsList[member.faction].flags:
    tiredLevel = 0
  if tiredLevel == 0 and member.order == rest and member.previousOrder != rest:
    memberSendRest(member = member, memberIndex = memberIndex)
  if (tiredLevel > 80 + member.attributes[conditionIndex].level) and
      member.order != rest and not inCombat:
    var canRest: bool = true
    if member.order == boarding and harpoonDuration == 0 and
        game.enemy.harpoonDuration == 0:
      canRest = false
    if canRest:
      member.previousOrder = member.order
      member.order = rest
      member.orderTime = 15
      if member.equipment[tool] > -1:
        updateCargo(ship = playerShip, protoIndex = member.inventory[
            member.equipment[tool]].protoIndex, amount = 1,
            durability = member.inventory[member.equipment[tool]].durability,
            quality = member.inventory[member.equipment[tool]].quality)
        updateInventory(memberIndex = memberIndex, amount = -1,
            inventoryIndex = member.equipment[tool], ship = playerShip,
                quality = member.inventory[member.equipment[tool]].quality)
        member.equipment[tool] = -1
      addMessage(message = member.name &
          " is too tired to work, they're going to rest.",
          mType = orderMessage, color = yellow)
      block findNewCabin:
        if findCabin(memberIndex = memberIndex) == -1:
          for module in playerShip.modules.mitems:
            if module.mType == ModuleType2.cabin and module.durability > 0:
              for owner in module.owner.mitems:
                if owner == -1:
                  owner = memberIndex
                  addMessage(message = member.name & " take " & module.name &
                      " as own cabin.", mType = otherMessage)
                  break findNewCabin
    else:
      addMessage(message = member.name &
          " is very tired but they can't go to rest.", mType = orderMessage, color = red)
      updateMorale(ship = playerShip, memberIndex = memberIndex,
          value = getRandom(min = -5, max = -1))
  normalizeStat(stat = tiredLevel, maxValue = 150)
  member.tired = tiredLevel
  if hungerLevel > 80:
    var consumeResult: int = 0
    for foodType in factionsList[member.faction].foodTypes:
      consumeResult = consume(itemType = foodType, memberIndex = memberIndex)
      if consumeResult > 0:
        break
    if hungerLevel - consumeResult < SkillRange.low:
      hungerLevel = SkillRange.low
    else:
      hungerLevel -= consumeResult
    if consumeResult == 0:
      addMessage(message = member.name &
          " is hungry, but they can't find anything to eat.",
          mType = otherMessage, color = red)
      updateMorale(ship = playerShip, memberIndex = memberIndex,
          value = getRandom(min = -10, max = -5))
  normalizeStat(stat = hungerLevel)
  member.hunger = hungerLevel
  if thirstLevel > 40:
    var consumeResult: int = 0
    for drinksType in factionsList[member.faction].drinksTypes:
      consumeResult = consume(itemType = drinksType, memberIndex = memberIndex)
      if consumeResult > 0:
        break
    if thirstLevel - consumeResult < SkillRange.low:
      thirstLevel = SkillRange.low
    else:
      thirstLevel -= consumeResult
    if consumeResult == 0:
      addMessage(message = member.name &
          " is thirsty, but they can't find anything to drink.",
          mType = otherMessage, color = red)
      updateMorale(ship = playerShip, memberIndex = memberIndex,
          value = getRandom(min = -20, max = -10))
  normalizeStat(stat = thirstLevel)
  member.thirst = thirstLevel
  normalizeStat(stat = healthLevel)
  member.health = healthLevel
  if member.order notin [repair, craft, upgrading]:
    member.orderTime = orderTime
  if member.skills.len == 0:
    member.contractLength -= minutes
    if member.contractLength < 0:
      member.contractLength = 0

proc updateCrew*(minutes: Positive; tiredPoints: Natural;
    inCombat: bool = false) {.raises: [KeyError, IOError,
    Exception], tags: [WriteIOEffect, RootEffect], contractual.} =
  ## Update the player's ship crew
  ##
  ## * minutes     - the amount of minutes which passed
  ## * tiredPoints - the amount of tired points which each crew member will get
  ## * inCombat    - if true, the player is in combat.
  var
    i: int = 0
    tiredLevel, hungerLevel, thirstLevel, healthLevel, orderTime: int = 0

  while i < playerShip.crew.len:
    var currentMinutes: int = minutes
    orderTime = playerShip.crew[i].orderTime
    var times: int = 0
    while currentMinutes > 0:
      if currentMinutes >= orderTime:
        currentMinutes -= orderTime
        times.inc
        orderTime = 15
      else:
        orderTime -= currentMinutes
        currentMinutes = 0
    healthLevel = playerShip.crew[i].health
    hungerLevel = playerShip.crew[i].hunger
    thirstLevel = playerShip.crew[i].thirst
    tiredLevel = playerShip.crew[i].tired
    if times > 0:
      if playerShip.crew[i].order == rest:
        memberRest(memberIndex = i, tiredLevel = tiredLevel,
            healthLevel = healthLevel, times = times)
      else:
        if playerShip.crew[i].order != talk:
          tiredLevel += times
        if tiredLevel > 100 + playerShip.crew[i].attributes[
            conditionIndex].level:
          tiredLevel = 100 + playerShip.crew[i].attributes[conditionIndex].level
        if tiredLevel >= 50 + playerShip.crew[i].attributes[
            conditionIndex].level:
          updateMorale(ship = playerShip, memberIndex = i, value = ((times /
              5) * (-1)).int)
        case playerShip.crew[i].order
        of pilot:
          if playerShip.speed == docked:
            tiredLevel = playerShip.crew[i].tired
          else:
            gainExp(amount = times, skillNumber = pilotingSkill, crewIndex = i)
        of engineer:
          if playerShip.speed == docked:
            tiredLevel = playerShip.crew[i].tired
          else:
            gainExp(amount = times, skillNumber = engineeringSkill, crewIndex = i)
        of gunner:
          if playerShip.speed == docked:
            tiredLevel = playerShip.crew[i].tired
        of heal:
          memberHeal(memberIndex = i, times = times)
        of clean:
          memberClean(memberIndex = i, times = times)
        of talk:
          if skyMap[playerShip.skyX][playerShip.skyY].baseIndex == 0:
            giveOrders(ship = playerShip, memberIndex = i, givenOrder = rest)
        of train:
          var skillIndex: int = 0
          block findSkillIndex:
            for module in playerShip.modules:
              if module.mType == ModuleType2.trainingRoom:
                for owner in module.owner:
                  if owner == i:
                    skillIndex = module.trainedSkill
                    break findSkillIndex
          if skillsList[skillIndex].tool.len > 0:
            var toolIndex: int = findTools(memberIndex = i,
                itemType = skillsList[skillIndex].tool, order = train,
                toolQuality = getTrainingToolQuality(memberIndex = i,
                skillIndex = skillIndex))
            if toolIndex > -1:
              for j in 1..times:
                gainExp(amount = getRandom(min = 1, max = 5),
                    skillNumber = skillIndex, crewIndex = i)
                damageItem(inventory = playerShip.crew[i].inventory,
                    itemIndex = toolIndex, memberIndex = i, ship = playerShip)
                toolIndex = findTools(memberIndex = i, itemType = skillsList[
                    skillIndex].tool, order = train)
                if toolIndex == -1:
                  break
              addMessage(message = playerShip.crew[i].name &
                  " trained a little " & skillsList[skillIndex].name & ".",
                  mType = orderMessage)
            if toolIndex == -1:
              addMessage(message = playerShip.crew[i].name &
                  " can't continue training because they don't have the proper tools.",
                  mType = orderMessage, color = red)
              giveOrders(ship = playerShip, memberIndex = i, givenOrder = rest)
        else:
          discard
    if tiredPoints > 0:
      let faction: FactionData = factionsList[playerShip.crew[i].faction]
      var deathReason: string = ""
      if faction.foodTypes.len > 0:
        if hungerLevel + tiredPoints > SkillRange.high:
          hungerLevel = SkillRange.high
        else:
          hungerLevel += tiredPoints
        if playerShip.crew[i].hunger == SkillRange.high:
          healthLevel -= tiredPoints
          updateMorale(ship = playerShip, memberIndex = i, value = -(tiredPoints))
          if healthLevel < 1:
            healthLevel = SkillRange.low
            deathReason = "starvation"
      if faction.drinksTypes.len > 0:
        if thirstLevel + tiredPoints > SkillRange.high:
          thirstLevel = SkillRange.high
        else:
          thirstLevel += tiredPoints
        if playerShip.crew[i].thirst == SkillRange.high:
          healthLevel -= tiredPoints
          updateMorale(ship = playerShip, memberIndex = i, value = -(tiredPoints))
          if healthLevel < 1:
            healthLevel = SkillRange.low
            deathReason = "dehydration"
      if healthLevel == SkillRange.low:
        if deathReason.len == 0:
          deathReason = "debugging"
        death(memberIndex = i, reason = deathReason, ship = playerShip)
        if i == 0:
          break
    if healthLevel > SkillRange.low:
      updateMember(member = playerShip.crew[i], tiredLevel = tiredLevel,
          healthLevel = healthLevel, hungerLevel = hungerLevel,
          thirstLevel = thirstLevel, memberIndex = i, orderTime = orderTime,
          minutes = minutes, inCombat = inCombat)
      i.inc
