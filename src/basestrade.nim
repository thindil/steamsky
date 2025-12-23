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

## Provides code related to buying various services in bases, like hiring
## recruirs, buying crafting recipes or heal wounded crew members.

import std/tables
import contracts, nimalyzer
import bases, basescargo, basestypes, config, game, game2, items, maps,
    messages, shipscrew, types, utils

{.push ruleOff: "objects".}
type
  AlreadyKnownError* = object of CatchableError
    ## Raised when the recipe is already known to the player
  CantHealError* = object of CatchableError
    ## Raised when the selected player's ship's crew member can't be healed
{.push ruleOn: "objects".}

proc checkMoney(price: Positive; message: string = "") {.raises: [
    NoMoneyError, NotEnoughMoneyError], tags: [], contractual.} =
  ## Check if there is enough money on the player's ship
  ##
  ## * price   - the amount of money to compare
  ## * message - the message shown when there is no or not enough money
  body:
    let amount: Natural = moneyAmount(inventory = playerShip.cargo)
    if amount == 0:
      raise newException(exceptn = NoMoneyError, message = message)
    if amount < price:
      raise newException(exceptn = NotEnoughMoneyError, message = message)

proc hireRecruit*(recruitIndex: Natural; cost: Positive; dailyPayment,
    tradePayment: Natural; contractLength: int) {.raises: [
    NoTraderError, NoMoneyError, NotEnoughMoneyError, KeyError, IOError,
    Exception], tags: [WriteIOEffect, RootEffect], contractual.} =
  ## Hire the selected recruit in the current base
  ##
  ## * recruitIndex   - the index of the recruit on the base's recruits' list
  ## * cost           - the initial cost of the recruit
  ## * dailyPayment   - the daily payment for the recruit
  ## * tradePayment   - the percent of trade gain as payment for the recruit
  ## * contractLength - the length of the contract. 0 means infinite contract
  body:
    let traderIndex: ExtendedNatural = findMember(order = talk)
    if traderIndex == -1:
      raise newException(exceptn = NoTraderError, message = "")
    var price: Natural = cost
    countPrice(price = price, traderIndex = traderIndex)
    let
      baseIndex: BasesRange = skyMap[playerShip.skyX][playerShip.skyY].baseIndex
      recruit: RecruitData = skyBases[baseIndex].recruits[recruitIndex]
    var inventory: seq[InventoryData] = @[]
    for item in recruit.inventory:
      inventory.add(y = InventoryData(protoIndex: item.index, amount: 1,
          name: "", durability: defaultItemDurability, price: 0,
          quality: item.quality))
    var morale: Natural = 0
    if "nomorale" in factionsList[skyBases[baseIndex].owner].flags:
      morale = 50
    else:
      morale = 50 + skyBases[baseIndex].reputation.level
      if morale > 100:
        morale = 100
    playerShip.crew.add(y = MemberData(name: recruit.name,
        gender: recruit.gender, health: 100, tired: 0, skills: recruit.skills,
        hunger: 0, thirst: 0,
        order: rest, previousOrder: rest, orderTime: 15,
        attributes: recruit.attributes, inventory: inventory,
        equipment: recruit.equipment, payment: [1: dailyPayment,
            2: tradePayment],
        contractLength: contractLength, morale: [1: morale, 2: 0],
        loyalty: morale, homeBase: recruit.homeBase, faction: recruit.faction))
    checkMoney(price = price, message = recruit.name)
    updateMoney(memberIndex = -1, amount = -price, quality = any)
    gainExp(amount = 1, skillNumber = talkingSkill, crewIndex = traderIndex)
    gainRep(baseIndex = baseIndex, points = 1)
    addMessage(message = "You hired " & recruit.name & " for " & $price & " " &
        moneyName & ".", mType = tradeMessage)
    skyBases[baseIndex].recruits.delete(i = recruitIndex)
    skyBases[baseIndex].population.dec
    updateGame(minutes = 5)

proc buyRecipe*(recipeIndex: string) {.raises: [CantBuyError,
    AlreadyKnownError, NoTraderError, KeyError, NotEnoughMoneyError,
    NoMoneyError, IOError, Exception], tags: [WriteIOEffect, RootEffect],
        contractual.} =
  ## Buy the selected crafting recipe from the base
  ##
  ## * recipeIndex - the index of the recipe on the list of available recipes
  ##                 in the game
  require:
    recipeIndex.len > 0
  body:
    let
      baseIndex: BasesRange = skyMap[playerShip.skyX][playerShip.skyY].baseIndex
      baseType: BaseType = skyBases[baseIndex].baseType
    if recipeIndex notin basesTypesList[baseType].recipes:
      raise newException(exceptn = CantBuyError, message = "")
    if recipeIndex in knownRecipes:
      raise newException(exceptn = AlreadyKnownError, message = "")
    let traderIndex: ExtendedNatural = findMember(order = talk)
    if traderIndex == -1:
      raise newException(exceptn = NoTraderError, message = "")
    var cost: Natural = 0
    if getPrice(baseType = baseType, itemIndex = recipesList[
        recipeIndex].resultIndex, quality = normal) > 0:
      cost = getPrice(baseType = baseType, itemIndex = recipesList[
          recipeIndex].resultIndex, quality = normal) * recipesList[
              recipeIndex].difficulty * 10
    else:
      cost = recipesList[recipeIndex].difficulty * 10
    cost = (cost.float * newGameSettings.pricesBonus).Natural
    if cost == 0:
      cost = 1
    countPrice(price = cost, traderIndex = traderIndex)
    let
      recipeName: string = itemsList[recipesList[recipeIndex].resultIndex].name
    checkMoney(price = cost, message = recipeName)
    updateMoney(memberIndex = -1, amount = -cost, quality = any)
    updateBaseCargo(protoIndex = moneyIndex, amount = cost, quality = normal)
    knownRecipes.add(y = recipeIndex)
    addMessage(message = "You bought the recipe for " & recipeName & " for " &
        $cost & " of " & moneyName & ".", mType = tradeMessage)
    gainExp(amount = 1, skillNumber = talkingSkill, crewIndex = traderIndex)
    gainRep(baseIndex = baseIndex, points = 1)
    updateGame(minutes = 5)

proc healCost*(cost, time: var Natural; memberIndex: int) {.raises: [KeyError],
    tags: [], contractual.} =
  ## Count the cost and time needed by the healing wounded crew members action
  ## in the base
  ##
  ## * cost        - the amount of money needed for the action
  ## * time        - the amount of time needed for complete the action
  ## * memberIndex - the index of the player's ship's crew member to heal. If
  ##                 equal to -1, heal the whole crew
  ##
  ## Returns modified cost and time parameters
  require:
    memberIndex < playerShip.crew.len
  body:
    if memberIndex > -1:
      time = 5 * (100 - playerShip.crew[memberIndex].health)
      cost = (5 * (100 - playerShip.crew[memberIndex].health)) * getPrice(
          baseType = "0", itemIndex = findProtoItem(itemType = factionsList[
          playerShip.crew[memberIndex].faction].healingTools), quality = normal)
    else:
      for member in playerShip.crew:
        time += (5 * (100 - member.health))
        cost += ((5 * (100 - member.health)) * getPrice(baseType = "0",
            itemIndex = findProtoItem(itemType = factionsList[
            member.faction].healingTools), quality = normal))
    cost = (cost.float * newGameSettings.pricesBonus).Natural
    if cost == 0:
      cost = 1
    countPrice(price = cost, traderIndex = findMember(order = talk))
    if time == 0:
      time = 1
    let baseIndex: BasesRange = skyMap[playerShip.skyX][
        playerShip.skyY].baseIndex
    if "temple" in factionsList[skyBases[baseIndex].owner].flags:
      cost = (cost / 2).Natural
      if cost == 0:
        cost = 1

proc healWounded*(memberIndex: int) {.raises: [CantHealError,
    NoTraderError, KeyError, NotEnoughMoneyError, IOError, NoMoneyError,
    Exception], tags: [WriteIOEffect, RootEffect], contractual.} =
  ## Heal the player's ship crew's wounded members in bases
  ##
  ## * memberIndex - the index of the wounded crew member to heal. If -1 then
  ##                 heal all the wounded crew members.
  require:
    memberIndex < playerShip.crew.len
  body:
    var cost, time: Natural = 0
    healCost(cost = cost, time = time, memberIndex = memberIndex)
    if cost == 0:
      raise newException(exceptn = CantHealError, message = "")
    let traderIndex: int = findMember(order = talk)
    if traderIndex == -1:
      raise newException(exceptn = NoTraderError, message = "")
    if memberIndex > -1:
      playerShip.crew[memberIndex].health = 100
      addMessage(message = "You paid for healing " & playerShip.crew[
          memberIndex].name & " for " & $cost & " " & moneyName & ".",
          mType = tradeMessage)
      giveOrders(ship = playerShip, memberIndex = memberIndex,
          givenOrder = rest, moduleIndex = -1, checkPriorities = false)
    else:
      for index, member in playerShip.crew.mpairs:
        if member.health < 100:
          member.health = 100
          giveOrders(ship = playerShip, memberIndex = index, givenOrder = rest,
              moduleIndex = -1, checkPriorities = false)
      addMessage(message = "You paid for healing all wounded crew members for " &
          $cost & " " & moneyName & ".", mType = tradeMessage)
    checkMoney(price = cost)
    updateMoney(memberIndex = -1, amount = -cost, quality = any)
    updateBaseCargo(protoIndex = moneyIndex, amount = cost, quality = normal)
    gainExp(amount = 1, skillNumber = talkingSkill, crewIndex = traderIndex)
    let baseIndex: BasesRange = skyMap[playerShip.skyX][
        playerShip.skyY].baseIndex
    gainRep(baseIndex = baseIndex, points = 1)
    updateGame(minutes = time)

proc trainCost*(memberIndex, skillIndex: Natural;
    traderIndex: int): Natural {.raises: [KeyError], tags: [], contractual.} =
  ## Count the cost needed to train the selected skill of the selected
  ## player's ship's crew's member.
  ##
  ## * memberIndex - the index of the member in the player's ship's crew
  ## * skillIndex  - the index of the skill to train
  ## * traderIndex - the index of the trader in the player's ship's crew
  ##
  ## Returns the amount of money needed to train the skill or 0 if the
  ## skill reached maximum level and can't be trained.
  require:
    memberIndex < playerShip.crew.len
  body:
    result = (100.0 * newGameSettings.pricesBonus).Natural
    for skill in playerShip.crew[memberIndex].skills:
      if skill.index == skillIndex:
        if skill.level == 100:
          return 0
        result = (((skill.level + 1) * 100).float *
            newGameSettings.pricesBonus).Natural
        if result == 0:
          result = 1
        break
    countPrice(price = result, traderIndex = traderIndex)

proc trainSkill*(memberIndex: Natural; skillIndex, amount: Positive;
    isAmount: bool = true) {.raises: [KeyError, IOError, Exception],
    tags: [WriteIOEffect, RootEffect], contractual.} =
  ## Train the selected skill of the selected player's ship's crew member in
  ## the base
  ##
  ## * memberIndex - the index of the member in the player's ship's crew
  ## * skillIndex  - the index of the skill to train
  ## * amount      - how many times train the skill or how much money spend
  ##                 on the training
  ## * isAmount    - if true, the amount parameter is the amount of training
  ##                 sessions. Otherwise it is the amount of money to spend
  require:
    memberIndex < playerShip.crew.len
    skillIndex < skillsList.len
  body:
    let traderIndex: int = findMember(order = talk)
    giveOrders(ship = playerShip, memberIndex = memberIndex, givenOrder = rest,
        moduleIndex = 0, checkPriorities = false)
    let baseIndex: BasesRange = skyMap[playerShip.skyX][
        playerShip.skyY].baseIndex
    var
      maxAmount: int = amount
      sessions, overallCost: Natural = 0
    while maxAmount > 0:
      let
        cost: Natural = trainCost(memberIndex = memberIndex,
            skillIndex = skillIndex, traderIndex = traderIndex)
        mAmount: Natural = moneyAmount(inventory = playerShip.cargo)
      if cost == 0 or mAmount < cost or (not isAmount and maxAmount < cost):
        break
      var gainedExp: Positive = getRandom(min = 10, max = 60) + playerShip.crew[
          memberIndex].attributes[skillsList[skillIndex].attribute].level
      if gainedExp > 100:
        gainedExp = 100
      gainExp(amount = gainedExp, skillNumber = skillIndex,
          crewIndex = memberIndex)
      updateMoney(memberIndex = -1, amount = -cost, quality = any)
      updateBaseCargo(protoIndex = moneyIndex, amount = cost, quality = normal)
      if traderIndex > -1:
        gainExp(amount = 5, skillNumber = talkingSkill, crewIndex = traderIndex)
      gainRep(baseIndex = baseIndex, points = 5)
      updateGame(minutes = 60)
      sessions.inc
      overallCost += cost
      maxAmount -= (if isAmount: 1 else: cost)
    if sessions > 0:
      addMessage(message = "You purchased " & $sessions &
          " training session(s) in " & skillsList[skillIndex].name & " for " &
          playerShip.crew[memberIndex].name & " for " & $overallCost & " " &
          moneyName & ".", mType = tradeMessage)
    else:
      updateOrders(ship = playerShip)
