# Copyright 2023-2026 Bartek thindil Jasicki
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

## Provides code related to trading with NPC, in bases and ships like buying or
## selling items.

import std/[math, strutils, tables]
import contracts
import bases, basescargo, basestypes, crewinventory, game, game2, items, maps,
  messages, ships, shipscargo, shipscrew, types, utils

proc generateTraderCargo*(protoIndex: Positive) {.raises: [
    KeyError], tags: [], contractual.} =
  ## Generate the list of items for trade.
  ##
  ## * protoIndex - the index of the ship's prototype from which the cargo will
  ##                be generated
  require:
    protoShipsList.hasKey(key = protoIndex)
  body:
    var traderShip: ShipRecord = createShip(protoIndex = protoIndex, name = "",
          x = playerShip.skyX, y = playerShip.skyY, speed = fullStop)
    traderCargo = @[]
    for item in traderShip.cargo:
      traderCargo.add(y = BaseCargo(protoIndex: item.protoIndex,
          amount: item.amount, durability: defaultItemDurability,
          price: itemsList[item.protoIndex].price, quality: getQuality(),
          maxDurability: defaultItemDurability, weight: 0))
    var cargoAmount: Natural = if traderShip.crew.len < 5: getRandom(min = 1, max = 3)
        elif traderShip.crew.len < 10: getRandom(min = 1, max = 5)
        else: getRandom(min = 1, max = 10)
    while cargoAmount > 0:
      var
        itemAmount: Positive = if traderShip.crew.len < 5: getRandom(min = 1, max = 100)
          elif traderShip.crew.len < 10: getRandom(min = 1, max = 500)
          else: getRandom(min = 1, max = 1000)
        itemIndex: Natural = getRandom(min = 1, max = itemsList.len)
        newItemIndex: Natural = 0
      for i in 1..itemsList.len:
        itemIndex.dec
        if itemIndex == 0:
          newItemIndex = i
          break
      let
        quality: ObjectQuality = getQuality()
        cargoItemIndex: int = findItem(inventory = traderShip.cargo,
          protoIndex = newItemIndex, itemQuality = quality)
      if cargoItemIndex > -1:
        traderCargo[cargoItemIndex].amount += itemAmount
        traderShip.cargo[cargoItemIndex].amount += itemAmount
      else:
        if freeCargo(amount = 0 - (itemsList[newItemIndex].weight *
            itemAmount)) > -1:
          traderCargo.add(y = BaseCargo(protoIndex: newItemIndex,
            amount: itemAmount, durability: defaultItemDurability,
            price: itemsList[ newItemIndex].price, quality: quality))
          traderShip.cargo.add(y = InventoryData(protoIndex: newItemIndex,
              amount: itemAmount, durability: defaultItemDurability, name: "",
              price: 0, quality: quality))
        else:
          cargoAmount = 1
      cargoAmount.dec

proc sellItems*(itemIndex: Natural; amount: string) {.raises: [
    NoTraderError, NoFreeCargoError, NoMoneyInBaseError, KeyError, ValueError,
    IOError, Exception], tags: [WriteIOEffect, RootEffect], contractual.} =
  ## Sell the selected item from the player's ship cargo to the trader
  ##
  ## * itemIndex - the index of the item in the player's ship cargo
  ## * amount    - the amount of the item to sell
  require:
    itemIndex < playerShip.cargo.len
  body:
    let traderIndex: int = findMember(order = talk)
    if traderIndex == -1:
      raise newException(exceptn = NoTraderError, message = "")
    let
      baseIndex: ExtendedBasesRange = skyMap[playerShip.skyX][playerShip.skyY].baseIndex
      protoIndex: Natural = playerShip.cargo[itemIndex].protoIndex
      playerItem: InventoryData = playerShip.cargo[itemIndex]
    var baseItemIndex: int = -1
    if baseIndex > 0:
      baseItemIndex = findBaseCargo(protoIndex = protoIndex, quality = playerItem.quality)
    else:
      for index, item in traderCargo:
        if item.protoIndex == protoIndex:
          baseItemIndex = index
          break
    var price: Natural = 0
    if baseItemIndex == -1:
      price = getPrice(baseType = skyBases[baseIndex].baseType,
          itemIndex = protoIndex, quality = normal)
    else:
      price = if baseIndex > 0:
          skyBases[baseIndex].cargo[baseItemIndex].price
        else:
          traderCargo[baseItemIndex].price
    let eventIndex: int = skyMap[playerShip.skyX][playerShip.skyY].eventIndex
    if eventIndex > -1 and eventsList[eventIndex].eType == doublePrice and
        eventsList[eventIndex].itemIndex == protoIndex:
      price *= 2
    let sellAmount: Positive = amount.parseInt
    var profit: Natural = price * sellAmount
    if playerShip.cargo[itemIndex].durability < 100:
      profit = (profit.float * (playerItem.durability.float / 100.0)).int
    countPrice(price = price, traderIndex = traderIndex, reduce = false)
    for index, member in playerShip.crew:
      if member.payment[2] == 0:
        continue
      if profit < 1:
        updateMorale(ship = playerShip, memberIndex = index, value = getRandom(
            min = -25, max = -5))
        addMessage(message = member.name &
            " is sad because doesn't get own part of profit.",
            mType = tradeMessage, color = red)
        profit = 0
        continue
      profit -= (profit.float * (member.payment[2].float / 100.0)).int
      if profit < 1:
        if profit < 0:
          updateMorale(ship = playerShip, memberIndex = index,
              value = getRandom(min = -12, max = -2))
          addMessage(message = member.name &
              " is sad because doesn't get own part of profit.",
              mType = tradeMessage, color = red)
        profit = 0
    if freeCargo(amount = itemsList[protoIndex].weight * sellAmount) - profit < 0:
      raise newException(exceptn = NoFreeCargoError, message = "")
    let itemName: string = itemsList[protoIndex].name
    if baseIndex > 0:
      if profit > skyBases[baseIndex].cargo[0].amount:
        raise newException(exceptn = NoMoneyInBaseError, message = itemName)
      updateBaseCargo(protoIndex = protoIndex, amount = sellAmount,
        durability = playerItem.durability, quality = playerItem.quality,
        maxDurability = playerItem.maxDurability, weight = playerItem.weight)
    else:
      if profit > traderCargo[0].amount:
        raise newException(exceptn = NoMoneyInBaseError, message = itemName)
      var cargoAdded: bool = false
      for item in traderCargo.mitems:
        if item.protoIndex == protoIndex and item.durability ==
            playerItem.durability and item.quality == playerItem.quality and
            item.maxDurability == playerItem.durability and item.weight ==
            playerItem.weight:
          item.amount += sellAmount
          cargoAdded = true
          break
      if not cargoAdded:
        traderCargo.add(y = BaseCargo(protoIndex: protoIndex,
          amount: sellAmount,
          durability: playerItem.durability,
          price: itemsList[protoIndex].price,
          quality: playerItem.quality,
          maxDurability: playerItem.maxDurability,
          weight: playerItem.weight))
    updateCargo(ship = playerShip, cargoIndex = itemIndex, amount = -sellAmount,
        price = playerItem.price, quality = playerItem.quality,
        maxDurability = playerItem.maxDurability, weight = playerItem.weight)
    updateCargo(ship = playerShip, protoIndex = moneyIndex, amount = profit, quality = normal)
    if baseIndex > 0:
      updateBaseCargo(protoIndex = moneyIndex, amount = -profit, quality = normal)
      gainRep(baseIndex = baseIndex, points = 1)
      if itemsList[protoIndex].reputation > skyBases[
          baseIndex].reputation.level:
        gainRep(baseIndex = baseIndex, points = 1)
    else:
      traderCargo[0].amount -= profit
    gainExp(amount = 1, skillNumber = talkingSkill, crewIndex = traderIndex)
    let gain: int = profit - (sellAmount * price)
    addMessage(message = "You sold " & $sellAmount & " " & itemName & " for " &
        $profit & " " & moneyName & "." & (if gain == 0: "" else: " You " & (
        if gain > 0: "gain " else: "lost ") & $(gain.abs) & " " & moneyName &
        " compared to the base price."), mType = tradeMessage)
    if baseIndex > 0 and eventIndex > -1:
      eventsList[eventIndex].time += 5
    updateGame(minutes = 5)

proc buyItems*(baseItemIndex: Natural; amount: string) {.raises: [
    NoTraderError, NoFreeCargoError, NoMoneyError, NotEnoughMoneyError,
    KeyError, ValueError, IOError, Exception], tags: [WriteIOEffect,
    RootEffect], contractual.} =
  ## Buy the selected item from the trader
  ##
  ## * baseItemIndex - the index of the item to buy in the trader's cargo
  ## * amount        - the amount of the item to buy
  let traderIndex: int = findMember(order = talk)
  if traderIndex == -1:
    raise newException(exceptn = NoTraderError, message = "")
  let
    baseIndex: ExtendedBasesRange = skyMap[playerShip.skyX][playerShip.skyY].baseIndex
    eventIndex: int = skyMap[playerShip.skyX][playerShip.skyY].eventIndex
  var
    itemIndex, price: Natural = 0
    itemName: string = ""
  if baseIndex > 0:
    itemIndex = skyBases[baseIndex].cargo[baseItemIndex].protoIndex
    itemName = itemsList[itemIndex].name
    price = skyBases[baseIndex].cargo[baseItemIndex].price
    if eventIndex > -1 and eventsList[eventIndex].eType == doublePrice and
        eventsList[eventIndex].itemIndex == itemIndex:
      price *= 2
  else:
    itemIndex = traderCargo[baseItemIndex].protoIndex
    itemName = itemsList[itemIndex].name
    price = traderCargo[baseItemIndex].price
  let buyAmount: Positive = amount.parseInt
  var cost: Natural = buyAmount * price
  countPrice(price = cost, traderIndex = traderIndex)
  if freeCargo(amount = cost - (itemsList[itemIndex].weight * buyAmount)) < 0:
    raise newException(exceptn = NoFreeCargoError, message = "")
  let moneyAmount: Natural = moneyAmount(inventory = playerShip.cargo)
  if moneyAmount == 0:
    raise newException(exceptn = NoMoneyError, message = itemName)
  if cost > moneyAmount:
    raise newException(exceptn = NotEnoughMoneyError, message = itemName)
  updateMoney(memberIndex = -1, amount = -cost, quality = any)
  if baseIndex > 0:
    updateBaseCargo(protoIndex = moneyIndex, amount = cost, quality = normal)
  else:
    traderCargo[0].amount += cost
  if baseIndex > 0:
    let item: BaseCargo = skyBases[baseIndex].cargo[baseItemIndex]
    updateCargo(ship = playerShip, protoIndex = itemIndex, amount = buyAmount,
        durability = item.durability, price = price, quality = item.quality,
        maxDurability = item.maxDurability, weight = item.weight)
    updateBaseCargo(cargoIndex = baseItemIndex.cint, amount = -buyAmount,
        durability = item.durability, quality = item.quality,
        maxDurability = item.maxDurability, weight = item.weight)
    gainRep(baseIndex = baseIndex, points = 1)
  else:
    let item: BaseCargo = traderCargo[baseItemIndex]
    updateCargo(ship = playerShip, protoIndex = itemIndex, amount = buyAmount,
        durability = item.durability, price = price, quality = item.quality,
        maxDurability = item.maxDurability, weight = item.weight)
    traderCargo[baseItemIndex].amount -= buyAmount
    if traderCargo[baseItemIndex].amount == 0:
      traderCargo.delete(i = baseItemIndex)
  gainExp(amount = 1, skillNumber = talkingSkill, crewIndex = traderIndex)
  let gain: int = (buyAmount * price) - cost
  addMessage(message = "You bought " & $buyAmount & " " & itemName & " for " &
      $cost & " " & moneyName & "." & (if gain == 0: "" else: "You " & (
      if gain > 0: "gain " else: "lost ") & $(gain.abs) & " " & moneyName &
      " compared to the base price."), mType = tradeMessage)
  if baseIndex == 0 and eventIndex > -1:
    eventsList[eventIndex].time += 5
  updateGame(minutes = 5)

proc getTradeData*(iIndex: int): tuple[protoIndex, maxSellAmount, maxBuyAmount,
    price: int; quality: ObjectQuality; maxDurability: ItemsDurability;
    weight: Natural] {.raises: [KeyError], tags: [], contractual.} =
  ## Get the data related to the item during trading
  ##
  ## * iIndex - the index of the item which data will be get. If positive, the
  ##            item is in the player's ship's cargo, negative in the trader's
  ##            cargo.
  ##
  ## Returns tuple with trade data: proto index of the item, max amount of item
  ## to sell, max amount item to buy, its price and quality
  result = (-1, 0, 0, 0, normal, defaultItemDurability, 0)
  var baseCargoIndex, cargoIndex: int = -1
  if iIndex < 0:
    baseCargoIndex = iIndex.abs
  else:
    cargoIndex = iIndex
  if cargoIndex > playerShip.cargo.high:
    return
  let baseIndex: int = skyMap[playerShip.skyX][playerShip.skyY].baseIndex
  if baseIndex == 0 and baseCargoIndex > traderCargo.high:
    return
  elif baseIndex > 0 and baseCargoIndex > skyBases[baseIndex].cargo.high:
    return
  if iIndex < 0:
    if baseIndex == 0:
      result.quality = traderCargo[baseCargoIndex].quality
    else:
      result.quality = skyBases[baseIndex].cargo[baseCargoIndex].quality
  else:
      result.quality = playerShip.cargo[cargoIndex].quality
  if iIndex < 0:
    if baseIndex == 0:
      result.maxDurability = traderCargo[baseCargoIndex].maxDurability
    else:
      result.maxDurability = skyBases[baseIndex].cargo[baseCargoIndex].maxDurability
  else:
      result.maxDurability = playerShip.cargo[cargoIndex].maxDurability
  if iIndex < 0:
    if baseIndex == 0:
      result.weight = traderCargo[baseCargoIndex].weight
    else:
      result.weight = skyBases[baseIndex].cargo[baseCargoIndex].weight
  else:
      result.weight = playerShip.cargo[cargoIndex].weight
  var itemIndex: int = iIndex
  if cargoIndex > -1:
    result.protoIndex = playerShip.cargo[cargoIndex].protoIndex
  else:
    result.protoIndex = (if baseIndex == 0: traderCargo[
        baseCargoIndex].protoIndex else: skyBases[baseIndex].cargo[
        baseCargoIndex].protoIndex)
  let baseType: string = (if baseIndex > 0: skyBases[baseIndex].baseType else: "0")
  if iIndex > -1:
    baseCargoIndex = findBaseCargo(protoIndex = result.protoIndex,
      durability = playerShip.cargo[cargoIndex].durability,
      quality = result.quality)
    if baseCargoIndex > -1:
      result.price = (if baseIndex > 0: skyBases[baseIndex].cargo[
          baseCargoIndex].price else: traderCargo[baseCargoIndex].price)
    else:
      result.price = getPrice(baseType = baseType, itemIndex = result.protoIndex, quality = result.quality)
  else:
    itemIndex = findItem(inventory = playerShip.cargo,
      protoIndex = result.protoIndex,
      durability = (if baseIndex > 0: skyBases[ baseIndex].cargo[
        baseCargoIndex].durability else: traderCargo[
        baseCargoIndex].durability), itemQuality = result.quality)
    result.price = (if baseIndex > 0: skyBases[baseIndex].cargo[
        baseCargoIndex].price else: traderCargo[baseCargoIndex].price)
  if itemIndex > -1:
    result.maxSellAmount = playerShip.cargo[itemIndex].amount
    var maxPrice: Natural = result.maxSellAmount * result.price
    countPrice(price = maxPrice, traderIndex = findMember(order = talk),
        reduce = false)
    if baseIndex > 0 and maxPrice > skyBases[baseIndex].cargo[0].amount:
      result.maxSellAmount = (result.maxSellAmount.float * (skyBases[baseIndex].cargo[
          0].amount.float / maxPrice.float)).floor.int
    elif baseIndex == 0 and maxPrice > traderCargo[0].amount:
      result.maxSellAmount = (result.maxSellAmount.float * (traderCargo[0].amount.float /
          maxPrice.float)).floor.int
    maxPrice = result.maxSellAmount * result.price
    if maxPrice > 0:
      countPrice(price = maxPrice, traderIndex = findMember(order = talk),
          reduce = false)
    var weight: int = freeCargo(amount = (itemsList[result.protoIndex].weight * result.maxSellAmount) - maxPrice)
    while weight < 0:
      result.maxSellAmount = (result.maxSellAmount.float * ((maxPrice + weight).float /
          maxPrice.float)).floor.int
      if result.maxSellAmount < 1:
        break
      maxPrice = result.maxSellAmount * result.price
      countPrice(price = maxPrice, traderIndex = findMember(order = talk),
          reduce = false)
      weight = freeCargo(amount = (itemsList[result.protoIndex].weight * result.maxSellAmount) - maxPrice)
    if baseIndex > 0 and countFreeCargo(baseIndex = baseIndex) == 0 and baseCargoIndex == -1:
      result.maxSellAmount = 0
  let moneyAmount: Natural = moneyAmount(inventory = playerShip.cargo)
  if baseCargoIndex > -1 and moneyAmount > 0 and ((baseIndex > -1 and
      isBuyable(baseType = baseType, itemIndex = result.protoIndex)) or
          baseIndex == 0):
    result.maxBuyAmount = (moneyAmount / result.price).int
    var maxPrice: Natural = result.maxBuyAmount * result.price
    if result.maxBuyAmount > 0:
      countPrice(price = maxPrice, traderIndex = findMember(order = talk))
      if maxPrice < result.maxBuyAmount * result.price:
        result.maxBuyAmount = (result.maxBuyAmount.float * ((result.maxBuyAmount.float *
            result.price.float) / maxPrice.float)).floor.int
      if baseIndex > 0 and result.maxBuyAmount > skyBases[baseIndex].cargo[
          baseCargoIndex].amount:
        result.maxBuyAmount = skyBases[baseIndex].cargo[baseCargoIndex].amount
      elif baseIndex == 0 and result.maxBuyAmount > traderCargo[
          baseCargoIndex].amount:
        result.maxBuyAmount = traderCargo[baseCargoIndex].amount
      maxPrice = result.maxBuyAmount * result.price
      countPrice(price = maxPrice, traderIndex = findMember(order = talk))
      var weight: int = freeCargo(amount = maxPrice - (itemsList[
          result.protoIndex].weight * result.maxBuyAmount))
      while weight < 0:
        result.maxBuyAmount = result.maxBuyAmount + (weight / itemsList[
            result.protoIndex].weight).int - 1
        if result.maxBuyAmount < 0:
          result.maxBuyAmount = 0
        if result.maxBuyAmount == 0:
          break
        maxPrice = result.maxBuyAmount * result.price
        countPrice(price = maxPrice, traderIndex = findMember(order = talk))
        weight = freeCargo(amount = maxPrice - (itemsList[
            result.protoIndex].weight * result.maxBuyAmount))
    if itemIndex == -1:
      itemIndex = -(baseCargoIndex)
