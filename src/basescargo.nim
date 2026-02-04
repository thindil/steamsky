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

## Provides code related to the sky bases' cargo, like generating it, finding
## items in them or updating the cargo.

import std/[random, sequtils, tables]
import contracts, nimalyzer
import bases, basestypes, crewinventory, game, items, maps, shipscargo, types, utils

proc generateCargo*() {.raises: [KeyError], tags: [],
    contractual.} =
  ## Generate the cargo in the selected sky base if needed
  let
    baseIndex: ExtendedBasesRange = skyMap[playerShip.skyX][
        playerShip.skyY].baseIndex
    population: Positive = (if getBasePopulation(baseIndex = baseIndex) >
        empty: skyBases[baseIndex].population else: 1)
  var chance: Natural = (if population < 150: 5 elif population <
      300: 10 else: 15)
  chance += daysDifference(dateToCompare = skyBases[baseIndex].visited)
  if skyBases[baseIndex].cargo.len == 0:
    chance = 101
  if getRandom(min = 1, max = 100) > chance:
    return
  var itemsAmount: Natural = case skyBases[baseIndex].size
    of small:
      16
    of medium:
      32
    of big:
      64
    else:
      0
  if skyBases[baseIndex].cargo.len == 0:
    skyBases[baseIndex].cargo.add(y = BaseCargo(protoIndex: moneyIndex,
        amount: getRandom(min = 50, max = 200) * population,
        durability: defaultItemDurability, price: 0, quality: normal))
    var keys: seq[Positive] = itemsList.keys.toSeq
    randomize()
    keys.shuffle()

    for key in keys:
      if isBuyable(baseType = skyBases[baseIndex].baseType, itemIndex = key,
          checkFlag = false):
        var amount: Natural = getRandom(min = 0, max = 100) * population
        if itemsAmount == 0:
          amount = 0
        else:
          itemsAmount.dec
        let quality: ObjectQuality = getQuality()
        skyBases[baseIndex].cargo.add(y = BaseCargo(protoIndex: key,
            amount: amount, durability: defaultItemDurability, price: getPrice(
            baseType = skyBases[baseIndex].baseType, itemIndex = key,
                quality = quality),
            quality: quality))
    if "blackmarket" in basesTypesList[skyBases[baseIndex].baseType].flags:
      var amount: Positive = (if population < 150: getRandom(min = 1,
          max = 10) elif population < 300: getRandom(min = 1,
          max = 20) else: getRandom(min = 1, max = 30))
      if amount > itemsAmount:
        amount = itemsAmount + 1
      for i in 1..amount:
        var itemIndex: Natural = getRandom(min = 1, max = itemsList.len)
        for j in 1..amount:
          itemIndex.dec
          if itemIndex == 0:
            if getPrice(baseType = skyBases[baseIndex].baseType,
                itemIndex = j, quality = normal) > 0:
              let quality: ObjectQuality = getQuality()
              skyBases[baseIndex].cargo.add(y = BaseCargo(protoIndex: j,
                  amount: getRandom(min = 0, max = 100) * population,
                  durability: defaultItemDurability, price: getPrice(
                  baseType = skyBases[baseIndex].baseType, itemIndex = j,
                      quality = quality),
                  quality: quality))
              break
            itemIndex.inc
    else:

      proc getMaxAmount(amount: Positive): Natural {.raises: [],
          tags: [], contractual.} =
        ## Get the max amount from the selected amount of items
        ##
        ## * amount - the amount of item which max value will be taken
        ##
        ## Returns the max amount of the selected item
        result = (amount / 2).int
        if result < 1:
          result = 1

      for item in skyBases[baseIndex].cargo.mitems:
        let roll: Positive = getRandom(min = 1, max = 100)
        if roll < 30 and item.amount > 0:
          item.amount -= getRandom(min = 1, max = getMaxAmount(
              amount = item.amount))
        elif roll < 60 and getBasePopulation(baseIndex = baseIndex) > empty:
          item.amount = (if item.amount == 0: getRandom(min = 1, max = 10) *
              population else: item.amount + getRandom(min = 1,
              max = getMaxAmount(amount = item.amount)))

proc findBaseCargo*(protoIndex: Natural;
    durability: ItemsDurability = defaultItemDurability; quality: ObjectQuality;
    maxDurability: ItemsDurability = defaultItemDurability;
    weight: Natural = 0): int {.raises: [], tags: [], contractual.} =
  ## Find the selected item in the currently visited base's cargo
  ##
  ## * protoIndex    - the index of the prototype to search
  ## * durability    - the durability of the item to search. Can be empty. If not
  ##                   set, the items will not be checked against it.
  ## * quality       - the quality of the item to search.
  ## * maxDurability - The maximum durability of the item to modify. Can be empty
  ## * weight        - The weight of the item. Can be empty
  ##
  ## The index of the item with the selected prototype index or -1 if nothing
  ## found.
  let baseIndex: ExtendedBasesRange = skyMap[playerShip.skyX][
      playerShip.skyY].baseIndex

  proc findCargo(localBaseCargo: seq[BaseCargo]): int {.raises: [],
      tags: [], contractual.} =
    ## Find the index of the item in the base's cargo
    ##
    ## * localBaseCargo - the cargo in which the item will be looked for
    ##
    ## Returns the index of the item in the base's cargo or -1 if the item
    ## not found
    result = -1
    for index, item in localBaseCargo:
      if durability < ItemsDurability.high or quality != normal:
        if item.protoIndex == protoIndex and item.durability == durability and
            item.quality == quality and item.maxDurability == maxDurability and
            item.weight == weight:
          return index
      else:
        if item.protoIndex == protoIndex:
          return index

  if baseIndex > 0:
    return findCargo(localBaseCargo = skyBases[baseIndex].cargo)
  return findCargo(localBaseCargo = traderCargo)

proc countFreeCargo*(baseIndex: ExtendedBasesRange): Natural {.raises: [],
    tags: [], contractual.} =
  ## Count the free space in the selected base's cargo
  ##
  ## * baseIndex - the index of the base which free space will be count
  ##
  ## Returns the amount of free space in the selected base
  result = case skyBases[baseIndex].size
    of small:
      32
    of medium:
      64
    of big:
      128
    else:
      return 0
  for item in skyBases[baseIndex].cargo:
    if item.amount > 0:
      result.dec
    if result == 0:
      break

proc updateBaseCargo*(protoIndex: Natural = 0; amount: int;
    durability: ItemsDurability = defaultItemDurability; cargoIndex: int = -1;
    quality: ObjectQuality;
    maxDurability: ItemsDurability = defaultItemDurability;
    weight: Natural = 0) {.raises: [KeyError, NoFreeSpaceError], tags: [],
    contractual.} =
  ## Update the selected item amount in the cargo of the base where the player
  ## is
  ##
  ## * protoIndex    - the index of the prototype of the item which amount will be
  ##                   upgraded. This argument or cargoIndex argument must always
  ##                   be set, but only one of them.
  ## * amount        - the amount which will be add or removed from the amount of
  ##                   the selected item
  ## * durability    - the durability of the item to search
  ## * cargoIndex    - the index of the item in the base's cargo which will be
  ##                   updated. This argument or protoIndex argument must always
  ##                   be set, but only one of them.
  ## * quality       - the quality of the item to search
  ## * maxDurability - The maximum durability of the item to modify. Can be empty
  ## * weight        - The weight of the item. Can be empty
  let
    baseIndex: ExtendedBasesRange = skyMap[playerShip.skyX][
        playerShip.skyY].baseIndex
    itemIndex: ExtendedNatural = if protoIndex > 0:
        findBaseCargo(protoIndex = protoIndex, durability = durability,
            quality = quality, maxDurability = maxDurability, weight = weight)
      else:
        cargoIndex
  {.ruleOff: "assignments".}
  if amount > 0:
    if itemIndex == -1:
      if countFreeCargo(baseIndex = baseIndex) == 0:
        raise newException(exceptn = NoFreeSpaceError, message = $protoIndex)
      skyBases[baseIndex].cargo.add(y = BaseCargo(protoIndex: protoIndex,
          amount: amount, durability: durability, price: getPrice(
          baseType = skyBases[baseIndex].baseType, itemIndex = protoIndex,
          quality = quality), quality: quality, maxDurability: maxDurability,
          weight: weight))
    else:
      skyBases[baseIndex].cargo[itemIndex].amount = skyBases[baseIndex].cargo[
          itemIndex].amount + amount
  else:
    skyBases[baseIndex].cargo[itemIndex].amount = skyBases[baseIndex].cargo[
        itemIndex].amount + amount
    if skyBases[baseIndex].cargo[itemIndex].amount == 0 and not isBuyable(
        baseType = skyBases[baseIndex].baseType, itemIndex = skyBases[
        baseIndex].cargo[itemIndex].protoIndex) and itemIndex > 0:
      skyBases[baseIndex].cargo.delete(i = itemIndex)
  {.ruleOn: "assignments".}

proc getLootData*(itemIndex: int): tuple[protoIndex, maxAmount,
    cargoMaxAmount: int; quality: ObjectQuality; maxDurability: ItemsDurability;
    weight: Natural] {.raises: [KeyError], tags: [], contractual.} =
  ## Get the data related to the item during looting a base
  ##
  ## * iIndex - the index of the item which data will be get. If positive, the
  ##            item is in the player's ship's cargo, negative in the base's
  ##            cargo.
  ##
  ## Returns tuple with trade data: proto index of the item, max amount of item
  ## to get, max amount item to drop, its quality, maximum durability and weight
  var
    cargoIndex: ExtendedNatural = -1
    baseCargoIndex: ExtendedNatural = 0
  if itemIndex < 0:
    baseCargoIndex = (itemIndex + 1).abs
  else:
    cargoIndex = itemIndex - 1
  let baseIndex: ExtendedBasesRange = skyMap[playerShip.skyX][
      playerShip.skyY].baseIndex
  if cargoIndex > playerShip.cargo.high or baseCargoIndex > skyBases[
      baseIndex].cargo.high:
    return
  result.protoIndex = (if cargoIndex > -1: playerShip.cargo[
      cargoIndex].protoIndex else: skyBases[baseIndex].cargo[
      baseCargoIndex].protoIndex)
  result.quality = (if cargoIndex > -1: playerShip.cargo[
        cargoIndex].quality else: skyBases[baseIndex].cargo[
        baseCargoIndex].quality)
  result.maxDurability = (if cargoIndex > -1: playerShip.cargo[
        cargoIndex].maxDurability else: skyBases[baseIndex].cargo[
        baseCargoIndex].maxDurability)
  result.weight = (if cargoIndex > -1: playerShip.cargo[
        cargoIndex].weight else: skyBases[baseIndex].cargo[
        baseCargoIndex].weight)
  if cargoIndex > 0:
    baseCargoIndex = findBaseCargo(protoIndex = result.protoIndex,
        quality = result.quality, maxDurability = result.maxDurability,
        weight = result.weight)
  else:
    cargoIndex = findItem(inventory = playerShip.cargo,
        protoIndex = result.protoIndex, itemQuality = result.quality,
        maxDurability = result.maxDurability, weight = result.weight)
  result.maxAmount = (if baseCargoIndex > -1: skyBases[baseIndex].cargo[
      baseCargoIndex].amount else: 0)
  let freeAmount: range[-100_000..100_000] = (if baseCargoIndex > -1: (
      freeCargo(amount = 0).float / itemsList[skyBases[baseIndex].cargo[
      baseCargoIndex].protoIndex].weight.float).Natural else: 0)
  result.cargoMaxAmount = (if cargoIndex > -1: playerShip.cargo[
      cargoIndex].amount.Natural else: 0)
  if result.maxAmount > freeAmount:
    result.maxAmount = freeAmount
