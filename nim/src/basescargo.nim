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

## Provides code related to the sky bases' cargo, like generating it, finding
## items in them or updating the cargo.

import std/tables
import contracts, nimalyzer
import basestypes, game, maps, types, utils

proc generateCargo*() {.sideEffect, raises: [KeyError], tags: [],
    contractual.} =
  ## Generate the cargo in the selected sky base if needed
  let
    baseIndex: ExtendedBasesRange = skyMap[playerShip.skyX][
        playerShip.skyY].baseIndex
    population: Positive = (if skyBases[baseIndex].population > 0: skyBases[
        baseIndex].population else: 1)
  var chance: int = (if population < 150: 5 elif population <
      300: 10 else: 15)
  chance += daysDifference(dateToCompare = skyBases[baseIndex].visited,
      currentDate = gameDate)
  if skyBases[baseIndex].cargo.len == 0:
    chance = 101
  if getRandom(min = 1, max = 100) > chance:
    return
  if skyBases[baseIndex].cargo.len == 0:
    skyBases[baseIndex].cargo.add(y = BaseCargo(protoIndex: moneyIndex,
        amount: getRandom(min = 50, max = 200) * population,
        durability: defaultItemDurability, price: 0))
    for i in itemsList.keys:
      if isBuyable(baseType = skyBases[baseIndex].baseType, itemIndex = i,
          checkFlag = false):
        skyBases[baseIndex].cargo.add(y = BaseCargo(protoIndex: i,
            amount: getRandom(min = 0, max = 100) * population,
            durability: defaultItemDurability, price: getPrice(
            baseType = skyBases[baseIndex].baseType, itemIndex = i)))
    if "blackmarket" in basesTypesList[skyBases[baseIndex].baseType].flags:
      let amount: Positive = (if population < 150: getRandom(min = 1,
          max = 10) elif population < 300: getRandom(min = 1,
          max = 20) else: getRandom(min = 1, max = 30))
      for i in 1 .. amount:
        var itemIndex: Natural = getRandom(min = 1, max = itemsList.len)
        for j in 1 .. amount:
          itemIndex.dec
          if itemIndex == 0:
            if getPrice(baseType = skyBases[baseIndex].baseType,
                itemIndex = j) > 0:
              skyBases[baseIndex].cargo.add(y = BaseCargo(protoIndex: j,
                  amount: getRandom(min = 0, max = 100) * population,
                  durability: defaultItemDurability, price: getPrice(
                  baseType = skyBases[baseIndex].baseType, itemIndex = j)))
              break
            itemIndex.inc
    else:

      proc getMaxAmount(amount: Positive): Natural {.sideEffect, raises: [],
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
        elif roll < 60 and skyBases[baseIndex].population > 0:
          item.amount = (if item.amount == 0: getRandom(min = 1, max = 10) *
              population else: item.amount + getRandom(min = 1,
              max = getMaxAmount(amount = item.amount)))

proc findBaseCargo*(protoIndex: Natural;
    durability: ItemsDurability = ItemsDurability.high): int {.sideEffect,
    raises: [], tags: [], contractual.} =
  ## Find the selected item in the currently visited base's cargo
  ##
  ## * protoIndex - the index of the prototype to search
  ## * durability - the durability of the item to search. Can be empty. If not
  ##                set, the items will not be checked against it.
  ##
  ## The index of the item with the selected prototype index or -1 if nothing
  ## found.
  let baseIndex: ExtendedBasesRange = skyMap[playerShip.skyX][
      playerShip.skyY].baseIndex

  proc findCargo(localBaseCargo: seq[BaseCargo]): int {.sideEffect, raises: [],
      tags: [], contractual.} =
    ## Find the index of the item in the base's cargo
    ##
    ## * localBaseCargo - the cargo in which the item will be looked for
    ##
    ## Returns the index of the item in the base's cargo or -1 if the item
    ## not found
    result = -1
    for index, item in localBaseCargo:
      if durability < ItemsDurability.high:
        if item.protoIndex == protoIndex and item.durability == durability:
          return index
      else:
        if item.protoIndex == protoIndex:
          return index

  if baseIndex > 0:
    return findCargo(localBaseCargo = skyBases[baseIndex].cargo)
  return findCargo(localBaseCargo = traderCargo)

proc updateBaseCargo*(protoIndex: Natural = 0; amount: int;
    durability: ItemsDurability = defaultItemDurability;
    cargoIndex: cint = -1) {.sideEffect, raises: [KeyError], tags: [],
        contractual.} =
  ## Update the selected item amount in the cargo of the base where the player
  ## is
  ##
  ## * protoIndex - the index of the prototype of the item which amount will be
  ##                upgraded. This argument or cargoIndex argument must always
  ##                be set, but only one of them.
  ## * amount     - the amount which will be add or removed from the amount of
  ##                the selected item
  ## * durability - the durability of the item to search
  ## * cargoIndex - the index of the item in the base's cargo which will be
  ##                updated. This argument or protoIndex argument must always
  ##                be set, but only one of them.
  let
    baseIndex: ExtendedBasesRange = skyMap[playerShip.skyX][
        playerShip.skyY].baseIndex
    itemIndex: int = if protoIndex > 0:
        findBaseCargo(protoIndex = protoIndex, durability = durability)
      else:
        cargoIndex
  {.ruleOff: "assignments".}
  if amount > 0:
    if itemIndex == -1:
      skyBases[baseIndex].cargo.add(y = BaseCargo(protoIndex: protoIndex,
          amount: amount, durability: durability, price: getPrice(
          baseType = skyBases[baseIndex].baseType, itemIndex = protoIndex)))
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

# Temporary code for interfacing with Ada

proc generateAdaCargo() {.raises: [], tags: [], exportc, contractual.} =
  ## Temporary C binding
  try:
    generateCargo()
  except KeyError:
    discard

proc findAdaBaseCargo(protoIndex, durability: cint): cint {.raises: [], tags: [
    ], exportc, contractual.} =
  ## Temporary C binding
  return findBaseCargo(protoIndex = protoIndex, durability = durability).cint + 1

proc updateAdaBaseCargo(protoIndex, amount, durability,
    cargoIndex: cint) {.raises: [], tags: [], exportc, contractual.} =
  ## Temporary C binding
  try:
    updateBaseCargo(protoIndex = protoIndex, amount = amount,
        durability = durability, cargoIndex = cargoIndex - 1)
  except KeyError:
    discard
