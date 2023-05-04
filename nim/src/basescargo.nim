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

import std/tables
import basestypes, game, maps, trades, types, utils

proc generateCargo*() {.sideEffect, raises: [KeyError], tags: [].} =
  ## Generate the cargo in the selected sky base if needed
  let
    baseIndex = skyMap[playerShip.skyX][playerShip.skyY].baseIndex
    population = (if skyBases[baseIndex].population > 0: skyBases[
        baseIndex].population else: 1)
  var chance = (if population < 150: 5 elif population < 300: 10 else: 15)
  chance = chance + daysDifference(dateToCompare = skyBases[baseIndex].visited,
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
      let amount = (if population < 150: getRandom(min = 1,
          max = 10) elif population < 300: getRandom(min = 1,
          max = 20) else: getRandom(min = 1, max = 30))
      for i in 1 .. amount:
        var itemIndex = getRandom(min = 1, max = itemsList.len)
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
      proc getMaxAmount(amount: Positive): Natural =
        result = (amount / 2).int
        if result < 1:
          result = 1
      for item in skyBases[baseIndex].cargo.mitems:
        let roll = getRandom(min = 1, max = 100)
        if roll < 30 and item.amount > 0:
          item.amount = item.amount - getRandom(min = 1, max = getMaxAmount(
              amount = item.amount))
        elif roll < 60 and skyBases[baseIndex].population > 0:
          item.amount = (if item.amount == 0: getRandom(min = 1, max = 10) *
              population else: item.amount + getRandom(min = 1,
              max = getMaxAmount(amount = item.amount)))

proc findBaseCargo*(protoIndex: Natural;
    durability: ItemsDurability = ItemsDurability.high): int {.sideEffect,
    raises: [], tags: [].} =
  let baseIndex = skyMap[playerShip.skyX][playerShip.skyY].baseIndex

  proc findCargo(localBaseCargo: seq[BaseCargo]): int =
    result = -1
    for index, item in localBaseCargo.pairs:
      if durability < ItemsDurability.high:
        if item.protoIndex == protoIndex and item.durability == durability:
          return index
      else:
        if item.protoIndex == protoIndex:
          return index

  if baseIndex > 0:
    return findCargo(localBaseCargo = skyBases[baseIndex].cargo)
  return findCargo(localBaseCargo = traderCargo)

# Temporary code for interfacing with Ada

proc generateAdaCargo() {.raises: [], tags: [], exportc.} =
  try:
    generateCargo()
  except KeyError:
    discard

proc findAdaBaseCargo(protoIndex, durability: cint): cint {.raises: [], tags: [], exportc.} =
  return findBaseCargo(protoIndex = protoIndex, durability = durability).cint + 1
