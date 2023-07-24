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
import crewinventory, game, ships, shipscargo, types, utils

proc generateTraderCargo*(protoIndex: Positive) {.sideEffect, raises: [
    KeyError], tags: [].} =
  ## Generate the list of items for trade.
  ##
  ## * protoIndex - the index of the ship's prototype from which the cargo will
  ##                be generated
  var traderShip = createShip(protoIndex = protoIndex, name = "",
        x = playerShip.skyX, y = playerShip.skyY, speed = fullStop)
  traderCargo = @[]
  for item in traderShip.cargo:
    traderCargo.add(BaseCargo(protoIndex: item.protoIndex, amount: item.amount,
        durability: defaultItemDurability, price: itemsList[
        item.protoIndex].price))
  var cargoAmount = if traderShip.crew.len < 5: getRandom(min = 1, max = 3)
      elif traderShip.crew.len < 10: getRandom(min = 1, max = 5)
      else: getRandom(min = 1, max = 10)
  while cargoAmount > 0:
    var
      itemAmount = if traderShip.crew.len < 5: getRandom(min = 1, max = 100)
        elif traderShip.crew.len < 10: getRandom(min = 1, max = 500)
        else: getRandom(min = 1, max = 1000)
      itemIndex = getRandom(min = 1, max = itemsList.len)
      newItemIndex = 0
    for i in 1 .. itemsList.len:
      itemIndex.dec
      if itemIndex == 0:
        newItemIndex = i
        break
    let cargoItemIndex = findItem(inventory = traderShip.cargo,
        protoIndex = newItemIndex)
    if cargoItemIndex > -1:
      traderCargo[cargoItemIndex].amount = traderCargo[cargoItemIndex].amount + itemAmount
      traderShip.cargo[cargoItemIndex].amount = traderShip.cargo[
          cargoItemIndex].amount + itemAmount
    else:
      if freeCargo(amount = 0 - (itemsList[newItemIndex].weight * itemAmount)) > -1:
        traderCargo.add(BaseCargo(protoIndex: newItemIndex, amount: itemAmount,
            durability: defaultItemDurability, price: itemsList[
            newItemIndex].price))
        traderShip.cargo.add(InventoryData(protoIndex: newItemIndex,
            amount: itemAmount, durability: defaultItemDurability, name: "", price: 0))
      else:
        cargoAmount = 1
    cargoAmount.dec

# Temporary code for interfacing with Ada

proc generateAdaTraderCargo(protoIndex: cint) {.raises: [], tags: [], exportc.} =
  try:
    generateTraderCargo(protoIndex = protoIndex)
  except KeyError:
    discard
