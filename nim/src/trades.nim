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
import crewinventory, game, ships, types, utils

proc generateTraderCargo*(protoIndex: Positive) =
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
    let cargoItemIndex = findItem(inventory = traderShip.cargo, protoIndex = newItemIndex)
    if cargoItemIndex > -1:
      traderCargo[cargoItemIndex].amount = traderCargo[cargoItemIndex].amount + itemAmount
      traderShip.cargo[cargoItemIndex].amount = traderShip.cargo[cargoItemIndex].amount + itemAmount
    else:
      discard
    cargoAmount.dec
