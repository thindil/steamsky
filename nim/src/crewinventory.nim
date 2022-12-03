# Copyright 2022 Bartek thindil Jasicki
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
import game, items, types, ships

proc freeInventory*(memberIndex: Positive; amount: int): int =
  result = 50 + playerShip.crew[memberIndex].attributes[strengthIndex].level
  for item in playerShip.crew[memberIndex].inventory:
    result = result - (itemsList[item.protoIndex].weight * item.amount)

proc updateInventory*(memberIndex: Positive; amount: int;
    protoIndex: Natural = 0; durability: ItemsDurability = 0; inventoryIndex,
    price: Natural; ship: var ShipRecord) =
  var itemIndex: int
  if inventoryIndex == 0:
    if durability > 0:
      itemIndex = findItem(inventory = ship.crew[memberIndex].inventory,
          protoIndex = protoIndex, durability = durability)
    else:
      itemIndex = findItem(inventory = ship.crew[memberIndex].inventory,
          protoIndex = protoIndex)
  else:
    itemIndex = inventoryIndex
