# Copyright 2022-2023 Bartek thindil Jasicki
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
import game, ships, types

proc updateCargo*(ship: var ShipRecord; protoIndex: Natural = 0; amount: int;
    durability: ItemsDurability = defaultItemDurability; cargoIndex,
    price: Natural = 0) {.sideEffect, raises: [], tags: [].} =
  ## Updated the selected ship cargo, add or remove items to it
  ##
  ## * ship       - The ship which cargo will be updated
  ## * protoIndex - The prototype index of the item which will be modified. Can
  ##                be empty if cargoIndex is set
  ## * amount     - The amount of the item to add or delete from the cargo
  ## * durability - The durability of the item to modify. Can be empty
  ## * cargoIndex - The ship cargo index of the item which will be modified. Can
  ##                be empty if protoIndex is set
  ## * price      - The price of the item which will be modified
  ##
  ## Returns the modified ship parameter
  var itemIndex: Natural = 0
  if protoIndex > 0 and cargoIndex == 0:
    for index, item in ship.cargo.pairs:
      if item.protoIndex == protoIndex and item.durability == durability:
        itemIndex = index
        break
  else:
    itemIndex = cargoIndex
  if itemIndex == 0 and (protoIndex == 0 or amount < 0):
    return
  if itemIndex == 0:
    ship.cargo.add(y = InventoryData(protoIndex: protoIndex, amount: amount,
        name: "", durability: durability, price: price))
    return
  let newAmount: int = ship.cargo[itemIndex].amount + amount
  if newAmount < 1:
    {.warning[UnsafeSetLen]: off.}
    ship.cargo.delete(i = itemIndex)
    {.warning[UnsafeSetLen]: on.}
    for module in ship.modules.mitems:
      if module.mType == ModuleType2.gun:
        if module.ammoIndex > itemIndex:
          module.ammoIndex.dec
        elif module.ammoIndex == itemIndex:
          module.ammoIndex = 0
    return
  ship.cargo[itemIndex].amount = newAmount
  ship.cargo[itemIndex].price = price

proc freeCargo*(amount: int; ship: ShipRecord = playerShip): int {.sideEffect,
    raises: [KeyError], tags: [].} =
  ## Get the amount of free space in the selected ship's cargo
  ##
  ## * amount - the amount of space which will be taken or add from the current cargo
  ## * ship   - the ship in which the free space will be check
  ##
  ## Returns the amount of free space after adding or removing the amount
  ## parameter.
  result = 0
  for module in ship.modules:
    if module.mType == cargoRoom and module.durability > 0:
      result = result + modulesList[module.protoIndex].maxValue
  for item in ship.cargo:
    result = result - (itemsList[item.protoIndex].weight * item.amount)
  result = result + amount

# Temporary code for interfacing with Ada

proc freeAdaCargo(amount: cint; getPlayerShip: cint = 1): cint {.exportc.} =
  if getPlayerShip == 1:
    return freeCargo(amount = amount).cint
  else:
    return freeCargo(amount = amount, ship = npcShip).cint
