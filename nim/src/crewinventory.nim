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

proc freeInventory*(memberIndex: Natural; amount: int): int {.sideEffect,
    raises: [], tags: [].} =
  ## FUNCTION
  ##
  ## Get the amount of free space in the selected player ship's crew member's
  ## inventory.
  ##
  ## PARAMETERS
  ##
  ## * memberIndex - the index of the crew member which inventory will be check
  ## * amount      - the amount of kilograms to add or remove during the check
  ##
  ## RETURNS
  ##
  ## The amount of kilograms of free space in the crew member's inventory
  result = 50 + playerShip.crew[memberIndex].attributes[strengthIndex].level
  for item in playerShip.crew[memberIndex].inventory:
    try:
      result = result - (itemsList[item.protoIndex].weight * item.amount)
    except KeyError:
      discard

# TODO: unfinished
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

# Temporary code for interfacing with Ada

proc freeAdaInventory(memberIndex, amount, crewAmount: cint; inventory: array[
    128, AdaInventoryData]): cint {.exportc.} =
  if playerShip.crew.len == 0:
    for i in 1..crewAmount:
      playerShip.crew.add(y = MemberData(homeBase: 1))
  playerShip.crew[memberIndex - 1].inventory = inventoryToNim(
      inventory = inventory)
  return freeInventory(memberIndex = (memberIndex - 1).Natural,
      amount = amount).cint
