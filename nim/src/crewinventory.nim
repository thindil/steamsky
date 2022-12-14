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

type CrewNoSpaceError* = object of CatchableError
  ## FUNCTION
  ##
  ## Raised when there is no space for new item in crew member inventory

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
  result = result + amount

proc itemIsUsed*(memberIndex, itemIndex: Natural): bool {.sideEffect, raises: [
    ], tags: [].} =
  ## FUNCTION
  ##
  ## Check if the item is currently used by the selected crew member in the
  ## player ship crew.
  ##
  ## PARAMETERS
  ##
  ## * memberIndex - the index of the crew member which will be check for usage
  ## * itemIndex   - the index of the item which will be check
  ##
  ## RETURNS
  ##
  ## True if the item is used, otherwise false
  return itemIndex in playerShip.crew[memberIndex].equipment

proc takeOffItem*(memberIndex, itemIndex: Natural) {.sideEffect, raises: [],
    tags: [].} =
  ## FUNCTION
  ##
  ## Stop using the selected item by the selected the player's ship crew
  ## member.
  ##
  ## PARAMETERS
  ##
  ## * memberIndex - the index of the crew member which will stop using the item
  ## * itemIndex   - the index of the item to stop using
  for i in playerShip.crew[memberIndex].equipment.low..playerShip.crew[
      memberIndex].equipment.high:
    if playerShip.crew[memberIndex].equipment[i] == itemIndex:
      playerShip.crew[memberIndex].equipment[i] = -1
      break

proc updateInventory*(memberIndex: Natural; amount: int;
    protoIndex: Natural = 0; durability: ItemsDurability = 0;
    inventoryIndex: int = -1; price: Natural = 0;
    ship: var ShipRecord) {.sideEffect, raises: [CrewNoSpaceError, KeyError],
    tags: [].} =
  ## FUNCTION
  ##
  ## Update the inventory of the selected crew member.
  ##
  ## PARAMETERS
  ##
  ## * memberIndex    - the index of the crew member which inventory will be updated
  ## * amount         - the amount of which the selected item will be updated
  ## * protoIndex     - the index of the prototype of the item which will be updated.
  ##                    If set 0, inventoryIndex must be set then. Default value is 0.
  ## * durability     - the durability of the item to update. Is greater than 0, the
  ##                    inventory will be looking for only items with that durability.
  ##                    Default value is 0.
  ## * inventoryIndex - the index of the item in the inventory. If set 0, protoIndex
  ##                    must be set then. Default value is 0.
  ## * price          - the price of the item to update. Default value is 0.
  ## * ship           - the ship in which the crew member inventory will be updated
  ##
  ## RETURNS
  ##
  ## The updated ship argument
  var itemIndex: int
  if inventoryIndex == -1:
    if durability > 0:
      itemIndex = findItem(inventory = ship.crew[memberIndex].inventory,
          protoIndex = protoIndex, durability = durability)
    else:
      itemIndex = findItem(inventory = ship.crew[memberIndex].inventory,
          protoIndex = protoIndex)
  else:
    itemIndex = inventoryIndex
  if amount > 0:
    let weight = if itemIndex > 0:
        itemsList[ship.crew[memberIndex].inventory[
            itemIndex].protoIndex].weight * amount
      else:
        itemsList[protoIndex].weight * amount
    if freeInventory(memberIndex = memberIndex, amount = 0 - weight) < 0:
      raise newException(exceptn = CrewNoSpaceError,
          message = ship.crew[memberIndex].name & " doesn't have any free space in their inventory.")
  else:
    if itemIsUsed(memberIndex = memberIndex, itemIndex = itemIndex):
      takeOffItem(memberIndex = memberIndex, itemIndex = itemIndex)
  if itemIndex == -1:
    ship.crew[memberIndex].inventory.add(y = InventoryData(
        protoIndex: protoIndex, amount: amount, name: itemsList[
        protoIndex].name, durability: durability, price: price))
  else:
    let newAmount = ship.crew[memberIndex].inventory[itemIndex].amount + amount
    if newAmount == 0:
      {.warning[UnsafeSetLen]: off.}
      ship.crew[memberIndex].inventory.delete(i = itemIndex)
      {.warning[UnsafeSetLen]: on.}
    else:
      ship.crew[memberIndex].inventory[itemIndex].amount = newAmount

# Temporary code for interfacing with Ada

proc freeAdaInventory(memberIndex, amount: cint): cint {.exportc.} =
  return freeInventory(memberIndex = (memberIndex - 1).Natural,
      amount = amount).cint

proc itemAdaIsUsed(memberIndex, itemIndex: cint): cint {.exportc.} =
  return itemIsUsed(memberIndex = (memberIndex - 1), itemIndex = (itemIndex - 1)).ord.cint

proc takeAdaOffItem(memberIndex, itemIndex: cint) {.exportc.} =
  takeOffItem(memberIndex = (memberIndex - 1), itemIndex = (itemIndex - 1))

proc equipmentToAda(memberIndex: cint; equipment: var array[0..6,
    cint]) {.exportc.} =
  for i in 0..6:
    equipment[i] = playerShip.crew[(memberIndex - 1)].equipment[
        i.EquipmentLocations].cint + 1

proc updateAdaInventory(memberIndex, amount, protoIndex, durability,
    inventoryIndex, price, inPlayerShip: cint): cint {.exportc.} =
  try:
    if inPlayerShip == 1:
      updateInventory(memberIndex = (memberIndex - 1), amount = amount,
          protoIndex = protoIndex, durability = durability, inventoryIndex = (
          inventoryIndex - 1), price = price, ship = playerShip)
    else:
      updateInventory(memberIndex = (memberIndex - 1), amount = amount,
          protoIndex = protoIndex, durability = durability, inventoryIndex = (
          inventoryIndex - 1), price = price, ship = npcShip)
    return 1
  except CrewNoSpaceError, KeyError:
    return 0
