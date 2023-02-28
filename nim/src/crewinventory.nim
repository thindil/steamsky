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
import game, shipscargo, types, utils

proc findItem*(inventory: seq[InventoryData];
    protoIndex: Natural = 0; itemType: string = "";
    durability: ItemsDurability = ItemsDurability.high;
    quality: Positive = 100): int {.sideEffect, raises: [], tags: [].} =
  ## Find the index of the selected item in the selected inventory
  ##
  ## * inventory  - the inventory in which the item will be looking for
  ## * protoIndex - the index of prototype item of the item to find. Can be
  ##                empty. If empty, itemType parameter must be set
  ## * itemType   - the type of prototype item of the item to find. Can be
  ##                empty. If empty, protoIndex parameter must be set
  ## * durability - the durability of the item to find. Can be empty
  ## * quality    - the quality of the item to find. Can be empty
  ##
  ## Returns the index of the item in the selected inventory which meet searching
  ## criteria or -1 if item not found.
  try:
    if protoIndex > 0:
      for index, item in inventory.pairs:
        if item.protoIndex == protoIndex and itemsList[protoIndex].value[1] <= quality:
          if durability < ItemsDurability.high and item.durability == durability:
            return index
          else:
            return index
    elif itemType.len > 0:
      for index, item in inventory.pairs:
        if itemsList[item.protoIndex].itemType == itemType and itemsList[
            item.protoIndex].value[1] <= quality:
          if durability < ItemsDurability.high and item.durability == durability:
            return index
          else:
            return index
  except KeyError:
    discard
  return -1

proc freeInventory*(memberIndex: Natural; amount: int): int {.sideEffect,
    raises: [], tags: [].} =
  ## Get the amount of free space in the selected player ship's crew member's
  ## inventory.
  ##
  ## * memberIndex - the index of the crew member which inventory will be check
  ## * amount      - the amount of kilograms to add or remove during the check
  ##
  ## Returns the amount of kilograms of free space in the crew member's inventory
  result = 50 + playerShip.crew[memberIndex].attributes[strengthIndex].level
  for item in playerShip.crew[memberIndex].inventory:
    try:
      result = result - (itemsList[item.protoIndex].weight * item.amount)
    except KeyError:
      discard
  result = result + amount

proc itemIsUsed*(memberIndex, itemIndex: Natural): bool {.sideEffect, raises: [
    ], tags: [].} =
  ## Check if the item is currently used by the selected crew member in the
  ## player ship crew.
  ##
  ## * memberIndex - the index of the crew member which will be check for usage
  ## * itemIndex   - the index of the item which will be check
  ##
  ## Returns true if the item is used, otherwise false
  return itemIndex in playerShip.crew[memberIndex].equipment

proc takeOffItem*(memberIndex, itemIndex: Natural) {.sideEffect, raises: [],
    tags: [].} =
  ## Stop using the selected item by the selected the player's ship crew
  ## member.
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
  ## Update the inventory of the selected crew member.
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
  ## Returns the updated ship argument
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

proc damageItem*(inventory: var seq[InventoryData]; itemIndex: Natural;
    skillLevel: Natural = 0; memberIndex: int = -1;
    ship: var ShipRecord) {.sideEffect, raises: [KeyError, CrewNoSpaceError],
    tags: [].} =
  ## Check if item in the inventory was damaged, if yes, update inventory and
  ## the ship cargo
  ##
  ## * inventory   - the inventory in which the item will be check
  ## * itemIndex   - the index of the item in the inventory which will be check
  ## * skillLevel  - the skill level of the crew member which affects chance to
  ##                 damage. Default value is 0 (no skill affecting).
  ## * memberIndex - the index of the crew member to which the item belongs. Default
  ##                 value is -1
  ## * ship        - the ship in which the item will be check
  ##
  ## Returns the updated parameters inventory and ship
  var
    item = inventory[itemIndex]
    damageChance = itemsList[item.protoIndex].value[1]
  if skillLevel > 0:
    damageChance = damageChance - (skillLevel / 5).int
    if damageChance < 1:
      damageChance = 1
  if getRandom(min = 1, max = 100) > damageChance:
    return
  if item.amount > 1:
    inventory.add(y = InventoryData(protoIndex: item.protoIndex,
        amount: item.amount - 1, name: item.name, durability: item.durability,
        price: item.price))
    item.amount = 1
  if item.durability > ItemsDurability.low:
    item.durability.dec
  # Item destroyed
  if item.durability == 0:
    if memberIndex == 0:
      updateCargo(ship = ship, cargoIndex = itemIndex, amount = -1)
    else:
      updateInventory(memberIndex = memberIndex, amount = -1,
          inventoryIndex = itemIndex, ship = ship)
    return
  inventory[itemIndex] = item
  var i = 0
  while i <= inventory.high:
    for j in inventory.low..inventory.high:
      if inventory[i].protoIndex == inventory[j].protoIndex and inventory[
          i].durability == inventory[j].durability and i != j:
        if memberIndex == -1:
          updateCargo(ship = ship, cargoIndex = j, amount = 0 - inventory[j].amount)
          updateCargo(ship = ship, cargoIndex = i, amount = inventory[j].amount)
        else:
          updateInventory(memberIndex = memberIndex, amount = 0 - inventory[
              j].amount, inventoryIndex = j, ship = ship)
          updateInventory(memberIndex = memberIndex, amount = inventory[
              j].amount, inventoryIndex = i, ship = ship)
        i.dec
        break
    i.inc

# Temporary code for interfacing with Ada

proc findAdaItem(inventory: array[128, AdaInventoryData]; protoIndex: cint;
    itemType: cstring; durability: cint; quality: cint): cint {.sideEffect,
    raises: [], tags: [], exportc.} =
  return (findItem(inventory = inventoryToNim(inventory = inventory),
      protoIndex = protoIndex, itemType = $itemType, durability = durability,
      quality = quality) + 1).cint

proc freeAdaInventory(memberIndex, amount: cint): cint {.raises: [], tags: [], exportc.} =
  return freeInventory(memberIndex = (memberIndex - 1).Natural,
      amount = amount).cint

proc itemAdaIsUsed(memberIndex, itemIndex: cint): cint {.raises: [], tags: [], exportc.} =
  return itemIsUsed(memberIndex = (memberIndex - 1), itemIndex = (itemIndex - 1)).ord.cint

proc takeAdaOffItem(memberIndex, itemIndex: cint) {.raises: [], tags: [], exportc.} =
  takeOffItem(memberIndex = (memberIndex - 1), itemIndex = (itemIndex - 1))

proc equipmentToAda(memberIndex: cint; equipment: var array[0..6,
    cint]) {.raises: [], tags: [], exportc.} =
  for i in 0..6:
    equipment[i] = playerShip.crew[(memberIndex - 1)].equipment[
        i.EquipmentLocations].cint + 1

proc updateAdaInventory(memberIndex, amount, protoIndex, durability,
    inventoryIndex, price, inPlayerShip: cint): cint {.raises: [], tags: [], exportc.} =
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

proc damageAdaItem(inventory: var array[128, AdaInventoryData]; itemIndex,
    skillLevel, memberIndex, inPlayerShip: cint) {.raises: [], tags: [], exportc.} =
  var nimInventory = inventoryToNim(inventory = inventory)
  try:
    if inPlayerShip == 1:
      damageItem(inventory = nimInventory, itemIndex = (itemIndex - 1),
          skillLevel = skillLevel, memberIndex = (memberIndex - 1), playerShip)
    else:
      damageItem(inventory = nimInventory, itemIndex = (itemIndex - 1),
          skillLevel = skillLevel, memberIndex = (memberIndex - 1), npcShip)
  except KeyError, CrewNoSpaceError:
    discard
  inventory = inventoryToAda(inventory = nimInventory)
