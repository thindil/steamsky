# Copyright 2022-2025 Bartek thindil Jasicki
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

## Provides code related to the player's ship's crew members' inventories,
## like finding items in them, counting free space, damaging items, etc.

import std/tables
import contracts
import game, shipscargo, types, utils

proc findItem*(inventory: seq[InventoryData];
    protoIndex: Natural = 0; itemType: string = "";
    durability: ItemsDurability = ItemsDurability.high;
    quality: Positive = 100): int {.raises: [], tags: [],
        contractual.} =
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
  ensure:
    result in -1..inventory.high
  body:
    result = -1
    try:
      if protoIndex > 0:
        for index, item in inventory:
          if item.protoIndex == protoIndex and itemsList[protoIndex].value[1] <= quality:
            if durability < ItemsDurability.high and item.durability != durability:
              continue
            return index
      elif itemType.len > 0:
        for index, item in inventory:
          if itemsList[item.protoIndex].itemType == itemType and itemsList[
              item.protoIndex].value[1] <= quality:
            if durability < ItemsDurability.high and item.durability != durability:
              continue
            return index
    except KeyError:
      discard

proc freeInventory*(memberIndex: Natural; amount: int): int {.raises: [], tags: [], contractual.} =
  ## Get the amount of free space in the selected player ship's crew member's
  ## inventory.
  ##
  ## * memberIndex - the index of the crew member which inventory will be check
  ## * amount      - the amount of kilograms to add or remove during the check
  ##
  ## Returns the amount of kilograms of free space in the crew member's inventory
  require:
    memberIndex in playerShip.crew.low..playerShip.crew.high
  body:
    result = 50 + playerShip.crew[memberIndex].attributes[strengthIndex].level
    for item in playerShip.crew[memberIndex].inventory:
      try:
        result -= (itemsList[item.protoIndex].weight * item.amount)
      except KeyError:
        discard
    result += amount

proc itemIsUsed*(memberIndex, itemIndex: Natural): bool {.raises: [
    ], tags: [], contractual.} =
  ## Check if the item is currently used by the selected crew member in the
  ## player ship crew.
  ##
  ## * memberIndex - the index of the crew member which will be check for usage
  ## * itemIndex   - the index of the item which will be check
  ##
  ## Returns true if the item is used, otherwise false
  require:
    memberIndex in playerShip.crew.low..playerShip.crew.high
  body:
    return itemIndex in playerShip.crew[memberIndex].equipment

proc takeOffItem*(memberIndex, itemIndex: Natural) {.raises: [],
    tags: [], contractual.} =
  ## Stop using the selected item by the selected the player's ship crew
  ## member.
  ##
  ## * memberIndex - the index of the crew member which will stop using the item
  ## * itemIndex   - the index of the item to stop using
  require:
    memberIndex in playerShip.crew.low..playerShip.crew.high
  body:
    for item in playerShip.crew[memberIndex].equipment.mitems:
      if item == itemIndex:
        item = -1
        break

proc updateInventory*(memberIndex: Natural; amount: int;
    protoIndex: Natural = 0; durability: ItemsDurability = 0;
    inventoryIndex: int = -1; price: Natural = 0;
    ship: var ShipRecord) {.raises: [CrewNoSpaceError, KeyError],
    tags: [], contractual.} =
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
  require:
    memberIndex in playerShip.crew.low..playerShip.crew.high
  body:
    var itemIndex: int = -1
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
      let weight: Positive = if itemIndex > 0:
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
      let newAmount: Natural = ship.crew[memberIndex].inventory[
          itemIndex].amount + amount
      if newAmount == 0:
        {.warning[UnsafeSetLen]: off.}
        ship.crew[memberIndex].inventory.delete(i = itemIndex)
        {.warning[UnsafeSetLen]: on.}
        for item in playerShip.crew[memberIndex].equipment.mitems:
          if item > itemIndex:
            item.dec
      else:
        ship.crew[memberIndex].inventory[itemIndex].amount = newAmount

proc damageItem*(inventory: var seq[InventoryData]; itemIndex: Natural;
    skillLevel: Natural = 0; memberIndex: int = -1;
    ship: var ShipRecord) {.raises: [KeyError, CrewNoSpaceError],
    tags: [], contractual.} =
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
    item: InventoryData = inventory[itemIndex]
    damageChance: int = itemsList[item.protoIndex].value[1]
  if skillLevel > 0:
    damageChance -= (skillLevel / 5).int
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
    if memberIndex == -1:
      updateCargo(ship = ship, cargoIndex = itemIndex, amount = -1)
    else:
      updateInventory(memberIndex = memberIndex, amount = -1,
          inventoryIndex = itemIndex, ship = ship)
    return
  inventory[itemIndex] = item
  var i: int = 0
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

proc getTrainingToolQuality*(memberIndex: Natural;
    skillIndex: Positive): Positive {.raises: [KeyError], tags: [],
        contractual.} =
  ## Get the required tools quality for the selected skill
  ##
  ## * memberIndex - the index of the crew member in the player ship
  ## * skillIndex  - the index of the skill which training tool quality will be get
  ##
  ## Returns numeric value for the minimum training tool quality required to
  ## train the selected skill.
  require:
    memberIndex in playerShip.crew.low..playerShip.crew.high
  body:
    result = 100
    for skill in playerShip.crew[memberIndex].skills:
      if skill.index == skillIndex:
        for quality in skillsList[skillIndex].toolsQuality:
          if skill.level <= quality.level:
            return quality.quality
