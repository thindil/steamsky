discard """
  exitcode: 0
"""

import std/tables
import ../../src/[crewinventory, game, items, types]

if itemsList.len == 0:
  loadItems("../bin/data/items.dat")

playerShip.crew = @[]
var member = MemberData(homeBase: 1)
const attribute = MobAttributeRecord(level: 1, experience: 0)
member.attributes = @[attribute, attribute, attribute, attribute]
member.inventory.add(InventoryData(amount: 1, protoIndex: 1, durability: 100))
member.inventory.add(InventoryData(amount: 1, protoIndex: 2, durability: 100))
for index, _ in member.equipment.mpairs:
  member.equipment[index] = -1
member.equipment[weapon] = 1
playerShip.crew.add(member)

discard freeInventory(0, 0)

assert not itemIsUsed(0, 0), "Failed to check if an item is not used by the player."
assert itemIsUsed(0, 1), "Failed to check if an item is used by the player"

takeOffItem(0, 1)
assert not itemIsUsed(0, 1), "Failed to take off an item from the player."

updateInventory(0, 1, 1, ship = playerShip)
assert playerShip.crew[0].inventory[0].amount == 2, "Failed to add an item to the player's inventory."
updateInventory(0, -1, 1, ship = playerShip)
assert playerShip.crew[0].inventory[0].amount == 1, "Failed to remove an item from the player's inventory."
try:
  updateInventory(0, 10_000, 1, ship = playerShip)
except CrewNoSpaceError:
  discard
assert playerShip.crew[0].inventory[0].amount == 1, "Failed to add too much items to the player's inventory."

for i in 1..100:
  if playerShip.crew[0].inventory.len == 0:
    break
  damageItem(playerShip.crew[0].inventory, 0, 0, 0, playerShip)

block:
  var inventory: seq[InventoryData]
  inventory.add(InventoryData(protoIndex: 66, amount: 1, name: "",
      durability: defaultItemDurability, price: 0))
  inventory.add(InventoryData(protoIndex: 67, amount: 1, name: "",
      durability: defaultItemDurability, price: 0))
  assert findItem(inventory, 67) == 1, "Failed to find an item in an inventory."
  assert findItem(inventory = inventory, itemType = "Weapon") == 0
  assert findItem(inventory, 500) == -1, "Failed to not find an item in an inventory"
  assert findItem(inventory = inventory, itemType = "asdasdas") == -1

assert getTrainingToolQuality(0, 1) == 100, "Failed to get an tool's quality required by a skill."
