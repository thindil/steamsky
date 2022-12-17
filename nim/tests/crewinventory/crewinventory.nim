discard """
  exitcode: 0
"""

import ../../src/[crewinventory, game, items, ships, types]

loadItems("../bin/data/items.dat")

var member = MemberData(homeBase: 1)
const attribute = MobAttributeRecord(level: 1, experience: 0)
member.attributes = @[attribute, attribute, attribute, attribute]
member.inventory.add(InventoryData(amount: 1, protoIndex: 1))
member.inventory.add(InventoryData(amount: 1, protoIndex: 2))
for index, _ in member.equipment.mpairs:
  member.equipment[index] = -1
member.equipment[weapon] = 1
playerShip.crew.add(member)

discard freeInventory(0, 0)

assert not itemIsUsed(0, 0)
assert itemIsUsed(0, 1)

takeOffItem(0, 1)
assert not itemIsUsed(0, 1)

updateInventory(0, 1, 1, ship = playerShip)
assert playerShip.crew[0].inventory[0].amount == 2
updateInventory(0, -1, 1, ship = playerShip)
assert playerShip.crew[0].inventory[0].amount == 1
try:
  updateInventory(0, 10_000, 1, ship = playerShip)
except CrewNoSpaceError:
  discard
assert playerShip.crew[0].inventory[0].amount == 1

for i in 1..100:
  damageItem(playerShip.crew[0].inventory, 0, 0, 0, playerShip)

block:
  var inventory: seq[InventoryData]
  inventory.add(InventoryData(protoIndex: 66, amount: 1, name: "",
      durability: defaultItemDurability, price: 0))
  inventory.add(InventoryData(protoIndex: 67, amount: 1, name: "",
      durability: defaultItemDurability, price: 0))
  assert findItem(inventory, 67) == 1
  assert findItem(inventory = inventory, itemType = "Weapon") == 0
  assert findItem(inventory, 500) == -1
  assert findItem(inventory = inventory, itemType = "asdasdas") == -1
