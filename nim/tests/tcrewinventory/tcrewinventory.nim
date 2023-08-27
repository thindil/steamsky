discard """
  exitcode: 0
  output: '''Loading the game data.
Testing freeInventory.
Testing itemIsUsed.
Testing takeOffItem.
Testing updateInventory.
Testing damageItem.
Testing findItem.
Testing getTrainingToolQuality.'''
"""

import std/tables
import ../../src/[crewinventory, game, items, types]

echo "Loading the game data."
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

echo "Testing freeInventory."
discard freeInventory(0, 0)

echo "Testing itemIsUsed."
try:
  assert not itemIsUsed(0, 0)
except AssertionDefect:
  writeLine(stderr, "Failed to check if an item is not used by the player.")
try:
  assert itemIsUsed(0, 1)
except AssertionDefect:
  writeLine(stderr, "Failed to check if an item is used by the player")

echo "Testing takeOffItem."
takeOffItem(0, 1)
try:
  assert not itemIsUsed(0, 1)
except AssertionDefect:
  writeLine(stderr, "Failed to take off an item from the player.")

echo "Testing updateInventory."
updateInventory(0, 1, 1, ship = playerShip)
try:
  assert playerShip.crew[0].inventory[0].amount == 2
except AssertionDefect:
  writeLine(stderr, "Failed to add an item to the player's inventory.")
updateInventory(0, -1, 1, ship = playerShip)
try:
  assert playerShip.crew[0].inventory[0].amount == 1
except AssertionDefect:
  writeLine(stderr, "Failed to remove an item from the player's inventory.")
try:
  updateInventory(0, 10_000, 1, ship = playerShip)
except CrewNoSpaceError:
  discard
try:
  assert playerShip.crew[0].inventory[0].amount == 1
except AssertionDefect:
  writeLine(stderr, "Failed to add too much items to the player's inventory.")

echo "Testing damageItem."
for i in 1..100:
  if playerShip.crew[0].inventory.len == 0:
    break
  damageItem(playerShip.crew[0].inventory, 0, 0, 0, playerShip)

echo "Testing findItem."
block:
  var inventory: seq[InventoryData]
  inventory.add(InventoryData(protoIndex: 66, amount: 1, name: "",
      durability: defaultItemDurability, price: 0))
  inventory.add(InventoryData(protoIndex: 67, amount: 1, name: "",
      durability: defaultItemDurability, price: 0))
  try:
    assert findItem(inventory, 67) == 1
  except AssertionDefect:
    writeLine(stderr, "Failed to find an item in an inventory by proto index.")
  try:
    assert findItem(inventory = inventory, itemType = "Weapon") == 0
  except AssertionDefect:
    writeLine(stderr, "Failed to find an item in an inventory by item type.")
  try:
    assert findItem(inventory, 500) == -1
  except AssertionDefect:
    writeLine(stderr, "Failed to not find an item in an inventory by proto index.")
  try:
    assert findItem(inventory = inventory, itemType = "asdasdas") == -1
  except AssertionDefect:
    writeLine(stderr, "Failed to not find an item in an inventory by item type.")

echo "Testing getTrainingToolQuality."
try:
  assert getTrainingToolQuality(0, 1) == 100
except AssertionDefect:
  writeLine(stderr, "Failed to get an tool's quality required by a skill.")
