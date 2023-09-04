discard """
  exitcode: 0
  output: '''Loading the game data.
Testing findProtoItem.
Testing getItemDamage.
Testing getItemName.
Testing getItemChanceToDamage.
Testing findTools.
Testing getRandomItem.'''
"""

import std/tables
import ../../src/[careers, config, factions, game, items, types]

echo "Loading the game data."
if itemsList.len == 0:
  loadData("../bin/data/game.dat")
  loadItems("../bin/data/items.dat")
  loadCareers("../bin/data/careers.dat")
  loadFactions("../bin/data/factions.dat")

echo "Testing findProtoItem."
try:
  assert findProtoItem("Iron") > 0
except AssertionDefect:
  writeLine(stderr, "Failed to find an existing item.")
try:
  assert findProtoItem("sfdsfsdfsdf") == 0
except AssertionDefect:
  writeLine(stderr, "Failed to not find a non-existsing item.")

echo "Testing getItemDamage."
try:
  assert getItemDamage(60) == "Damaged"
except AssertionDefect:
  writeLine(stderr, "Failed to get an item damage level.")
try:
  assert getItemDamage(60, true) == "damaged"
except AssertionDefect:
  writeLine(stderr, "Failed to get lowered an item damage level.")

echo "Testing getItemName."
block:
  var item = InventoryData(protoIndex: 2, amount: 1, name: "", durability: 80, price: 0)
  try:
    assert getItemName(item) == "Basic Ration (slightly used)"
  except AssertionDefect:
    writeLine(stderr, "Failed to get an item name with lowered damage info.")
  try:
    assert getItemName(item, false) == "Basic Ration"
  except AssertionDefect:
    writeLine(stderr, "Failed to get an item name.")
  try:
    assert getItemName(item, true, false) == "Basic Ration (Slightly used)"
  except AssertionDefect:
    writeLine(stderr, "Failed to get an item name with damage info.")
  item.name = "New name"
  try:
    assert getItemName(item, false) == "New name"
  except AssertionDefect:
    writeLine(stderr, "Failed to get an item with new name.")

echo "Testing getItemChanceToDamage."
gameSettings.showNumbers = false
try:
  assert getItemChanceToDamage(3) == "Small"
except AssertionDefect:
  writeLine(stderr, "Failed to get chance to damage as string for 3.")
try:
  assert getItemChanceToDamage(30) == "Very high"
except AssertionDefect:
  writeLine(stderr, "Failed to get chance to damage as string for 10.")
gameSettings.showNumbers = true
try:
  assert getItemChanceToDamage(3) == " 3%"
except AssertionDefect:
  writeLine(stderr, "Failed to get chance to damage as percent for 3.")
try:
  assert getItemChanceToDamage(30) == " 30%"
except AssertionDefect:
  writeLine(stderr, "Failed to get chance to damage as percent for 30.")

playerShip.modules = @[]
playerShip.modules.add(ModuleData(mType: cargoRoom, protoIndex: 7))
playerShip.cargo = @[]
playerShip.cargo.add(InventoryData(protoIndex: 53, amount: 1))
playerShip.crew = @[]
var member = MemberData(homeBase: 1)
const attribute = MobAttributeRecord(level: 1, experience: 0)
member.attributes = @[attribute, attribute, attribute, attribute]
member.inventory.add(InventoryData(amount: 1, protoIndex: 1))
member.inventory.add(InventoryData(amount: 1, protoIndex: 2))
for index, _ in member.equipment.mpairs:
  member.equipment[index] = -1
member.equipment[weapon] = 1
playerShip.crew.add(member)

echo "Testing findTools."
try:
  assert findTools(0, "Bucket", clean) > -1
except AssertionDefect:
  writeLine(stderr, "Failed to find an existing tool.")
try:
  assert findTools(0, "sfewrwer", talk) == -1
except AssertionDefect:
  writeLine(stderr, "Failed to not find a non-existing tool.")

echo "Testing getRandomItem."
let itemIndex = getRandomItem(weaponsList, weapon, 20, 20, "POLEIS")
try:
  assert itemIndex > 0 and itemsList.hasKey(itemIndex)
except AssertionDefect:
  writeLine(stderr, "Failed to get a random item.")
