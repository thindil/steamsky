discard """
  exitcode: 0
"""

import std/tables
import ../../src/[careers, config, factions, game, items, types]

if itemsList.len == 0:
  loadData("../bin/data/game.dat")
  loadItems("../bin/data/items.dat")
  loadCareers("../bin/data/careers.dat")
  loadFactions("../bin/data/factions.dat")

assert findProtoItem("Iron") > 0
assert findProtoItem("sfdsfsdfsdf") == 0

assert getItemDamage(60) == "Damaged"
assert getItemDamage(60, true) == "damaged"

block:
  var item = InventoryData(protoIndex: 2, amount: 1, name: "", durability: 80, price: 0)
  assert getItemName(item) == "Basic Ration (slightly used)"
  assert getItemName(item, false) == "Basic Ration"
  assert getItemName(item, true, false) == "Basic Ration (Slightly used)"
  item.name = "New name"
  assert getItemName(item, false) == "New name"

gameSettings.showNumbers = 0
assert getItemChanceToDamage(3) == "Small"
assert getItemChanceToDamage(30) == "Very high"
gameSettings.showNumbers = 1
assert getItemChanceToDamage(3) == " 3%"
assert getItemChanceToDamage(30) == " 30%"

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

assert findTools(0, "Bucket", clean) > -1
assert findTools(0, "sfewrwer", talk) == -1

let itemIndex = getRandomItem(weaponsList, weapon, 20, 20, "POLEIS")
assert itemIndex > 0 and itemsList.hasKey(itemIndex)
