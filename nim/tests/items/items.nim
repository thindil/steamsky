discard """
  exitcode: 0
"""

import std/tables
import ../../src/[config, items]

loadItems("../bin/data/items.dat")

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

block:
  var inventory = initTable[Positive, InventoryData]()
  inventory[1] = InventoryData(protoIndex: 66, amount: 1, name: "",
      durability: defaultItemDurability, price: 0)
  inventory[2] = InventoryData(protoIndex: 67, amount: 1, name: "",
      durability: defaultItemDurability, price: 0)
  assert findItem(inventory, 67) == 2
  assert findItem(inventory = inventory, itemType = "Weapon") == 1
  assert findItem(inventory, 500) == 0
  assert findItem(inventory = inventory, itemType = "asdasdas") == 0
