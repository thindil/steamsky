discard """
  exitcode: 0
"""

import ../../src/items

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
