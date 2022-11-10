discard """
  exitcode: 0
"""

import ../../src/items

loadItems("../bin/data/items.dat")

assert findProtoItem("Iron") > 0
assert findProtoItem("sfdsfsdfsdf") == 0

assert getItemDamage(60) == "Damaged"
assert getItemDamage(60, true) == "damaged"
