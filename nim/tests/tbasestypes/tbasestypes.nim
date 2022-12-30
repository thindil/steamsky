discard """
  exitcode: 0
"""

import ../../src/[basestypes, game, items]

loadData("../bin/data/game.dat")
loadItems("../bin/data/items.dat")
loadBasesTypes("../bin/data/bases.dat")

assert getPrice("0", 1) == 0
assert getPrice("1", 2) > 0

assert not isBuyable("0", 1)
assert isBuyable("1", 2)
