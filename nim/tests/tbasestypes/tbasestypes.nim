discard """
  exitcode: 0
"""

import std/tables
import ../../src/[basestypes, game, items]

if itemsList.len == 0:
  loadData("../bin/data/game.dat")
  loadItems("../bin/data/items.dat")
if basesTypesList.len == 0:
  loadBasesTypes("../bin/data/bases.dat")

assert getPrice("0", 1) == 0, "Failed to get the price of a non-buyable item."
assert getPrice("1", 2) > 0, "Failed to get the price of a buyable item."

assert not isBuyable("0", 1), "Failed to check if an item is non-buyable."
assert isBuyable("1", 2), "Failed to check if an item is buyable."
