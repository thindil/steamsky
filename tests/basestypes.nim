import unittest2
import ../src/items
include ../src/basestypes

suite "Unit tests for basestypes module":
  checkpoint "Loading the game data."
  loadData("bin/data/game.dat".Path)
  loadItems("bin/data/items.dat".Path)
  loadBasesTypes("bin/data/bases.dat".Path)

  test "Get the price of a non-buyable item.":
    check:
      getPrice("0", 1, quality = normal) == 0

  test "Get the price of a buyable item.":
    check:
      getPrice("1", 2, quality = normal) > 0

  test "Check if the item is non-buyable.":
    check:
      not isBuyable("0", 1)

  test "Check if the item is buyable.":
    check:
      isBuyable("1", 2)
