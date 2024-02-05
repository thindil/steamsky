import unittest2
import ../src/items
include ../src/basestypes

suite "Unit tests for basestypes module":
  checkpoint "Loading the game data."
  loadData("../bin/data/game.dat")
  loadItems("../bin/data/items.dat")
  loadBasesTypes("../bin/data/bases.dat")

  test "Testing getPrice.":
    checkpoint "Get the price of a non-buyable item."
    check:
      getPrice("0", 1) == 0
    checkpoint "Get the price of a buyable item."
    check:
      getPrice("1", 2) > 0

  test "Testing isBuyable.":
    checkpoint "Check if the item is non-buyable."
    check:
      not isBuyable("0", 1)
    checkpoint "Check if the item is buyable."
    check:
      isBuyable("1", 2)
