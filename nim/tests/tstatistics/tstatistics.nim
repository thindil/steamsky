discard """
  exitcode: 0
"""

import std/tables
import ../../src/[game, goals, statistics]

if goalsList.len == 0:
  loadGoals("../bin/data/goals.dat")

gameStats.craftingOrders = @[]
updateCraftingOrders("1")
assert gameStats.craftingOrders.len == 1

gameStats.finishedGoals = @[]
updateFinishedGoals("1")
assert gameStats.finishedGoals.len == 1
updateFinishedGoals("Sdfdsf")
assert gameStats.finishedGoals.len == 1
