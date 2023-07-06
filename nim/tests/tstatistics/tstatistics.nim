discard """
  exitcode: 0
"""

import std/tables
import ../../src/[game, goals, statistics]

if goalsList.len == 0:
  loadGoals("../bin/data/goals.dat")

gameStats.craftingOrders = @[]
updateCraftingOrders("1")
assert gameStats.craftingOrders.len == 1, "Failed to update the amount of finished crafting orders."

gameStats.finishedGoals = @[]
updateFinishedGoals("1")
assert gameStats.finishedGoals.len == 1, "Failed to update the amount of finished goals."
updateFinishedGoals("Sdfdsf")
assert gameStats.finishedGoals.len == 1, "Failed to not update the amount of finished goals with non-existing goal."

gameStats.points = 0
assert getGamePoints() == 0, "Failed to get the player's game points."

gameStats.finishedMissions = @[]
updateFinishedMissions("DESTROY")
assert gameStats.finishedMissions.len == 1, "Failed to update the amount of finished missions."

gameStats.points = 100
clearGameStats()
assert gameStats.points == 0, "Failed to clear the game statistics."
