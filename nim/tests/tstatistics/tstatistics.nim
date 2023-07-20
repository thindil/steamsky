discard """
  exitcode: 0
  output: '''Loading the game data.
Testing updateCraftingOrders.
Testing updateFinishedGoals.
Testing getGamePoints.
Testing updateFinishedMissions.
Testing clearGameStats.'''
"""

import std/tables
import ../../src/[game, goals, statistics]

echo "Loading the game data."
if goalsList.len == 0:
  loadGoals("../bin/data/goals.dat")

echo "Testing updateCraftingOrders."
gameStats.craftingOrders = @[]
updateCraftingOrders("1")
assert gameStats.craftingOrders.len == 1, "Failed to update the amount of finished crafting orders."

echo "Testing updateFinishedGoals."
gameStats.finishedGoals = @[]
updateFinishedGoals("1")
assert gameStats.finishedGoals.len == 1, "Failed to update the amount of finished goals."
updateFinishedGoals("Sdfdsf")
assert gameStats.finishedGoals.len == 1, "Failed to not update the amount of finished goals with non-existing goal."

echo "Testing getGamePoints."
gameStats.points = 0
assert getGamePoints() == 0, "Failed to get the player's game points."

echo "Testing updateFinishedMissions."
gameStats.finishedMissions = @[]
updateFinishedMissions("DESTROY")
assert gameStats.finishedMissions.len == 1, "Failed to update the amount of finished missions."

echo "Testing clearGameStats."
gameStats.points = 100
clearGameStats()
assert gameStats.points == 0, "Failed to clear the game statistics."
