discard """
  exitcode: 0
  output: '''Loading the game data.
Testing updateCraftingOrders.
Testing updateFinishedGoals.
Testing getGamePoints.
Testing updateFinishedMissions.
Testing clearGameStats.
Testing updateKilledMobs.'''
"""

import std/tables
import ../../src/[game, goals, statistics, types]

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

echo "Testing updateKilledMobs."
gameStats.killedMobs = @[]
updateKilledMobs(MemberData(morale: [1: 50.Natural, 2: 0.Natural],
    homeBase: 1, faction: "POLEIS", orders: [0.Natural, 0, 0, 1, 1, 1, 2, 1, 1,
    1, 0, 0], order: upgrading, loyalty: 100, skills: @[SkillInfo(index: 4,
    level: 4,
    experience: 0)], attributes: @[MobAttributeRecord(level: 3, experience: 0),
    MobAttributeRecord(level: 3, experience: 0), MobAttributeRecord(level: 3,
    experience: 0), MobAttributeRecord(level: 3, experience: 0)]), "POLEIS")
assert gameStats.killedMobs.len == 1, "Failed to update the amount of killed mobs."
