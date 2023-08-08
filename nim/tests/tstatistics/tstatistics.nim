discard """
  exitcode: 0
  output: '''Loading the game data.
Testing updateCraftingOrders.
Testing updateFinishedGoals.
Testing getGamePoints.
Testing updateFinishedMissions.
Testing clearGameStats.
Testing updateKilledMobs.
Testing updateDestroyedShips.'''
"""

import std/tables
import ../../src/[careers, crafts, factions, game, goals, items, mobs,
    shipmodules, ships, statistics, types]

echo "Loading the game data."
if goalsList.len == 0:
  loadGoals("../bin/data/goals.dat")
if itemsList.len == 0:
  loadData("../bin/data/game.dat")
  loadItems("../bin/data/items.dat")
  loadCareers("../bin/data/careers.dat")
  loadFactions("../bin/data/factions.dat")
  loadModules("../bin/data/shipmodules.dat")
if recipesList.len == 0:
  loadRecipes("../bin/data/recipes.dat")
if protoMobsList.len == 0:
  loadMobs("../bin/data/mobs.dat")
if protoShipsList.len == 0:
  loadShips("../bin/data/ships.dat")

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

echo "Testing updateDestroyedShips."
gameStats.destroyedShips = @[]
updateDestroyedShips("Tiny pirates ship")
assert gameStats.destroyedShips.len == 1, "Failed to update the amount of destroyed ships."
updateDestroyedShips("Sfdsfdsf")
assert gameStats.destroyedShips.len == 1, "Failed to not update the amount of destroyed ships with non-existing ship."
