discard """
  exitcode: 0
  output: '''Loading the game data.
Testing damageModule.
Testing countCombatValue.
Testing generateShipName.'''
"""

import std/tables
import ../../src/[careers, crafts, factions, game, items, mobs, ships, ships2,
    shipmodules, types]

echo "Loading the game data."
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

echo "Testing damageModule."
playerShip.modules = @[]
playerShip.modules.add(ModuleData(mType: cargoRoom, protoIndex: 7,
    durability: 100))
damageModule(playerShip, 0, 10, "during tests")
assert playerShip.modules[0].durability == 90, "Failed to damage the player's ship's module."

echo "Testing countCombatValue."
discard countCombatValue()

echo "Testing generateShipName."
assert generateShipName("POLEIS").len() > 0, "Failed to generate random name for a ship."

