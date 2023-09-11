discard """
  exitcode: 0
  output: '''Loading the game data.
Testing getCabinQuality.
Testing damageModule.
Testing countShipWeigth.
Testing createShip.'''
"""

import std/tables
import ../../src/[careers, crafts, factions, game, items, maps, mobs, ships,
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

echo "Testing getCabinQuality."
try:
  assert getCabinQuality(10) == "Empty room"
except AssertionDefect:
  writeLine(stderr, "Failed to get the cabin's quality.")

echo "Testing damageModule."
playerShip.modules = @[]
playerShip.modules.add(ModuleData(mType: cargoRoom, protoIndex: 7,
    durability: 100))
damageModule(playerShip, 0, 10, "during tests")
try:
  assert playerShip.modules[0].durability == 90
except AssertionDefect:
  writeLine(stderr, "Failed to damage the player's ship's module.")

echo "Testing countShipWeigth."
discard countShipWeight(playerShip)

echo "Testing createShip."
for base in skyBases.mitems:
  base.owner = "POLEIS"
for x in MapXRange.low .. MapXRange.high:
  for y in MapYRange.low .. MapYRange.high:
    skyMap[x][y].baseIndex = 1
let newShip = createShip(2, "", 5, 5, fullSpeed)
try:
  assert newShip.name == "Tiny pirates ship"
except AssertionDefect:
  writeLine(stderr, "Failed to create a new ship.")
