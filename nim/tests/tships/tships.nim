discard """
  exitcode: 0
"""

import std/tables
import ../../src/[careers, crafts, factions, game, items, maps, mobs, ships, shipmodules, types]

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

assert getCabinQuality(10) == "Empty room"

playerShip.modules = @[]
playerShip.modules.add(ModuleData(mType: cargoRoom, protoIndex: 7,
    durability: 100))
damageModule(playerShip, 0, 10, "during tests")
assert playerShip.modules[0].durability == 90

discard countShipWeight(playerShip)

for base in skyBases.mitems:
  base.owner = "POLEIS"
for x in MapXRange.low .. MapXRange.high:
  for y in MapYRange.low .. MapYRange.high:
    skyMap[x][y].baseIndex = 1
let newShip = createShip(2, "", 5, 5, fullSpeed)
assert newShip.name == "Tiny pirates ship"
