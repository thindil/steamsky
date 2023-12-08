import ../src/[careers, crafts, factions, game, items, maps, mobs, ships, shipmodules, types]
import unittest2

suite "Unit tests for ships module":

  checkpoint "Loading the game data."
  loadData("../bin/data/game.dat")
  loadItems("../bin/data/items.dat")
  loadCareers("../bin/data/careers.dat")
  loadFactions("../bin/data/factions.dat")
  loadModules("../bin/data/shipmodules.dat")
  loadRecipes("../bin/data/recipes.dat")
  loadMobs("../bin/data/mobs.dat")
  loadShips("../bin/data/ships.dat")

  test "Testing getCabinQuality.":
    check:
      getCabinQuality(10) == "Empty room"

  test "Testing damageModule.":
    playerShip.modules = @[]
    playerShip.modules.add(ModuleData(mType: cargoRoom, protoIndex: 7,
        durability: 100))
    damageModule(playerShip, 0, 10, "during tests")
    check:
      playerShip.modules[0].durability == 90

  test "Testing countShipWeigth.":
    discard countShipWeight(playerShip)

  test "Testing createShip.":
    for base in skyBases.mitems:
      base.owner = "POLEIS"
    for x in MapXRange.low .. MapXRange.high:
      for y in MapYRange.low .. MapYRange.high:
        skyMap[x][y].baseIndex = 1
    let newShip = createShip(2, "", 5, 5, fullSpeed)
    check:
      newShip.name == "Tiny pirates ship"
