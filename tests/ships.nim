import ../src/[careers, crafts, factions, items, shipmodules]
import unittest2
include ../src/ships

suite "Unit tests for ships module":

  checkpoint "Loading the game data."
  loadData("bin/data/game.dat")
  loadItems("bin/data/items.dat".Path)
  loadCareers("bin/data/careers.dat")
  loadFactions("bin/data/factions.dat".Path)
  loadModules("bin/data/shipmodules.dat".Path)
  loadRecipes("bin/data/recipes.dat".Path)
  loadMobs("bin/data/mobs.dat".Path)
  loadShips("bin/data/ships.dat".Path)

  test "Getting a cabin's quality.":
    check:
      getCabinQuality(10) == "Empty room"

  test "Damaging the player's ship module.":
    playerShip.modules = @[]
    playerShip.modules.add(ModuleData(mType: cargoRoom, protoIndex: 7,
        durability: 100))
    damageModule(playerShip, 0, 10, "during tests")
    check:
      playerShip.modules[0].durability == 90

  test "Couting the player's ship weight.":
    discard countShipWeight(playerShip)

  test "Creating a ship.":
    for base in skyBases.mitems:
      base.owner = "POLEIS"
    for x in MapXRange.low .. MapXRange.high:
      for y in MapYRange.low .. MapYRange.high:
        skyMap[x][y].baseIndex = 1
    let newShip = createShip(2, "", 5, 5, fullSpeed)
    check:
      newShip.name == "Tiny pirates ship"
