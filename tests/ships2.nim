import std/paths
import ../src/[careers, crafts, factions, game, items, mobs, ships, ships2,
    shipmodules, types]
import unittest2

suite "Unit tests for ships2 module":

  checkpoint "Loading the game data."
  loadData("bin/data/game.dat")
  loadItems("bin/data/items.dat".Path)
  loadCareers("bin/data/careers.dat")
  loadFactions("bin/data/factions.dat".Path)
  loadModules("bin/data/shipmodules.dat")
  loadRecipes("bin/data/recipes.dat".Path)
  loadMobs("bin/data/mobs.dat".Path)
  loadShips("bin/data/ships.dat")

  test "Damaging a module.":
    playerShip.modules = @[]
    playerShip.modules.add(ModuleData(mType: cargoRoom, protoIndex: 7,
        durability: 100))
    damageModule(playerShip, 0, 10, "during tests")
    check:
      playerShip.modules[0].durability == 90

  test "Counting a combat value.":
    discard countCombatValue()

  test "Generating a ship's name.":
    check:
      generateShipName("POLEIS").len() > 0

