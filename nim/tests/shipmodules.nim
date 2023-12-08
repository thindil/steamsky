import ../src/[careers, factions, game, items, shipmodules]
import unittest2

suite "Unit tests for shipmodules module":

  checkpoint "Loading the game data."
  loadData("../bin/data/game.dat")
  loadItems("../bin/data/items.dat")
  loadCareers("../bin/data/careers.dat")
  loadFactions("../bin/data/factions.dat")
  loadModules("../bin/data/shipmodules.dat")

  test "Testing getModuleType.":
    checkpoint "Get the type of the module"
    check:
      getModuleType(1) == "Hull"
    checkpoint "Get the type of the module with two words"
    check:
      getModuleType(6) == "Alchemy lab"
