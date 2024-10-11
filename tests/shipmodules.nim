import ../src/[careers, factions]
import unittest2
include ../src/shipmodules

suite "Unit tests for shipmodules module":

  checkpoint "Loading the game data."
  loadData("bin/data/game.dat")
  loadItems("bin/data/items.dat")
  loadCareers("bin/data/careers.dat")
  loadFactions("bin/data/factions.dat")
  loadModules("bin/data/shipmodules.dat")

  test "Get the type of the module":
    check:
      getModuleType(1) == "Hull"

  test "Get the type of the module with two words":
    check:
      getModuleType(6) == "Alchemy lab"
