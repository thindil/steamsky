import ../src/[careers, factions]
import unittest2
include ../src/shipmodules

suite "Unit tests for shipmodules module":

  checkpoint "Loading the game data."
  loadData("bin/data/game.dat".Path)
  loadItems("bin/data/items.dat".Path)
  loadCareers("bin/data/careers.dat".Path)
  loadFactions("bin/data/factions.dat".Path)
  loadModules("bin/data/shipmodules.dat".Path)

  test "Get the type of the module":
    check:
      getModuleType(1) == "Hull"

  test "Get the type of the module with two words":
    check:
      getModuleType(6) == "Alchemy lab"
