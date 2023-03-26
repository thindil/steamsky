discard """
  exitcode: 0
"""

import std/tables
import ../../src/[careers, factions, game, items, ships, shipmodules, types]

if itemsList.len == 0:
  loadData("../bin/data/game.dat")
  loadItems("../bin/data/items.dat")
  loadCareers("../bin/data/careers.dat")
  loadFactions("../bin/data/factions.dat")
  loadModules("../bin/data/shipmodules.dat")

assert getCabinQuality(10) == "Empty room"

assert generateShipName("POLEIS").len() > 0

playerShip.modules = @[]
playerShip.modules.add(ModuleData(mType: cargoRoom, protoIndex: 7,
    durability: 100))
damageModule(playerShip, 0, 10, "during tests")
assert playerShip.modules[0].durability == 90

discard countCombatValue()
