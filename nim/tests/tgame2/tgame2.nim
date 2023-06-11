discard """
  exitcode: 0
"""

import std/tables
import ../../src/[basestypes, careers, factions, game, game2, items, types]

if itemsList.len == 0:
  loadData("../bin/data/game.dat")
  loadItems("../bin/data/items.dat")
  loadCareers("../bin/data/careers.dat")
  loadFactions("../bin/data/factions.dat")
if basesTypesList.len == 0:
  loadBasesTypes("../bin/data/bases.dat")

gameDate = DateRecord(year: 1600, month: 1, day: 1, hour: 8, minutes: 0)
playerShip.modules = @[]
playerShip.modules.add(ModuleData(mType: ModuleType2.engine, protoIndex: 3,
    durability: 100, fuelUsage: 4, power: 2000, disabled: false))
playerShip.cargo = @[]
playerShip.cargo.add(InventoryData(protoIndex: 1, amount: 100, durability: 100))
skyBases[1].baseType = "1"

updateGame(1)
gameDate.minutes = 1
