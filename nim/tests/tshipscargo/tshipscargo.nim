discard """
  exitcode: 0
"""

import std/tables
import ../../src/[careers, factions, game, items, shipscargo, types]

if itemsList.len == 0:
  loadData("../bin/data/game.dat")
  loadItems("../bin/data/items.dat")
  loadCareers("../bin/data/careers.dat")
  loadFactions("../bin/data/factions.dat")

playerShip.modules = @[]
playerShip.modules.add(ModuleData(mType: cargoRoom, protoIndex: 7))
playerShip.cargo = @[]
playerShip.cargo.add(InventoryData(protoIndex: 1, amount: 100, durability: 100))

updateCargo(playerShip, 1, -1)
assert playerShip.cargo[0].amount == 99
updateCargo(playerShip, 1, 1)
assert playerShip.cargo[0].amount == 100
updateCargo(playerShip, 40, -1)

assert freeCargo(1) > freeCargo(0)

assert getItemAmount("Fuel") == 100
