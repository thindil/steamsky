discard """
  exitcode: 0
  output: '''Loading the game data.
Testing repairShip.'''
"""

import std/tables
import ../../src/[game, careers, factions, items, shipmodules, shipsrepairs, types]

echo "Loading the game data."
if itemsList.len == 0:
  loadData("../bin/data/game.dat")
  loadItems("../bin/data/items.dat")
  loadCareers("../bin/data/careers.dat")
  loadFactions("../bin/data/factions.dat")
if modulesList.len == 0:
  loadModules("../bin/data/shipmodules.dat")

playerShip.modules = @[]
playerShip.modules.add(ModuleData(mType: ModuleType2.engine, protoIndex: 3,
    durability: 99, fuelUsage: 4, power: 2000, disabled: false,
    maxDurability: 100))
playerShip.cargo = @[]
playerShip.cargo.add(InventoryData(protoIndex: 61, amount: 5, durability: 100))
playerShip.cargo.add(InventoryData(protoIndex: 5, amount: 100, durability: 100))
playerShip.crew = @[]
playerShip.crew.add(MemberData(morale: [1: 50.Natural, 2: 0.Natural],
    homeBase: 1, faction: "POLEIS", orders: [0.Natural, 0, 0, 1, 1, 1, 2, 1, 1,
    1, 0, 0], order: repair, loyalty: 100, skills: @[SkillInfo(index: 4,
    level: 4,
    experience: 0)], attributes: @[MobAttributeRecord(level: 3, experience: 0),
    MobAttributeRecord(level: 3, experience: 0), MobAttributeRecord(level: 3,
    experience: 0), MobAttributeRecord(level: 3, experience: 0)]))
playerShip.crew[0].equipment[tool] = -1

echo "Testing repairShip."
repairShip(15)
assert playerShip.modules[0].durability == 100, "Failed to repair the ship's module."
