import std/paths
import ../src/[careers, factions, shipmodules]
import unittest2
include ../src/shipsrepairs

suite "Unit tests for shipsrepairs module":

  checkpoint "Loading the game data."
  loadData("bin/data/game.dat".Path)
  loadItems("bin/data/items.dat".Path)
  loadCareers("bin/data/careers.dat".Path)
  loadFactions("bin/data/factions.dat".Path)
  loadModules("bin/data/shipmodules.dat".Path)

  playerShip.modules = @[]
  playerShip.modules.add(ModuleData(mType: ModuleType2.engine, protoIndex: 3,
      durability: 99, fuelUsage: 4, power: 2000, disabled: false,
      maxDurability: 100))
  playerShip.cargo = @[]
  playerShip.cargo.add(InventoryData(protoIndex: 61, amount: 5,
      durability: 100))
  playerShip.cargo.add(InventoryData(protoIndex: 5, amount: 100,
      durability: 100))
  playerShip.crew = @[]
  playerShip.crew.add(MemberData(morale: [1: 50.Natural, 2: 0.Natural],
      homeBase: 1, faction: "POLEIS", orders: [0.Natural, 0, 0, 1, 1, 1, 2, 1,
      1, 1, 0, 0], order: repair, loyalty: 100, skills: @[SkillInfo(index: 4,
      level: 4, experience: 0)], attributes: @[MobAttributeRecord(level: 3,
      experience: 0), MobAttributeRecord(level: 3, experience: 0),
      MobAttributeRecord(level: 3, experience: 0), MobAttributeRecord(level: 3,
      experience: 0)], equipment: [-1, -1, -1, -1, -1, -1, -1]))
  playerShip.crew[0].equipment[tool] = -1

  test "Repairing the player's ship.":
    repairShip(15)
    check:
      playerShip.modules[0].durability == 100
