import std/paths
import ../src/[careers, factions, shipmodules]
import unittest2
include ../src/basesship

suite "Unit tests for basesship module":
  checkpoint "Loading the game data."
  loadData("bin/data/game.dat".Path)
  loadItems("bin/data/items.dat".Path)
  loadCareers("bin/data/careers.dat".Path)
  loadFactions("bin/data/factions.dat".Path)
  loadBasesTypes("bin/data/bases.dat".Path)
  loadModules("bin/data/shipmodules.dat".Path)

  skyBases[1].reputation = ReputationData(level: 1, experience: 1)
  playerShip.skyX = 1
  playerShip.skyY = 1
  playerShip.crew = @[]
  playerShip.crew.add(MemberData(morale: [1: 50.Natural, 2: 0.Natural],
      homeBase: 1, faction: "POLEIS", orders: [0.Natural, 0, 0, 1, 1, 1, 2, 1, 1,
      1, 0, 0], order: talk, loyalty: 100, skills: @[SkillInfo(index: 4,
      level: 4,
      experience: 0)], attributes: @[MobAttributeRecord(level: 3,
          experience: 0),
      MobAttributeRecord(level: 3, experience: 0), MobAttributeRecord(level: 3,
      experience: 0), MobAttributeRecord(level: 3, experience: 0)], health: 100))
  playerShip.modules = @[]
  playerShip.modules.add(ModuleData(mType: ModuleType2.hull, protoIndex: 1,
      durability: 100, maxDurability: 100, maxModules: 10))
  playerShip.cargo = @[]
  playerShip.cargo.add(InventoryData(protoIndex: 1, amount: 100,
      durability: 100))
  playerShip.speed = docked
  skyMap[1][1].baseIndex = 1
  skyBases[1].population = 100
  skyBases[1].baseType = "1"
  skyBases[1].owner = "POLEIS"
  generateCargo()
  gameDate = DateRecord(year: 1600, month: 1, day: 1, hour: 8, minutes: 0)

  test "Paying for docking.":
    payForDock()
    check:
      playerShip.cargo[0].amount == 90

  test "Counting repair cost for the selected module.":
    var cost, time: Natural = 0
    playerShip.modules[0].durability -= 5
    repairCost(cost, time, 0)
    check:
      cost > 0 and time > 0

  test "Counting repair cost for the whole ship":
    var cost, time: Natural = 0
    repairCost(cost, time, -1)
    check:
      cost > 0 and time > 0
