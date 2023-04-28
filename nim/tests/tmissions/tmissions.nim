discard """
  exitcode: 0
"""

import std/tables
import ../../src/[careers, factions, game, items, maps, missions, shipmodules, types]

if itemsList.len == 0:
  loadData("../bin/data/game.dat")
  loadItems("../bin/data/items.dat")
  loadCareers("../bin/data/careers.dat")
  loadFactions("../bin/data/factions.dat")
  loadModules("../bin/data/shipmodules.dat")

playerShip.skyX = 1
playerShip.skyY = 1
playerShip.crew = @[]
playerShip.crew.add(MemberData(morale: [1: 50.Natural, 2: 0.Natural],
    homeBase: 1, faction: "POLEIS", orders: [0.Natural, 0, 0, 1, 1, 1, 2, 1, 1,
    1, 0, 0], order: talk, loyalty: 100, skills: @[SkillInfo(index: 4, level: 4,
    experience: 0)], attributes: @[MobAttributeRecord(level: 3, experience: 0),
    MobAttributeRecord(level: 3, experience: 0), MobAttributeRecord(level: 3,
    experience: 0), MobAttributeRecord(level: 3, experience: 0)]))
playerShip.crew.add(MemberData(morale: [1: 50.Natural, 2: 0.Natural],
    homeBase: 1, faction: "POLEIS", orders: [0.Natural, 0, 0, 1, 1, 1, 0, 1, 1,
    1, 0, 0], order: gunner, loyalty: 100))
playerShip.modules = @[]
playerShip.modules.add(ModuleData(mType: cargoRoom, protoIndex: 7, durability: 100))
playerShip.cargo = @[]
playerShip.cargo.add(InventoryData(protoIndex: 1, amount: 100, durability: 100))
skyMap[1][1].baseIndex = 1
skyBases[1] = BaseRecord(skyX: 1, skyY: 1)

acceptedMissions = @[]
acceptedMissions.add(y = MissionData(mType: explore, time: 1, targetX: 1,
    targetY: 1, reward: 1, startBase: 1, finished: true, multiplier: 0.0, target: 0))
deleteMission(0, false)
assert acceptedMissions.len == 0

skyBases[1].missionsDate = DateRecord(year: 0, month: 0, day: 0, hour: 0, minutes: 0)
generateMissions()
