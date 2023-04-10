discard """
  exitcode: 0
"""

import std/tables
import ../../src/[careers, config, crew, factions, game, items, maps, types]

if itemsList.len == 0:
  loadData("../bin/data/game.dat")
  loadItems("../bin/data/items.dat")
  loadCareers("../bin/data/careers.dat")
  loadFactions("../bin/data/factions.dat")

assert generateMemberName('M', "POLEIS").len() > 0

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
playerShip.modules.add(ModuleData(mType: ModuleType2.cabin, protoIndex: 4,
    durability: 100, owner: @[0]))
skyMap[1][1].baseIndex = 1

dailyPayment()

gameSettings.showNumbers = 0
assert getAttributeLevelName(3) == "Very low"
assert getAttributeLevelName(12) == "Below average"
assert getAttributeLevelName(48) == "Very high"
gameSettings.showNumbers = 1
assert getAttributeLevelName(3) == "3"
assert getAttributeLevelName(12) == "12"
assert getAttributeLevelName(48) == "48"

gameSettings.showNumbers = 0
assert getSkillLevelName(9) == "Beginner"
assert getSkillLevelName(54) == "Respected"
assert getSkillLevelName(92) == "Legendary"
gameSettings.showNumbers = 1
assert getSkillLevelName(9) == "9"
assert getSkillLevelName(54) == "54"
assert getSkillLevelName(92) == "92"

assert findCabin(0) == 0
assert findCabin(100) == -1
