discard """
  exitcode: 0
  output: '''Loading the game data.
Testing getAttributeLevelName.
Testing getSkillLevelName.
Testing findCabin.
Testing updateCrew.'''
"""

import std/tables
import ../../src/[careers, config, crew, factions, game, items, maps, types]

echo "Loading the game data."
if itemsList.len == 0:
  loadData("../bin/data/game.dat")
  loadItems("../bin/data/items.dat")
  loadCareers("../bin/data/careers.dat")
  loadFactions("../bin/data/factions.dat")

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

echo "Testing getAttributeLevelName."
gameSettings.showNumbers = false
assert getAttributeLevelName(3) == "Very low", "Failed to get attribute level name for value 3."
assert getAttributeLevelName(12) == "Below average", "Failed to get attribute level name for value 12."
assert getAttributeLevelName(48) == "Very high", "Failed to get attribute level name for value 48."
gameSettings.showNumbers = true
assert getAttributeLevelName(3) == "3", "Failed to get attribute level number for value 3."
assert getAttributeLevelName(12) == "12", "Failed to get attribute level number for value 12."
assert getAttributeLevelName(48) == "48", "Failed to get attribute level number for value 48."

echo "Testing getSkillLevelName."
gameSettings.showNumbers = false
assert getSkillLevelName(9) == "Beginner", "Failed to get skill level name for value 9."
assert getSkillLevelName(54) == "Respected", "Failed to get skill level name for value 54."
assert getSkillLevelName(92) == "Legendary", "Failed to get skill level name for value 92."
gameSettings.showNumbers = true
assert getSkillLevelName(9) == "9", "Failed to get skill level number for value 9."
assert getSkillLevelName(54) == "54", "Failed to get skill level number for value 54."
assert getSkillLevelName(92) == "92", "Failed to get skill level number for value 92."

echo "Testing findCabin."
assert findCabin(0) == 0, "Failed to find a cabin of the player."
assert findCabin(100) == -1, "Failed to not find a cabin for a non-existing crew member."

echo "Testing updateCrew."
playerShip.crew[0].health = 100
updateCrew(1, 1)
playerShip.crew[0].health = 0
saveDirectory = ""
updateCrew(1, 1)
