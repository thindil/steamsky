import std/paths
import ../src/[careers, factions]
import unittest2
include ../src/crew

suite "Unit tests for crew module":

  checkpoint "Loading the game data."
  loadData("bin/data/game.dat".Path)
  loadItems("bin/data/items.dat".Path)
  loadCareers("bin/data/careers.dat".Path)
  loadFactions("bin/data/factions.dat".Path)

  playerShip.skyX = 1
  playerShip.skyY = 1
  playerShip.crew = @[]
  playerShip.crew.add(MemberData(morale: [1: 50.Natural, 2: 0.Natural],
      homeBase: 1, faction: "POLEIS", orders: [0.Natural, 0, 0, 1, 1, 1, 2, 1, 1,
      1, 0, 0], order: talk, loyalty: 100, skills: @[SkillInfo(index: 4, level: 4,
      experience: 0)], attributes: @[MobAttributeRecord(level: 3, experience: 0),
      MobAttributeRecord(level: 3, experience: 0), MobAttributeRecord(level: 3,
      experience: 0), MobAttributeRecord(level: 3, experience: 0)], name: "Laeran"))
  playerShip.crew.add(MemberData(morale: [1: 50.Natural, 2: 0.Natural],
      homeBase: 1, faction: "POLEIS", orders: [0.Natural, 0, 0, 1, 1, 1, 0, 1, 1,
      1, 0, 0], order: gunner, loyalty: 100, skills: @[SkillInfo(index: 4, level: 4,
      experience: 0)], attributes: @[MobAttributeRecord(level: 3, experience: 0),
      MobAttributeRecord(level: 3, experience: 0), MobAttributeRecord(level: 3,
      experience: 0), MobAttributeRecord(level: 3, experience: 0)]))
  playerShip.modules = @[]
  playerShip.modules.add(ModuleData(mType: ModuleType2.cabin, protoIndex: 4,
      durability: 100, owner: @[0]))
  skyMap[1][1].baseIndex = 1

  test "Testing daily payment.":
    dailyPayment()

  test "Testing attribute level name for value 3":
    gameSettings.showNumbers = false
    check:
      getAttributeLevelName(3) == "Very low"

  test "Testing attribute level name for value 12":
    gameSettings.showNumbers = false
    check:
      getAttributeLevelName(12) == "Below average"

  test "Testing attribute level name for value 48":
    gameSettings.showNumbers = false
    check:
      getAttributeLevelName(48) == "Very high"

  test "Testing attribute level name for value 3":
    gameSettings.showNumbers = true
    check:
      getAttributeLevelName(3) == "3"

  test "Testing attribute level name for value 12":
    gameSettings.showNumbers = true
    check:
      getAttributeLevelName(12) == "12"

  test "Testing attribute level name for value 48":
    gameSettings.showNumbers = true
    check:
      getAttributeLevelName(48) == "48"

  test "Testing skill level name for value 9":
    gameSettings.showNumbers = false
    check:
      getSkillLevelName(9) == "Beginner"

  test "Testing skill level name for value 54":
    gameSettings.showNumbers = false
    check:
      getSkillLevelName(54) == "Respected"

  test "Testing skill level name for value 92":
    gameSettings.showNumbers = false
    check:
      getSkillLevelName(92) == "Legendary"

  test "Testing skill level name for value 9":
    gameSettings.showNumbers = true
    check:
      getSkillLevelName(9) == "9"

  test "Testing skill level name for value 54":
    gameSettings.showNumbers = true
    check:
      getSkillLevelName(54) == "54"

  test "Testing skill level name for value 92":
    gameSettings.showNumbers = true
    check:
      getSkillLevelName(92) == "92"

  test "Testing finding a cabin for the player":
    check:
      findCabin(0) == 0

  test "Testing finding a cabin for a non-exisiting crew member":
    check:
      findCabin(100) == -1

  test "Testing update healthy crew member":
    playerShip.crew[0].health = 100
    updateCrew(1, 1)

  test "Testing updating a dead crew member":
    playerShip.crew[0].health = 0
    saveDirectory = "".Path
    updateCrew(1, 1)
