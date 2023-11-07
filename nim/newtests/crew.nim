import ../src/[careers, config, crew, factions, game, items, maps, types]
import unittest2

suite "Unit tests for crew module":

  checkpoint "Loading the game data."
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

  test "Testing dailyPayment":
    dailyPayment()

  test "Testing getAttributeLevelName.":
    checkpoint "Testing attribute level name as string"
    gameSettings.showNumbers = false
    checkpoint "Testing attribute level name for value 3"
    check:
      getAttributeLevelName(3) == "Very low"
    checkpoint "Testing attribute level name for value 12"
    check:
      getAttributeLevelName(12) == "Below average"
    checkpoint "Testing attribute level name for value 48"
    check:
      getAttributeLevelName(48) == "Very high"
    checkpoint "Testing attribute level name as number"
    gameSettings.showNumbers = true
    checkpoint "Testing attribute level name for value 3"
    check:
      getAttributeLevelName(3) == "3"
    checkpoint "Testing attribute level name for value 12"
    check:
      getAttributeLevelName(12) == "12"
    checkpoint "Testing attribute level name for value 48"
    check:
      getAttributeLevelName(48) == "48"

  test "Testing getSkillLevelName.":
    checkpoint "Testing skill level name as string"
    gameSettings.showNumbers = false
    checkpoint "Testing skill level name for value 9"
    check:
      getSkillLevelName(9) == "Beginner"
    checkpoint "Testing skill level name for value 54"
    check:
      getSkillLevelName(54) == "Respected"
    checkpoint "Testing skill level name for value 92"
    check:
      getSkillLevelName(92) == "Legendary"
    checkpoint "Testing skill level name as number"
    gameSettings.showNumbers = true
    checkpoint "Testing skill level name for value 9"
    check:
      getSkillLevelName(9) == "9"
    checkpoint "Testing skill level name for value 54"
    check:
      getSkillLevelName(54) == "54"
    checkpoint "Testing skill level name for value 92"
    check:
      getSkillLevelName(92) == "92"

  test "Testing findCabin.":
    checkpoint "Testing finding a cabin for the player"
    check:
      findCabin(0) == 0
    checkpoint "Testing finding a cabin for a non-exisiting crew member"
    check:
      findCabin(100) == -1

  test "Testing updateCrew.":
    checkpoint "Testing update healthy crew member"
    playerShip.crew[0].health = 100
    updateCrew(1, 1)
    checkpoint "Testing updating a dead crew member"
    playerShip.crew[0].health = 0
    saveDirectory = ""
    updateCrew(1, 1)
