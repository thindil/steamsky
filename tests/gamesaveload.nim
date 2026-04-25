import unittest2
include ../src/gamesaveload

suite "Unit tests for gamesaveload module":

  const attribute = initMobAttributeRecord(level = 3, experience = 0)
  playerShip.crew = @[]
  playerShip.crew.add(y = initMemberData(name = "Laeran", morale = [1: 50.Natural, 2: 0.Natural],
      homeBase = 1, faction = "POLEIS", orders = [0.Natural, 0, 0, 1, 1, 1, 2, 1, 1,
      1, 0, 0], order = talk, loyalty = 100, skills = @[initSkillInfo(index = 4, level = 4,
      experience = 0)], attributes = @[attribute, attribute, attribute, attribute]))
  playerShip.name = "Anaria"

  test "Generating name for save file.":
    let oldName = saveName
    generateSaveName()
    check:
      oldName != saveName
