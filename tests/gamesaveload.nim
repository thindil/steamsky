import unittest2
include ../src/gamesaveload

suite "Unit tests for gamesaveload module":

  playerShip.crew = @[]
  playerShip.crew.add(MemberData(name: "Laeran", morale: [1: 50.Natural, 2: 0.Natural],
      homeBase: 1, faction: "POLEIS", orders: [0.Natural, 0, 0, 1, 1, 1, 2, 1, 1,
      1, 0, 0], order: talk, loyalty: 100, skills: @[SkillInfo(index: 4, level: 4,
      experience: 0)], attributes: @[MobAttributeRecord(level: 3, experience: 0),
      MobAttributeRecord(level: 3, experience: 0), MobAttributeRecord(level: 3,
      experience: 0), MobAttributeRecord(level: 3, experience: 0)]))
  playerShip.name = "Anaria"

  test "Generating name for save file.":
    let oldName = saveName
    generateSaveName()
    check:
      oldName != saveName
