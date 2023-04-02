discard """
  exitcode: 0
"""

import std/tables
import ../../src/[careers, crew, factions, game, items, types]

if itemsList.len == 0:
  loadData("../bin/data/game.dat")
  loadItems("../bin/data/items.dat")
  loadCareers("../bin/data/careers.dat")
  loadFactions("../bin/data/factions.dat")

assert generateMemberName('M', "POLEIS").len() > 0

playerShip.crew = @[]
playerCareer = "general"
playerShip.crew.add(MemberData(skills: @[SkillInfo(index: 4, level: 4,
    experience: 0), SkillInfo(index: 2, level: 4, experience: 0)], homeBase: 1,
    attributes: @[MobAttributeRecord(level: 3, experience: 0),
    MobAttributeRecord(level: 3, experience: 0), MobAttributeRecord(level: 3,
    experience: 0), MobAttributeRecord(level: 3, experience: 0)], health: 100))

gainExp(10, 4, 0)
assert playerShip.crew[0].skills[0].experience == 10
