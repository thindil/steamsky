discard """
  exitcode: 0
"""

import std/tables
import ../../src/[careers, crew, factions, game, items, ships, types]

if itemsList.len == 0:
  loadData("../bin/data/game.dat")
  loadItems("../bin/data/items.dat")
  loadCareers("../bin/data/careers.dat")
  loadFactions("../bin/data/factions.dat")

assert generateMemberName('M', "POLEIS").len() > 0

playerShip.crew = @[]
playerShip.crew.add(MemberData(skills: @[SkillInfo(index: 4, level: 1,
    experience: 0)], homeBase: 1))

assert getTrainingToolQuality(0, 1) == 100
