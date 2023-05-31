discard """
  exitcode: 0
"""

import std/tables
import ../../src/[careers, factions, game, gainexp, maps, items, types]

if itemsList.len == 0:
  loadData("../bin/data/game.dat")
  loadItems("../bin/data/items.dat")
  loadCareers("../bin/data/careers.dat")
  loadFactions("../bin/data/factions.dat")

playerCareer = "general"
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
skyMap[1][1].baseIndex = 1

gainExp(10, 4, 0)
assert playerShip.crew[0].skills[0].experience == 10

skyBases[1].reputation = ReputationData(level: 1, experience: 1)
gainRep(1, 1)
assert skyBases[1].reputation.experience == 2, "Failed to gain reputation in a base."
gainRep(1, -1)
assert skyBases[1].reputation.experience == 1, "Failed to lose reputation in a base."

