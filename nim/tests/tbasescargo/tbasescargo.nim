discard """
  exitcode: 0
"""

import std/tables
import ../../src/[basescargo, basestypes, careers, factions, game, items, maps, types]

if basesTypesList.len == 0:
  loadData("../bin/data/game.dat")
  loadItems("../bin/data/items.dat")
  loadCareers("../bin/data/careers.dat")
  loadFactions("../bin/data/factions.dat")
  loadBasesTypes("../bin/data/bases.dat")

skyBases[1].reputation = ReputationData(level: 1, experience: 1)
playerShip.skyX = 1
playerShip.skyY = 1
playerShip.crew = @[]
playerShip.crew.add(MemberData(morale: [1: 50.Natural, 2: 0.Natural],
    homeBase: 1, faction: "POLEIS", orders: [0.Natural, 0, 0, 1, 1, 1, 2, 1, 1,
    1, 0, 0], order: talk, loyalty: 100, skills: @[SkillInfo(index: 4, level: 4,
    experience: 0)], attributes: @[MobAttributeRecord(level: 3, experience: 0),
    MobAttributeRecord(level: 3, experience: 0), MobAttributeRecord(level: 3,
    experience: 0), MobAttributeRecord(level: 3, experience: 0)], health: 100))
skyMap[1][1].baseIndex = 1
skyBases[1].population = 100
skyBases[1].baseType = "1"
skyBases[1].owner = "POLEIS"
gameDate = DateRecord(year: 1600, month: 1, day: 1, hour: 8, minutes: 0)
generateCargo()
assert skyBases[1].cargo.len > 0
