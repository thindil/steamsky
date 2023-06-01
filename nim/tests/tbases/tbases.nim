discard """
  exitcode: 0
"""

import ../../src/[bases, basestypes, careers, factions, game, items, maps, types]

loadData("../bin/data/game.dat")
loadItems("../bin/data/items.dat")
loadCareers("../bin/data/careers.dat")
loadFactions("../bin/data/factions.dat")
loadBasesTypes("../bin/data/bases.dat")

assert generateBaseName("POLEIS").len() > 0, "Failed to generate a base's name."

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
var price: Natural = 100

skyBases[1].reputation = ReputationData(level: 1, experience: 1)
gainRep(1, 1)
assert skyBases[1].reputation.experience == 2, "Failed to gain reputation in a base."
gainRep(1, -1)
assert skyBases[1].reputation.experience == 1, "Failed to lose reputation in a base."

countPrice(price, 0, false)
assert price > 100, "Failed to raise a price in a base."
price = 100
countPrice(price, 0)
assert price < 100, "Failed to reduce a price in a base."

updatePopulation()

skyBases[1].population = 100
skyBases[1].baseType = "1"
skyBases[1].owner = "POLEIS"
gameDate = DateRecord(year: 1600, month: 1, day: 1, hour: 8, minutes: 0)
skyBases[1].recruits = @[]
generateRecruits()
assert skyBases[1].recruits.len > 0, "Failed to generate recruits in a base."
skyBases[1].recruits = @[]
skyBases[1].recruitDate = DateRecord(year: 0, month: 0, day: 0, hour: 0, minutes: 0)
skyBases[1].reputation.level = -50
generateRecruits()
assert skyBases[1].recruits.len > 0, "Failed to generate recruits in a base with negative reputation."
skyBases[1].recruits = @[]
skyBases[1].recruitDate = DateRecord(year: 0, month: 0, day: 0, hour: 0, minutes: 0)
skyBases[1].reputation.level = 0
generateRecruits()
assert skyBases[1].recruits.len > 0, "Failed to generate recruits in a base with zero reputation."

updatePrices()
