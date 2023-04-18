discard """
  exitcode: 0
"""

import ../../src/[bases, careers, factions, game, items, maps, types]

loadData("../bin/data/game.dat")
loadItems("../bin/data/items.dat")
loadCareers("../bin/data/careers.dat")
loadFactions("../bin/data/factions.dat")

assert generateBaseName("POLEIS").len() > 0

skyBases[1].reputation = ReputationData(level: 1, experience: 1)
gainRep(1, 1)
assert skyBases[1].reputation.experience == 2
gainRep(1, -1)
assert skyBases[1].reputation.experience == 1

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

countPrice(price, 0, false)
assert price > 100
price = 100
countPrice(price, 0)
assert price < 100

updatePopulation()
