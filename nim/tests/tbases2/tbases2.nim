discard """
  exitcode: 0
"""

import std/tables
import ../../src/[bases, bases2, basestypes, careers, crafts, factions, game,
    items, maps, mobs, ships, types, utils]

if basesTypesList.len == 0:
  loadData("../bin/data/game.dat")
  loadItems("../bin/data/items.dat")
  loadCareers("../bin/data/careers.dat")
  loadFactions("../bin/data/factions.dat")
  loadBasesTypes("../bin/data/bases.dat")
if recipesList.len == 0:
  loadRecipes("../bin/data/recipes.dat")
if protoMobsList.len == 0:
  loadMobs("../bin/data/mobs.dat")
if protoShipsList.len == 0:
  loadShips("../bin/data/ships.dat")

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
skyBases[1].population = 100
skyBases[1].baseType = "1"
skyBases[1].owner = "POLEIS"
skyBases[1].askedForEvents = DateRecord(year: 0, month: 0, day: 0, hour: 0, minutes: 0)
gameDate = DateRecord(year: 1600, month: 1, day: 1, hour: 8, minutes: 0)
for index, base in skyBases.mpairs:
  if index == 1:
    continue
  base.skyX = getRandom(1, 1_024)
  base.skyY = getRandom(1, 1_024)
  base.baseType = $getRandom(0, 4)
  base.owner = "POLEIS"

askForEvents()
assert eventsList.len > 0, "Failed to generate new events."
