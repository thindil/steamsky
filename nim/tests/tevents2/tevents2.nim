discard """
  exitcode: 0
  output: '''Loading the game data.
Testing checkForEvent.'''
"""

import std/tables
import ../../src/[careers, crafts, events2, factions, game, items, maps, mobs,
    ships, shipmodules, types]

echo "Loading the game data."
if factionsList.len == 0:
  loadData("../bin/data/game.dat")
  loadItems("../bin/data/items.dat")
  loadCareers("../bin/data/careers.dat")
  loadFactions("../bin/data/factions.dat")
  loadModules("../bin/data/shipmodules.dat")
if recipesList.len == 0:
  loadRecipes("../bin/data/recipes.dat")
if protoMobsList.len == 0:
  loadMobs("../bin/data/mobs.dat")
if protoShipsList.len == 0:
  loadShips("../bin/data/ships.dat")

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
playerShip.modules.add(ModuleData(mType: ModuleType2.armor, protoIndex: 57,
    durability: 100))
playerShip.modules.add(ModuleData(mType: ModuleType2.turret, protoIndex: 86,
    durability: 100))
playerShip.modules.add(ModuleData(mType: ModuleType2.gun, protoIndex: 160,
    durability: 100, damage: 100))
skyMap[1][1].baseIndex = 1
skyBases[1].population = 100
skyBases[1].baseType = "1"
skyBases[1].owner = "POLEIS"
gameDate = DateRecord(year: 1600, month: 1, day: 1, hour: 8, minutes: 0)
skyBases[1].recruits = @[]
for x in 1 .. 1024:
  for y in 1 .. 1024:
    skyMap[x][y].eventIndex = -1
    skyMap[x][y].baseIndex = 0
skyMap[playerShip.skyX][playerShip.skyY].baseIndex = 1
skyMap[playerShip.skyX][playerShip.skyY].eventIndex = -1

echo "Testing checkForEvent."
discard checkForEvent()
