discard """
  exitcode: 0
  output: '''Loading the game data.
Testing generateEnemies.
Testing updateEvents.
Testing deleteEvent.
Testing recoverBase.
Testing generateTraders.'''
"""

import std/tables
import ../../src/[careers, crafts, events, factions, game, items, maps, mobs,
    ships, shipmodules, types]

echo "Loading the game data."
loadData("../bin/data/game.dat")
loadModules("../bin/data/shipmodules.dat")
loadCareers("../bin/data/careers.dat")
loadFactions("../bin/data/factions.dat")
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

echo "Testing generateEnemies."
var enemies: seq[Positive] = @[]
generateEnemies(enemies)
try:
  assert enemies.len > 0
except AssertionDefect:
  writeLine(stderr, "Failed to generate the list of enemy ships.")

echo "Testing updateEvents."
eventsList = @[]
updateEvents(1)

echo "Testing deleteEvent."
eventsList.add(EventData(eType: doublePrice, skyX: 1, skyY: 1, itemIndex: 1, time: 10))
deleteEvent(0)
try:
  assert eventsList.len == 0
except AssertionDefect:
  writeLine(stderr, "Failed to delete an event.")

echo "Testing recoverBase."
skyBases[2].population = 0
recoverBase(2)
try:
  assert skyBases[2].population > 0
except AssertionDefect:
  writeLine(stderr, "Failed to recover a base.")

echo "Testing generateTraders."
generateTraders()
