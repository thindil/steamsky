discard """
  exitcode: 0
  output: '''Loading the game data.
Testing startCombat.'''
"""

import std/tables
import ../../src/[basestypes, careers, combat, crafts, factions, game, items,
    mobs, shipmodules, ships, types]

echo "Loading the game data."
if basesTypesList.len == 0:
  loadData("../bin/data/game.dat")
  loadItems("../bin/data/items.dat")
  loadCareers("../bin/data/careers.dat")
  loadFactions("../bin/data/factions.dat")
  loadBasesTypes("../bin/data/bases.dat")
if modulesList.len == 0:
  loadModules("../bin/data/shipmodules.dat")
if recipesList.len == 0:
  loadRecipes("../bin/data/recipes.dat")
if protoMobsList.len == 0:
  loadMobs("../bin/data/mobs.dat")
if protoShipsList.len == 0:
  loadShips("../bin/data/ships.dat")

playerShip.skyX = 200
playerShip.skyY = 200
playerShip.crew = @[]
playerShip.crew.add(MemberData(morale: [1: 50.Natural, 2: 0.Natural],
    homeBase: 1, faction: "POLEIS", orders: [0.Natural, 0, 0, 1, 1, 1, 2, 1, 1,
    1, 0, 0], order: talk, loyalty: 100, skills: @[SkillInfo(index: 4, level: 4,
    experience: 0)], attributes: @[MobAttributeRecord(level: 3, experience: 0),
    MobAttributeRecord(level: 3, experience: 0), MobAttributeRecord(level: 3,
    experience: 0), MobAttributeRecord(level: 3, experience: 0)], health: 100))
playerShip.modules = @[]
playerShip.modules.add(ModuleData(mType: ModuleType2.armor, protoIndex: 57,
    durability: 100))
playerShip.modules.add(ModuleData(mType: ModuleType2.turret, protoIndex: 86,
    durability: 100))
playerShip.modules.add(ModuleData(mType: ModuleType2.gun, protoIndex: 160,
    durability: 100, damage: 100, owner: @[-1]))

echo "Testing startCombat."
discard startCombat(2)
