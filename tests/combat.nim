import std/paths
import ../src/[basestypes, careers, crafts, factions, mobs, shipmodules]
import unittest2
include ../src/combat

suite "Unit tests for combat module":
  checkpoint "Loading the game data."
  loadData("bin/data/game.dat".Path)
  loadItems("bin/data/items.dat".Path)
  loadCareers("bin/data/careers.dat".Path)
  loadFactions("bin/data/factions.dat".Path)
  loadBasesTypes("bin/data/bases.dat".Path)
  loadModules("bin/data/shipmodules.dat".Path)
  loadRecipes("bin/data/recipes.dat".Path)
  loadMobs("bin/data/mobs.dat".Path)
  loadShips("bin/data/ships.dat".Path)

  playerShip.skyX = 200
  playerShip.skyY = 200
  playerShip.crew = @[]
  playerShip.crew.add(MemberData(morale: [1: 50.Natural, 2: 0.Natural],
      homeBase: 1, faction: "POLEIS", orders: [0.Natural, 0, 0, 1, 1, 1, 2, 1,
      1, 1, 0, 0], order: talk, loyalty: 100, skills: @[initSkillInfo(index = 4,
      level = 4, experience = 0)], attributes: @[initMobAttributeRecord(level = 3,
      experience = 0), initMobAttributeRecord(level = 3, experience = 0),
      initMobAttributeRecord(level = 3, experience = 0), initMobAttributeRecord(level = 3,
      experience = 0)], health: 100, name: "Laeran"))
  playerShip.modules = @[]
  playerShip.modules.add(y = initModuleData(mType = ModuleType2.armor, protoIndex = 57,
      durability = 100, maxDurability = 100, name = "Armor", weight = 1))
  playerShip.modules.add(y = initModuleData(mType = ModuleType2.turret, protoIndex = 86,
      durability = 100, maxDurability = 100, name = "Turret", weight = 1))
  playerShip.modules.add(y = initModuleData(mType = ModuleType2.gun, protoIndex = 160,
      durability = 100, damage = 100, owner = @[-1], maxDurability = 100, name = "Gun", weight = 1))

  test "Starting combat.":
    discard startCombat(2)

  test "Executing a combat's turn.":
    combatTurn()
