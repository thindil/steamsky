import std/paths
import ../src/[careers, crafts, mobs, ships, shipmodules]
import unittest2
include ../src/events2

suite "Unit tests for events2 module":

  checkpoint "Loading the game data."
  loadData("bin/data/game.dat".Path)
  loadItems("bin/data/items.dat".Path)
  loadBasesTypes("bin/data/bases.dat".Path)
  loadCareers("bin/data/careers.dat".Path)
  loadFactions("bin/data/factions.dat".Path)
  loadModules("bin/data/shipmodules.dat".Path)
  loadRecipes("bin/data/recipes.dat".Path)
  loadMobs("bin/data/mobs.dat".Path)
  loadShips("bin/data/ships.dat".Path)

  playerShip.skyX = 1
  playerShip.skyY = 1
  playerShip.crew = @[]
  playerShip.crew.add(MemberData(morale: [1: 50.Natural, 2: 0.Natural],
      homeBase: 1, faction: "POLEIS", orders: [0.Natural, 0, 0, 1, 1, 1, 2, 1, 1,
      1, 0, 0], order: talk, loyalty: 100, skills: @[SkillInfo(index: 4,
      level: 4,
      experience: 0)], attributes: @[MobAttributeRecord(level: 3,
          experience: 0),
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
  playerShip.modules.add(ModuleData(mType: ModuleType2.gun, protoIndex: 9,
      durability: 100, damage: 100))
  playerShip.cargo = @[]
  playerShip.cargo.add(InventoryData(protoIndex: 1, amount: 2000,
      durability: 100))
  playerShip.cargo.add(InventoryData(protoIndex: 4, amount: 100,
      durability: 100))
  playerShip.cargo.add(InventoryData(protoIndex: 23, amount: 100,
      durability: 100))
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

  test "Check for event at a Poleis base":
    discard checkForEvent()

  test "Check for events at a Pirates base":
    skyBases[1].owner = "PIRATES"
    discard checkForEvent()

  test "Check for events in a Poleis base":
    skyBases[1].owner = "POLEIS"
    playerShip.speed = docked
    discard checkForEvent()
