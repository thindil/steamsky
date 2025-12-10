import std/paths
import ../src/[careers, crafts, events, factions, game, items, maps, mobs, ships, shipmodules, types]
import unittest2

suite "Unit tests for events module":

  checkpoint "Loading the game data."
  loadData("bin/data/game.dat")
  loadItems("bin/data/items.dat".Path)
  loadModules("bin/data/shipmodules.dat".Path)
  loadCareers("bin/data/careers.dat")
  loadFactions("bin/data/factions.dat".Path)
  loadRecipes("bin/data/recipes.dat".Path)
  loadMobs("bin/data/mobs.dat".Path)
  loadShips("bin/data/ships.dat")

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
  eventsList = @[]

  test "Generating enemies.":
    var enemies: seq[Positive] = @[]
    generateEnemies(enemies)
    check:
      enemies.len > 0

  test "Updating events.":
    updateEvents(1)

  test "Deleting an event.":
    eventsList.add(EventData(eType: doublePrice, skyX: 1, skyY: 1, itemIndex: 1, time: 10))
    deleteEvent(0)
    check:
      eventsList.len == 0

  test "Recovering a base.":
    skyBases[2].population = 0
    recoverBase(2)
    check:
      skyBases[2].population > 0

  test "Generating traders.":
    var traders: seq[Positive] = @[]
    generateTraders(ships = traders)
    check:
      traders.len > 0
