import std/paths
import ../src/[careers, crafts, factions, mobs, reputation, shipmodules]
import unittest2
include ../src/trades

suite "Unit tests for trades module":

  checkpoint "Loading the game data."
  loadData("bin/data/game.dat".Path)
  loadItems("bin/data/items.dat".Path)
  loadCareers("bin/data/careers.dat".Path)
  loadFactions("bin/data/factions.dat".Path)
  loadModules("bin/data/shipmodules.dat".Path)
  loadRecipes("bin/data/recipes.dat".Path)
  loadMobs("bin/data/mobs.dat".Path)
  loadShips("bin/data/ships.dat".Path)
  loadBasesTypes("bin/data/bases.dat".Path)

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
      experience: 0), MobAttributeRecord(level: 3, experience: 0)], health: 100))
  playerShip.crew.add(MemberData(morale: [1: 50.Natural, 2: 0.Natural],
      homeBase: 1, faction: "POLEIS", orders: [0.Natural, 0, 0, 1, 1, 1, 0, 1, 1,
      1, 0, 0], order: gunner, loyalty: 100, attributes: @[MobAttributeRecord(
          level: 3, experience: 0),
      MobAttributeRecord(level: 3, experience: 0), MobAttributeRecord(level: 3,
      experience: 0), MobAttributeRecord(level: 3, experience: 0)], health: 100))
  playerShip.modules = @[]
  playerShip.modules.add(ModuleData(mType: ModuleType2.hull, protoIndex: 1,
      durability: 100, maxModules: 10))
  playerShip.modules.add(ModuleData(mType: cargoRoom, protoIndex: 7,
      durability: 100, maxDurability: 100))
  playerShip.modules.add(ModuleData(mType: turret, protoIndex: 8,
      durability: 100, maxDurability: 100, gunIndex: 3))
  playerShip.modules.add(ModuleData(mType: gun, protoIndex: 9, durability: 100,
      maxDurability: 100, damage: 10, owner: @[1]))
  playerShip.modules.add(ModuleData(mType: ModuleType2.cabin, protoIndex: 4,
      durability: 100, owner: @[0]))
  playerShip.cargo = @[]
  playerShip.cargo.add(InventoryData(protoIndex: 1, amount: 100,
      durability: 100))
  playerShip.cargo.add(InventoryData(protoIndex: 3, amount: 200,
      durability: 100))
  playerShip.speed = docked
  skyMap[1][1].baseIndex = 1
  skyMap[1][1].eventIndex = -1
  skyBases[1].population = 100
  skyBases[1].baseType = "1"
  skyBases[1].owner = "POLEIS"
  skyBases[1].reputation = ReputationData(level: 1, experience: 1)
  for i in 2 .. 100:
    skyBases[i].population = 100
  generateCargo()
  gameDate = DateRecord(year: 1600, month: 1, day: 1, hour: 8, minutes: 0)
  resetReputations()

  test "Generating a trader's cargo.":
    generateTraderCargo(96)
    check:
      traderCargo.len > 0

  test "Selling an item.":
    sellItems(1, "1")

  test "Buying an item.":
    generateCargo()
    updateBaseCargo(skyBases[1].cargo[2].protoIndex, 2, quality = normal)
    buyItems(2, "1")
