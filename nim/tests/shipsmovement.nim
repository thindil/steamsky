import ../src/[basestypes, careers, crafts, factions, items, mobs, shipmodules]
import unittest2
include ../src/shipsmovement

suite "Unit tests for shipsmovement module":

  checkpoint "Loading the game data."
  loadData("../bin/data/game.dat")
  loadItems("../bin/data/items.dat")
  loadCareers("../bin/data/careers.dat")
  loadFactions("../bin/data/factions.dat")
  loadBasesTypes("../bin/data/bases.dat")
  loadModules("../bin/data/shipmodules.dat")
  loadRecipes("../bin/data/recipes.dat")
  loadMobs("../bin/data/mobs.dat")
  loadShips("../bin/data/ships.dat")

  playerShip.modules = @[]
  playerShip.modules.add(ModuleData(mType: ModuleType2.engine, protoIndex: 3,
      durability: 100, fuelUsage: 4, power: 2000, disabled: false,
      maxDurability: 100))
  playerShip.modules.add(ModuleData(mType: ModuleType2.cockpit, protoIndex: 5,
      durability: 100, maxDurability: 100))
  playerShip.cargo = @[]
  playerShip.cargo.add(InventoryData(protoIndex: 1, amount: 100, durability: 100))
  playerShip.crew = @[]
  playerShip.crew.add(MemberData(morale: [1: 50.Natural, 2: 0.Natural],
      homeBase: 1, faction: "POLEIS", orders: [2.Natural, 0, 0, 1, 1, 1, 1, 1, 1,
      1, 0, 0], order: pilot, loyalty: 100, skills: @[SkillInfo(index: 4,
      level: 4,
      experience: 0)], attributes: @[MobAttributeRecord(level: 3, experience: 0),
      MobAttributeRecord(level: 3, experience: 0), MobAttributeRecord(level: 3,
      experience: 0), MobAttributeRecord(level: 3, experience: 0)]))
  playerShip.crew.add(MemberData(morale: [1: 50.Natural, 2: 0.Natural],
      homeBase: 1, faction: "POLEIS", orders: [0.Natural, 2, 0, 1, 1, 1, 1, 1, 1,
      1, 0, 0], order: engineer, loyalty: 100, skills: @[SkillInfo(index: 4,
      level: 4,
      experience: 0)], attributes: @[MobAttributeRecord(level: 3, experience: 0),
      MobAttributeRecord(level: 3, experience: 0), MobAttributeRecord(level: 3,
      experience: 0), MobAttributeRecord(level: 3, experience: 0)]))
  playerShip.crew.add(MemberData(morale: [1: 50.Natural, 2: 0.Natural],
      homeBase: 1, faction: "POLEIS", orders: [0.Natural, 0, 0, 1, 1, 1, 2, 1, 1,
      1, 0, 0], order: pilot, loyalty: 100, skills: @[SkillInfo(index: 4,
      level: 4,
      experience: 0)], attributes: @[MobAttributeRecord(level: 3, experience: 0),
      MobAttributeRecord(level: 3, experience: 0), MobAttributeRecord(level: 3,
      experience: 0), MobAttributeRecord(level: 3, experience: 0)]))
  playerShip.crew.add(MemberData(morale: [1: 50.Natural, 2: 0.Natural],
      homeBase: 1, faction: "POLEIS", orders: [0.Natural, 0, 0, 1, 1, 1, 2, 1, 1,
      1, 0, 0], order: engineer, loyalty: 100, skills: @[SkillInfo(index: 4,
      level: 4,
      experience: 0)], attributes: @[MobAttributeRecord(level: 3, experience: 0),
      MobAttributeRecord(level: 3, experience: 0), MobAttributeRecord(level: 3,
      experience: 0), MobAttributeRecord(level: 3, experience: 0)]))
  skyMap[1][1].baseIndex = 1
  for base in skyBases.mitems:
    base.population = 100
  skyBases[1].baseType = "1"
  skyBases[1].owner = "POLEIS"
  skyBases[1].missionsDate = DateRecord(year: 0, month: 0, day: 0, hour: 0, minutes: 0)

  test "Testing waitInPlace.":
    waitInPlace(1)

  test "Testing realSpeed.":
    checkpoint "Get the real speed of the docked ship."
    playerShip.speed = docked
    check:
      realSpeed(playerShip) == 0
    checkpoint "Get the real speed of the ship with full speed."
    playerShip.speed = fullSpeed
    check:
      realSpeed(playerShip) > 0
    checkpoint "Get the speed docked ship."
    playerShip.speed = docked
    check:
      realSpeed(playerShip, true) > 0

  test "Testing dockShip.":
    checkpoint "Undock the ship from a base."
    check:
      dockShip(false).len == 0
    checkpoint "Dock the ship to a base."
    check:
      dockShip(true).len == 0

  test "Testing countFuelNeeded.":
    check:
      countFuelNeeded() == -4

  test "Testing changeShipSpeed.":
    checkpoint "Change speed of the docked ship."
    check:
      changeShipSpeed(fullSpeed).len == 0
    checkpoint "Undock the ship."
    check:
      dockShip(false).len == 0
    checkpoint "Changee the speed of the ship"
    check:
      changeShipSpeed(fullStop).len == 0
    checkpoint "Dock the ship."
    check:
      dockShip(true).len == 0

  test "Testing moveShip.":
    var
      newX, newY = 1
      message = ""
    playerShip.speed = fullSpeed
    check:
      moveShip(newX, newY, message) != 0
    checkpoint "Check X movement"
    check:
      playerShip.skyX == 2
    checkpoint "Check Y movement"
    check:
      playerShip.skyY == 2
