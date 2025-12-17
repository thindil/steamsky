import std/paths
import ../src/[careers, factions, game, items, shipmodules, shipscargo, types]
import unittest2

suite "Unit tests for shipscargo module":

  checkpoint "Loading the game data."
  loadData("bin/data/game.dat".Path)
  loadItems("bin/data/items.dat".Path)
  loadCareers("bin/data/careers.dat".Path)
  loadFactions("bin/data/factions.dat".Path)
  loadModules("bin/data/shipmodules.dat".Path)

  playerShip.modules = @[]
  playerShip.modules.add(ModuleData(mType: cargoRoom, protoIndex: 7,
      durability: 100, maxDurability: 100))
  playerShip.cargo = @[]
  playerShip.cargo.add(InventoryData(protoIndex: 1, amount: 100,
      durability: 100))
  playerShip.cargo.add(InventoryData(protoIndex: 3, amount: 200,
      durability: 100))
  playerShip.crew = @[]
  playerShip.crew.add(MemberData(morale: [1: 50.Natural, 2: 0.Natural],
      homeBase: 1, faction: "POLEIS", orders: [0.Natural, 0, 0, 1, 1, 1, 2, 1,
      1, 1, 0, 0], order: talk, loyalty: 100, skills: @[SkillInfo(index: 4,
      level: 4, experience: 0)], attributes: @[MobAttributeRecord(level: 3,
      experience: 0), MobAttributeRecord(level: 3, experience: 0),
      MobAttributeRecord(level: 3, experience: 0), MobAttributeRecord(level: 3,
      experience: 0)], health: 100))

  test "Remove an item from the player's ship cargo":
    updateCargo(playerShip, 1, -1, quality = normal)
    check:
      playerShip.cargo[0].amount == 99

  test "Add an item to the player's ship cargo":
    updateCargo(playerShip, 1, 1, quality = normal)
    check:
      playerShip.cargo[0].amount == 100
    updateCargo(playerShip, 40, -1, quality = normal)

  test "Checking a free space in the player's ship cargo.":
    check:
      freeCargo(1) > freeCargo(0)

  test "Getting amount of an item in the player's ship cargo.":
    check:
      getItemAmount("Fuel") == 100

  test "Getting amount of items in the player's ship cargo.":
    check:
      getItemsAmount("Drinks") == 200
