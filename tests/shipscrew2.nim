import std/paths
import ../src/[careers, factions, maps, shipmodules]
import unittest2
include ../src/shipscrew2

suite "Unit tests for shipscrew2 module":

  checkpoint "Loading the game data."
  loadData("bin/data/game.dat".Path)
  loadItems("bin/data/items.dat".Path)
  loadCareers("bin/data/careers.dat".Path)
  loadFactions("bin/data/factions.dat".Path)
  loadModules("bin/data/shipmodules.dat".Path)

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
  playerShip.crew[0].inventory.add(InventoryData(amount: 1, protoIndex: 1, durability: 100))
  playerShip.crew[0].inventory.add(InventoryData(amount: 1, protoIndex: 2, durability: 100))
  playerShip.modules = @[]
  playerShip.modules.add(ModuleData(mType: cargoRoom, protoIndex: 7,
      durability: 100, maxDurability: 100))
  playerShip.cargo = @[]
  playerShip.cargo.add(InventoryData(protoIndex: 1, amount: 100,
      durability: 100))
  playerShip.cargo.add(InventoryData(protoIndex: 3, amount: 200,
      durability: 100))
  skyMap[1][1].baseIndex = 1

  test "Deleting a crew member.":
    let oldLength = playerShip.crew.len
    deleteMember(1, playerShip)
    check:
      playerShip.crew.len == (oldLength - 1)

  test "Killing a crew member.":
    playerShip.crew.add(MemberData(morale: [1: 50.Natural, 2: 0.Natural],
        homeBase: 1, faction: "POLEIS", orders: [0.Natural, 0, 0, 1, 1, 1, 0, 1, 1,
        1, 0, 0], order: gunner, loyalty: 100))
    playerShip.cargo = @[]
    let oldLength = playerShip.crew.len
    death(1, "Test death", playerShip)
    checkpoint "Check if the crew member was killed"
    check:
      playerShip.crew.len == oldLength - 1
    checkpoint "Check if the body was created"
    check:
      playerShip.cargo.len == 1

  test "Getting the current order of a crew member.":
    check:
      getCurrentOrder(0) == "Talking with others"

  test "Moving items from crew member inventory to the ship cargo.":
    let oldLength = playerShip.crew[0].inventory.len
    moveItem(itemIndex = 0, amount = 1, memberIndex = 0)
    check:
      playerShip.crew[0].inventory.len == (oldLength - 1)
