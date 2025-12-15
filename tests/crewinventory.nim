import std/paths
import ../src/[crewinventory, game, items, types]
import unittest2

suite "Unit tests for crewinventory module":

  checkpoint "Loading the game data."
  loadData("bin/data/game.dat".Path)
  loadItems("bin/data/items.dat".Path)

  playerShip.crew = @[]
  var member = MemberData(homeBase: 1)
  const attribute = MobAttributeRecord(level: 1, experience: 0)
  member.attributes = @[attribute, attribute, attribute, attribute]
  member.inventory.add(InventoryData(amount: 1, protoIndex: 1, durability: 100))
  member.inventory.add(InventoryData(amount: 1, protoIndex: 2, durability: 100))
  for index, _ in member.equipment.mpairs:
    member.equipment[index] = -1
  member.equipment[weapon] = 1
  playerShip.crew.add(member)
  var inventory: seq[InventoryData]
  inventory.add(InventoryData(protoIndex: 66, amount: 1, name: "",
      durability: defaultItemDurability, price: 0))
  inventory.add(InventoryData(protoIndex: 67, amount: 1, name: "",
      durability: defaultItemDurability, price: 0))

  test "Checking free inventory space.":
    discard freeInventory(0, 0)

  test "Check if an item is not used by the player.":
    check:
      not itemIsUsed(0, 0)

  test "Check if an item is used by the player.":
    check:
      itemIsUsed(0, 1)

  test "Take off item.":
    takeOffItem(0, 1)
    check:
      not itemIsUsed(0, 1)

  test "Adding an item to the player's inventory.":
    updateInventory(0, 1, 1, ship = playerShip, quality = normal)
    check:
      playerShip.crew[0].inventory[0].amount == 2

  test "Removing an item from the player's inventory.":
    updateInventory(0, -1, 1, ship = playerShip, quality = normal)
    check:
      playerShip.crew[0].inventory[0].amount == 1

  test "Adding too much items to the player's inventory.":
    expect CrewNoSpaceError:
      updateInventory(0, 10_000, 1, ship = playerShip, quality = normal)
    check:
      playerShip.crew[0].inventory[0].amount == 1

  test "Damaging item":
    for i in 1..100:
      if playerShip.crew[0].inventory.len == 0:
        break
      damageItem(playerShip.crew[0].inventory, 0, 0, 0, playerShip)

  test "Find an item in inventory by proto index.":
    check:
      findItem(inventory, 67, itemQuality = normal) == 1

  test "Find an item in inventory by item type.":
    check:
      findItem(inventory = inventory, itemType = "Weapon",
          itemQuality = normal) == 0

  test "Not find an item in inventory by proto index.":
    check:
      findItem(inventory, 500, itemQuality = normal) == -1

  test "Not find an item in inventory with item type":
    check:
      findItem(inventory = inventory, itemType = "asdasdas",
          itemQuality = normal) == -1

  test "Getting quality of a training tool.":
    check:
      getTrainingToolQuality(0, 1) == 100
