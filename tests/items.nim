import std/[paths, tables]
import ../src/[careers, config, factions, game, items, types]
import unittest2

suite "Unit tests for items module":

  checkpoint "Loading the game data."
  loadData("bin/data/game.dat".Path)
  loadItems("bin/data/items.dat".Path)
  loadCareers("bin/data/careers.dat".Path)
  loadFactions("bin/data/factions.dat".Path)
  playerShip.modules = @[]
  playerShip.modules.add(ModuleData(mType: cargoRoom, protoIndex: 7))
  playerShip.cargo = @[]
  playerShip.cargo.add(InventoryData(protoIndex: 53, amount: 1))
  playerShip.crew = @[]
  var member = MemberData(homeBase: 1)
  const attribute = MobAttributeRecord(level: 1, experience: 0)
  member.attributes = @[attribute, attribute, attribute, attribute]
  member.inventory.add(InventoryData(amount: 1, protoIndex: 1))
  member.inventory.add(InventoryData(amount: 1, protoIndex: 2))
  for index, _ in member.equipment.mpairs:
    member.equipment[index] = -1
  member.equipment[weapon] = 1
  playerShip.crew.add(member)
  var item = InventoryData(protoIndex: 2, amount: 1, name: "", durability: 80, price: 0)
  var inventory: seq[InventoryData]
  inventory.add(InventoryData(protoIndex: 66, amount: 1, name: "",
      durability: defaultItemDurability, price: 0))
  inventory.add(InventoryData(protoIndex: 67, amount: 1, name: "",
      durability: defaultItemDurability, price: 0))


  test "Find an existing item":
    check:
      findProtoItem("Iron") > 0

  test "Not find a non-existing item":
    check:
      findProtoItem("sfdsfsdfsdf") == 0

  test "Get damage info":
    check:
      getItemDamage(60) == "Damaged"

  test "Get damage info with lowercasing":
    check:
      getItemDamage(60, true) == "damaged"

  test "Get an item name with lowered damage info":
    check:
      getItemName(item) == "Basic Ration (slightly used)"

  test "Get an item name":
    check:
      getItemName(item, false) == "Basic Ration"

  test "Get an item name with damage info":
    check:
      getItemName(item, true, false) == "Basic Ration (Slightly used)"

  test "Get an item name with new name":
    item.name = "New name"
    check:
      getItemName(item, false) == "New name"

  test "Get chance to damage for 3 as string":
    gameSettings.showNumbers = false
    check:
      getItemChanceToDamage(3) == "Small"

  test "Get chance to damage for 30 as string":
    check:
      getItemChanceToDamage(30) == "Very high"

  test "Get chance to damage for 3 as number":
    gameSettings.showNumbers = true
    check:
      getItemChanceToDamage(3) == " 3%"

  test "Get chance to damage for 30 as number":
    check:
      getItemChanceToDamage(30) == " 30%"

  test "Find an existing tool":
    check:
      findTools(0, "Bucket", clean) > -1

  test "Not find a non-existing tool":
    check:
      findTools(0, "sfewrwer", talk) == -1

  test "Getting a random item.":
    let itemIndex = getRandomItem(weaponsList, weapon, 20, 20, "POLEIS")
    check:
      itemIndex > 0 and itemsList.hasKey(itemIndex)

  test "Finding no money in inventory":
    check:
      moneyAmount(inventory) == 0

  test "Getting money amount from inventory":
    check:
      moneyAmount(playerShip.crew[0].inventory) == 1

  test "Add money to an inventory":
    updateMoney(0, 10, any)
    check:
      moneyAmount(playerShip.crew[0].inventory) == 11

  test "Removing money from an inventory":
    updateMoney(0, -10, any)
    check:
      moneyAmount(playerShip.crew[0].inventory) == 1
