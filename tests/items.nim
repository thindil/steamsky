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
  playerShip.modules.add(y = initModuleData(mType = cargoRoom, protoIndex = 7,
      durability = 100, maxDurability = 100, name = "Cargo", weight = 1))
  playerShip.cargo = @[]
  playerShip.cargo.add(y = initInventoryData(protoIndex = 53, amount = 1))
  playerShip.cargo.add(y = initInventoryData(protoIndex = 106, amount = 1))
  playerShip.crew = @[]
  var member = MemberData(homeBase: 1)
  const attribute = initMobAttributeRecord(level = 3, experience = 0)
  member.attributes = @[attribute, attribute, attribute, attribute]
  member.inventory.add(y = initInventoryData(amount = 1, protoIndex = 1))
  member.inventory.add(y = initInventoryData(amount = 1, protoIndex = 2))
  for index, _ in member.equipment.mpairs:
    member.equipment[index] = -1
  member.equipment[weapon] = 1
  playerShip.crew.add(member)
  var item = initInventoryData(protoIndex = 2, amount = 1, name = "", durability = 80, price = 0)
  var inventory: seq[InventoryData]
  inventory.add(y = initInventoryData(protoIndex = 66, amount = 1, name = "",
      durability = defaultItemDurability, price = 0))
  inventory.add(y = initInventoryData(protoIndex = 67, amount = 1, name = "",
      durability = defaultItemDurability, price = 0))

  test "Find an existing item":
    check:
      findProtoItem("Iron") > 0

  test "Not find a non-existing item":
    check:
      findProtoItem("sfdsfsdfsdf") == 0

  test "Get damage info":
    let item = initInventoryData(protoIndex = 2, amount = 1, name = "", durability = 60, price = 0)
    check:
      getItemDamage(item = item) == "Damaged"

  test "Get damage info with lowercasing":
    let item = initInventoryData(protoIndex = 2, amount = 1, name = "", durability = 60, price = 0)
    check:
      getItemDamage(item = item, toLower = true) == "damaged"

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

  test "Get chance to damage for 5 as string":
    gameSettings.showNumbers = false
    check:
      getItemChanceToDamage(1) == "Below average"

  test "Get chance to damage for 20 as string":
    check:
      getItemChanceToDamage(0) == "Very high"

  test "Get chance to damage for 5 as number":
    gameSettings.showNumbers = true
    check:
      getItemChanceToDamage(1) == " 5%"

  test "Get chance to damage for 20 as number":
    check:
      getItemChanceToDamage(0) == " 20%"

  test "Find an existing tool":
    check:
      findTools(0, "Bucket", clean) > -1

  test "Not find a non-existing tool":
    check:
      findTools(0, "sfewrwer", talk) == -1

  test "Getting a random item":
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

  test "Setting breakChance field":
    setToolsList()
    setBreakChance()
    check:
      itemsList[47].breakChance == 10

  test "Getting the normal item's max durability":
    let item = initInventoryData(protoIndex = 2, amount = 1)
    check:
      getItemMaxDurability(item = item) == defaultItemDurability


  test "Getting the special item's max durability":
    let item = initInventoryData(protoIndex = 2, amount = 1, craftBonus = moreDurable)
    check:
      getItemMaxDurability(item = item) == 120
