import std/tables
import ../src/[careers, config, factions, game, items, types]
import unittest2

suite "Unit tests for items module":

  checkpoint "Loading the game data."
  loadData("../bin/data/game.dat")
  loadItems("../bin/data/items.dat")
  loadCareers("../bin/data/careers.dat")
  loadFactions("../bin/data/factions.dat")
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


  test "Testing findProtoItem.":
    checkpoint "Find an existing item"
    check:
      findProtoItem("Iron") > 0
    checkpoint "Not find a non-existing item"
    check:
      findProtoItem("sfdsfsdfsdf") == 0

  test "Testing getItemDamage.":
    checkpoint "Get damage info"
    check:
      getItemDamage(60) == "Damaged"
    checkpoint "Get damage info with lowercasing"
    check:
      getItemDamage(60, true) == "damaged"

  test "Testing getItemName.":
    var item = InventoryData(protoIndex: 2, amount: 1, name: "", durability: 80, price: 0)
    checkpoint "Get an item name with lowered damage info"
    check:
      getItemName(item) == "Basic Ration (slightly used)"
    checkpoint "Get an item name"
    check:
      getItemName(item, false) == "Basic Ration"
    checkpoint "Get an item name with damage info"
    check:
      getItemName(item, true, false) == "Basic Ration (Slightly used)"
    checkpoint "Get an item name with new name"
    item.name = "New name"
    check:
      getItemName(item, false) == "New name"

  test "Testing getItemChanceToDamage.":
    gameSettings.showNumbers = false
    checkpoint "Get chance to damage for 3 as string"
    check:
      getItemChanceToDamage(3) == "Small"
    checkpoint "Get chance to damage for 30 as string"
    check:
      getItemChanceToDamage(30) == "Very high"
    gameSettings.showNumbers = true
    checkpoint "Get chance to damage for 3 as number"
    check:
      getItemChanceToDamage(3) == " 3%"
    checkpoint "Get chance to damage for 30 as number"
    check:
      getItemChanceToDamage(30) == " 30%"

  test "Testing findTools.":
    checkpoint "Find an existing tool"
    check:
      findTools(0, "Bucket", clean) > -1
    checkpoint "Not find a non-existing tool"
    check:
      findTools(0, "sfewrwer", talk) == -1

  test "Testing getRandomItem.":
    let itemIndex = getRandomItem(weaponsList, weapon, 20, 20, "POLEIS")
    check:
      itemIndex > 0 and itemsList.hasKey(itemIndex)
