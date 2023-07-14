discard """
  exitcode: 0
  output: '''Loading the game data.
Testing updateCargo.
Testing freeCargo.
Testing getItemAmount.
Testing getItemsAmount.'''
"""

import std/tables
import ../../src/[careers, factions, game, items, shipscargo, types]

echo "Loading the game data."
if itemsList.len == 0:
  loadData("../bin/data/game.dat")
  loadItems("../bin/data/items.dat")
  loadCareers("../bin/data/careers.dat")
  loadFactions("../bin/data/factions.dat")

playerShip.modules = @[]
playerShip.modules.add(ModuleData(mType: cargoRoom, protoIndex: 7))
playerShip.cargo = @[]
playerShip.cargo.add(InventoryData(protoIndex: 1, amount: 100, durability: 100))
playerShip.cargo.add(InventoryData(protoIndex: 3, amount: 200, durability: 100))
playerShip.crew = @[]
playerShip.crew.add(MemberData(morale: [1: 50.Natural, 2: 0.Natural],
    homeBase: 1, faction: "POLEIS", orders: [0.Natural, 0, 0, 1, 1, 1, 2, 1, 1,
    1, 0, 0], order: talk, loyalty: 100, skills: @[SkillInfo(index: 4, level: 4,
    experience: 0)], attributes: @[MobAttributeRecord(level: 3, experience: 0),
    MobAttributeRecord(level: 3, experience: 0), MobAttributeRecord(level: 3,
    experience: 0), MobAttributeRecord(level: 3, experience: 0)], health: 100))

echo "Testing updateCargo."
updateCargo(playerShip, 1, -1)
assert playerShip.cargo[0].amount == 99, "Failed to remove an item from the player's ship's cargo."
updateCargo(playerShip, 1, 1)
assert playerShip.cargo[0].amount == 100, "Failed to add an item to the player's ship's cargo."
updateCargo(playerShip, 40, -1)

echo "Testing freeCargo."
assert freeCargo(1) > freeCargo(0), "Failed to count free cargo space in the player's ship."

echo "Testing getItemAmount."
assert getItemAmount("Fuel") == 100, "Failed to get amount of fuel in the player's ship's cargo."

echo "Testing getItemsAmount."
assert getItemsAmount("Drinks") == 200, "Failed to get amount of drinks in the player's ship's cargo."
