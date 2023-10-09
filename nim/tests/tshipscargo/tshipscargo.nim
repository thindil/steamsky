discard """
  exitcode: 0
  output: '''Loading the game data.
Testing updateCargo.
Testing freeCargo.
Testing getItemAmount.
Testing getItemsAmount.'''
"""

import std/tables
import ../../src/[careers, factions, game, items, shipmodules, shipscargo, types]

echo "Loading the game data."
if itemsList.len == 0:
  loadData("../bin/data/game.dat")
  loadItems("../bin/data/items.dat")
  loadCareers("../bin/data/careers.dat")
  loadFactions("../bin/data/factions.dat")
  loadModules("../bin/data/shipmodules.dat")

playerShip.modules = @[]
playerShip.modules.add(ModuleData(mType: cargoRoom, protoIndex: 7,
    durability: 100, maxDurability: 100))
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
try:
  assert playerShip.cargo[0].amount == 99
except AssertionDefect:
  writeLine(stderr,  "Failed to remove an item from the player's ship's cargo.")
updateCargo(playerShip, 1, 1)
try:
  assert playerShip.cargo[0].amount == 100
except AssertionDefect:
  writeLine(stderr, "Failed to add an item to the player's ship's cargo.")
updateCargo(playerShip, 40, -1)

echo "Testing freeCargo."
try:
  assert freeCargo(1) > freeCargo(0)
except AssertionDefect:
  writeLine(stderr, "Failed to count free cargo space in the player's ship.")

echo "Testing getItemAmount."
try:
  assert getItemAmount("Fuel") == 100
except AssertionDefect:
  writeLine(stderr, "Failed to get amount of fuel in the player's ship's cargo.")

echo "Testing getItemsAmount."
try:
  assert getItemsAmount("Drinks") == 200
except AssertionDefect:
  writeLine(stderr, "Failed to get amount of drinks in the player's ship's cargo.")
