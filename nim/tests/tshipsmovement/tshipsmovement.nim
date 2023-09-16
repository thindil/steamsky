discard """
  exitcode: 0
  output: '''Loading the game data.
Testing waitInPlace.
Testing realSpeed.
Testing dockShip.
Testing countFuelNeeded.
Testing changeShipSpeed.'''
"""

import std/tables
import ../../src/[basestypes, careers, crafts, factions, game, items, maps,
    mobs, ships, shipsmovement, shipmodules, types]

echo "Loading the game data."
if basesTypesList.len == 0:
  loadData("../bin/data/game.dat")
  loadItems("../bin/data/items.dat")
  loadCareers("../bin/data/careers.dat")
  loadFactions("../bin/data/factions.dat")
  loadBasesTypes("../bin/data/bases.dat")
if modulesList.len == 0:
  loadModules("../bin/data/shipmodules.dat")
if recipesList.len == 0:
  loadRecipes("../bin/data/recipes.dat")
if protoMobsList.len == 0:
  loadMobs("../bin/data/mobs.dat")
if protoShipsList.len == 0:
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
skyBases[1].population = 100
skyBases[1].baseType = "1"
skyBases[1].owner = "POLEIS"

echo "Testing waitInPlace."
waitInPlace(1)

echo "Testing realSpeed."
playerShip.speed = docked
try:
  assert realSpeed(playerShip) == 0
except AssertionDefect:
  writeLine(stderr, "Failed to get the real speed of the docked ship.")
playerShip.speed = fullSpeed
try:
  assert realSpeed(playerShip) > 0
except AssertionDefect:
  writeLine(stderr, "Failed to get the real speed of the ship with full speed.")
playerShip.speed = docked
try:
  assert realSpeed(playerShip, true) > 0
except AssertionDefect:
  writeLine(stderr, "Failed to get info about the real speed of the docked ship.")

echo "Testing dockShip."
try:
  assert dockShip(false).len == 0
except AssertionDefect:
  writeLine(stderr, "Failed to undock the player's ships from a base.")
try:
  assert dockShip(true).len == 0
except AssertionDefect:
  writeLine(stderr, "Failed to dock the player's ships to a base.")

echo "Testing countFuelNeeded."
try:
  assert countFuelNeeded() == -4
except AssertionDefect:
  writeLine(stderr, "Failed to count the amount of fuel needed for travel.")

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
echo "Testing changeShipSpeed."
try:
  assert changeShipSpeed(fullSpeed).len == 0
except AssertionDefect:
  writeLine(stderr, "Failed to change speed of the docked ship.")
try:
  assert dockShip(false).len == 0
except AssertionDefect:
  writeLine(stderr, "Failed to undock the ship.")
try:
  assert changeShipSpeed(fullStop).len == 0
except AssertionDefect:
  writeLine(stderr, "Failed to change speed of the ship.")
try:
  assert dockShip(true).len == 0
except AssertionDefect:
  writeLine(stderr, "Failed to dock the ship again to the base, second time.")
