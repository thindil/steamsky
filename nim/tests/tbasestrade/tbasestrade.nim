discard """
  exitcode: 0
  output: '''Loading the game data.
Testing hireRecruit.
Testing buyRecipes.
Testing healCost.
Testing healWounded.
Testing trainCost.
Testing trainSkill.'''
"""

import std/tables
import ../../src/[bases, basescargo, basestrade, basestypes, careers, crafts,
    factions, game, items, maps, mobs, ships, shipmodules, types]

echo "Loading the game data."
if itemsList.len == 0:
  loadData("../bin/data/game.dat")
  loadItems("../bin/data/items.dat")
  loadCareers("../bin/data/careers.dat")
  loadFactions("../bin/data/factions.dat")
  loadModules("../bin/data/shipmodules.dat")
if recipesList.len == 0:
  loadRecipes("../bin/data/recipes.dat")
if protoMobsList.len == 0:
  loadMobs("../bin/data/mobs.dat")
if protoShipsList.len == 0:
  loadShips("../bin/data/ships.dat")
if basesTypesList.len == 0:
  loadBasesTypes("../bin/data/bases.dat")

playerShip.skyX = 1
playerShip.skyY = 1
playerShip.crew = @[]
playerShip.crew.add(MemberData(morale: [1: 50.Natural, 2: 0.Natural],
    homeBase: 1, faction: "POLEIS", orders: [0.Natural, 0, 0, 1, 1, 1, 2, 1, 1,
    1, 0, 0], order: talk, loyalty: 100, skills: @[SkillInfo(index: 4, level: 4,
    experience: 0)], attributes: @[MobAttributeRecord(level: 3, experience: 0),
    MobAttributeRecord(level: 3, experience: 0), MobAttributeRecord(level: 3,
    experience: 0), MobAttributeRecord(level: 3, experience: 0)], health: 100))
playerShip.modules = @[]
playerShip.modules.add(ModuleData(mType: ModuleType2.hull, protoIndex: 1,
    durability: 100, maxModules: 10))
playerShip.modules.add(ModuleData(mType: cargoRoom, protoIndex: 7,
    durability: 100, maxDurability: 100))
playerShip.modules.add(ModuleData(mType: turret, protoIndex: 8, durability: 100,
    maxDurability: 100, gunIndex: 3))
playerShip.modules.add(ModuleData(mType: gun, protoIndex: 9, durability: 100,
    maxDurability: 100, damage: 10, owner: @[1]))
playerShip.modules.add(ModuleData(mType: ModuleType2.cabin, protoIndex: 4,
    durability: 100, owner: @[0]))
playerShip.cargo = @[]
playerShip.cargo.add(InventoryData(protoIndex: 1, amount: 2000,
    durability: 100))
playerShip.cargo.add(InventoryData(protoIndex: 3, amount: 200, durability: 100))
playerShip.speed = docked
skyMap[1][1].baseIndex = 1
skyMap[1][1].eventIndex = -1
skyBases[1].population = 100
skyBases[1].baseType = "1"
skyBases[1].owner = "POLEIS"
skyBases[1].reputation = ReputationData(level: 1, experience: 1)
for i in 2 .. 100:
  skyBases[i].population = 100
generateCargo()
gameDate = DateRecord(year: 1600, month: 1, day: 1, hour: 8, minutes: 0)
playerCareer = "general"

echo "Testing hireRecruit."
generateCargo()
generateRecruits()
skyBases[1].recruits[0].price = 10
hireRecruit(0, 1, 0, 0, -1)
try:
  assert playerShip.crew.len == 2
except AssertionDefect:
  writeLine(stderr, "Failed to hire a recruit.")

echo "Testing buyRecipes."
let
  recipes = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
  recipesAmount = knownRecipes.len
for recipe in recipes:
  if recipe in basesTypesList["1"].recipes:
    buyRecipe(recipe)
    break
try:
  assert knownRecipes.len == recipesAmount + 1
except AssertionDefect:
  writeLine(stderr, "Failed to buy a crafting recipe.")

echo "Testing healCost."
var cost, time: Natural = 0
healCost(cost, time, 0)
try:
  assert cost == 1 and time == 1
except AssertionDefect:
  writeLine(stderr, "Failed to count heal cost of a not wounded crew member.")
playerShip.crew[0].health -= 10
healCost(cost, time, 0)
try:
  assert cost > 1 and time > 1
except AssertionDefect:
  writeLine(stderr, "Failed to count heal cost of a wounded crew member.")

echo "Testing healWounded."
playerShip.crew[0].health = 90
healWounded(0)
try:
  assert playerShip.crew[0].health == 100
except AssertionDefect:
  writeLine(stderr, "Failed to heal a wounded crew member.")
playerShip.crew[0].health = 90
healWounded(-1)
try:
  assert playerShip.crew[0].health == 100
except AssertionDefect:
  writeLine(stderr, "Failed to heal the whole crew.")

echo "Testing trainCost."
try:
  assert trainCost(0, 1) > 0
except AssertionDefect:
  writeLine(stderr, "Failed to count the train cost of the skill for a crew member.")

echo "Testing trainSkill."
trainSkill(0, 1, 1)
try:
  assert playerShip.crew[0].skills.len == 2
except AssertionDefect:
  writeLine(stderr, "Failed to train a new skill in the base.")
