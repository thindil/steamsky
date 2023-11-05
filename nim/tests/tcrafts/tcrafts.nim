discard """
  exitcode: 0
  output: '''Loading the game data.
Testing setRecipe.
Testing checkRecipe.
Testing manufacturing.'''
"""

import std/tables
import ../../src/[careers, crafts, factions, game, items, shipmodules, shipscargo, types]

echo "Loading the game data."
loadData("../bin/data/game.dat")
loadItems("../bin/data/items.dat")
loadCareers("../bin/data/careers.dat")
loadFactions("../bin/data/factions.dat")
if recipesList.len == 0:
  loadRecipes("../bin/data/recipes.dat")
  loadModules("../bin/data/shipmodules.dat")

playerShip.modules = @[]
playerShip.modules.add(ModuleData(mType: ModuleType2.workshop, protoIndex: 6,
    durability: 100))
playerShip.modules.add(ModuleData(mType: ModuleType2.cargoRoom, protoIndex: 7,
    durability: 100))
updateCargo(playerShip, 6, 10)

echo "Testing setRecipe."
setRecipe(0, 10, "1")
try:
  assert playerShip.modules[0].craftingAmount == 10
except AssertionDefect:
  writeLine(stderr, "Failed to set the amount for the crafting order.")
try:
  assert playerShip.modules[0].craftingIndex == "1"
except AssertionDefect:
  writeLine(stderr, "Failed to set the index for the crafting order.")

echo "Testing checkRecipe."
try:
  assert checkRecipe("1") == 10
except AssertionDefect:
  writeLine(stderr, "Failed to check the possible amount of crafted items for the recipe.")

echo "Testing manufacturing."
manufacturing(15)

