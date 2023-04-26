discard """
  exitcode: 0
"""

import std/tables
import ../../src/[crafts, game, items, shipmodules, shipscargo, types]

if itemsList.len == 0:
  loadData("../bin/data/game.dat")
  loadItems("../bin/data/items.dat")
if recipesList.len == 0:
  loadRecipes("../bin/data/recipes.dat")
  loadModules("../bin/data/shipmodules.dat")

playerShip.modules = @[]
playerShip.modules.add(ModuleData(mType: ModuleType2.workshop, protoIndex: 6,
    durability: 100))
playerShip.modules.add(ModuleData(mType: ModuleType2.cargoRoom, protoIndex: 7,
    durability: 100))
updateCargo(playerShip, 6, 10)

setRecipe(0, 10, "1")
assert playerShip.modules[0].craftingAmount == 10
assert playerShip.modules[0].craftingIndex == "1"

assert checkRecipe("1") == 10

manufacturing(15)

