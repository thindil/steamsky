discard """
  exitcode: 0
"""

import std/tables
import ../../src/[crafts, game, items, ships, shipscargo, types]

if itemsList.len == 0:
  loadData("../bin/data/game.dat")
  loadItems("../bin/data/items.dat")
  loadRecipes("../bin/data/recipes.dat")

playerShip.modules = @[]
playerShip.modules.add(ModuleData(mType: ModuleType2.workshop, protoIndex: 6))
updateCargo(playerShip, 6, 10)

setRecipe(0, 10, "1")
assert playerShip.modules[0].craftingAmount == 10
assert playerShip.modules[0].craftingIndex == "1"

