import ../src/[careers, crafts, factions, game, items, shipmodules, shipscargo, types]
import unittest2

suite "Unit tests for crafts module":

  checkpoint "Loading the game data."
  loadData("../bin/data/game.dat")
  loadItems("../bin/data/items.dat")
  loadCareers("../bin/data/careers.dat")
  loadFactions("../bin/data/factions.dat")
  loadRecipes("../bin/data/recipes.dat")
  loadModules("../bin/data/shipmodules.dat")

  playerShip.modules = @[]
  playerShip.modules.add(ModuleData(mType: ModuleType2.workshop, protoIndex: 6,
      durability: 100))
  playerShip.modules.add(ModuleData(mType: ModuleType2.cargoRoom, protoIndex: 7,
      durability: 100))
  updateCargo(playerShip, 6, 10)

  test "Testing setRecipe.":
    setRecipe(0, 10, "1")
    checkpoint "Set the amount for a crafting order."
    check:
      playerShip.modules[0].craftingAmount == 10
    checkpoint "Set the index for the crafting order"
    check:
      playerShip.modules[0].craftingIndex == "1"

  test "Testing checkRecipe.":
    check:
      checkRecipe("1") == 10

  test "Testing manufacturing.":
    manufacturing(15)

