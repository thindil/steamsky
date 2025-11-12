import ../src/[careers, factions, shipmodules]
import unittest2
include ../src/crafts

suite "Unit tests for crafts module":

  checkpoint "Loading the game data."
  loadData("bin/data/game.dat")
  loadItems("bin/data/items.dat")
  loadCareers("bin/data/careers.dat")
  loadFactions("bin/data/factions.dat")
  loadRecipes("bin/data/recipes.dat")
  loadModules("bin/data/shipmodules.dat")

  playerShip.modules = @[]
  playerShip.modules.add(ModuleData(mType: ModuleType2.workshop, protoIndex: 6,
      durability: 100))
  playerShip.modules.add(ModuleData(mType: ModuleType2.cargoRoom, protoIndex: 7,
      durability: 100))
  updateCargo(playerShip, 6, 10, quality = normal)
  updateCargo(playerShip, 50, 1, quality = normal)

  test "Set the amount for a crafting order.":
    setRecipe(0, 10, "1")
    check:
      playerShip.modules[0].craftingAmount == 10

  test "Set the index for the crafting order":
    check:
      playerShip.modules[0].craftingIndex == "1"

  test "Checking a recipe.":
    check:
      checkRecipe("1") == 10

  test "Manufacturing.":
    manufacturing(15)

  test "Setting recipe data":
    let recipe = setRecipeData("1")
    check:
      recipe.tool == "CookingSet"

  test "Getting a workshop's recipe's name":
    setRecipe(0, 10, "1")
    check:
      getWorkshopRecipeName(0) == "Manufacturing 10x Basic Ration"

  test "Cancelling crafting order":
    cancelCraftOrder(moduleIndex = 0)
    check:
      playerShip.modules[0].craftingAmount == 0

  test "Testing recipe difficulty name for value 5":
    gameSettings.showNumbers = false
    check:
      getRecipeDifficultyName(difficulty = 5) == "Trivial"

  test "Testing recipe difficulty name for value 12":
    gameSettings.showNumbers = false
    check:
      getRecipeDifficultyName(difficulty = 12) == "Easy"

  test "Testing recipe difficulty numeric name for value 20":
    gameSettings.showNumbers = true
    check:
      getRecipeDifficultyName(difficulty = 20) == "20"

  test "Testing no tools needed":
    check:
      checkTool(toolNeeded = "None")

  test "Testing for required tools":
    check:
      checkTool(toolNeeded = "CookingSet")
