import unittest2
include ../src/game2

suite "Unit tests for game2 module":

  checkpoint "Loading the game data."
  loadData("bin/data/game.dat")
  loadItems("bin/data/items.dat")
  loadCareers("bin/data/careers.dat")
  loadBasesTypes("bin/data/bases.dat")
  loadFactions("bin/data/factions.dat")
  loadModules("bin/data/shipmodules.dat")
  loadRecipes("bin/data/recipes.dat")
  loadMobs("bin/data/mobs.dat")
  loadShips("bin/data/ships.dat")
  loadGoals("bin/data/goals.dat")

  gameDate = DateRecord(year: 1600, month: 1, day: 1, hour: 8, minutes: 0)
  playerShip.modules = @[]
  playerShip.modules.add(ModuleData(mType: ModuleType2.engine, protoIndex: 3,
      durability: 100, fuelUsage: 4, power: 2000, disabled: false))
  playerShip.cargo = @[]
  playerShip.cargo.add(InventoryData(protoIndex: 1, amount: 100,
      durability: 100))
  skyBases[1].baseType = "1"
  playerShip.crew = @[]

  test "Updating the game.":
    updateGame(1)
    check:
      gameDate.minutes == 1

  test "Ending the game.":
    let oldSaveDir = saveDirectory
    saveDirectory = "tests/"
    addMessage("Test message", otherMessage)
    endGame(false)
    saveDirectory = oldSaveDir
    check:
      messagesAmount() == 0

  test "Starting a new game.":
    let oldSaveDir = saveDirectory
    saveDirectory = "tests/"
    newGame()
    saveDirectory = oldSaveDir
    check:
      playerShip.crew.len > 1
