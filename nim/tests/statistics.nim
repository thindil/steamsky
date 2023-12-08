import ../src/[careers, crafts, factions, game, goals, items, mobs,
    shipmodules, ships, statistics, types]
import unittest2

suite "Unit tests for statistics module":

  checkpoint "Loading the game data."
  loadGoals("../bin/data/goals.dat")
  loadData("../bin/data/game.dat")
  loadItems("../bin/data/items.dat")
  loadCareers("../bin/data/careers.dat")
  loadFactions("../bin/data/factions.dat")
  loadModules("../bin/data/shipmodules.dat")
  loadRecipes("../bin/data/recipes.dat")
  loadMobs("../bin/data/mobs.dat")
  loadShips("../bin/data/ships.dat")

  test "Testing updateCraftingOrders.":
    gameStats.craftingOrders = @[]
    updateCraftingOrders("1")
    check:
      gameStats.craftingOrders.len == 1

  test "Testing updateFinishedGoals.":
    gameStats.finishedGoals = @[]
    checkpoint "Update an existing goal"
    updateFinishedGoals("1")
    check:
      gameStats.finishedGoals.len == 1
    checkpoint "Update a non-existing goal"
    updateFinishedGoals("Sdfdsf")
    check:
      gameStats.finishedGoals.len == 1

  test "Testing getGamePoints.":
    gameStats.points = 0
    check:
      getGamePoints() == 0

  test "Testing updateFinishedMissions.":
    gameStats.finishedMissions = @[]
    updateFinishedMissions("DESTROY")
    check:
      gameStats.finishedMissions.len == 1

  test "Testing clearGameStats.":
    gameStats.points = 100
    clearGameStats()
    check:
      gameStats.points == 0

  test "Testing updateKilledMobs.":
    gameStats.killedMobs = @[]
    updateKilledMobs(MemberData(morale: [1: 50.Natural, 2: 0.Natural],
        homeBase: 1, faction: "POLEIS", orders: [0.Natural, 0, 0, 1, 1, 1, 2, 1, 1,
        1, 0, 0], order: upgrading, loyalty: 100, skills: @[SkillInfo(index: 4,
        level: 4,
        experience: 0)], attributes: @[MobAttributeRecord(level: 3, experience: 0),
        MobAttributeRecord(level: 3, experience: 0), MobAttributeRecord(level: 3,
        experience: 0), MobAttributeRecord(level: 3, experience: 0)]), "POLEIS")
    check:
      gameStats.killedMobs.len == 1

  test "Testing updateDestroyedShips.":
    gameStats.destroyedShips = @[]
    checkpoint "Update an existing ship stats"
    updateDestroyedShips("Tiny pirates ship")
    check:
      gameStats.destroyedShips.len == 1
    checkpoint "Update a non-existing ship stats"
    updateDestroyedShips("Sfdsfdsf")
    check:
      gameStats.destroyedShips.len == 1
