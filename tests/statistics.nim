import std/paths
import ../src/[careers, crafts, factions, game, goals, items, mobs,
    shipmodules, ships, statistics, types]
import unittest2

suite "Unit tests for statistics module":

  checkpoint "Loading the game data."
  loadGoals("bin/data/goals.dat".Path)
  loadData("bin/data/game.dat".Path)
  loadItems("bin/data/items.dat".Path)
  loadCareers("bin/data/careers.dat".Path)
  loadFactions("bin/data/factions.dat".Path)
  loadModules("bin/data/shipmodules.dat".Path)
  loadRecipes("bin/data/recipes.dat".Path)
  loadMobs("bin/data/mobs.dat".Path)
  loadShips("bin/data/ships.dat".Path)

  test "Updating an crafting orders goals' list.":
    gameStats.craftingOrders = @[]
    updateCraftingOrders("1")
    check:
      gameStats.craftingOrders.len == 1

  test "Update an existing goal":
    gameStats.finishedGoals = @[]
    updateFinishedGoals("1")
    check:
      gameStats.finishedGoals.len == 1

  test "Update a non-existing goal":
    updateFinishedGoals("Sdfdsf")
    check:
      gameStats.finishedGoals.len == 1

  test "Getting the player's points.":
    gameStats.points = 0
    check:
      getGamePoints() == 0

  test "Updating the list of finished missions.":
    gameStats.finishedMissions = @[]
    updateFinishedMissions("DESTROY")
    check:
      gameStats.finishedMissions.len == 1

  test "Clearing the game's statistics.":
    gameStats.points = 100
    clearGameStats()
    check:
      gameStats.points == 0

  test "Updating the list of killed mobs.":
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

  test "Update an existing ship stats":
    gameStats.destroyedShips = @[]
    updateDestroyedShips("Tiny pirates ship")
    check:
      gameStats.destroyedShips.len == 1

  test "Update a non-existing ship stats":
    updateDestroyedShips("Sfdsfdsf")
    check:
      gameStats.destroyedShips.len == 1
