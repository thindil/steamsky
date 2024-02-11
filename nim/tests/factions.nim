import unittest2
include ../src/factions

suite "Unit tests for factions module":

  checkpoint "Loading the game data."
  loadData("../bin/data/game.dat")
  loadItems("../bin/data/items.dat")
  loadCareers("../bin/data/careers.dat")
  loadFactions("../bin/data/factions.dat")

  test "Testing getReputation.":
    checkpoint "Gaining reputation in the same faction."
    check:
      getReputation("POLEIS", "POLEIS") == 0
    checkpoint "Gaining reputation in enemy faction."
    check:
      getReputation("POLEIS", "PIRATES") == -10

  test "Testing isFriendly.":
    checkpoint "Checking if friendly factions are friendly"
    check:
      isFriendly("POLEIS", "INDEPENDENT")
    checkpoint "Checking if enemy factions are unfriendly"
    check:
      not isFriendly("POLEIS", "PIRATES")

  test "Testing getRandomFaction.":
    let factionIndex = getRandomFaction()
    checkpoint "Get random faction index"
    check:
      factionIndex.len > 0
    checkpoint "Get existing random faction index"
    check:
      factionIndex in factionsList
