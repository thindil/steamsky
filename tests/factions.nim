import unittest2
include ../src/factions

suite "Unit tests for factions module":

  checkpoint "Loading the game data."
  loadData("bin/data/game.dat".Path)
  loadItems("bin/data/items.dat".Path)
  loadCareers("bin/data/careers.dat".Path)
  loadFactions("bin/data/factions.dat".Path)

  test "Gaining reputation in the same faction.":
    check:
      getReputation("POLEIS", "POLEIS") == 0

  test "Gaining reputation in enemy faction.":
    check:
      getReputation("POLEIS", "PIRATES") == -10

  test "Checking if friendly factions are friendly":
    check:
      isFriendly("POLEIS", "INDEPENDENT")

  test "Checking if enemy factions are unfriendly":
    check:
      not isFriendly("POLEIS", "PIRATES")

  test "Get random faction index":
    let factionIndex = getRandomFaction()
    check:
      factionIndex.len > 0

  test "Get existing random faction index":
    let factionIndex = getRandomFaction()
    check:
      factionIndex in factionsList
