import unittest2
import ../src/[careers, items]
include ../src/reputation

suite "Unit tests for reputation module":

  checkpoint "Loading the game data."
  loadData("bin/data/game.dat")
  loadItems("bin/data/items.dat")
  loadCareers("bin/data/careers.dat")
  loadFactions("bin/data/factions.dat")
  checkpoint "Setting the tests."
  skyBases[1].owner = "POLEIS"

  test "Reset reputations.":
    resetReputations()
    check:
      getReputation(factionIndex = newGameSettings.playerFaction) == 1
