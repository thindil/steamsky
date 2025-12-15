import std/paths
import unittest2
import ../src/[careers, items]
include ../src/reputation

suite "Unit tests for reputation module":

  checkpoint "Loading the game data."
  loadData("bin/data/game.dat".Path)
  loadItems("bin/data/items.dat".Path)
  loadCareers("bin/data/careers.dat".Path)
  loadFactions("bin/data/factions.dat".Path)
  checkpoint "Setting the tests."
  skyBases[1].owner = "POLEIS"

  test "Reset reputations.":
    resetReputations()
    check:
      getReputation(factionIndex = newGameSettings.playerFaction) == 1

  test "Gaining reputation with a faction.":
    updateReputation(baseIndex = 1, amount = 501)
    check:
      getReputation(factionIndex = newGameSettings.playerFaction) == 2

  test "Losing reputation with a faction.":
    updateReputation(baseIndex = 1, amount = -2)
    check:
      getReputation(factionIndex = newGameSettings.playerFaction) == 1

  test "Get reputation's level's text":
    check:
      getReputationText(reputationLevel = -70) == "Outlaw"
