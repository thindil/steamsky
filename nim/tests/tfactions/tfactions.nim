discard """
  exitcode: 0
  output: '''Loading the game data.
Testing getReputation.
Testing isFriendly.
Testing getRandomFaction.'''
"""

import std/tables
import ../../src/[factions, careers, game, items]

echo "Loading the game data."
if itemsList.len == 0:
  loadData("../bin/data/game.dat")
  loadItems("../bin/data/items.dat")
  loadCareers("../bin/data/careers.dat")
  loadFactions("../bin/data/factions.dat")

echo "Testing getReputation."
assert getReputation("POLEIS", "POLEIS") == 0, "Failed to get reputation for the same faction."
assert getReputation("POLEIS", "PIRATES") == -10, "Failed to get reputation for enemy factions."

echo "Testing isFriendly."
assert isFriendly("POLEIS", "INDEPENDENT"), "Failed to check if friendly factions are friendly."
assert not isFriendly("POLEIS", "PIRATES"), "Failed to check if enemies factions are unfriendly."

echo "Testing getRandomFaction."
let factionIndex = getRandomFaction()
assert factionIndex.len > 0, "Failed to get random faction index."
assert factionIndex in factionsList, "Failed to get existing random faction index."
